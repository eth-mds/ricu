# ==============================================================================
#
# Callback for circulatory failure
# Implementation by `prockenschaub`
# from: https://github.com/prockenschaub/icuDG-preprocessing/blob/main/R/callback-circ-fail.R
#
# ==============================================================================

circ_fail <- function (..., lact_thresh = 2, map_thresh = 65, 
                       fill_for = hours(3L), cond_win = mins(45L),
                       cond_dur = mins(30L), keep_components = FALSE, 
                       interval = NULL, by_ref = FALSE) {
  cnc <- c("map", "cf_treat", "lact")
  res <- ricu:::collect_dots(cnc, interval, ...)
  assert_that(lact_thresh >= 0, map_thresh >= 0,
              ricu:::is_interval(fill_for), ricu:::is_interval(cond_win), 
              ricu:::is_interval(cond_dur), is.flag(keep_components),
              units(cond_win) == units(cond_dur), cond_dur < cond_win)
  
  map <- res[["map"]]
  cf_treat <- res[["cf_treat"]]
  lact <- res[["lact"]]
  
  if (!by_ref) {
    map <- copy(map)
    cf_treat <- copy(cf_treat)
    lact <- copy(lact)
  }
  
  id <- id_vars(map)
  step_size <- interval(map)
  
  assert_that(units(step_size) == units(cond_win))
  
  p <- as.numeric(cond_dur) / as.numeric(cond_win)
  steps <- as.integer(cond_win / as.numeric(step_size))
  
  # Interpolate lactate values
  map_times <- map[, .SD, .SDcols = meta_vars(map)]
  map_limits <- ricu::collapse(map_times, as_win_tbl = FALSE)
  grid_times <- fill_gaps(map_times, map_limits)
  lact <- interpolate_lactate(lact, grid_times, lact_thresh, fill_for)
  
  # Combine MAP, vasopress/inotrope meds, and lactate to define cf
  res <- merge_lst(list(map, cf_treat, lact))
  
  .rmean <- function(x) frollmean(x, steps, align = "center")
  .thresh <- function(x, op, val) fifelse(!is.na(x), op(x, val), FALSE)
  
  res[, miss      := pmax(.rmean(is.na(map)), .rmean(is.na(lact))) == 1, by = c(id)]
  res[, low_map   := .rmean(.thresh(map, `<`, map_thresh)), by = c(id)]
  res[, treated   := .rmean(!is.na(cf_treat)), by = c(id)]
  res[, high_lact := .rmean(.thresh(lact, `>`, lact_thresh)), by = c(id)]
  res[, circ_fail := fcase(
    miss,  NA,
    (low_map <= p | treated <= p) & high_lact <= p, FALSE,
    (low_map >  p | treated >  p) & high_lact >  p, TRUE,
    default = NA
  )]
  
  cols_rm <- c("miss", "low_map", "treated", "high_lact")
  if (!keep_components) {
    cols_rm <- c(cols_rm, "map", "cf_treat", "lact")
  }
  res <- rm_cols(res, cols_rm, skip_absent = TRUE, by_ref = TRUE)
  
  res
}

approx <- function(x, y = NULL, xout, ...) {
  if (length(x) == 1) {
    return(list(x = x, y = y))
  } 
  stats::approx(x, y, xout, ...)
}

# TODO: bring in line with ricu::replace_na 
replace_na <- function(x, val, type = "const", max_n = Inf, ...) {
  seq_num <- rleid(is.na(x))
  imp_cnt <- sapply(split(x, seq_num), function(x) seq_along(x))
  
  if (type == "nocb") {
    imp_cnt <- Map(rev, imp_cnt)
  }
  
  imp_cnt <- Reduce(c, imp_cnt)
  
  if (identical(type, "const")) {
    repl <- data.table::nafill(x, type, val, ...)
  }
  else {
    repl <- data.table::nafill(x, type, ...)
  }
  
  fifelse(imp_cnt <= max_n, repl, x)
}

interpolate_lactate <- function(df, grid_times, thresh, fill_win = hours(3L)) {
  id <- id_vars(df)
  ind <- index_var(df)
  val <- data_var(df)
  
  step_size <- interval(df)
  fill_win <- ricu:::re_time(fill_win, step_size)
  
  df[, abn := .SD[[val]] >= thresh]  # TODO: generalise to  allow for < thresh
  df[, tdiff := c(diff(.SD[[ind]]), NA_real_), by = c(id)]
  
  # Linearly interpolate everything
  rep_to_interpol_grid <- function(x, d) {
    rep(x, times = fifelse(is.na(d), 1., as.numeric(d) / as.numeric(step_size)))
  }
  
  int_lin_cond <- expr(abn == shift(abn, type = "lead") | tdiff < 2 * fill_win)
  df[, int_lin := eval(int_lin_cond), by = c(id)] 
  
  df <- df[, c(
    # Expand all existing columns to match length of interpolation
    lapply(.SD, rep_to_interpol_grid, d = tdiff),
    # Linearly interpolate every observation (for speed, ffill/bfill later)
    approx(.SD[[ind]], .SD[[val]], xout = seq(as.numeric(.SD[[ind]][1]), as.numeric(.SD[[ind]][.N]), by = as.numeric(step_size)))
  ), 
  by = c(id)
  ]
  
  df[x == get(ind), int_lin := TRUE]
  df[int_lin == FALSE , y := NA_real_]
  
  # Replace linear with forward/backward fill if consecutive values crossed 
  # thresh and are more than `fill_win` apart
  max_n <- as.numeric(fill_win) / as.numeric(step_size)
  df[, y := fifelse(!int_lin, replace_na(y, type = "locf", max_n = max_n), y), by = c(id)]
  df[, y := fifelse(!int_lin, replace_na(y, type = "nocb", max_n = max_n), y), by = c(id)]
  
  # Clean up table
  df[, c(ind) := as.difftime(x, units = attr(step_size, "units"))]
  df[, c(val) := y]
  df <- df[, .SD, .SDcols = c(id, ind, val, "abn")]
  
  # Forward/backward fill the first and last observation over grid
  df <- merge(df, grid_times, all = TRUE)
  df[, abn_int := replace_na(as.integer(abn), type = "nocb"), by = c(id)]
  df[, c(val) := fcase(
    !is.na(abn), .SD[[val]],                                            # If not first, do nothing
    abn_int == 0, replace_na(.SD[[val]], type = "nocb"),                # If first val normal, infinite bfill
    abn_int == 1, replace_na(.SD[[val]], type = "nocb", max_n = max_n), # If first val abnormal, bfill `max_n` steps
    rep(TRUE, .N), .SD[[val]]
  ), 
  by = c(id)
  ]
  df[, abn_int := replace_na(as.integer(abn), type = "locf"), by = c(id)]
  df[, c(val) := fcase(
    !is.na(abn), .SD[[val]],                                            # If not last, do nothing
    abn_int == 0, replace_na(.SD[[val]], type = "locf"),                # If last val normal, infinite ffill
    abn_int == 1, replace_na(.SD[[val]], type = "locf", max_n = max_n), # If last val abnormal, ffill `max_n` steps
    rep(TRUE, .N), .SD[[val]]
  ), 
  by = c(id)
  ]
  
  df[, .SD, .SDcols = c(id, ind, val)]
}

cf_treat <- function(..., interval = NULL) {
  
  cnc <- c("epi_dur", "norepi_dur", "dopa_dur", "dobu_dur", "adh_dur", "phn_dur", 
           "levo_dur", "milrin_dur", "teophyllin_dur")
  res <- ricu:::collect_dots(cnc, interval, ..., merge_dat = TRUE)
  unt <- ricu::time_unit(res)
  
  res <- res[, c(cnc) := lapply(.SD, as.difftime, units = unt), .SDcols = cnc]
  res <- res[, c("cf_treat", cnc) := list(pmax(
    get("dopa_dur"), get("norepi_dur"), get("dobu_dur"), get("epi_dur"),
    get("adh_dur"), get("phn_dur"), get("levo_dur"), get("milrin_dur"), get("teophyllin_dur"),
    na.rm = TRUE), NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL)
  ]
  
  res <- expand(res, index_var(res), "cf_treat")
  res <- unique(res)
  res <- res[, c("cf_treat") := TRUE]
  
  res
}
