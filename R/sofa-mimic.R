
mimic_pao2 <- function(time_scale = "hours", step_size = 1L,
                       data_env = "mimic") {

  message("fetching pao2")

  mimic_get_data_items(50821L, "d_labitems", data_env,
    value_names = "pao2",
    time_scale = time_scale,
    step_size = step_size,
    unit_handler = fix_case_allow_na,
    expected_unit = "mm Hg",
    agg_fun = min
  )
}

mimic_fio2 <- function(add_chart_data = TRUE, time_scale = "hours",
                       step_size = 1L, data_env = "mimic") {

  message("fetching fio2")

  lab <- mimic_get_data_items(50816L, "d_labitems", data_env,
    value_names = if (add_chart_data) "fi_lab" else "fio2",
    time_scale = time_scale,
    step_size = step_size,
    unit_handler = fix_percent,
    expected_unit = "%",
    agg_fun = max
  )

  if (add_chart_data) {

    chart <- mimic_get_data_items(c(3420L, 223835L), "d_items", data_env,
      value_names = "fi_chart",
      time_scale = time_scale,
      step_size = step_size,
      split_items = FALSE,
      unit_handler = fix_percent,
      expected_unit = "%",
      agg_fun = max
    )

    res <- merge(lab, chart, all = TRUE)
    res <- res[, fio2 := ifelse(is.na(fi_lab), fi_chart, fi_lab)]
    res <- res[, c("fi_lab", "fi_chart") := NULL]

  } else {
    res <- lab
  }

  res <- res[fio2 <= 1, fio2 := fio2 * 100]
  res <- res[fio2 > 0, ]

  res
}

mimic_pafi <- function(pao2 = mimic_pao2(...), fio2 = mimic_fio2(...),
                       win_length = hours(2L),
                       mode = c("match_vals", "extreme_vals", "fill_gaps"),
                       ...) {

  assert_that(is_ts_tbl(pao2), is_ts_tbl(fio2), same_by_cols(pao2, fio2),
              has_cols(pao2, "pao2"), has_cols(fio2, "fio2"),
              is_time(win_length, allow_neg = FALSE))

  mode <- match.arg(mode)

  if (identical(mode, "match_vals")) {

    res <- rbind(
      fio2[pao2, on = by_cols(fio2), roll = win_length],
      pao2[fio2, on = by_cols(fio2), roll = win_length]
    )
    res <- unique(res)

  } else {

    res <- merge(pao2, fio2, all = TRUE)

    if (identical(mode, "fill_gaps")) {
      res <- fill_gaps(res)
    }

    win_expr <- substitute(
      list(min_pa = min_fun(pao2), max_fi = max_fun(fio2)),
      list(min_fun = min_or_na, max_fun = max_or_na)
    )
    res <- slide_quo(res, win_expr, before = win_length, full_window = FALSE)

    setnames(res, c("min_pa", "max_fi"), c("pao2", "fio2"))
  }

  res <- res[, pafi := 100 * pao2 / fio2]
  res <- res[, c("pao2", "fio2") := NULL]

  res
}

mimic_vent_start <- function(time_scale = "mins", step_size = 1L,
                             data_env = "mimic") {

  message("fetching mechanical ventilation start info")

  cv_ids <- c(     1L,     60L,    218L,    221L,    223L,    436L,    437L,
                 444L,    445L,    448L,    449L,    450L,    459L,    501L,
                 502L,    503L,    505L,    506L,    535L,    543L,    639L,
                 654L,    667L,    668L,    669L,    670L,    671L,    672L,
                 681L,    682L,    683L,    684L,    686L,   1211L,   1340L,
                1486L,   1600L,   1655L,   2000L,   3459L,   5865L,   5866L)
  mv_ids <- c(220339L, 223848L, 223849L, 224419L, 224684L, 224685L, 224686L,
              224687L, 224695L, 224696L, 224697L, 224700L, 224701L, 224702L,
              224703L, 224704L, 224705L, 224706L, 224707L, 224709L, 224738L,
              224746L, 224747L, 224750L, 226873L, 227187L)

  res <- mimic_get_data_items(c(cv_ids, mv_ids), "d_items", data_env,
    unit_cols = NULL,
    value_names = "vent_start",
    split_items = FALSE,
    time_scale = time_scale,
    step_size = step_size,
    agg_fun = first_elem
  )

  set(res, j = "vent_start", value = NULL)

  res
}

mimic_vent_stop <- function(time_scale = "mins", step_size = 1L,
                            data_env = "mimic") {

  get_di <- function(x) {
    mimic_get_data_items(x, "d_items", data_env,
      unit_cols = NULL,
      value_names = "vent_end",
      split_items = FALSE,
      time_scale = time_scale,
      step_size = step_size,
      agg_fun = first_elem
    )
  }

  message("fetching mechanical ventilation vent_stop info")

  res <- rbind(get_di(c(227194L, 225468L, 225477L)),
               get_di(c(467L, 469L, 226732L)))
  set(res, j = "vent_end", value = NULL)

  unique(res)
}

mimic_vent <- function(vent_start = mimic_vent_start(data_env = data_env),
                       vent_stop = mimic_vent_stop(data_env = data_env),
                       win_length = hours(6L), min_length = mins(10L),
                       time_scale = "hours", step_size = 1L,
                       data_env = "mimic") {

  final_units <- function(x) {
    units(x) <- time_scale
    round_to(x, step_size)
  }

  assert_that(same_by_cols(vent_start, vent_stop),
              same_time_cols(vent_start, vent_stop),
              is_time(win_length, allow_neg = FALSE),
              is_time(min_length, allow_neg = FALSE),
              min_length < win_length, step_time(vent_start) < min_length)

  units(win_length) <- time_unit(vent_start)
  units(min_length) <- time_unit(vent_start)

  vent_start[, start_time := hadm_time]
  vent_stop[ , stop_time  := hadm_time]

  on.exit({
    set(vent_start, j = "start_time", value = NULL)
    set(vent_stop,  j = "stop_time",  value = NULL)
  })

  merged <- vent_stop[vent_start, roll = -win_length, on = by_cols(vent_start)]

  merged <- merged[is.na(stop_time), stop_time := start_time + win_length]
  merged <- merged[stop_time - start_time >= min_length, ]

  merged <- merged[, c("start_time", "stop_time") := list(
    final_units(start_time), final_units(stop_time)
  )]

  res <- unique(
    expand_limits(merged, min_col = "start_time", max_col = "stop_time",
                  step_size = step_size, id_cols = "hadm_id")
  )
  res <- res[, vent := TRUE]

  res
}

mimic_coag <- function(time_scale = "hours", step_size = 1L,
                       data_env = "mimic") {

  message("fetching platelet counts")

  mimic_get_data_items(51265L, "d_labitems", data_env,
    value_names = "coag",
    time_scale = time_scale,
    step_size = step_size,
    unit_handler = allow_na,
    expected_unit = "K/uL",
    agg_fun = min
  )
}

mimic_bili <- function(time_scale = "hours", step_size = 1L,
                       data_env = "mimic") {

  message("fetching bilirubin measurements")

  mimic_get_data_items(50885L, "d_labitems", data_env,
    value_names = "bili",
    time_scale = time_scale,
    step_size = step_size,
    unit_handler = allow_na,
    expected_unit = "mg/dL",
    agg_fun = max
  )
}

mimic_map <- function(time_scale = "hours", step_size = 1L,
                      data_env = "mimic") {

  message("fetching map measurements")

  items <- c(52L, 443L, 456L, 6702L, 220052L, 220181L, 225312L)

  mimic_get_data_items(items, "d_items", data_env,
    value_names = "map",
    time_scale = time_scale,
    step_size = step_size,
    split_items = FALSE,
    expected_unit = "mmHg",
    agg_fun = min
  )
}

mimic_vaso <- function(time_scale = "hours", step_size = 1L,
                       data_env = "mimic") {

  fix_rate <- function(x, unit_col, val_col) {

    drop <- x[[unit_col]] %in% "mcgmin"

    if (any(drop)) {
      message("Removing ", sum(drop), " absolute (w.r.t. body weight) rates.")
      x <- x[!drop, ]
    }

    x[[unit_col]][x[[unit_col]] %in% "mcgkgmin"] <- "mcg/kg/min"

    x
  }

  get_di <- function(itms, name, split = FALSE) {
    mimic_get_data_items(itms, "d_items", data_env,
      value_cols = "rate",
      unit_cols = "rateuom",
      value_names = name,
      time_scale = time_scale,
      step_size = step_size,
      split_items = split,
      unit_handler = fix_rate,
      expected_unit = "mcg/kg/min",
      agg_fun = max
    )
  }

  message("fetching vasopressor dosages")

  cv_ids <- list(norepi = c(30047L, 30120L),
                 epi    = c(30044L, 30119L, 30309L),
                 dopa   = c(30043L, 30125L, 30307L),
                 dobu   = c(30042L, 30306L))

  mv_ids <- list(norepi = 221906L,
                 epi    = 221289L,
                 dopa   = 221662L,
                 dobu   = 221653L)

  cv_dat <- reduce(merge, Map(get_di, cv_ids, names(cv_ids)), all = TRUE)
  mv_dat <- reduce(merge, get_di(unlist(mv_ids), names(mv_ids), split = TRUE),
                   all = TRUE)

  make_unique(rbind(cv_dat, mv_dat), fun = max)
}

mimic_gcs <- function(win_length = hours(6L), set_na_max = TRUE,
                      time_scale = "hours", step_size = 1L,
                      data_env = "mimic") {

  fix_gcs <- function(x, ...) {

    err <- is_val(x[["error"]], 1L)
    if (any(err)) {
      message("Removing ", sum(err), " rows due to error == 1L")
      x <- x[!err, ]
    }

    assert_that(!any(x[["valuenum"]] == 0))

    sed <- (x[["itemid"]] ==    723L & x[["value"]] == "1.0 ET/Trach") |
           (x[["itemid"]] == 223900L & x[["value"]] == "No Response-ETT")

    if (any(sed)) {
      x <- x[sed, valuenum := 0]
    }

    assert_that(!any(is.na(x[["valuenum"]])))

    x
  }

  get_di <- function(itms, name) {
    mimic_get_data_items(itms, "d_items", data_env,
      extra_cols = c("error", "value"),
      value_names = name,
      time_scale = time_scale,
      step_size = step_size,
      split_items = FALSE,
      unit_handler = fix_gcs,
      expected_unit = NA_character_,
      agg_fun = min
    )
  }

  repl_na <- function(x, val) {

    repl <- is.na(x)
    if (any(repl)) {
      message("Replacing ", sum(repl), " values with ", val, ".")
      x[repl] <- val
    }

    x
  }

  impute_na_prev <- function(x) {

    res <- x[length(x)]

    if (is.na(res)) {
      not_na <- !is.na(x)
      if (any(not_na)) return(tail(x[not_na], n = 1L))
    }

    res
  }

  message("fetching gcs scores")

  itms <- list(eye    = c(184L, 220739L),
               verbal = c(723L, 223900L),
               motor  = c(454L, 223901L))
  evm <- names(itms)

  res <- Map(get_di, itms, names(itms))
  res <- reduce(merge, res, all = TRUE)

  zeros <- reduce(`|`, res[, lapply(.SD, is_val, 0), .SDcols = evm])

  if (any(zeros)) {
    message("Setting ", sum(zeros), " rows to max gcs values due to sedation.")
    res <- res[zeros, c(evm) := list(4, 5, 6)]
  }

  expr <- substitute(list(eye_imp = fun(eye), verb_imp = fun(verbal),
                          mot_imp = fun(motor)),
                     list(fun = impute_na_prev))
  res <- slide_quo(res, expr, before = win_length)

  if (set_na_max) {
    res <- res[, c(evm) := list(
      repl_na(eye_imp, 4), repl_na(verb_imp, 5), repl_na(mot_imp, 6)
    )]
  }

  res <- res[, gcs := eye_imp + verb_imp + mot_imp]

  res <- set(res, j = c(evm, "eye_imp", "verb_imp", "mot_imp"),
             value = NULL)

  res
}

mimic_crea <- function(time_scale = "hours", step_size = 1L,
                       data_env = "mimic") {

  message("fetching creatinine measurements")

  mimic_get_data_items(50912L, "d_labitems", data_env,
    value_names = "crea",
    time_scale = time_scale,
    step_size = step_size,
    unit_handler = allow_na,
    expected_unit = "mg/dL",
    agg_fun = max
  )
}

mimic_urine24 <- function(min_win = hours(12L), time_scale = "hours",
                          step_size = 1L, data_env = "mimic") {

  handle_ml <- function(x, unit_col, val_col) {
    x <- x[x[[val_col]] > 0, ]
    x[[unit_col]][is_val(x[[unit_col]], "ml")] <- "mL"
    x
  }

  convert_dt <- function(x) as.numeric(x, time_scale)

  urine_sum <- local({

    min_steps <- ceiling(convert_dt(min_win) / step_size)
    step_factor <- convert_dt(hours(24L)) / step_size

    function(x) {
      if (length(x) <= min_steps) return(NA_real_)
      else sum(x, na.rm = TRUE) * step_factor / length(x)
    }
  })

  message("fetching urine measurements")

  assert_that(is_time(min_win, allow_neg = FALSE), min_win <= hours(24L))

  itms <- c( 40055L,  40056L,  40057L,  40065L,  40069L,  40085L,  40086L,
             40094L,  40096L,  40405L,  40428L,  40473L,  40715L,  43175L,
            226557L, 226558L, 226559L, 226560L, 226561L, 226563L, 226564L,
            226565L, 226566L, 226567L, 226584L, 227510L)

  res <- mimic_get_data_items(itms, "d_items", data_env,
    id_cols = c("hadm_id", "icustay_id"),
    value_names = "urine",
    time_scale = time_scale,
    step_size = step_size,
    split_items = FALSE,
    unit_handler = handle_ml,
    expected_unit = "mL",
    agg_fun = sum,
    id_names = c("hadm_id", "icustay_id")
  )

  icu_limits <- mimic_get_icu_stays("min", "max", NULL, time_scale, step_size,
                                    data_env)

  res <- fill_gaps(res, limits = icu_limits)

  expr <- substitute(list(urine_24 = win_agg_fun(urine)),
                     list(win_agg_fun = urine_sum))

  res <- slide_quo(res, expr, hours(24L))

  res <- rm_ts_cols(res, "icustay_id")

  res
}

mimic_sofa_vars <- function(pafi  = mimic_pafi(..., data_env = data_env),
                            vent  = mimic_vent(..., data_env = data_env),
                            coag  = mimic_coag(..., data_env = data_env),
                            bili  = mimic_bili(..., data_env = data_env),
                            map   = mimic_map(..., data_env = data_env),
                            vaso  = mimic_vaso(..., data_env = data_env),
                            gcs   = mimic_gcs(..., data_env = data_env),
                            crea  = mimic_crea(..., data_env = data_env),
                            urine = mimic_urine24(..., data_env = data_env),
                            ..., data_env = "mimic") {

  tables <- list(pafi, vent, coag, bili, map, vaso, gcs, crea, urine)

  assert_that(
    all(vapply(tables, same_by_cols,   logical(1L), tables[[1L]])),
    all(vapply(tables, same_time_cols, logical(1L), tables[[1L]]))
  )

  dat <- reduce(merge, tables, all = TRUE)

  time_scale <- time_unit(dat)
  time_col <- ts_index(dat)

  hadm <- mimic_get_icu_stays(NULL, NULL, "disch", time_scale, ts_step(dat),
                              data_env)
  hadm <- hadm[, list(disch = max(disch)), by = "hadm_id"]

  limits <- dat[, list(min = min(get(time_col)), max = max(get(time_col))),
                by = c(ts_key(dat))]
  limits <- merge(limits, hadm, by.x = ts_key(dat), by.y = "hadm_id",
                  all.x = TRUE)
  limits <- limits[, list(min = min(as.difftime(0, units = time_scale), min),
                          max = max(max, disch, na.rm = TRUE)), by = "hadm_id"]

  fill_gaps(dat, limits = limits)
}

mimic_sofa <- function(...) {

  tbl <- mimic_sofa_vars(...)
  tbl <- sofa_window(tbl)
  tbl <- sofa_compute(tbl)

  tbl
}
