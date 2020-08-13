
all_flag <- function(x, val_var, ...) {
  set(x, j = val_var, value = rep(TRUE, nrow(x)))
}

vent_flag <- function(x, val_var, ...) {
  x <- x[as.logical(get(val_var)), ]
  set(x, j = c(index_var(x), val_var),
      value = list(x[[val_var]], rep(TRUE, nrow(x))))
}

percent_as_numeric <- function(x, val_var, ...) {
  set(x, j = val_var, value = as.numeric(sub("%", "", x[[val_var]])))
}

force_numeric <- function(x) {
  res <- suppressWarnings(as.numeric(x))
  new_na <- sum(is.na(res) & !is.na(x))
  if (new_na > 0L) {
    progress_msg("  lost ", new_na, " (", prcnt(new_na, length(x)),
                 ") entries due to `force_numeric()`")
  }
  res
}

force_numeric_var <- function(x, col) {
  set(x, j = col, value = force_numeric(x[[col]]))
}

force_numeric_vars <- function(x, cols) {

  for (col in cols) {
    x <- force_numeric_var(x, col)
  }

  x
}

force_numeric_val_var <- function(x, val_var, ...) {
  force_numeric_var(x, val_var)
}

eicu_body_weight <- function(x, val_var, weight_var, env, ...) {

  do_calc <- function(rate, w1, w2) rate / fifelse(is.na(w1), w2, w1)

  idc <- id_vars(x)

  weight <- load_id("patient", env, cols = "admissionweight", id_var = idc)

  x <- merge(x, weight, all.x = TRUE, by = idc)
  x <- force_numeric_vars(x, c(val_var, weight_var))
  x <- x[, c(val_var) := do_calc(get(val_var), get(weight_var),
                                 get("admissionweight"))]

  rm_cols(x, "admissionweight")
}

combine_date_time <- function(x, date_var, date_shift = hours(12L), ...) {
  idx <- index_var(x)
  x[, c(idx) := fifelse(is.na(get(idx)),
                        get(date_var) + date_shift, get(idx))]
}

shift_all_date <- function(x, shift = hours(12L), ...) {
  idx <- index_var(x)
  x[, c(idx) := get(idx) + shift]
}

mimic_abx_shift_flag <- function(x, val_var, ...) {
  all_flag(shift_all_date(x, hours(12L)), val_var)
}

mimic_sampling <- function(x, val_var, aux_time, ...) {
  x <- combine_date_time(x, aux_time, hours(12L))
  set(x, j = val_var, value = !is.na(x[[val_var]]))
}

eicu_sampling <- function(x, val_var, ...) {
  x <- set(x, j = val_var, value = not_val(x[[val_var]], "no growth"))
  x
}

transform_fun <- function(fun) {
  assert_that(is.function(fun))
  function(x, val_var, ...) {
    set(x, j = val_var, value = fun(x[[val_var]]))
  }
}

multiply_by <- function(factor) {
  transform_fun(function(x) x * factor)
}

fahr_to_cels <- function(x) (x - 32) * 5 / 9

distribute_amount <- function(x, val_var, amount_var, end_var, ...) {

  unit <- time_unit(x)
  step <- time_step(x)
  inte <- as.double(hours(1L), units = unit)

  orig_cols <- colnames(x)

  idc <- id_vars(x)
  idx <- index_var(x)

  expand <- function(start, end, id, amount, rate) {
    seq <- seq(as.numeric(start), as.numeric(end), inte)
    tim <- as.difftime(round_to(seq, step), units = unit)
    res <- list(id, tim, amount / length(seq))
    names(res) <- c(idc, idx, val_var)
    res
  }

  x <- x[get(end_var) - get(idx) >= 0, ]
  x <- x[, expand(get(idx), get(end_var), get(idc), get(amount_var),
                  get(val_var)),
         by = seq_len(nrow(x))]
  x <- x[, c(setdiff(colnames(x), orig_cols)) := NULL]

  x
}

mimic_age <- function(x, val_var, ...) {
  x <- set(x, j = val_var,
    value = as.double(`units<-`(x[[val_var]], "days") / -365))
  x <- set(x, i = which(x[[val_var]] > 90), j = val_var, value = 90)
  x
}

eicu_age <- function(x, val_var, ...) {
  x <- set(x, which(x[[val_var]] == "> 89"), j = val_var, value = 90)
  x <- set(x, j = val_var, value = as.numeric(x[[val_var]]))
  x
}

hirid_vent_start <- function(x, val_var, ...) {
  all_flag(x[get(val_var) == 1, ], val_var)
}

hirid_vent_end <- function(x, val_var, ...) {
  all_flag(x[get(val_var) > 2, ], val_var)
}

hirid_trach <- function(x, val_var, ...) {
  all_flag(x[get(val_var) == 2, ], val_var)
}

mimic_death <- function(x, val_var, ...) {
  set(x, j = val_var, value = x[[val_var]] == 1L)
}

eicu_death <- function(x, val_var, ...) {
  set(x, j = val_var, value = x[[val_var]] == "Expired")
}

hirid_death <- function(x, val_var, item_var, ...) {

  threshold <- function(x, col, thresh) {
    set(x, j = col, value = x[[col]] <= thresh)
  }

  score <- function(x, id, val) x[, data.table::last(get(val)), by = c(id)]

  idc <- id_vars(x)

  tmp <- split(x, by = item_var, keep.by = FALSE)
  tmp <- lapply(tmp, threshold, val_var, 40)
  tmp <- lapply(tmp, score, idc, val_var)
  res <- rbind_lst(tmp)
  res <- res[, any(get("V1")), by = idc]

  rename_cols(res, val_var, "V1", by_ref = TRUE)
}

mf_sex <- function(x, val_var, ...) {
  x <- x[get(val_var) == "M", c(val_var) := "Male"]
  x <- x[get(val_var) == "F", c(val_var) := "Female"]
  x
}

crp_dl_to_l <- function(x, val_var, unit_var, ...) {
  x[grepl("mg/dl", get(unit_var), ignore.case = TRUE),
    c(val_var, unit_var) := list(get(val_var) * 10, "mg/L")]
}

eicu_total_co2 <- function(x, val_var, unit_var, ...) {
  x[get(unit_var) != "mm(hg)", ]
}

eicu_calcium <- function(x, val_var, unit_var, ...) {
  x[grepl("mmol/l", get(unit_var), ignore.case = TRUE),
    c(val_var, unit_var) := list(get(val_var) * 4, "mg/dL")]
}

eicu_fio2 <- function(x, val_var, unit_var, ...) {
  x[get(unit_var) != "lpm", ]
}

eicu_magnesium <- function(x, val_var, unit_var, ...) {
  x[grepl("meq/l", get(unit_var), ignore.case = TRUE),
    c(val_var, unit_var) := list(get(val_var) / 1.215, "mEq/L")]
}

mimic_adx <- function(x, val_var, ...) {

  map <- c(MED   = "med",   SURG  = "surg", CMED = "med",  CSURG  = "surg",
           VSURG = "surg",  NSURG = "surg", NB   = "other", NMED  = "med",
           ORTHO = "surg",  TRAUM = "surg", OMED = "med",   GU    = "other",
           NBB   = "other", TSURG = "surg", GYN  = "other", PSURG = "surg",
           OBS   = "other", ENT   = "surg", DENT = "surg",  PSYCH = "other")

  set(x, j = val_var, value = map[x[[val_var]]])
}

eicu_adx <- function(x, val_var, ...) {

  path <- strsplit(x[[val_var]], split = "|", fixed = TRUE)
  keep <- chr_ply(path, `[[`, 2L) == "All Diagnosis"

  x <- x[keep, ]
  path <- path[keep]

  cats <- fifelse(
    chr_ply(path, `[[`, 5L) %chin% c("Genitourinary", "Transplant"),
    "other",
    fifelse(chr_ply(path, `[[`, 3L) == "Operative", "surg", "med")
  )

  set(x, j = val_var, value = cats)
}

hirid_vaso <- function(x, val_var, unit_var, env, ...) {

  ids <- id_vars(x)

  x <- x[get(val_var) > 0, ]
  x <- x[get(unit_var) == "mg",
    c(val_var, unit_var) := list(1000 * get(val_var), "\u00b5g")
  ]

  old_row <- nrow(x)
  x <- x[get(unit_var) == "\u00b5g", ]
  dif_row <- old_row - nrow(x)

  if (dif_row > 0) {
    progress_msg("    lost ", dif_row, " (", prcnt(dif_row, old_row),
                 ") entries due to unexpected units")
  }

  sex <- load_id("general", env, cols = "sex", id_var = ids)
  sex <- sex[,
    c("weight", "sex") := list(fifelse(get("sex") == "M", 85, 65), NULL)
  ]

  x <- merge(dt_gforce(x, "sum", vars = val_var), sex, by = ids)

  frac <- 1 / as.double(interval(x), units = "mins")

  x[, c(val_var, unit_var, "weight") := list(
    frac * get(val_var) / get("weight"), "mcg/kg/min", NULL)
  ]
}

hirid_insulin <- function(x, ...) dt_gforce(x, "sum")

hirid_urine <- function(x, val_var, unit_var, ...) {

  do_diff <- function(x) {
    res <- c(x[1L], diff(x))
    res[res < 0] <- 0
    res
  }

  idx <- id_vars(x)

  x[, c(val_var, unit_var) := list(do_diff(get(val_var)), "mL"), by = idx]
}

los_callback <- function(x, patient_ids, id_type, interval) {

  as_day <- function(x) as.double(x, units = "days")

  win <- x[["win_type"]]
  cfg <- as_id_cfg(x)

  if (identical(win, id_type)) {

    res <- id_map(x, id_vars(cfg[id_type]), id_vars(cfg[win]), NULL, "end")

    res <- res[, c("val_var", "end") := list(as_day(get("end")), NULL)]

  } else {

    res <- id_map(x, id_vars(cfg[id_type]), id_vars(cfg[win]), "start", "end")

    res <- res[, c("val_var", "start", "end") := list(
      as_day(get("end") - get("start")), NULL, NULL
    )]

    res <- rm_cols(res, id_vars(cfg[win]), by_ref = TRUE)

    if (cfg[win] > cfg[id_type]) {
      res <- unique(res)
    }
  }

  merge_patid(res, patient_ids)
}
