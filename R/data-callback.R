
all_flag <- function(x, val_col, ...) {
  set(x, j = val_col, value = rep(TRUE, nrow(x)))
}

vent_flag <- function(x, val_col, time_col, ...) {
  x <- x[as.logical(get(val_col)), ]
  set(x, j = c(time_col, val_col),
      value = list(x[[val_col]], rep(TRUE, nrow(x))))
}

percent_as_numeric <- function(x, val_col, ...) {
  set(x, j = val_col, value = as.numeric(sub("%", "", x[[val_col]])))
}

force_numeric_col <- function(x, col) {
  set(x, j = col, value = force_numeric(x[[col]]))
}

force_numeric_cols <- function(x, cols) {
  for (col in cols) force_numeric_col(x, col)
  x
}

force_numeric_val_col <- function(x, val_col, ...) {
  force_numeric_col(x, val_col)
}

eicu_body_weight <- function(x, id_col, val_col, weight_col, source, ...) {

  do_calc <- function(rate, w1, w2) rate / fifelse(is.na(w1), w2, w1)

  weight <- data_id_quo(source, "patient", cols = "admissionweight",
                        id_col = id_col)

  x <- merge(x, weight, all.x = TRUE, by = id_col)
  x <- force_numeric_cols(x, c(val_col, weight_col))
  x <- x[, c(val_col) := do_calc(get(val_col), get(weight_col),
                                 get("admissionweight"))]
  x <- rm_cols(x, "admissionweight")
  x
}

combine_date_time <- function(x, time_col, date_col, date_shift = hours(12L),
                              ...) {
  x <- x[, c(time_col) := fifelse(is.na(get(time_col)),
                                  get(date_col) + date_shift, get(time_col))]
  x
}

shift_all_date <- function(x, time_col, shift = hours(12L), ...) {
  x <- x[, c(time_col) := get(time_col) + shift]
  x
}

mimic_abx_shift_flag <- function(x, val_col, time_col, ...) {
  x <- shift_all_date(x, time_col, hours(12L))
  x <- all_flag(x, val_col)
  x
}

mimic_sampling <- function(x, val_col, time_col, aux_time, ...) {
  x <- combine_date_time(x, aux_time, time_col, hours(12L))
  x <- set(x, j = val_col, value = !is.na(x[[val_col]]))
  x
}

eicu_sampling <- function(x, val_col, ...) {
  set(x, j = val_col, value = not_val(x[[val_col]], "no growth"))
}

multiply_by <- function(factor) {
  factor <- force(factor)
  function(x, val_col, ...) {
    x[, c(val_col) := get(val_col) * factor]
  }
}

multiply_hirid_albu <- multiply_by(0.1)
multiply_hirid_crea <- multiply_by(0.011312)
multiply_hirid_calc <- multiply_by(4.008)
multiply_hirid_fibr <- multiply_by(100)
multiply_hirid_hemo <- multiply_by(0.)
multiply_hirid_magn <- multiply_by(2.431)
multiply_hirid_gluc <- multiply_by(18.016)
multiply_hirid_phos <- multiply_by(9.497)
multiply_hirid_urea <- multiply_by(2.8)
multiply_hirid_bili <- multiply_by(0.058467)

fahrenheit_to_celsius <- function(x, val_col, ...) {
  x[, c(val_col) := (get(val_col) - 32) * 5 / 9]
}

distribute_amount <- function(x, val_col, id_col, time_col, amount_col,
                              end_col, ...) {

  unit <- time_unit(x)
  step <- time_step(x)
  inte <- as.double(hours(1L), units = unit)

  orig_cols <- colnames(x)

  expand <- function(start, end, id, amount, rate) {
    seq <- seq(as.numeric(start), as.numeric(end), inte)
    tim <- as.difftime(round_to(seq, step), units = unit)
    res <- list(id, tim, amount / length(seq))
    names(res) <- c(id_col, time_col, val_col)
    res
  }

  x <- x[get(end_col) - get(time_col) >= 0, ]
  x <- x[, expand(get(time_col), get(end_col), get(id_col), get(amount_col),
                  get(val_col)),
         by = seq_len(nrow(x))]
  x <- x[, c(setdiff(colnames(x), orig_cols)) := NULL]

  x
}

mimic_age <- function(x, val_col, ...) {
  x <- as_id_tbl(x)
  x <- set(x, j = val_col,
    value = as.double(`units<-`(x[[val_col]], "days") / -365))
  x <- set(x, i = which(x[[val_col]] > 90), j = val_col, value = 90)
  x
}

eicu_age <- function(x, val_col, ...) {
  x <- set(x, which(x[[val_col]] == "> 89"), j = val_col, value = 90)
  x <- set(x, j = val_col, value = as.numeric(x[[val_col]]))
  x
}

hirid_vent_start <- function(x, val_col, ...) {
  all_flag(x[get(val_col) == 1, ], val_col)
}

hirid_vent_end <- function(x, val_col, ...) {
  all_flag(x[get(val_col) > 2, ], val_col)
}

hirid_trach <- function(x, val_col, ...) {
  all_flag(x[get(val_col) == 2, ], val_col)
}

mimic_death <- function(x, val_col, ...) {
  set(x, j = val_col, value = x[[val_col]] == 1L)
}

eicu_death <- function(x, val_col, ...) {
  set(x, j = val_col, value = x[[val_col]] == "Expired")
}

hirid_death <- function(x, id_col, val_col, item_col, ...) {

  threshold <- function(x, col, thresh) {
    set(x, j = col, value = x[[col]] <= thresh)
  }

  score <- function(x, id, val) x[, data.table::last(get(val)), by = c(id)]

  tmp <- split(x, by = item_col, keep.by = FALSE)
  tmp <- lapply(tmp, threshold, val_col, 40)
  tmp <- lapply(tmp, score, id_col, val_col)
  tmp <- reduce(merge, tmp, all = TRUE)

  tmp <- tmp[, c(val_col, "V1.x", "V1.y") := list(get("V1.x") | get("V1.y"),
                                                  NULL, NULL)]

  as_id_tbl(tmp, id(x), id_opts(x))
}

mf_sex <- function(x, val_col, ...) {
  x <- x[get(val_col) == "M", c(val_col) := "Male"]
  x <- x[get(val_col) == "F", c(val_col) := "Female"]
  x
}

crp_dl_to_l <- function(x, val_col, unit_col, ...) {
  x[grepl("mg/dl", get(unit_col), ignore.case = TRUE),
    c(val_col, unit_col) := list(get(val_col) * 10, "mg/L")]
}

eicu_total_co2 <- function(x, val_col, unit_col, ...) {
  x[get(unit_col) != "mm(hg)", ]
}

eicu_calcium <- function(x, val_col, unit_col, ...) {
  x[grepl("mmol/l", get(unit_col), ignore.case = TRUE),
    c(val_col, unit_col) := list(get(val_col) * 4, "mg/dL")]
}

eicu_fio2 <- function(x, val_col, unit_col, ...) {
  x[get(unit_col) != "lpm", ]
}

eicu_magnesium <- function(x, val_col, unit_col, ...) {
  x[grepl("meq/l", get(unit_col), ignore.case = TRUE),
    c(val_col, unit_col) := list(get(val_col) / 1.215, "mEq/L")]
}
