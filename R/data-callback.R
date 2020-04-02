
all_flag <- function(x, val_col, ...) {
  x <- set(x, j = val_col, value = rep(TRUE, nrow(x)))
  x
}

vent_flag <- function(x, val_col, time_col, ...) {
  x <- x[as.logical(get(val_col)), ]
  x <- set(x, j = c(time_col, val_col),
           value = list(x[[val_col]], rep(TRUE, nrow(x))))
  x
}

percent_as_numeric <- function(x, val_col, ...) {
  set(x, j = val_col, value = as.numeric(sub("%", "", x[[val_col]])))
  x
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

eicu_body_weight <- function(x, val_col, weight_col, ...) {

  do_calc <- function(rate, w1, w2) rate / fifelse(is.na(w1), w2, w1)

  weight <- get_table("patient_weight", "eicu", envir = "aux")

  x <- merge(x, weight, all.x = TRUE, by = "patienthealthsystemstayid")
  on.exit(set(x, j = "weight", value = NULL))

  x <- force_numeric_cols(x, c(val_col, weight_col))
  x <- x[, c(val_col) := do_calc(get(val_col), get(weight_col), get("weight"))]
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

multiply_by <- function(factor) {
  function(x, val_col, ...) {
    x <- set(x, j = val_col, value = x[[val_col]] * factor)
    x
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
multiply_hirid_urea <- multiply_by(6.006)
multiply_hirid_bili <- multiply_by(0.058467)

fahrenheit_to_celsius <- function(x, val_col, ...) {
  x <- set(x, j = val_col, value = (x[[val_col]] - 32) * 5 / 9)
  x
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

  x <- set(x, j = setdiff(colnames(x), orig_cols), value = NULL)
  x
}

mimic_age <- function(x, val_col, unit, ...) {
  x <- set(x, j = val_col,
    value = as.double(`units<-`(x[[val_col]], "days") / -365))
  x
}

hirid_vent_start <- function(x, val_col, ...) {
  all_flag(x[get(val_col) == 1, ], val_col)
}

hirid_vent_end <- function(x, val_col, ...) {
  all_flag(x[get(val_col) > 2, ])
}

hirid_trach <- function(x, val_col, ...) {
  all_flag(x[get(val_col) == 2, ])
}
