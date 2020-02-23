
check_mmhg <- function(x, unit_col, ...) {

  assert_that(
    all(is.na(x[[unit_col]]) |
      grepl("mm hg", x[[unit_col]], ignore.case = TRUE))
  )

	x
}

all_flag <- function(x, val_col, ...) {
  x <- set(x, j = val_col, value = rep(TRUE, nrow(x)))
  x
}

vent_flag <- function(x, time_col, val_col, ...) {
  x <- x[as.logical(get(val_col)), ]
  x <- set(x, j = c(time_col, val_col),
           value = list(x[[val_col]], rep(TRUE, nrow(x))))
  x
}

percent_as_numeric <- function(x, val_col, ...) {
  set(x, j = val_col, value = as.numeric(sub("%", "", x[[val_col]])))
  x
}

delayedAssign("eicu_patient_weight", local({
  patient <- eicu_tbl_quo("patient", cols = c("patienthealthsystemstayid",
                                              "admissionweight"))
  patient <- patient[, list(weight = mean(admissionweight, na.rm = TRUE)),
                     by = "patienthealthsystemstayid"]
  patient[!is.na(weight), ]
}))

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

adjust_body_weight <- function(x, val_col, weight_col, ...) {

  do_calc <- function(rate, w1, w2) rate / fifelse(is.na(w1), w2, w1)

  x <- merge(x, eicu_patient_weight, all.x = TRUE,
             by = "patienthealthsystemstayid")
  on.exit(x[, weight := NULL])

  x <- force_numeric_cols(x, c(val_col, weight_col))
  x <- x[, c(val_col) := do_calc(get(val_col), get(weight_col), get("weight"))]
  x
}

combine_date_time <- function(x, time_col, val_col, ...) {
  x <- x[, c(time_col, val_col) := list(
    fifelse(is.na(get(val_col)), get(time_col), get(val_col)),
    fifelse(is.na(get(val_col)), hours(24L), hours(0L))
  )]
  x
}
