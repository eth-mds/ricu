
aggregate_vals <- function(tbl, fun = mean,
                           by_cols = c("hadm_id", "icustay_id", "rel_time"),
                           val_col = setdiff(colnames(tbl), by_cols),
                           new_col = val_col, ...) {

  if (is.null(val_col)) val_col <- setdiff(colnames(tbl), by_cols)
  if (is.null(by_cols)) by_cols <- setdiff(colnames(tbl), val_col)

  assert_that(
    data.table::is.data.table(tbl), is.function(fun), is.string(val_col),
    is.character(by_cols), length(by_cols) > 0L,
    all(c(val_col, by_cols) %in% colnames(tbl))
  )

  if (nrow(tbl) == 0) return(tbl)

  units <- attr(tbl[[val_col]], "units")

  tbl <- tbl[, setNames(lapply(.SD, fun, ...), new_col), .SDcols = val_col,
             by = by_cols]

  if (!is.null(units)) {
    data.table::setattr(tbl[[new_col]], "units", units)
  }

  tbl
}

mimic_get_admissions <- function(cols = c("hadm_id", "admittime"),
                                 data_env = "mimic") {

  adm <- get_table("admissions", data_env)
  adm[, cols]
}

mimic_get_icu_stays <- function(icu_in = "icu_in", icu_out = "icu_out",
                                disch = "disch", time_scale = "hours",
                                round_fun = round, data_env = "mimic") {

  adm <- mimic_get_admissions(c("hadm_id", "admittime", "dischtime"), data_env)
  icu <- get_table("icustays", data_env)
  icu <- icu[, c("hadm_id", "icustay_id", "intime", "outtime")]

  dat <- merge(adm, icu, by = "hadm_id")

  row_ok <- complete.cases(dat)
  if (any(!row_ok)) {
    message("deleting ", sum(!row_ok), " icu stays due to missingness")
    dat <- dat[row_ok, ]
  }

  dat <- dat[, c(icu_in, icu_out, disch) := c(
    if (!is.null(icu_in))
      list(round_fun(difftime(intime, admittime, units = time_scale))),
    if (!is.null(icu_out))
      list(round_fun(difftime(outtime, admittime, units = time_scale))),
    if (!is.null(disch))
      list(round_fun(difftime(dischtime, admittime, units = time_scale)))
  )]

  dat[, c("hadm_id", "icustay_id", icu_in, icu_out, disch), with = FALSE]
}
