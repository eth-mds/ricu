
#' @export
mimic_tbl <- function(table, row_expr, ...) {
  mimic_tbl_(table, null_or_subs(row_expr), ...)
}

#' @export
mimic_tbl_ <- function(table, row_quo = NULL, cols = NULL,
                       interval = hours(1L), envir = "mimic") {

  time_fun <- function(x, y) {
    round_to(difftime(x, y, units = units(interval)), as.numeric(interval))
  }

  assert_that(is_time(interval, allow_neg = FALSE))

  dat <- prt::subset_quo(get_table(table, envir), row_quo, cols)

  is_date <- vapply(dat, inherits, logical(1L), "POSIXt")

  if (any(is_date)) {

    date_cols <- colnames(dat)[is_date]

    adm <- mimic_admissions(cols = c("hadm_id", "admittime"), data_env = envir)
    dat <- merge(dat, adm, by = "hadm_id", all.x = TRUE)

    dat <- dat[, c(date_cols) := lapply(.SD, time_fun, admittime),
               .SDcols = date_cols]

    set(dat, j = "admittime", value = NULL)
  }

  dat
}

#' @export
mimic_ts <- function(table, row_expr, ...) {
  mimic_ts_(table, null_or_subs(row_expr), ...)
}

#' @export
mimic_ts_ <- function(table, row_quo = NULL, cols = NULL,
                      id_cols = "hadm_id", time_col = "charttime",
                      id_names = id_cols, time_name = "hadm_time",
                      interval = hours(1L), envir = "mimic") {

  if (!is.null(cols)) {

    if (is.numeric(cols)) {
      cols <- colnames(get_table(table, envir))
    }

    assert_that(is.character(cols))

    cols <- unique(c(id_cols, time_col, cols))
  }

  res <- mimic_tbl_(table, row_quo, cols, interval, envir)
  res <- setnames(res, c(id_cols, time_col), c(id_names, time_name))

  new_ts_tbl(res, id_names, time_name, as.numeric(interval))
}

mimic_admit_difftime <- function(dat, data_env = "mimic",
  time_col = "charttime", time_name = "hadm_time", time_scale = "hours",
  step_size = 1L) {

  adm <- mimic_admissions(data_env = data_env)

  nrow_before <- nrow(dat)
  dat <- merge(dat, adm, by = "hadm_id", all = FALSE)
  nrow_rm <- nrow_before - nrow(dat)

  if (nrow_rm > 0L) {
    message("Lost ", nrow_rm, " rows determining `", time_name, "`.")
  }

  dat <- dat[, c(time_name) := round_to(
    difftime(eval(as.name(time_col)), admittime, units = time_scale), step_size
  )]

  dat
}

mimic_admissions <- function(cols = c("hadm_id", "admittime"),
                             data_env = "mimic") {

  adm <- get_table("admissions", data_env)
  adm[, cols]
}

mimic_get_icu_stays <- function(icu_in = "icu_in", icu_out = "icu_out",
                                disch = "disch", time_scale = "hours",
                                step_size = 1L, data_env = "mimic") {

  dt_fun <- function(x, y) {
    list(round_to(difftime(x, y, units = time_scale), step_size))
  }

  adm <- mimic_admissions(c("hadm_id", "admittime", "dischtime"), data_env)
  icu <- get_table("icustays", data_env)
  icu <- icu[, c("hadm_id", "icustay_id", "intime", "outtime")]

  dat <- merge(adm, icu, by = "hadm_id")

  row_ok <- complete.cases(dat)
  if (any(!row_ok)) {
    message("deleting ", sum(!row_ok), " icu stays due to missingness")
    dat <- dat[row_ok, ]
  }

  dat <- dat[, c(icu_in, icu_out, disch) := c(
    if (!is.null(icu_in)) dt_fun(intime, admittime),
    if (!is.null(icu_out)) dt_fun(outtime, admittime),
    if (!is.null(disch)) dt_fun(dischtime, admittime)
  )]

  dat[, c("hadm_id", "icustay_id", icu_in, icu_out, disch), with = FALSE]
}
