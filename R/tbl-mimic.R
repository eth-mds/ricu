
#' @export
mimic_ts <- function(table, row_expr, ...) {
  mimic_ts_quo(table, null_or_subs(row_expr), ...)
}

#' @export
mimic_ts_quo <- function(table, row_quo = NULL, cols = NULL,
                         id_cols = "hadm_id", time_col = "charttime",
                         interval = hours(1L), envir = "mimic") {

  if (!is.null(cols)) {
    cols <- c(id_cols, time_col, cols)
  }

  res <- mimic_tbl_quo(table, row_quo, cols, interval, envir)

  as_ts_tbl(res, id_cols, time_col, interval)
}

#' @export
mimic_tbl <- function(table, row_expr, ...) {
  mimic_tbl_quo(table, null_or_subs(row_expr), ...)
}

#' @export
mimic_tbl_quo <- function(table, row_quo = NULL, cols = NULL,
                          interval = hours(1L), envir = "mimic") {

  time_fun <- function(x, y) {
    round_to(difftime(x, y, units = units(interval)), as.numeric(interval))
  }

  assert_that(is_time(interval, allow_neg = FALSE))

  if (!is.null(cols)) {

    assert_that(is.character(cols))

    if (table == "admissions") cols <- c("admittime", cols)
    else                       cols <- c("hadm_id", cols)
  }

  dat <- prt::subset_quo(get_table(table, envir), row_quo, unique(cols))

  is_date <- vapply(dat, inherits, logical(1L), "POSIXt")

  if (any(is_date)) {

    date_cols <- colnames(dat)[is_date]

    if (table == "admissions") {
      date_cols <- setdiff(date_cols, "admittime")
    } else {
      adm <- get_table("admissions", envir)[, c("hadm_id", "admittime")]
      dat <- merge(dat, adm, by = "hadm_id", all.x = TRUE)
    }

    if (length(date_cols)) {
      dat <- dat[, c(date_cols) := lapply(.SD, time_fun, admittime),
                 .SDcols = date_cols]
    }

    if ("admittime" %in% colnames(dat)) {
      set(dat, j = "admittime", value = NULL)
    }
  }

  dat
}
