
#' @export
hirid_ts <- function(table, row_expr, ...) {
  hirid_ts_quo(table, null_or_subs(row_expr), ...)
}

#' @export
hirid_ts_quo <- function(table, row_quo = NULL, cols = NULL,
                         id_cols = "patientid", time_col = "datetime",
                         interval = hours(1L), source = "hirid") {

  if (!is.null(cols)) {
    cols <- c(id_cols, time_col, cols)
  }

  res <- hirid_tbl_quo(table, row_quo, cols, interval, source)

  as_ts_tbl(res, id_cols, time_col, interval)
}

#' @export
hirid_tbl <- function(table, row_expr, ...) {
  hirid_tbl_quo(table, null_or_subs(row_expr), ...)
}

#' @export
hirid_tbl_quo <- function(table, row_quo = NULL, cols = NULL,
                          interval = hours(1L), source = "hirid") {

  time_fun <- function(x, y) {
    round_to(difftime(x, y, units = units(interval)), as.numeric(interval))
  }

  assert_that(is_time(interval, allow_neg = FALSE))

  if (!is.null(cols)) {

    assert_that(is.character(cols))

    if (table == "general") {
      cols <- setdiff(cols, "admissiontime")
    }

    cols <- c("patientid", cols)

  }

  dat <- prt::subset_quo(get_table(table, source), row_quo, unique(cols))

  is_date <- vapply(dat, inherits, logical(1L), "POSIXt")

  if (any(is_date)) {

    assert_that(!identical(table, "general"))

    date_cols <- colnames(dat)[is_date]

    adm <- get_table("general", source)[, c("patientid", "admissiontime")]
    dat <- merge(dat, adm, by = "patientid", all.x = TRUE)

    dat <- dat[, c(date_cols) := lapply(.SD, time_fun, admissiontime),
               .SDcols = date_cols]

    if ("admissiontime" %in% colnames(dat)) {
      set(dat, j = "admissiontime", value = NULL)
    }
  }

  dat
}
