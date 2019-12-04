
#' @export
eicu_ts <- function(table, row_expr, ...) {
  eicu_ts_quo(table, null_or_subs(row_expr), ...)
}

#' @export
eicu_ts_quo <- function(table, row_quo = NULL, cols = NULL,
                        id_cols = "hadm_id", time_col = "charttime",
                        interval = hours(1L), envir = "eicu") {

  if (!is.null(cols)) {
    cols <- c(id_cols, time_col, cols)
  }

  res <- eicu_tbl_quo(table, row_quo, cols, interval, envir)

  as_ts_tbl(res, id_cols, time_col, interval)
}

#' @export
eicu_tbl <- function(table, row_expr, ...) {
  eicu_tbl_quo(table, null_or_subs(row_expr), ...)
}

#' @export
eicu_tbl_quo <- function(table, row_quo = NULL, cols = NULL,
                         interval = hours(1L), envir = "eicu") {

  time_fun <- function(x, y) {
    res <- as.difftime(x - y, units = "mins")
    units(res) <- units(interval)
    round_to(res, as.numeric(interval))
  }

  assert_that(is_time(interval, allow_neg = FALSE))

  tbl <- get_table(table, envir)

  if (!is.null(cols)) {

    assert_that(is.character(cols))

    if (table == "patient") {
      get_cols <- c("hospitaladmitoffset", setdiff(cols, "unitadmitoffset"))
    } else {
      get_cols <- c("patientunitstayid",
                    setdiff(cols, "patienthealthsystemstayid"))
    }
  }

  dat <- prt::subset_quo(get_table(table, envir), row_quo, unique(get_cols))

  is_date <- vapply(dat, is.integer, logical(1L)) &
    grepl("offset$", colnames(dat))

  if (any(is_date)) {

    date_cols <- colnames(dat)[is_date]

    if (table == "patient") {

      date_cols <- setdiff(date_cols, "hospitaladmitoffset")

      if ("unitadmitoffset" %in% cols) {
        date_cols <- c(date_cols, "unitadmitoffset")
        dat <- dat[, unitadmitoffset := 0]
      }

    } else {

      adm <- get_table("patient", envir)
      adm <- adm[, c("patientunitstayid", "patienthealthsystemstayid",
                     "hospitaladmitoffset")]
      dat <- merge(dat, adm, by = "patientunitstayid", all.x = TRUE)
    }

    dat <- dat[, c(date_cols) := lapply(.SD, time_fun, hospitaladmitoffset),
               .SDcols = date_cols]
  }

  set(dat, j = setdiff(colnames(dat), cols), value = NULL)

  dat
}
