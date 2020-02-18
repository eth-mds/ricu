
#' @export
eicu_ts <- function(table, row_expr, ...) {
  eicu_ts_quo(table, null_or_subs(row_expr), ...)
}

#' @export
eicu_ts_quo <- function(table, row_quo = NULL, cols = NULL,
                        id_cols = "patienthealthsystemstayid",
                        time_col = "observationoffset",
                        interval = hours(1L), envir = "eicu") {

  if (!is.null(cols)) {
    cols <- c(id_cols, time_col, cols)
  }

  res <- eicu_tbl_quo(table, row_quo, cols, interval, envir)

  as_ts_tbl(res, id_cols, time_col, interval)
}

eicu_ts_unit_quo <- function(table, row_quo = NULL, cols = NULL, ...,
                             val_cols, unit_cols) {

  assert_that(is.character(val_cols), is.character(unit_cols),
              length(val_cols) > 0L, same_length(val_cols, unit_cols))

  if (!is.null(cols)) {
    cols <- c(cols, unit_cols[val_cols %in% cols])
  }

  res <- eicu_ts_quo(table, row_quo, cols, ...)

  hits <- val_cols %in% colnames(res)

  if (any(hits)) {
    res <- update_ts_def(res, new_ts_unit(val_cols[hits], unit_cols[hits]))
  }

  res
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

  if (!is.null(cols)) {

    assert_that(is.character(cols))

    if (table == "patient") {
      get_cols <- c("hospitaladmitoffset", setdiff(cols, "unitadmitoffset"))
    } else {
      get_cols <- c("patientunitstayid",
                    setdiff(cols, "patienthealthsystemstayid"))
    }

  } else{
    get_cols <- NULL
  }

  dat <- prt::subset_quo(get_table(table, envir), row_quo, unique(get_cols))

  is_date <- vapply(dat, is.integer, logical(1L)) &
    grepl("offset$", colnames(dat))

  if (any(is_date)) {

    date_cols <- colnames(dat)[is_date]

    if (table == "patient") {

      if (!"hospitaladmitoffset" %in% cols) {
        date_cols <- setdiff(date_cols, "hospitaladmitoffset")
      }

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

    if (length(date_cols)) {
      dat <- dat[, c(date_cols) := lapply(.SD, time_fun, hospitaladmitoffset),
                 .SDcols = date_cols]
    }
  }

  to_rm <- setdiff(colnames(dat), cols)

  if (length(to_rm) > 0L) {
    set(dat, j = to_rm, value = NULL)
  }

  dat
}
