
#' @export
eicu_ts <- function(table, row_expr, ...) {
  eicu_ts_quo(table, null_or_subs(row_expr), ...)
}

#' @export
eicu_ts_quo <- function(table, row_quo = NULL, cols = NULL,
                        id_cols = "patienthealthsystemstayid",
                        time_col = "observationoffset",
                        interval = hours(1L), source = "eicu") {

  if (!is.null(cols)) {
    cols <- c(id_cols, time_col, cols)
  }

  res <- eicu_tbl_quo(table, row_quo, cols, interval, source)

  as_ts_tbl(res, id_cols, time_col, interval)
}

#' @export
eicu_id <- function(table, row_expr, ...) {
  eicu_id_quo(table, null_or_subs(row_expr), ...)
}

#' @export
eicu_id_quo <- function(table, row_quo = NULL, cols = NULL,
                        id_cols = "patienthealthsystemstayid",
                        interval = hours(1L), source = "eicu") {

  if (!is.null(cols)) {
    cols <- c(id_cols, cols)
  }

  res <- eicu_tbl_quo(table, row_quo, cols, interval, source)

  as_id_tbl(res, id_cols)
}

#' @export
eicu_tbl <- function(table, row_expr, ...) {
  eicu_tbl_quo(table, null_or_subs(row_expr), ...)
}

#' @export
eicu_tbl_quo <- function(table, row_quo = NULL, cols = NULL,
                         interval = hours(1L), source = "eicu") {

  time_fun <- function(x, y) {
    res <- as.difftime(x - y, units = "mins")
    units(res) <- units(interval)
    round_to(res, as.numeric(interval))
  }

  assert_that(is_time(interval, allow_neg = FALSE))

  tbl <- get_table(table, source)
  adm_cols <- character(0L)

  if (!is.null(cols)) {

    assert_that(is.character(cols))

    if (table == "patient") {
      get_cols <- c("hospitaladmitoffset", setdiff(cols, "unitadmitoffset"))
    } else {
      get_cols <- c("patientunitstayid",
                    setdiff(cols, "patienthealthsystemstayid"))
      adm_cols <- intersect("patienthealthsystemstayid", cols)
    }

  } else{
    get_cols <- NULL
  }

  dat <- prt::subset_quo(tbl, row_quo, unique(get_cols))
  adm <- get_table("patient", source)

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
        dat <- set(dat, j = "unitadmitoffset", value = 0)
      }

    } else {

      adm <- adm[, c("patientunitstayid", "hospitaladmitoffset", adm_cols)]
      dat <- merge(dat, adm, by = "patientunitstayid", all.x = TRUE)
    }

    if (length(date_cols)) {
      dat <- dat[, c(date_cols) := lapply(
        .SD, time_fun, get("hospitaladmitoffset")), .SDcols = date_cols]
    }

  } else if (length(adm_cols)) {

    adm <- adm[, c("patientunitstayid", adm_cols)]
    dat <- merge(dat, adm, by = "patientunitstayid", all.x = TRUE)
  }

  to_rm <- setdiff(colnames(dat), cols)

  if (length(to_rm) > 0L) {
    set(dat, j = to_rm, value = NULL)
  }

  dat
}
