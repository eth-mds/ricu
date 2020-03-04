
#' @export
mimic_ts <- function(table, row_expr, ...) {
  mimic_ts_quo(table, null_or_subs(row_expr), ...)
}

#' @export
mimic_ts_quo <- function(table, row_quo = NULL, cols = NULL,
                         id_cols = "hadm_id", time_col = "charttime",
                         interval = hours(1L), source = "mimic") {

  if (!is.null(cols)) {
    cols <- c(id_cols, time_col, cols)
  }

  res <- mimic_tbl_quo(table, row_quo, cols, interval, source)

  as_ts_tbl(res, id_cols, time_col, interval)
}

#' @export
mimic_tbl <- function(table, row_expr, ...) {
  mimic_tbl_quo(table, null_or_subs(row_expr), ...)
}

#' @export
mimic_tbl_quo <- function(table, row_quo = NULL, cols = NULL,
                          interval = hours(1L), source = "mimic") {

  time_fun <- function(x, y) {
    round_to(difftime(x, y, units = units(interval)), as.numeric(interval))
  }

  assert_that(is_time(interval, allow_neg = FALSE))

  tbl <- get_table(table, source)
  adm_cols <- character(0L)

  if (!is.null(cols)) {

    assert_that(is.character(cols))

    if (table == "admissions") {
      tbl_cols <- c("admittime", cols)
    } else if (table == "patients") {
      tbl_cols <- c("subject_id", setdiff(cols, "hadm_id"))
      adm_cols <- intersect("hadm_id", cols)
    } else {
      tbl_cols <- c("hadm_id", cols)
    }

  } else {

    tbl_cols <- NULL
  }

  dat <- prt::subset_quo(tbl, row_quo, unique(tbl_cols))

  is_date <- vapply(dat, inherits, logical(1L), "POSIXt")

  if (any(is_date)) {

    date_cols <- colnames(dat)[is_date]

    if (table == "admissions") {

      date_cols <- setdiff(date_cols, "admittime")

    } else {

      adm <- get_table("admissions", source)

      if (table == "patients") {
        adm <- adm[, c("subject_id", "admittime", adm_cols)]
        dat <- merge(dat, adm, by = "subject_id", all.x = TRUE)
      } else {
        adm <- adm[, c("hadm_id", "admittime", adm_cols)]
        dat <- merge(dat, adm, by = "hadm_id", all.x = TRUE)
      }
    }

    if (length(date_cols)) {
      dat <- dat[, c(date_cols) := lapply(.SD, time_fun, admittime),
                 .SDcols = date_cols]
    }

    to_rm <- setdiff(colnames(dat), cols)

    if (length(to_rm)) {
      set(dat, j = to_rm, value = NULL)
    }
  }

  dat[]
}
