
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

mimic_ts_unit_quo <- function(table, row_quo = NULL, cols = NULL, ...,
                              val_cols, unit_cols) {

  assert_that(is.character(val_cols), is.character(unit_cols),
              length(val_cols) > 0L, same_length(val_cols, unit_cols))

  if (!is.null(cols)) {
    cols <- c(cols, unit_cols[val_cols %in% cols])
  }

  res <- mimic_ts_quo(table, row_quo, cols, ...)

  hits <- val_cols %in% colnames(res)

  if (any(hits)) {
    res <- update_ts_def(res, new_ts_unit(val_cols[hits], unit_cols[hits]))
  }

  res
}

mimic_ts_date_time_quo <- function(table, row_quo = NULL, cols = NULL,
                                   id_cols = "hadm_id", time_col = "charttime",
                                   interval = hours(1L), envir = "mimic",
                                   date_cols, time_cols) {

  fix_time <- function(time_col, date_col, ind_col) {

    res <- res[, c(ind_col) := is.na(get(time_col))]

    if (all(res[[ind_col]])) {
      set(res, j = ind_col, value = NULL)
      NA_character_
    } else {
      ind_col
    }
  }

  assert_that(is.character(date_cols), is.character(time_cols),
              length(date_cols) > 0L, same_length(date_cols, time_cols))

  if (!is.null(cols)) {
    cols <- c(id_cols, time_col, cols)
    get_cols <- c(cols, date_cols[time_cols %in% cols])
  } else {
    get_cols <- NULL
  }

  res <- mimic_tbl_quo(table, row_quo, get_cols, interval, envir)

  hits <- time_cols %in% colnames(res)

  if (any(hits)) {

    time_cols <- time_cols[hits]
    date_cols <- date_cols[hits]

    inds <- Map(fix_time, time_cols, date_cols, paste0(time_cols, "_date"))
    inds <- new_ts_date(time_cols, inds)
  }

  res <- as_ts_tbl(res, id_cols, time_col, interval)

  if (any(hits)) {
    res <- update_ts_def(res, inds)
  }

  if (!is.null(cols)) {
    res <- rm_cols(res, setdiff(get_cols, cols))
  }

  res
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

    dat <- dat[, c(date_cols) := lapply(.SD, time_fun, admittime),
               .SDcols = date_cols]

    set(dat, j = "admittime", value = NULL)
  }

  dat
}
