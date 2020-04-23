
mimic_tbl_quo <- function(table, row_quo = NULL, cols = NULL,
                          interval = hours(1L), source = "mimic") {

  difftime_tbl_quo(source, table, row_quo, cols,
                   c("icustay_id", "hadm_id", "subject_id"), interval)
}

hirid_tbl_quo <- function(table, row_quo = NULL, cols = NULL,
                          interval = hours(1L), source = "hirid") {

  difftime_tbl_quo(source, table, row_quo, cols, "patientid", interval)
}

eicu_tbl_quo <- function(table, row_quo = NULL, cols = NULL,
                         interval = hours(1L), source = "eicu") {

  time_fun <- function(x) {
    re_time(as.difftime(x, units = "mins"), interval)
  }

  dat <- default_data_fun(table, row_quo, cols, source = source)

  id_col <- "patientunitstayid"

  date_cols <- colnames(dat)[
    vapply(dat, is.integer, logical(1L)) & grepl("offset$", colnames(dat))
  ]

  if (length(date_cols)) {

    dat <- na.omit(dat, date_cols)

    if (id_col %in% colnames(dat)) {
      dat <- dat[, c(date_cols) := lapply(.SD, time_fun), .SDcols = date_cols]
    } else {
      warning("In order to return relative times, an ID column is required.")
    }
  }

  if (id_col %in% colnames(dat)) {
    dat <- na.omit(dat, id_col)
  }

  dat
}

default_data_fun <- function(table, row_quo = NULL, cols = NULL, interval,
                             source) {

  if (!missing(interval)) {
    warning("Specifying a time interval has no effect.")
  }

  prt::subset_quo(get_tbl(table, source), row_quo, unique(cols))
}

difftime_tbl_quo <- function(src, tbl, row, col, id_cand, ival) {

  time_fun <- function(x, y, int) {
    round_to(difftime(x, y, units = units(int)), as.numeric(int))
  }

  dat <- default_data_fun(tbl, row, col, source = src)

  id_col    <- intersect(id_cand, colnames(dat))[1L]
  date_cols <- colnames(dat)[vapply(dat, inherits, logical(1L), "POSIXt")]

  if (length(date_cols)) {

    if (is.na(id_col)) {

      warning("In order to return relative times, an ID column is required.")

    } else {

      dat <- merge(dat, get_tbl(id_col, src, "aux"), by.x = id_col,
                   by.y = "id")

      dat <- dat[, c(date_cols) := lapply(.SD, time_fun, get("origin"), ival),
                 .SDcols = date_cols]

      dat <- dat[, c("origin") := NULL]
    }

  } else if (!is.na(id_col)) {
    dat <- na.omit(dat, id_col)
  }

  dat

}
