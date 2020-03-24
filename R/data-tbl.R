
mimic_tbl_quo <- function(table, row_quo = NULL, cols = NULL,
                          interval = hours(1L), source = "mimic") {

  time_fun <- function(x, y) {
    round_to(difftime(x, y, units = units(interval)), as.numeric(interval))
  }

  dat <- prt::subset_quo(get_table(table, source), row_quo, unique(cols))

  date_cols <- colnames(dat)[vapply(dat, inherits, logical(1L), "POSIXt")]

  id_cand <- c("icustay_id", "hadm_id", "subject_id")
  id_col    <- intersect(id_cand, colnames(dat))[1L]
  start_col <- setNames(c("dob", "admittime", "intime"), id_cand)[id_col]

  if (length(date_cols)) {

    if (is.na(id_col)) {

      warning("In order to return relative times, an ID column is required.")

    } else {

      id_map <- get_table("id_map", source, "aux")
      id_map <- unique(id_map[, c(id_col, start_col), with = FALSE])

      dat <- merge(dat, id_map, by = id_col)

      dat <- dat[, c(date_cols) := lapply(.SD, time_fun, get(start_col)),
                 .SDcols = date_cols]

      dat <- set(dat, j = start_col, value = NULL)
    }

  } else if (!is.na(id_col)) {
    dat <- na.omit(dat, id_col)
  }

  dat
}

eicu_tbl_quo <- function(table, row_quo = NULL, cols = NULL,
                         interval = hours(1L), source = "eicu") {

  time_fun <- function(x) {
    res <- as.difftime(x, units = "mins")
    units(res) <- units(interval)
    round_to(res, as.numeric(interval))
  }

  dat <- prt::subset_quo(get_table(table, source), row_quo, unique(cols))

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

hirid_tbl_quo <- function(table, row_quo = NULL, cols = NULL,
                          interval = hours(1L), source = "hirid") {

  time_fun <- function(x, y) {
    round_to(difftime(x, y, units = units(interval)), as.numeric(interval))
  }

  dat <- prt::subset_quo(get_table(table, source), row_quo, unique(cols))

  id_col    <- "patientid"
  start_col <- "admissiontime"

  date_cols <- colnames(dat)[vapply(dat, inherits, logical(1L), "POSIXt")]

  if (length(date_cols)) {

    if (id_col %in% colnames(dat)) {

      dat <- merge(dat, get_table("id_map", source, "aux"), by = id_col)

      dat <- dat[, c(date_cols) := lapply(.SD, time_fun, get(start_col)),
                 .SDcols = date_cols]

      dat <- set(dat, j = start_col, value = NULL)

    } else {
      warning("In order to return relative times, an ID column is required.")
    }

  } else if (id_col %in% colnames(dat)) {
    dat <- na.omit(dat, id_col)
  }

  dat
}
