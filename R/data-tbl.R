
mimic_tbl_quo <- function(table, row_quo = NULL, cols = NULL,
                          interval = hours(1L), source = "mimic") {

  time_fun <- function(x, y) {
    round_to(difftime(x, y, units = units(interval)), as.numeric(interval))
  }

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
  adm <- get_table("admissions", source)

  is_date <- vapply(dat, inherits, logical(1L), "POSIXt")

  if (any(is_date)) {

    date_cols <- colnames(dat)[is_date]

    if (table == "admissions") {

      date_cols <- setdiff(date_cols, "admittime")

    } else {

      if (table == "patients") {
        adm <- adm[, c("subject_id", "admittime", adm_cols)]
        dat <- merge(dat, adm, by = "subject_id", all.x = TRUE)
      } else {
        adm <- adm[, c("hadm_id", "admittime", adm_cols)]
        dat <- merge(dat, adm, by = "hadm_id", all.x = TRUE)
      }
    }

    if (length(date_cols)) {
      dat <- dat[, c(date_cols) := lapply(.SD, time_fun, get("admittime")),
                 .SDcols = date_cols]
    }

  } else if (length(adm_cols)) {

    adm <- adm[, c("subject_id", adm_cols)]
    dat <- merge(dat, adm, by = "subject_id", all.x = TRUE)
  }

  to_rm <- setdiff(colnames(dat), cols)

  if (length(to_rm)) {
    set(dat, j = to_rm, value = NULL)
  }

  dat
}

eicu_tbl_quo <- function(table, row_quo = NULL, cols = NULL,
                         interval = hours(1L), source = "eicu") {

  time_fun <- function(x, y) {
    res <- as.difftime(x - y, units = "mins")
    units(res) <- units(interval)
    round_to(res, as.numeric(interval))
  }

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

hirid_tbl_quo <- function(table, row_quo = NULL, cols = NULL,
                          interval = hours(1L), source = "hirid") {

  time_fun <- function(x, y) {
    round_to(difftime(x, y, units = units(interval)), as.numeric(interval))
  }

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

    dat <- dat[, c(date_cols) := lapply(.SD, time_fun, get("admissiontime")),
               .SDcols = date_cols]

    if ("admissiontime" %in% colnames(dat)) {
      set(dat, j = "admissiontime", value = NULL)
    }
  }

  dat
}
