
mimic_get_admissions <- function(cols = c("hadm_id", "admittime"),
                                 data_env = "mimic") {

  adm <- get_table("admissions", data_env)
  adm[, cols]
}

mimic_get_icu_stays <- function(icu_in = "icu_in", icu_out = "icu_out",
                                disch = "disch", time_scale = "hours",
                                step_size = 1L, data_env = "mimic") {

  dt_fun <- function(x, y) {
    list(round_to(difftime(x, y, units = time_scale), step_size))
  }

  adm <- mimic_get_admissions(c("hadm_id", "admittime", "dischtime"), data_env)
  icu <- get_table("icustays", data_env)
  icu <- icu[, c("hadm_id", "icustay_id", "intime", "outtime")]

  dat <- merge(adm, icu, by = "hadm_id")

  row_ok <- complete.cases(dat)
  if (any(!row_ok)) {
    message("deleting ", sum(!row_ok), " icu stays due to missingness")
    dat <- dat[row_ok, ]
  }

  dat <- dat[, c(icu_in, icu_out, disch) := c(
    if (!is.null(icu_in)) dt_fun(intime, admittime),
    if (!is.null(icu_out)) dt_fun(outtime, admittime),
    if (!is.null(disch)) dt_fun(dischtime, admittime)
  )]

  dat[, c("hadm_id", "icustay_id", icu_in, icu_out, disch), with = FALSE]
}
