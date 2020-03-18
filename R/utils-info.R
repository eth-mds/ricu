
#' @export
icu_stays <- function(source, in_time = "intime", out_time = "outtime",
                      interval = hours(1L)) {

  assert_that(is.string(in_time), is.string(out_time))

  switch(
    as_src(source),
    mimic = mimic_icu_stays(in_time, out_time, interval, source),
    eicu  = eicu_icu_stays(in_time, out_time, interval, source),
    hirid = NULL,
    stop("Data source not recognized.")
  )
}

mimic_icu_stays <- function(in_time, out_time, interval, source) {
  res <- data_ts_quo(source, "icustays", NULL, c("icustay_id", "outtime"),
                     "hadm_id", "intime", interval = interval)
  res <- rename_cols(res, c(in_time, out_time), c("intime", "outtime"))
  res
}

eicu_icu_stays <- function(in_time, out_time, interval, source) {
  res <- data_ts_quo(source, "patient", NULL,
                     c("patientunitstayid", "unitdischargeoffset"),
                     "patienthealthsystemstayid", "unitadmitoffset",
                      interval = interval)
  res <- rename_cols(res, c(in_time, out_time),
                     c("unitadmitoffset", "unitdischargeoffset"))
  res
}
