
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
  res <- mimic_ts_quo("icustays", NULL, c("icustay_id", "outtime"),
                      "hadm_id", "intime", interval = interval,
                      source = source)
  res <- rename_cols(res, c(in_time, out_time), c("intime", "outtime"))
  res
}

eicu_icu_stays <- function(in_time, out_time, interval, source) {
  res <- eicu_ts_quo("patient", NULL,
                     c("patientunitstayid", "unitdischargeoffset"),
                     "patienthealthsystemstayid", "unitadmitoffset",
                      interval = interval, source = source)
  res <- rename_cols(res, c(in_time, out_time),
                     c("unitadmitoffset", "unitdischargeoffset"))
  res
}

#' @export
patient_weight <- function(source, weight_col = "weight") {

  assert_that(is.string(weight_col))

  switch(
    as_src(source),
    mimic = stop("TODO"),
    eicu  = eicu_patient_weight(weight_col, source),
    hirid = NULL,
    stop("Data source not recognized.")
  )
}

eicu_patient_weight <- function(weight_col, source) {

  res <- eicu_tbl_quo("patient", cols = c("patienthealthsystemstayid",
                                          "admissionweight"), source = source)

  res <- res[, list(weight = mean(get("admissionweight"), na.rm = TRUE)),
             by = "patienthealthsystemstayid"]

  res <- res[!is.na(get("weight")), ]
  res <- setnames(res, "weight", weight_col)

  res
}
