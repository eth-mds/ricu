
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
patient_age <- function(source, age_col = "age") {

  switch(
    as_src(source),
    mimic = mimic_age(age_col, source),
    eicu  = eicu_age(age_col, source),
    hirid = hirid_age(age_col, source),
    stop("Data source not recognized.")
  )
}

mimic_age <- function(age_col, source) {

  res <- mimic_tbl("patients", cols = c("hadm_id", "dob"),
                   interval = days(365L), source = source)
  res <- setcolorder(res, c("hadm_id", "dob"))
  res <- set(res, j = "dob", value = as.numeric(-res[["dob"]] / 365))
  res <- setnames(res, "dob", age_col)

  res
}

eicu_age <- function(age_col, source) {

  res <- eicu_tbl("patient", cols = c("patienthealthsystemstayid", "age"),
                  source = source)
  res <- setnames(res, "age", age_col)

  res
}

hirid_age <- function(age_col, source) {

  res <- hirid_tbl("general", cols = c("patientid", "age"), source = source)
  res <- setnames(res, "age", age_col)

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

  res <- res[!is.na(weight), ]
  res <- setnames(res, "weight", weight_col)

  res
}
