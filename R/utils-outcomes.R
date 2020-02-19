
#' @export
outcome <- function(source, type = c("mortality", "extended_stay"), ...) {

  type <- match.arg(type)

  switch(
    match.arg(type),
    mortality = switch(
      as_src(source),
      mimic = mimic_mortality(...),
      eicu  = eicu_mortality(...),
      hirid = stop("TODO"),
      stop("Data source not recognized.")
    ),
    extended_stay = stop("TODO")
  )

}

mimic_mortality <- function(id_col = "hadm_id") {

  if (!identical(id_col, "hadm_id")) stop("TODO")

  res <- mimic_tbl("admissions", cols = c(id_col, "hospital_expire_flag"))
  res[, `:=`(c("expired", "hospital_expire_flag"),
             list(as.logical(hospital_expire_flag), NULL))][]
}

eicu_mortality <- function(id_col = "patienthealthsystemstayid") {

  assert_that(is.string(id_col),
              id_col %in% c("patienthealthsystemstayid", "patientunitstayid"))

  res <- eicu_tbl("patient", cols = c(id_col, "hospitaldischargestatus"))
  res <- res[, `:=`(c("expired", "hospitaldischargestatus"),
                    list(hospitaldischargestatus == "Expired", NULL))]

  res[, list(expired = any(expired)), by = id_col]
}
