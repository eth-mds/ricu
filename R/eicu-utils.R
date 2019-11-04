
eicu_get_admissions <- function(
  cols = c("patientunitstayid", "patienthealthsystemstayid",
           "hospitaladmitoffset"),
   data_env = "eicu") {

  adm <- get_table("patient", data_env)
  adm[, cols]
}

