
#' @export
icu_stays <- function(source, interval = hours(1L)) {

  switch(
    as_src(source),
    mimic = mimic_ts_quo("icustays", NULL, c("icustay_id", "outtime"),
                         "hadm_id", "intime", interval, source),
    eicu  = eicu_ts_quo("patient", NULL,
                        c("patientunitstayid", "unitdischargeoffset"),
                        "patienthealthsystemstayid", "unitadmitoffset",
                        interval, source),
    hirid = stop("TODO"),
    stop("Data source not recognized.")
  )

}
