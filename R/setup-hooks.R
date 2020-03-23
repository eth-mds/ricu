
setup_mimic_aux_tables <- function(source) {

  mimic_id_map <- function(src) {

    pat <- get_table("patients", src)[, c("subject_id", "dob")]
    adm <- get_table("admissions", src)[,
      c("subject_id", "hadm_id", "admittime", "dischtime")
    ]

    res <- merge(pat, adm, by = "subject_id")

    icu <- get_table("icustays", src)[,
      c("hadm_id", "icustay_id", "intime", "outtime")
    ]

    merge(res, icu, by = "hadm_id")
  }

  delayedAssign("id_map", mimic_id_map(source),
                assign.env = get_source(source, "aux"))
}

setup_eicu_aux_tables <- function(source) {

  eicu_id_map <- function(src) {

    dob <- function(x, y) mean(x + y, na.rm = TRUE)

    time_cols <- c("hospitaladmitoffset", "hospitaldischargeoffset",
                   "unitdischargeoffset")
    id_cols   <- c("uniquepid", "patienthealthsystemstayid",
                   "patientunitstayid")

    res <- get_table("patient", src)[, c(id_cols, "age", time_cols)]

    res[["age"]][res[["age"]] == "> 89"] <- "90"
    res[["age"]] <- as.difftime(-as.numeric(res[["age"]]) * 365 * 24 * 60,
                                units = "mins")
    res <- rename_cols(res, "birthoffset", "age")

    res <- res[, c(time_cols) := lapply(.SD, as.difftime, units = "mins"),
               .SDcols = time_cols]

    res <- res[, c("birthoffset") := dob(get("birthoffset"),
                                         get("hospitaladmitoffset")),
               by = "uniquepid"]

    res
  }


  delayedAssign("id_map", eicu_id_map(source),
                assign.env = get_source(source, "aux"))

  delayedAssign("patient_weight",
                load_concepts(source, "weight"),
                assign.env = get_source(source, "aux"))
}

setup_hirid_aux_tables <- function(source) {

  id_map_cols <- c("patientid", "admissiontime")

  delayedAssign("id_map",
                get_table("general", source)[, id_map_cols],
                assign.env = get_source(source, "aux"))
}
