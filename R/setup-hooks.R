
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

  id_map_cols <- c("patienthealthsystemstayid", "patientunitstayid",
                   "hospitaladmitoffset", "unitdischargeoffset")

  delayedAssign("id_map",
                get_table("patient", source)[, id_map_cols],
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
