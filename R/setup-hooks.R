
origin_tbl <- function(src, tbl, id, orig) {
  res <- get_table(tbl, src)[, c(id, orig)]
  res <- rename_cols(res, c("id", "origin"), c(id, orig))
  res
}

setup_mimic_aux_tables <- function(source) {

  mimic_id_map <- function(src) {

    as_dt <- function(x, y) round_to(difftime(x, y, units = "mins"))

    pat <- get_table("patients", src)[, c("subject_id", "dob")]
    adm <- get_table("admissions", src)[,
      c("subject_id", "hadm_id", "admittime", "dischtime")
    ]

    res <- merge(pat, adm, by = "subject_id")

    icu <- get_table("icustays", src)[,
      c("hadm_id", "icustay_id", "intime", "outtime")
    ]

    res <- merge(res, icu, by = "hadm_id")

    dt_cols <- c("dob", "admittime", "dischtime", "outtime")

    res <- res[, c(dt_cols) := lapply(.SD, as_dt, get("intime")),
               .SDcols = dt_cols]

    res <- rm_cols(res, "intime")
  }

  delayedAssign("id_map", mimic_id_map(source),
                assign.env = get_source(source, "aux"))

  delayedAssign("subject_id",
                origin_tbl(source, "patients", "subject_id", "dob"),
                assign.env = get_source(source, "aux"))

  delayedAssign("hadm_id",
                origin_tbl(source, "admissions", "hadm_id", "admittime"),
                assign.env = get_source(source, "aux"))

  delayedAssign("icustay_id",
                origin_tbl(source, "icustays", "icustay_id", "intime"),
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

  delayedAssign("patientid",
                origin_tbl(source, "general", "patientid", "admissiontime"),
                assign.env = get_source(source, "aux"))
}
