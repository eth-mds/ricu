
origin_tbl <- function(src, tbl, id, orig) {
  res <- get_table(tbl, src)[, c(id, orig)]
  res <- rename_cols(res, c("id", "origin"))
  res
}

map_id_cols <- function(subset = NULL, as_factor = FALSE) {

  opts <- c("patient", "hadm", "icustay")

  if (is.null(subset)) {
    return(opts)
  }

  assert_that(is.character(subset), length(subset) > 0L,
              all(subset %in% opts))

  res <- intersect(opts, subset)

  if (as_factor) {
    res <- factor(res, opts, ordered = TRUE)
  }

  res
}

map_in_cols <- function(ids = map_id_cols()) {
  setNames(c("birth", "hosp_in", "icu_in"), map_id_cols())[map_id_cols(ids)]
}

map_out_cols <- function(ids = map_id_cols()) {
  setNames(c("death", "hosp_out", "icu_out"), map_id_cols())[map_id_cols(ids)]
}

setup_mimic_aux_tables <- function(source) {

  mimic_id_map <- function(src) {

    as_dt <- function(x, y) round_to(difftime(x, y, units = "mins"))

    pat <- get_table("patients", src)[, c("subject_id", "dob", "dod")]
    adm <- get_table("admissions", src)[,
      c("subject_id", "hadm_id", "admittime", "dischtime")
    ]

    res <- merge(pat, adm, by = "subject_id")

    icu <- get_table("icustays", src)[,
      c("hadm_id", "icustay_id", "intime", "outtime")
    ]

    res <- merge(res, icu, by = "hadm_id")

    dt_cols <- c("dob", "dod", "admittime", "dischtime", "outtime")

    res <- res[, c(dt_cols) := lapply(.SD, as_dt, get("intime")),
               .SDcols = dt_cols]
    res <- rm_cols(res, "intime")
    res <- data.table::set(res, j = "intime", value = mins(0L))

    res <- data.table::setcolorder(res,
      c("subject_id", "hadm_id", "icustay_id", "dob", "admittime",
        "intime", "dod", "dischtime", "outtime")
    )

    res <- rename_cols(res, c(map_id_cols(), map_in_cols(), map_out_cols()))

    res
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

    id_cols   <- c("patienthealthsystemstayid", "patientunitstayid")
    time_cols <- c("hospitaladmitoffset", "hospitaldischargeoffset",
                   "unitdischargeoffset")

    res <- get_table("patient", src)[, c(id_cols, time_cols)]
    res <- res[, c("icu_in") := mins(0L)]

    res <- res[, c(time_cols) := lapply(.SD, as.difftime, units = "mins"),
               .SDcols = time_cols]

    res <- data.table::setcolorder(res,
      c(id_cols, time_cols[1L], "icu_in", time_cols[2L:3L])
    )

    ids <- map_id_cols(c("hadm", "icustay"))

    res <- rename_cols(res, c(ids, map_in_cols(ids), map_out_cols(ids)))

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
