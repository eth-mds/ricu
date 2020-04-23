
origin_tbl <- function(src, tbl, id, orig) {
  res <- get_tbl(tbl, src)[, c(id, orig)]
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

as_dt_min <- function(x, y) round_to(difftime(x, y, units = "mins"))

setup_mimic_aux_tables <- function(cfg) {

  mimic_id_map <- function(src) {

    pat <- get_tbl("patients", src)[, c("subject_id", "dob", "dod")]
    adm <- get_tbl("admissions", src)[,
      c("subject_id", "hadm_id", "admittime", "dischtime")
    ]

    res <- merge(pat, adm, by = "subject_id")

    icu <- get_tbl("icustays", src)[,
      c("hadm_id", "icustay_id", "intime", "outtime")
    ]

    res <- merge(res, icu, by = "hadm_id")

    dt_cols <- c("dob", "dod", "admittime", "dischtime", "outtime")

    res <- res[, c(dt_cols) := lapply(.SD, as_dt_min, get("intime")),
               .SDcols = dt_cols]
    res <- rm_cols(res, "intime")
    res <- res[, c("intime") := mins(0L)]

    res <- data.table::setcolorder(res,
      c("subject_id", "hadm_id", "icustay_id", "dob", "admittime",
        "intime", "dod", "dischtime", "outtime")
    )

    res <- rename_cols(res, c(map_id_cols(), map_in_cols(), map_out_cols()))

    res
  }

  source <- get_source(cfg)

  list(
    id_map = mimic_id_map(source),
    subject_id = origin_tbl(source, "patients", "subject_id", "dob"),
    hadm_id = origin_tbl(source, "admissions", "hadm_id", "admittime"),
    icustay_id = origin_tbl(source, "icustays", "icustay_id", "intime")
  )
}

setup_eicu_aux_tables <- function(cfg) {

  eicu_id_map <- function(src) {

    id_cols   <- c("patienthealthsystemstayid", "patientunitstayid")
    time_cols <- c("hospitaladmitoffset", "hospitaldischargeoffset",
                   "unitdischargeoffset")

    res <- get_tbl("patient", src)[, c(id_cols, time_cols)]
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

  source <- get_source(cfg)

  list(id_map = eicu_id_map(source))
}

setup_hirid_aux_tables <- function(cfg) {

  hirid_id_map <- function(src) {

    ind_fun <- function(id, index) {

      tmp <- data.table::setDT(list(id = id, index = index))

      ind <- tmp[, .I[which.max(get("index"))], by = "id"][["V1"]]

      res <- logical(nrow(tmp))
      res[ind] <- TRUE

      res
    }

    quo <- substitute(my_fun(patientid, datetime), list(my_fun = ind_fun))
    dat <- prt::subset_quo(get_tbl("observations", src), quo,
                           c("patientid", "datetime"), part_safe = TRUE)

    adm <- origin_tbl(src, "general", "patientid", "admissiontime")
    dat <- merge(dat, adm, by.x = "patientid", by.y = "id")

    new <- c(map_in_cols("icustay"), map_out_cols("icustay"))
    dat <- dat[, c(new) := list(mins(0L),
                                as_dt_min(get("datetime"), get("origin")))]

    dat <- rm_cols(dat, setdiff(colnames(dat), c("patientid", new)))
    dat <- rename_cols(dat, "icustay", "patientid")

    dat
  }

  source <- get_source(cfg)

  list(
    id_map = hirid_id_map(source),
    patientid = origin_tbl(source, "general", "patientid", "admissiontime")
  )
}
