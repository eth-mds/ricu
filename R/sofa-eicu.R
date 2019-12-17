
eicu_pao2 <- function(interval = hours(1L), envir = "eicu") {

  message("fetching pao2")

  res <- eicu_lab("labresult", quote(labname == "paO2"),
                  interval = interval, envir = envir)
  res <- rm_cols(res, "labmeasurenameinterface")
  res <- rename_cols(res, c("hadm_id", "hadm_time", "pao2"),
                          c(key(res), index(res), "labresult"))

  make_unique(res, fun = min)
}

eicu_fio2 <- function(add_chart_data = TRUE, interval = hours(1L),
                         envir = "eicu") {

  message("fetching fio2")

  lab <- eicu_lab("labresult", quote(labname == "FiO2"),
                  interval = interval, envir = envir)
  lab <- rm_cols(lab, "labmeasurenameinterface")
  lab <- rename_cols(lab,
    c("hadm_id", "hadm_time", if (add_chart_data) "fi_lab" else "fio2"),
    c(key(lab), index(lab), "labresult")
  )

  lab <- make_unique(lab, fun = max)

  if (add_chart_data) {

    chart <- eicu_resp_chart("respchartvalue",
                             quote(respchartvaluelabel == "FiO2"),
                             interval = interval, envir = envir)

    chart <- rename_cols(chart, c("hadm_id", "hadm_time", "fi_chart"),
                                c(key(chart), index(chart), "respchartvalue"))

    chart <- make_unique(chart, fun = max)
    chart <- chart[, fi_chart := suppressWarnings(as.numeric(fi_chart))]

    res <- merge(lab, chart, all = TRUE)
    res <- res[, fio2 := fifelse(is.na(fi_lab), fi_chart, fi_lab)]
    res <- res[, c("fi_lab", "fi_chart") := NULL]

  } else {
    res <- lab
  }

  res <- res[fio2 <= 1, fio2 := fio2 * 100]
  res <- res[fio2 > 0, ]

  res
}

eicu_pafi <- function(pao2 = eicu_pao2(interval = interval, envir = envir),
                      fio2 = eicu_fio2(interval = interval, envir = envir),
                      ...,
                      interval = hours(1L), envir = "eicu") {

  sofa_pafi(pao2, fio2, ...)
}

eicu_vent_start <- function(interval = mins(1L), envir = "eicu") {

  message("fetching mechanical ventilation start info")

  care1 <- eicu_resp_care(rows = quote(ventstartoffset != 0),
                          time_col = "ventstartoffset", interval = interval,
                          envir = envir)
  care1 <- rename_cols(care1, c("hadm_id", "hadm_time"),
                              c(key(care1), index(care1)))

  care2 <- eicu_resp_care(rows = quote(priorventstartoffset != 0),
                          time_col = "priorventstartoffset",
                          interval = interval, envir = envir)
  care2 <- rename_cols(care2, c("hadm_id", "hadm_time"),
                              c(key(care2), index(care2)))

  chart <- eicu_resp_chart(
    rows = quote(
      respcharttypecat == "respFlowPtVentData" | (
        respchartvaluelabel == "RT Vent On/Off" &
        respchartvalue %in% c("Continued", "Start")
      )
    ),
    interval = interval, envir = envir
  )

  chart <- rename_cols(chart, c("hadm_id", "hadm_time"),
                              c(key(chart), index(chart)))

  unique(rbind(chart, care1, care2))
}

eicu_vent_stop <- function(interval = mins(1L), envir = "eicu") {

  message("fetching mechanical ventilation stop info")

  care1 <- eicu_resp_care(rows = quote(ventendoffset != 0),
                          time_col = "ventendoffset", interval = interval,
                          envir = envir)
  care1 <- rename_cols(care1, c("hadm_id", "hadm_time"),
                              c(key(care1), index(care1)))

  care2 <- eicu_resp_care(rows = quote(priorventendoffset != 0),
                          time_col = "priorventendoffset",
                          interval = interval, envir = envir)
  care2 <- rename_cols(care2, c("hadm_id", "hadm_time"),
                              c(key(care2), index(care2)))

  chart <- eicu_resp_chart(
    rows = quote(
      respchartvaluelabel == "RT Vent On/Off" &
        respchartvalue %in% c("Off", "off", "Suspended")
    ),
    interval = interval, envir = envir
  )

  chart <- rename_cols(chart, c("hadm_id", "hadm_time"),
                              c(key(chart), index(chart)))

  unique(rbind(chart, care1, care2))
}

eicu_vent <- function(vent_start = eicu_vent_start(envir = envir),
                      vent_stop = eicu_vent_stop(envir = envir),
                      ..., envir = "eicu") {

  sofa_vent(vent_start, vent_stop, ...)
}

eicu_coag <- function(interval = hours(1L), envir = "eicu") {

  message("fetching platelet counts")

  res <- eicu_lab("labresult", quote(labname == "platelets x 1000"),
                  interval = interval, envir = envir)
  res <- rm_cols(res, "labmeasurenameinterface")
  res <- rename_cols(res, c("hadm_id", "hadm_time", "coag"),
                          c(key(res), index(res), "labresult"))

  make_unique(res, fun = min)
}

eicu_bili <- function(interval = hours(1L), envir = "eicu") {

  message("fetching bilirubin measurements")

  res <- eicu_lab("labresult", quote(labname %in% "total bilirubin"),
                  interval = interval, envir = envir)
  res <- rm_cols(res, "labmeasurenameinterface")
  res <- rename_cols(res, c("hadm_id", "hadm_time", "bili"),
                          c(key(res), index(res), "labresult"))

  make_unique(res, fun = max)
}

eicu_map <- function(interval = hours(1L), envir = "eicu") {

  aper <- eicu_vital_aperiod("noninvasivemean", quote(!is.na(noninvasivemean)),
                             interval = interval, envir = envir)
  aper <- rename_cols(aper, c("hadm_id", "hadm_time", "map"),
                            c(key(aper), index(aper), "noninvasivemean"))
  peri <- eicu_vital_period("systemicmean", quote(!is.na(systemicmean)),
                            interval = interval, envir = envir)
  peri <- rename_cols(peri, c("hadm_id", "hadm_time", "map"),
                            c(key(peri), index(peri), "systemicmean"))

  make_unique(rbind(aper, peri), fun = min)
}

eicu_vaso <- function(interval = hours(1L), envir = "eicu") {

  get_drug <- function(regex, name, pat) {

    tbl <- eicu_inf_drug(c("drugname", "drugrate", "patientweight"),
                         substitute(grepl(rx, drugname, ignore.case = TRUE),
                                    list(rx = regex)),
                         interval = interval, envir = envir)

    tbl <- tbl[, c("drugrate", "patientweight") := suppressWarnings(
      list(as.numeric(drugrate), as.numeric(patientweight)))
    ]

    tbl <- merge(tbl, pat, by = key(tbl), all.x = TRUE)
    tbl <- tbl[is.na(patientweight), patientweight := admweight]

    tbl <- tbl[
      grepl(" \\(\\)$", drugname),
      drugname := sub(" \\(\\)$", " (mcg/min)", drugname)
    ]
    tbl <- tbl[
      grepl("\\(mg/", drugname), c("drugname", "drugrate") := list(
      sub("\\(mg/", "(mcg/", drugname), drugrate * 1000)
    ]
    tbl <- tbl[
      grepl("/hr\\)$", drugname), c("drugname", "drugrate") := list(
      sub("/hr\\)$", "/min)", drugname), drugrate / 60)
    ]
    tbl <- tbl[
      grepl("mcg/min", drugname), c("drugname", "drugrate") := list(
      sub("mcg/min", "mcg/kg/min", drugname), drugrate / patientweight)
    ]

    # currently all [volume/time] measurements are lost

    tbl <- tbl[grep("\\(mcg/kg/min\\)$", drugname), ]
    tbl <- tbl[!is.na(drugrate), ]
    tbl <- tbl[, c("drugname", "patientweight", "admweight") := NULL]

    tbl <- rename_cols(tbl, c("hadm_id", "hadm_time", name),
                            c(key(tbl), index(tbl), "drugrate"))

    make_unique(tbl, fun = max)
  }

  patient <- get_table("patient", envir)
  patient <- patient[, c("patienthealthsystemstayid", "admissionweight")]
  patient <- patient[, list(admweight = mean(admissionweight, na.rm = TRUE)),
                     by = "patienthealthsystemstayid"]

  res <- Map(get_drug, c("norepi|levophed", "^epineph", "^dopa", "dobu"),
                       c("norepi",          "epi",      "dopa",  "dobu"),
             MoreArgs = list(pat = patient))

  reduce(merge, res, all = TRUE)
}

eicu_gcs <- function(win_length = hours(6L), set_na_max = TRUE, ...,
                     interval = hours(1L), envir = "eicu") {

  message("fetching gcs scores")

  tbl <- eicu_nurse_chart("nursingchartvalue",
                          quote(nursingchartcelltypevalname == "GCS Total"),
                          interval = interval, envir = envir)
  tbl <- rename_cols(tbl, c("hadm_id", "hadm_time", "gcs"),
                            c(key(tbl), index(tbl), "nursingchartvalue"))

  sed <- eicu_vent(..., interval = interval, envir = envir)

  res <- merge(tbl, sed, by = id_cols(tbl), all.x = TRUE)
  res <- res[is.na(vent), vent := FALSE]

  if (any(res[["vent"]])) {
    message("setting ", sum(res[["vent"]]),
            " rows to max gcs values due to ventilation.")
    res <- res[(vent), gcs := 15]
  }

  res <- rm_cols(res, "vent")
  res <- make_unique(res, fun = min)

  # TODO: expand before sliding, also in mimic

  expr <- substitute(list(gcs_imp = fun(gcs)), list(fun = carry_backwards))
  res <- slide_quo(res, expr, before = win_length)

  if (set_na_max) {
    res[, c("gcs", "gcs_imp") := list(replace_na(gcs_imp, 15), NULL)]
  } else {
    rename_cols(res, "gcs", "gcs_imp")
  }
}

eicu_crea <- function(interval = hours(1L), envir = "eicu") {

  message("fetching bilirubin measurements")

  res <- eicu_lab("labresult", quote(labname %in% "creatinine"),
                  interval = interval, envir = envir)
  res <- rm_cols(res, "labmeasurenameinterface")
  res <- rename_cols(res, c("hadm_id", "hadm_time", "crea"),
                          c(key(res), index(res), "labresult"))

  make_unique(res, fun = max)
}

eicu_urine24 <- function(min_win = hours(12L), interval = hours(1L),
                         envir = "eicu") {

  message("fetching urine measurements")

  urine <- eicu_in_out("cellvaluenumeric",
                       quote(celllabel %in% c("Urine", "URINE CATHETER")),
                       id_cols = c("patienthealthsystemstayid",
                                   "patientunitstayid"),
                       interval = interval, envir = envir)

  urine <- rename_cols(urine, c("hadm_id", "icustay_id", "hadm_time", "urine"),
                       c(key(urine), index(urine), "cellvaluenumeric"))

  urine <- make_unique(urine, fun = sum_or_na)

  icu_limits <- eicu_patient("unitdischargeoffset",
                             id_cols = c("patienthealthsystemstayid",
                                         "patientunitstayid"),
                             interval = interval, envir = envir)

  icu_limits <- rename_cols(icu_limits,
    c("hadm_id", "icustay_id", "min", "max"),
    c(key(icu_limits), "unitadmitoffset", "unitdischargeoffset")
  )

  sofa_urine24(urine, icu_limits, min_win, interval)
}

eicu_sofa_vars <- function(
  pafi  = eicu_pafi(interval = interval, envir = envir),
  vent  = eicu_vent(interval = interval, envir = envir),
  coag  = eicu_coag(interval = interval, envir = envir),
  bili  = eicu_bili(interval = interval, envir = envir),
  map   = eicu_map(interval = interval, envir = envir),
  vaso  = eicu_vaso(interval = interval, envir = envir),
  gcs   = eicu_gcs(interval = interval, envir = envir),
  crea  = eicu_crea(interval = interval, envir = envir),
  urine = eicu_urine24(interval = interval, envir = envir),
  interval = hours(1L), envir = "eicu") {

  hadm <- eicu_patient(time_col = "hospitaldischargeoffset",
                       interval = interval(pafi), envir = envir)
  hadm <- rename_cols(hadm, c("hadm_id", "hadm_time"),
                            c(key(hadm), index(hadm)))

  sofa_vars(pafi, vent, coag, bili, map, vaso, gcs, crea, urine, unique(hadm))
}

#' @export
eicu_sofa <- function(interval = hours(1L), envir = "eicu") {

  tbl <- eicu_sofa_vars(interval = interval, envir = envir)
  tbl <- sofa_window(tbl)
  tbl <- sofa_compute(tbl)

  tbl
}
