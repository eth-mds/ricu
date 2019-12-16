
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
