
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
                      interval = hours(1L), envir = "mimic") {

  sofa_pafi(pao2, fio2, ...)
}
