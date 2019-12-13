
mimic_pao2 <- function(interval = hours(1L), envir = "mimic") {

  message("fetching pao2")

  res <- mimic_lab(cols = "valuenum", rows = quote(itemid == 50821L),
                   interval = interval, envir = envir)
  res <- rm_cols(res, "valueuom")
  res <- rename_cols(res, c("hadm_time", "pao2"), c(index(res), "valuenum"))

  make_unique(res, fun = min)
}

mimic_fio2 <- function(add_chart_data = TRUE, interval = hours(1L),
                       envir = "mimic") {

  message("fetching fio2")

  lab <- mimic_lab(cols = "valuenum", rows = quote(itemid == 50816L),
                   interval = interval, envir = envir)
  lab <- rm_cols(lab, "valueuom")
  lab <- rename_cols(lab,
    c("hadm_time", if (add_chart_data) "fi_lab" else "fio2"),
    c(index(lab), "valuenum")
  )

  lab <- make_unique(lab, fun = max)

  if (add_chart_data) {

    chart <- mimic_chart(cols = "valuenum",
                         rows = quote(itemid %in% c(3420L, 223835L)),
                         interval = interval, envir = envir)
    chart <- rm_cols(chart, "valueuom")
    chart <- rename_cols(chart, c("hadm_time", "fi_chart"),
                         c(index(chart), "valuenum"))

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

mimic_pafi <- function(pao2 = mimic_pao2(interval = interval, envir = envir),
                       fio2 = mimic_fio2(interval = interval, envir = envir),
                       ...,
                       interval = hours(1L), envir = "mimic") {

  sofa_pafi(pao2, fio2, ...)
}

mimic_vent_start <- function(interval = mins(1L), envir = "mimic") {

  message("fetching mechanical ventilation start info")

  cv_ids <- c(     1L,     60L,    218L,    221L,    223L,    436L,    437L,
                 444L,    445L,    448L,    449L,    450L,    459L,    501L,
                 502L,    503L,    505L,    506L,    535L,    543L,    639L,
                 654L,    667L,    668L,    669L,    670L,    671L,    672L,
                 681L,    682L,    683L,    684L,    686L,   1211L,   1340L,
                1486L,   1600L,   1655L,   2000L,   3459L,   5865L,   5866L)
  mv_ids <- c(220339L, 223848L, 223849L, 224419L, 224684L, 224685L, 224686L,
              224687L, 224695L, 224696L, 224697L, 224700L, 224701L, 224702L,
              224703L, 224704L, 224705L, 224706L, 224707L, 224709L, 224738L,
              224746L, 224747L, 224750L, 226873L, 227187L)

  res <- mimic_chart(rows = substitute(itemid %in% ids,
                                       list(ids = c(cv_ids, mv_ids))),
                     interval = interval, envir = envir)
  res <- rename_cols(res, "hadm_time", index(res))

  unique(res)
}

mimic_vent_stop <- function(interval = mins(1L), envir = "mimic") {

  message("fetching mechanical ventilation stop info")

  proc <- mimic_proc_mv(rows = quote(itemid %in% c(227194L, 225468L, 225477L)),
                        interval = interval, envir = envir)
  proc <- rename_cols(proc, "hadm_time", index(proc))

  chart <- mimic_chart(rows = quote(itemid %in% c(467L, 469L, 226732L)),
                       interval = interval, envir = envir)
  chart <- rename_cols(chart, "hadm_time", index(chart))

  unique(rbind(proc, chart))
}

mimic_vent <- function(vent_start = mimic_vent_start(envir = envir),
                       vent_stop = mimic_vent_stop(envir = envir),
                       win_length = hours(6L), min_length = mins(10L),
                       interval = hours(1L), envir = "mimic") {

  final_units <- function(x) {
    units(x) <- units(interval)
    round_to(x, as.double(interval))
  }

  assert_that(same_ts(vent_start, vent_stop),
              is_time(win_length, allow_neg = FALSE),
              is_time(min_length, allow_neg = FALSE),
              min_length < win_length, interval(vent_start) < min_length)

  units(win_length) <- time_unit(vent_start)
  units(min_length) <- time_unit(vent_start)

  vent_start[, start_time := get(index(vent_start))]
  vent_stop[ , stop_time  := get(index(vent_stop))]

  on.exit({
    set(vent_start, j = "start_time", value = NULL)
    set(vent_stop,  j = "stop_time",  value = NULL)
  })

  merged <- vent_stop[vent_start, roll = -win_length, on = id_cols(vent_start)]

  merged <- merged[is.na(stop_time), stop_time := start_time + win_length]
  merged <- merged[stop_time - start_time >= min_length, ]

  merged <- merged[, c("start_time", "stop_time") := list(
    final_units(start_time), final_units(stop_time)
  )]

  res <- unique(
    expand_limits(merged, min_col = "start_time", max_col = "stop_time",
                  step_size = as.double(interval), id_cols = key(vent_start))
  )
  res <- res[, vent := TRUE]

  res
}

mimic_coag <- function(interval = hours(1L), envir = "mimic") {

  message("fetching platelet counts")

  res <- mimic_lab(cols = "valuenum", rows = quote(itemid == 51265L),
                   interval = interval, envir = envir)
  res <- rm_cols(res, "valueuom")
  res <- rename_cols(res, c("hadm_time", "coag"), c(index(res), "valuenum"))

  make_unique(res, fun = min)
}

mimic_bili <- function(interval = hours(1L), envir = "mimic") {

  message("fetching bilirubin measurements")

  res <- mimic_lab(cols = "valuenum", rows = quote(itemid == 50885L),
                   interval = interval, envir = envir)
  res <- rm_cols(res, "valueuom")
  res <- rename_cols(res, c("hadm_time", "bili"), c(index(res), "valuenum"))

  make_unique(res, fun = max)
}

mimic_map <- function(interval = hours(1L), envir = "mimic") {

  message("fetching map measurements")

  items <- c(52L, 443L, 456L, 6702L, 220052L, 220181L, 225312L)

  chart <- mimic_chart(cols = "valuenum",
                       rows = substitute(itemid %in% ids, list(ids = items)),
                       interval = interval, envir = envir)
  chart <- rm_cols(chart, "valueuom")
  chart <- rename_cols(chart, c("hadm_time", "map"),
                       c(index(chart), "valuenum"))

  make_unique(chart, fun = min)
}

mimic_vaso <- function(interval = hours(1L), envir = "mimic") {

  get_items <- function(items, fun) {

    res <- vector("list", length(items))

    for (i in seq_along(items)) {

      tmp <- fun(cols = "rate",
                 rows = substitute(itemid %in% ids, list(ids = items[[i]])),
                 interval = interval, envir = envir)
      tmp <- rm_cols(tmp, "rateuom")
      tmp <- rename_cols(tmp, c("hadm_time", names(items)[i]),
                         c(index(tmp), "rate"))

      res[[i]] <- make_unique(tmp, fun = max)
    }

    reduce(merge, res, all = TRUE)
  }

  message("fetching vasopressor dosages")

  cv_ids <- list(norepi = c(30047L, 30120L),
                 epi    = c(30044L, 30119L, 30309L),
                 dopa   = c(30043L, 30125L, 30307L),
                 dobu   = c(30042L, 30306L))

  mv_ids <- list(norepi = 221906L,
                 epi    = 221289L,
                 dopa   = 221662L,
                 dobu   = 221653L)

  cv_dat <- get_items(cv_ids, mimic_input_cv)
  mv_dat <- get_items(mv_ids, mimic_input_mv)

  make_unique(rbind(cv_dat, mv_dat), fun = max)
}

mimic_gcs <- function(win_length = hours(6L), set_na_max = TRUE,
                      interval = hours(1L), envir = "mimic") {

  get_di <- function(itms, name) {

    res <- mimic_chart(cols = c("valuenum", if (name == "verbal") "value"),
                       rows = substitute(itemid %in% ids & !fun(error, 1L),
                                         list(ids = itms, fun = is_val)),
                       interval = interval, envir = envir)

    res <- res[!is.na(valuenum), ]

    if (name == "verbal") {
      assert_that(!any(res[["valuenum"]] == 0))
      res[value == "1.0 ET/Trach" | value == "No Response-ETT", valuenum := 0]
    }

    res <- rm_cols(res, "valueuom", "value")
    res <- rename_cols(res, c("hadm_time", name), c(index(res), "valuenum"))

    make_unique(res,
      fun = if (name == "verbal") function(x) min(x[x > 0], max(x)) else min
    )
  }

  repl_na <- function(x, val) {

    repl <- is.na(x)
    if (any(repl)) {
      message("replacing ", sum(repl), " gcs values with ", val)
      x[repl] <- val
    }

    x
  }

  impute_na_prev <- function(x) {

    res <- x[length(x)]

    if (is.na(res)) {
      not_na <- !is.na(x)
      if (any(not_na)) return(tail(x[not_na], n = 1L))
    }

    res
  }

  message("fetching gcs scores")

  itms <- list(eye    = c(184L, 220739L),
               verbal = c(723L, 223900L),
               motor  = c(454L, 223901L))
  nams <- names(itms)

  res <- Map(get_di, itms, nams)

  res <- reduce(merge, res, all = TRUE)

  zeros <- reduce(`|`, res[, lapply(.SD, is_val, 0), .SDcols = nams])

  if (any(zeros)) {
    message("setting ", sum(zeros), " rows to max gcs values due to sedation")
    res <- res[zeros, c(nams) := list(4, 5, 6)]
  }

  expr <- substitute(list(eye_imp = fun(eye), verb_imp = fun(verbal),
                          mot_imp = fun(motor)),
                     list(fun = impute_na_prev))
  res <- slide_quo(res, expr, before = win_length)

  if (set_na_max) {
    res <- res[, c(nams) := list(
      repl_na(eye_imp, 4), repl_na(verb_imp, 5), repl_na(mot_imp, 6)
    )]
  }

  res <- res[, gcs := eye_imp + verb_imp + mot_imp]

  res <- set(res, j = c(nams, "eye_imp", "verb_imp", "mot_imp"),
             value = NULL)

  res
}

mimic_crea <- function(interval = hours(1L), envir = "mimic") {

  message("fetching creatinine measurements")

  res <- mimic_lab(cols = "valuenum", rows = quote(itemid == 50912L),
                   interval = interval, envir = envir)
  res <- rm_cols(res, "valueuom")
  res <- rename_cols(res, c("hadm_time", "crea"), c(index(res), "valuenum"))

  make_unique(res, fun = max)
}

mimic_urine24 <- function(min_win = hours(12L), interval = hours(1L),
                          envir = "mimic") {

  convert_dt <- function(x) as.double(x, units(interval))

  urine_sum <- local({

    min_steps <- ceiling(convert_dt(min_win) / as.double(interval))
    step_factor <- convert_dt(hours(24L)) / as.double(interval)

    function(x) {
      if (length(x) <= min_steps) return(NA_real_)
      else sum(x, na.rm = TRUE) * step_factor / length(x)
    }
  })

  message("fetching urine measurements")

  assert_that(is_time(min_win, allow_neg = FALSE), min_win <= hours(24L))

  itms <- c( 40055L,  40056L,  40057L,  40065L,  40069L,  40085L,  40086L,
             40094L,  40096L,  40405L,  40428L,  40473L,  40715L,  43175L,
            226557L, 226558L, 226559L, 226560L, 226561L, 226563L, 226564L,
            226565L, 226566L, 226567L, 226584L, 227510L)

  res <- mimic_output("value",
                      substitute(itemid %in% ids, list(ids = itms)),
                      id_cols = c("hadm_id", "icustay_id"),
                      interval = interval, envir = envir)

  res <- rm_cols(res, "valueuom")
  res <- rename_cols(res, c("hadm_time", "urine"), c(index(res), "value"))
  res <- make_unique(res, fun = sum)

  icu_limits <- mimic_icustays("outtime", id_cols = c("hadm_id", "icustay_id"),
                               interval = interval, envir = envir)

  res <- fill_gaps(res, limits = icu_limits, min_col = "intime",
                   max_col = "outtime")

  expr <- substitute(list(urine_24 = win_agg_fun(urine)),
                     list(win_agg_fun = urine_sum))

  res <- slide_quo(res, expr, hours(24L))

  res <- rm_cols(res, "icustay_id")

  res
}

mimic_sofa_vars <- function(
  pafi  = mimic_pafi(interval = interval, envir = envir),
  vent  = mimic_vent(interval = interval, envir = envir),
  coag  = mimic_coag(interval = interval, envir = envir),
  bili  = mimic_bili(interval = interval, envir = envir),
  map   = mimic_map(interval = interval, envir = envir),
  vaso  = mimic_vaso(interval = interval, envir = envir),
  gcs   = mimic_gcs(interval = interval, envir = envir),
  crea  = mimic_crea(interval = interval, envir = envir),
  urine = mimic_urine24(interval = interval, envir = envir),
  interval = hours(1L), envir = "mimic") {

  tables <- list(pafi, vent, coag, bili, map, vaso, gcs, crea, urine)

  assert_that(all(vapply(tables, same_ts, logical(1L), tables[[1L]])))

  dat <- reduce(merge, tables, all = TRUE)

  hadm <- mimic_admissions(interval = interval(pafi), envir = envir)

  limits <- dat[, list(min = min(get(index(dat))), max = max(get(index(dat)))),
                by = c(key(dat))]
  limits <- merge(limits, hadm, by.x = key(dat), by.y = "hadm_id",
                  all.x = TRUE)

  limits <- limits[,
    list(hadm_id = hadm_id,
         min = pmin(as.difftime(0, units = time_unit(dat)), min, na.rm = TRUE),
         max = pmax(max, dischtime, na.rm = TRUE)),
  ]

  fill_gaps(dat, limits = limits)
}

#' @export
mimic_sofa <- function(interval = hours(1L), envir = "mimic") {

  tbl <- mimic_sofa_vars(interval = interval, envir = envir)
  tbl <- sofa_window(tbl)
  tbl <- sofa_compute(tbl)

  tbl
}
