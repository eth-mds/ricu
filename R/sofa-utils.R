
#' @export
sofa_data <- function(source, pafi_win_length = hours(2L),
                      pafi_mode = c("match_vals", "extreme_vals", "fill_gaps"),
                      fix_na_fio2 = TRUE, vent_win_length = hours(6L),
                      vent_min_win = mins(10L), gcs_win_length = hours(6L),
                      fix_na_gcs = TRUE, urine_min_win = hours(12L),
                      interval = hours(1L), patient_ids = NULL,
                      icu_limits = icu_stays(source, interval = interval),
                      col_cfg = get_col_config(source),
                      dictionary = get_config("concept-dict")) {

  assert_that(
    is_time(pafi_win_length, allow_neg = FALSE), pafi_win_length > interval,
    is.flag(fix_na_fio2), is_time(vent_win_length, allow_neg = FALSE),
    is_time(vent_min_win, allow_neg = FALSE), vent_min_win < vent_win_length,
    is_time(gcs_win_length, allow_neg = FALSE), gcs_win_length > interval,
    is.flag(fix_na_gcs), is_time(urine_min_win, allow_neg = FALSE),
    urine_min_win > interval, all.equal(interval(icu_limits), interval),
    urine_min_win <= hours(24L)
  )

  sel_non_null <- function(x, sel) Filter(Negate(is.null), x[sel])

  pafi_mode <- match.arg(pafi_mode)

  vent_conc <- c("vent_start", "vent_end")
  gcs_conc <- c("gcs_eye", "gcs_motor", "gcs_verbal", "gcs_total")
  sed_conc <- c("tracheostomy", "rass_scale")
  urine_conc <- c("urine_cumulative", "urine_events", "urine_hourly")

  agg_funs <- c(
    pa_o2 = "min", fi_o2 = "max", vent_start = "sum", vent_end = "sum",
    platelet_count = "min", bilirubin_total = "max", mean_bp = "min",
    norepinephrine = "max", epinephrine = "max", dopamine = "max",
    dobutamine = "max", gcs_eye = "min", gcs_motor = "min", gcs_verbal = "min",
    gcs_total = "min", tracheostomy = "sum", rass_scale = "min",
    creatinine = "max", urine_cumulative = "max", urine_events = "sum",
    urine_hourly = "sum"
  )

  dat_dict  <- get_concepts(source, setdiff(names(agg_funs), vent_conc),
                            dictionary)
  vent_dict <- get_concepts(source, vent_conc, dictionary)

  dat <- c(
    load_concepts(source, dat_dict,  patient_ids, col_cfg, agg_funs,
                  interval, merge_data = FALSE),
    load_concepts(source, vent_dict, patient_ids, col_cfg, agg_funs,
                  mins(1L), merge_data = FALSE)
  )

  dat[["pafi"]] <- sofa_pafi(
    dat[["pa_o2"]], dat[["fi_o2"]], pafi_win_length, pafi_mode, fix_na_fio2
  )

  dat[["vent"]] <- sofa_vent(
    dat[["vent_start"]], dat[["vent_end"]], vent_win_length, vent_min_win,
    interval
  )

  dat[["gcs"]] <- sofa_gcs(
    reduce(merge, sel_non_null(dat, gcs_conc), all = TRUE),
    reduce(merge, c(list(dat[["vent"]]), sel_non_null(dat, sed_conc)),
           all = TRUE),
    gcs_win_length, fix_na_gcs
  )

  dat[["urine"]] <- sofa_urine(
    reduce(merge, sel_non_null(dat, urine_conc), all = TRUE),
    icu_limits, urine_min_win, interval
  )

  dat[c("pa_o2", "fi_o2", vent_conc, gcs_conc, sed_conc, urine_conc)] <- NULL

  res <- reduce(merge, dat, all = TRUE)

  res <- res[is_true(pafi < 200) & !is_true(vent), pafi := 200]
  res <- res[, vent := NULL]

  res <- rename_cols(res,
    c("coag", "bili", "map", "dopa", "norepi", "dobu", "epi", "crea", "urine"),
    c("platelet_count", "bilirubin_total", "mean_bp", "dopamine",
      "norepinephrine", "dobutamine", "epinephrine", "creatinine", "urine_24")
  )

  res
}

sofa_pafi <- function(pao2, fio2, win_length, mode, fix_na_fio2) {

  assert_that(same_ts(pao2, fio2),
              has_cols(pao2, "pa_o2"), has_cols(fio2, "fi_o2"))

  if (identical(mode, "match_vals")) {

    res <- rbind(
      fio2[pao2, on = id_cols(fio2), roll = win_length],
      pao2[fio2, on = id_cols(fio2), roll = win_length]
    )
    res <- unique(res)

  } else {

    res <- merge(pao2, fio2, all = TRUE)

    if (identical(mode, "fill_gaps")) {
      res <- fill_gaps(res)
    }

    win_expr <- substitute(
      list(min_pa = min_fun(pa_o2), max_fi = max_fun(fi_o2)),
      list(min_fun = min_or_na, max_fun = max_or_na)
    )
    res <- slide_quo(res, win_expr, before = win_length, full_window = FALSE)

    rename_cols(res, c("pa_o2", "fi_o2"), c("min_pa", "max_fi"))
  }

  if (fix_na_fio2) {
    res <- res[is.na(fi_o2), fi_o2 := 21]
  }

  res <- res[!is.na(pa_o2) & !is.na(fi_o2) & fi_o2 != 0, ]
  res <- res[, pafi := 100 * pa_o2 / fi_o2]
  res <- res[, c("pa_o2", "fi_o2") := NULL]

  res
}

sofa_vent <- function(start, stop, win_length, min_length, interval) {

  final_units <- function(x) {
    units(x) <- units(interval)
    round_to(x, as.double(interval))
  }

  assert_that(same_ts(start, stop),
              interval(start) < min_length)

  units(win_length) <- time_unit(start)
  units(min_length) <- time_unit(start)

  start[, start_time := get(index(start))]
  stop[ , stop_time  := get(index(stop))]

  on.exit({
    set(start, j = "start_time", value = NULL)
    set(stop,  j = "stop_time",  value = NULL)
  })

  merged <- stop[start, roll = -win_length, on = id_cols(start)]

  merged <- merged[is.na(stop_time), stop_time := start_time + win_length]
  merged <- merged[stop_time - start_time >= min_length, ]

  merged <- merged[, c("start_time", "stop_time") := list(
    final_units(start_time), final_units(stop_time)
  )]

  res <- unique(
    expand_limits(merged, min_col = "start_time", max_col = "stop_time",
                  step_size = as.double(interval), id_cols = key(start))
  )
  res <- res[, vent := TRUE]

  res
}

sofa_gcs <- function(gcs, sed, win_length, set_na_max) {

  determine_sed <- function(fun, col, tbl) {

    if (col %in% data_cols(tbl)) {
      set(tbl, j = col, value = is_true(fun(tbl[[col]])))
    }

    invisible(NULL)
  }

  sed_feats <- c("tracheostomy", "rass_scale", "vent")
  sed_funs <- list(function(x) x > 0, function(x) x <= -2, identity)

  assert_that(same_interval(gcs, sed), all(data_cols(sed) %in% sed_feats))

  Map(determine_sed, sed_funs, sed_feats, list(sed))

  sed <- sed[, is_sed := Reduce(`|`, .SD), .SDcols = data_cols(sed)]
  sed <- set(sed, j = intersect(data_cols(sed), sed_feats), value = NULL)

  dat <- merge(gcs, sed, all = TRUE)

  gcs_names <- c("gcs_eye", "gcs_verbal", "gcs_motor")

  dat <- dat[is_true(is_sed), c(gcs_names) := list(4, 5, 6)]

  # TODO: perhaps expand before sliding?

  expr <- substitute(list(eye_imp = fun(gcs_eye), verb_imp = fun(gcs_verbal),
                          mot_imp = fun(gcs_motor), gcs = gcs_total),
                     list(fun = carry_backwards))
  dat <- slide_quo(dat, expr, before = win_length)

  if (set_na_max) {
    dat <- dat[, c(gcs_names) := list(
      replace_na(eye_imp, 4), replace_na(verb_imp, 5), replace_na(mot_imp, 6)
    )]
  }

  dat <- dat[, gcs := fifelse(is.na(gcs), eye_imp + verb_imp + mot_imp, gcs)]

  dat <- set(dat, j = c(gcs_names, "eye_imp", "verb_imp", "mot_imp"),
             value = NULL)

  dat
}

sofa_urine <- function(urine, limits, min_win, interval) {

  convert_dt <- function(x) as.double(x, units(interval))

  urine_sum <- local({

    min_steps <- ceiling(convert_dt(min_win) / as.double(interval))
    step_factor <- convert_dt(hours(24L)) / as.double(interval)

    function(x) {
      if (length(x) < min_steps) return(NA_real_)
      else sum(x, na.rm = TRUE) * step_factor / length(x)
    }
  })

  assert_that(identical(key(urine), key(limits)),
              has_name(limits, c("intime", "outtime")),
              all.equal(interval(urine), interval(limits)))

  if (!identical(data_cols(urine), "urine_events")) {
    stop("TODO")
  }

  res <- fill_gaps(urine, limits = limits, min_col = "intime",
                   max_col = "outtime")

  expr <- substitute(list(urine_24 = win_agg_fun(urine_events)),
                     list(win_agg_fun = urine_sum))

  slide_quo(res, expr, hours(24L))
}

sofa_vars <- function(pafi, vent, coag, bili, map, vaso, gcs, crea, urine,
                      admissions) {

  tables <- list(pafi, vent, coag, bili, map, vaso, gcs, crea, urine)

  assert_that(all(vapply(tables, same_ts, logical(1L), admissions)))

  dat <- reduce(merge, tables, all = TRUE)

  limits <- dat[, list(min = min(get(index(dat))), max = max(get(index(dat)))),
                by = c(key(dat))]
  limits <- merge(limits, admissions, by.x = key(dat), by.y = key(admissions),
                  all.x = TRUE)

  limits <- limits[,
    list(hadm_id = hadm_id,
         min = pmin(as.difftime(0, units = time_unit(dat)), min, na.rm = TRUE),
         max = pmax(max, hadm_time, na.rm = TRUE)),
  ]

  fill_gaps(dat, limits = limits)
}

sofa_window <- function(tbl,
                        pafi_win_fun   = min_or_na, vent_win_fun  = last_elem,
                        coag_win_fun   = min_or_na, bili_win_fun  = max_or_na,
                        map_win_fun    = min_or_na, dopa_win_fun  = max_or_na,
                        norepi_win_fun = max_or_na, dobu_win_fun  = max_or_na,
                        epi_win_fun    = max_or_na, gcs_win_fun   = min_or_na,
                        crea_win_fun   = max_or_na, urine_win_fun = min_or_na,
                        win_length = hours(24L)) {

  need_cols <- c("pafi", "vent", "coag", "bili", "map", "dopa", "norepi",
                 "dobu", "epi", "gcs", "crea", "urine_24")

  assert_that(has_cols(tbl, need_cols), has_no_gaps(tbl),
              is_time(win_length, allow_neg = FALSE))

  message("computing worst values over window")

  tbl <- slide_quo(tbl, substitute(
    list(
      pafi_win = pafi_wf(pafi),
      vent_win = vent_wf(vent),
      coag_win = coag_wf(coag),
      bili_win = bili_wf(bili),
      map_win = map_wf(map),
      dopa_win = dopa_wf(dopa),
      norepi_win = norepi_wf(norepi),
      dobu_win = dobu_wf(dobu),
      epi_win = epi_wf(epi),
      gcs_win = gcs_wf(gcs),
      crea_win = crea_wf(crea),
      urine_24_win = urine_wf(urine_24)
    ), list(
      pafi_wf = pafi_win_fun,
      vent_wf = vent_win_fun,
      coag_wf = coag_win_fun,
      bili_wf = bili_win_fun,
      map_wf = map_win_fun,
      dopa_wf = dopa_win_fun,
      norepi_wf = norepi_win_fun,
      dobu_wf = dobu_win_fun,
      epi_wf = epi_win_fun,
      gcs_wf = gcs_win_fun,
      crea_wf = crea_win_fun,
      urine_wf = urine_win_fun
    )
  ), before = win_length, full_window = FALSE)

  rename_cols(tbl, need_cols, paste0(need_cols, "_win"))
}

#' @export
sofa_compute <- function(tbl, na_val = 0L, na_val_resp = na_val,
                         na_val_coag = na_val, na_val_liver = na_val,
                         na_val_cardio = na_val, na_val_cns = na_val,
                         na_val_renal = na_val, impute_fun = NULL) {

  need_cols <- c("pafi", "coag", "bili", "map", "dopa", "norepi",
                 "dobu", "epi", "gcs", "crea", "urine")

  assert_that(has_cols(tbl, need_cols))

  sofa_cols <- c(
    "sofa_resp", "sofa_coag", "sofa_liver", "sofa_cardio", "sofa_cns",
    "sofa_renal"
  )

  tbl <- tbl[,
    c(sofa_cols) := list(
      sofa_resp(pafi, na_val_resp), sofa_coag(coag, na_val_coag),
      sofa_liver(bili, na_val_liver),
      sofa_cardio(map, dopa, norepi, dobu, epi, na_val_cardio),
      sofa_cns(gcs, na_val_cns), sofa_renal(crea, urine, na_val_renal)
    )
  ]

  tbl <- rm_cols(tbl, need_cols)

  if (!is.null(impute_fun)) {
    tbl <- tbl[, c(sofa_cols) := lapply(.SD, impute_fun), .SDcols = sofa_cols,
               by = key(tbl)]
  }

  tbl <- tbl[, sofa_score := sofa_resp + sofa_coag + sofa_liver +
                             sofa_cardio + sofa_cns + sofa_renal]

  tbl
}

sofa_resp <- function(pafi, na_val) {
  fifelse(
    is_true(pafi < 100), 4L, fifelse(
      is_true(pafi < 200), 3L, fifelse(
        is_true(pafi < 300), 2L, fifelse(
          is_true(pafi < 400), 1L, 0L, na_val
        )
      )
    )
  )
}

sofa_coag <- function(x, na_val) {
  fifelse(is.na(x), na_val, 4L - findInterval(x, c(20, 50, 100, 150)))
}

sofa_liver <- function(x, na_val) {
  fifelse(is.na(x), na_val, findInterval(x, c(1.2, 2, 6, 12)))
}

sofa_cardio <- function(map, dopa, norepi, dobu, epi, na_val) {
  fifelse(
    is_true(dopa > 15 | epi > 0.1 | norepi > 0.1), 4L, fifelse(
      is_true(dopa > 5 | epi <= 0.1 | norepi <= 0.1), 3L, fifelse(
        is_true(dopa <= 5 | !is.na(dobu)), 2L, fifelse(
          is_true(map < 70), 1L, 0L, na_val
        )
      )
    )
  )
}

sofa_cns <- function(x, na_val) {
  fifelse(is.na(x), na_val, 4L - findInterval(x, c(6, 10, 13, 15)))
}

sofa_renal <- function(cre, uri, na_val) {
  fifelse(
    is_true(cre >= 5 | uri < 200), 4L, fifelse(
      is_true((cre >= 3.5 & cre < 5) | uri < 500), 3L, fifelse(
        is_true(cre >= 2 & cre < 3.5), 2L, fifelse(
          is_true(cre >= 1.2 & cre < 2), 1L, 0L, na_val
        )
      )
    )
  )
}
