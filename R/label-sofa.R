
#' @export
sofa_data <- function(source, pafi_win_length = hours(2L),
                      pafi_mode = c("match_vals", "extreme_vals", "fill_gaps"),
                      fix_na_fio2 = TRUE, vent_win_length = hours(6L),
                      vent_min_win = mins(10L), gcs_win_length = hours(6L),
                      fix_na_gcs = TRUE, urine_min_win = hours(12L),
                      interval = hours(1L), id_type = "hadm",
                      patient_ids = NULL,
                      icu_limits = icu_stays(source, interval = interval),
                      col_cfg = get_col_config(source, "all"),
                      dictionary = read_dictionary("concept-dict")) {

  if (!identical(id_type, "hadm")) stop ("TODO")

  assert_that(
    is_time(pafi_win_length, allow_neg = FALSE), pafi_win_length > interval,
    is.flag(fix_na_fio2), is_time(vent_win_length, allow_neg = FALSE),
    is_time(vent_min_win, allow_neg = FALSE), vent_min_win < vent_win_length,
    is_time(gcs_win_length, allow_neg = FALSE), gcs_win_length > interval,
    is.flag(fix_na_gcs), is_time(urine_min_win, allow_neg = FALSE),
    urine_min_win > interval, urine_min_win <= hours(24L)
  )

  if (!is.null(icu_limits)) {
    assert_that(all.equal(interval(icu_limits), interval))
  }

  sel_non_null <- function(x, sel) Filter(Negate(is.null), x[sel])

  pafi_mode <- match.arg(pafi_mode)

  vent_conc <- c("vent_start", "vent_end")
  gcs_conc <- c("gcs_eye", "gcs_motor", "gcs_verbal", "gcs_total")
  sed_conc <- c("tracheostomy", "rass_scale")
  urine_conc <- c("urine_cumulative", "urine_events")

  agg_funs <- c(
    pa_o2 = "min", fi_o2 = "max", vent_start = "sum", vent_end = "sum",
    platelet_count = "min", bilirubin_total = "max", mean_bp = "min",
    norepinephrine = "max", epinephrine = "max", dopamine = "max",
    dobutamine = "max", gcs_eye = "min", gcs_motor = "min", gcs_verbal = "min",
    gcs_total = "min", tracheostomy = "sum", rass_scale = "min",
    creatinine = "max", urine_cumulative = "max", urine_events = "sum"
  )

  dat_dict  <- dictionary[setdiff(names(agg_funs), vent_conc), source = source]
  vent_dict <- dictionary[vent_conc, source = source]

  dat <- c(
    load_concepts(source, dat_dict, id_type, patient_ids, col_cfg, agg_funs,
                  interval, merge_data = FALSE),
    load_concepts(source, vent_dict, id_type, patient_ids, col_cfg, agg_funs,
                  mins(1L), merge_data = FALSE)
  )
  names(dat) <- chr_ply(dat, data_cols)

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

  res <- res[is_true(get("pafi") < 200) & !is_true(get("vent")),
             c("pafi") := 200]

  res <- rm_cols(res, "vent")

  rename <- c(
    platelet_count = "coag", bilirubin_total = "bili", mean_bp = "map",
    dopamine = "dopa", norepinephrine = "norepi", dobutamine = "dobu",
    epinephrine = "epi", creatinine = "crea", urine_24 = "urine"
  )
  rename <- rename[intersect(names(rename), colnames(res))]

  res <- rename_cols(res, rename, names(rename))

  res
}

sofa_pafi <- function(pao2, fio2, win_length, mode, fix_na_fio2) {

  assert_that(same_ts(pao2, fio2),
              has_cols(pao2, "pa_o2"), has_cols(fio2, "fi_o2"))

  pa_o2 <- fi_o2 <- NULL

  if (identical(mode, "match_vals")) {

    res <- rbind(
      fio2[pao2, on = meta_cols(fio2), roll = win_length],
      pao2[fio2, on = meta_cols(fio2), roll = win_length]
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
  res <- res[, c("pafi") := 100 * pa_o2 / fi_o2]
  res <- res[, c("pa_o2", "fi_o2") := NULL]

  res
}

sofa_vent <- function(start, stop, win_length, min_length, interval) {

  final_units <- function(x) {
    units(x) <- units(interval)
    round_to(x, as.double(interval))
  }

  assert_that(interval(start) < min_length)

  datetime <- start_time <- stop_time <- NULL

  units(win_length) <- time_unit(start)
  units(min_length) <- time_unit(start)

  if (is.null(stop)) {

    merged <- data.table::copy(start)
    merged <- merged[,
      c("start_time", "stop_time") := list(datetime, datetime + win_length)
    ]

  } else {

    assert_that(same_ts(start, stop))

    start[, start_time := get(index(start))]
    stop[ , stop_time  := get(index(stop))]

    on.exit({
      set(start, j = "start_time", value = NULL)
      set(stop,  j = "stop_time",  value = NULL)
    })

    merged <- stop[start, roll = -win_length, on = meta_cols(start)]
    merged <- merged[is.na(stop_time), stop_time := start_time + win_length]
  }

  merged <- merged[stop_time - start_time >= min_length, ]

  merged <- merged[, c("start_time", "stop_time") := list(
    final_units(start_time), final_units(stop_time)
  )]

  res <- unique(
    expand_limits(merged, min_col = "start_time", max_col = "stop_time",
                  step_size = as.double(interval), id_cols = id(start))
  )
  res <- res[, c("vent") := TRUE]

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

  sed <- sed[, c("is_sed") := Reduce(`|`, .SD), .SDcols = data_cols(sed)]
  sed <- set(sed, j = intersect(data_cols(sed), sed_feats), value = NULL)

  dat <- merge(gcs, sed, all = TRUE)

  dat <- dat[is_true(get("is_sed")),
             c("gcs_eye", "gcs_verbal", "gcs_motor") := list(4, 5, 6)]

  if ("gcs_total" %in% colnames(dat)) {

    expr <- substitute(
      list(eye_imp = fun(gcs_eye), verb_imp = fun(gcs_verbal),
           mot_imp = fun(gcs_motor), tot_imp = fun(gcs_total)),
      list(fun = carry_backwards)
    )

  } else {

    expr <- substitute(list(eye_imp = fun(gcs_eye), verb_imp = fun(gcs_verbal),
                            mot_imp = fun(gcs_motor)),
                       list(fun = carry_backwards))
  }

  dat <- fill_gaps(dat)
  dat <- slide_quo(dat, expr, before = win_length)

  if (set_na_max) {

    if ("tot_imp" %in% colnames(dat)) {

      dat <- dat[, c("eye_imp", "verb_imp", "mot_imp", "tot_imp") := list(
        replace_na(get("eye_imp"), 4), replace_na(get("verb_imp"), 5),
        replace_na(get("mot_imp"), 6), replace_na(get("tot_imp"), 15)
      )]

    } else {

      dat <- dat[, c("eye_imp", "verb_imp", "mot_imp") := list(
        replace_na(get("eye_imp"), 4), replace_na(get("verb_imp"), 5),
        replace_na(get("mot_imp"), 6)
      )]
    }
  }

  if ("tot_imp" %in% colnames(dat)) {

    dat <- dat[, c("gcs") := fifelse(
      is.na(get("tot_imp")),
      get("eye_imp") + get("verb_imp") + get("mot_imp"),
      get("tot_imp")
    )]

  } else {

    dat <- dat[, c("gcs") := get("eye_imp") + get("verb_imp") + get("mot_imp")]
  }

  dat <- rm_cols(dat, c("eye_imp", "verb_imp", "mot_imp", "tot_imp"))

  dat
}

sofa_urine <- function(urine, limits, min_win, interval) {

  convert_dt <- function(x) as.double(x, units(interval))

  do_diff <- function(x) {
    res <- c(diff(x), 0)
    ifelse(res < 0, x + res, res)
  }

  urine_sum <- local({

    min_steps <- ceiling(convert_dt(min_win) / as.double(interval))
    step_factor <- convert_dt(hours(24L)) / as.double(interval)

    function(x) {
      if (length(x) < min_steps) return(NA_real_)
      else sum(x, na.rm = TRUE) * step_factor / length(x)
    }
  })

  if (has_name(urine, "urine_cumulative")) {
    urine <- urine[, c("urine_events") := do_diff(get("urine_cumulative")),
                   by = id(urine)]
  }

  if (is.null(limits)) {
    limits <- as_ts_tbl(
      urine[, list(intime = min(get(index(urine))),
                   outtime = max(get(index(urine)))), by = id(urine)],
      id(urine), id_opts(urine), "intime", interval(urine)
    )
  }

  assert_that(identical(id(urine), id(limits)),
              has_name(limits, c("intime", "outtime")),
              all.equal(interval(urine), interval(limits)))

  limits <- merge(limits, unique(urine[, id(limits), with = FALSE]),
                  all.y = TRUE, by = id(limits))

  res <- fill_gaps(urine, limits = limits, min_col = "intime",
                   max_col = "outtime")

  expr <- substitute(list(urine_24 = win_agg_fun(urine_events)),
                     list(win_agg_fun = urine_sum))

  slide_quo(res, expr, hours(24L))
}

#' @export
sofa_window <- function(tbl,
                        pafi_win_fun  = min_or_na, coag_win_fun   = min_or_na,
                        bili_win_fun  = max_or_na, map_win_fun    = min_or_na,
                        dopa_win_fun  = max_or_na, norepi_win_fun = max_or_na,
                        dobu_win_fun  = max_or_na, epi_win_fun    = max_or_na,
                        gcs_win_fun   = min_or_na, crea_win_fun   = max_or_na,
                        urine_win_fun = min_or_na, win_length = hours(24L)) {

  need_cols <- c("pafi", "coag", "bili", "map", "dopa", "norepi", "dobu",
                 "epi", "gcs", "crea", "urine")

  assert_that(has_cols(tbl, need_cols),
              is_time(win_length, allow_neg = FALSE))

  tbl <- fill_gaps(tbl)

  tbl <- slide_quo(tbl, substitute(
    list(
      pafi_win = pafi_wf(pafi),
      coag_win = coag_wf(coag),
      bili_win = bili_wf(bili),
      map_win = map_wf(map),
      dopa_win = dopa_wf(dopa),
      norepi_win = norepi_wf(norepi),
      dobu_win = dobu_wf(dobu),
      epi_win = epi_wf(epi),
      gcs_win = gcs_wf(gcs),
      crea_win = crea_wf(crea),
      urine_win = urine_wf(urine)
    ), list(
      pafi_wf = pafi_win_fun,
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

  need_cols <- c("pafi", "coag", "bili", "map", "dopa", "norepi", "dobu",
                 "epi", "gcs", "crea", "urine")

  assert_that(has_cols(tbl, need_cols))

  sofa_cols <- c(
    "sofa_resp", "sofa_coag", "sofa_liver", "sofa_cardio", "sofa_cns",
    "sofa_renal"
  )

  tbl <- tbl[,
    c(sofa_cols) := list(
      sofa_resp(get("pafi"), na_val_resp),
      sofa_coag(get("coag"), na_val_coag),
      sofa_liver(get("bili"), na_val_liver),
      sofa_cardio(get("map"), get("dopa"), get("norepi"), get("dobu"),
                  get("epi"), na_val_cardio),
      sofa_cns(get("gcs"), na_val_cns),
      sofa_renal(get("crea"), get("urine"), na_val_renal)
    )
  ]

  tbl <- rm_cols(tbl, need_cols)

  if (!is.null(impute_fun)) {
    tbl <- tbl[, c("sofa_cols") := lapply(.SD, impute_fun),
               .SDcols = sofa_cols, by = c(id(tbl))]
  }

  tbl <- tbl[, c("sofa_score") := sofa_resp + sofa_coag + sofa_liver +
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
