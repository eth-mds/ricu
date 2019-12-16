
sofa_pafi <- function(pao2, fio2, win_length = hours(2L),
                      mode = c("match_vals", "extreme_vals", "fill_gaps")) {

  assert_that(same_ts(pao2, fio2),
              has_cols(pao2, "pao2"), has_cols(fio2, "fio2"),
              is_time(win_length, allow_neg = FALSE))

  mode <- match.arg(mode)

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
      list(min_pa = min_fun(pao2), max_fi = max_fun(fio2)),
      list(min_fun = min_or_na, max_fun = max_or_na)
    )
    res <- slide_quo(res, win_expr, before = win_length, full_window = FALSE)

    rename_cols(res, c("pao2", "fio2"), c("min_pa", "max_fi"))
  }

  res <- res[, pafi := 100 * pao2 / fio2]
  res <- res[, c("pao2", "fio2") := NULL]

  res
}

sofa_vent <- function(vent_start, vent_stop, win_length = hours(6L),
                      min_length = mins(10L), interval = hours(1L)) {

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

sofa_compute <- function(tbl, na_val = 0L, na_val_resp = na_val,
                         na_val_coag = na_val, na_val_liver = na_val,
                         na_val_cardio = na_val, na_val_cns = na_val,
                         na_val_renal = na_val, impute_fun = NULL) {

  need_cols <- c("pafi", "vent", "coag", "bili", "map", "dopa", "norepi",
                 "dobu", "epi", "gcs", "crea", "urine_24")

  assert_that(has_cols(tbl, need_cols), has_no_gaps(tbl))

  message("computing sofa scores")

  sofa_cols <- c(
    "sofa_resp", "sofa_coag", "sofa_liver", "sofa_cardio", "sofa_cns",
    "sofa_renal"
  )

  tbl <- tbl[,
    c(sofa_cols) := list(
      sofa_resp(pafi, vent, na_val_resp), sofa_coag(coag, na_val_coag),
      sofa_liver(bili, na_val_liver),
      sofa_cardio(map, dopa, norepi, dobu, epi, na_val_cardio),
      sofa_cns(gcs, na_val_cns), sofa_renal(crea, urine_24, na_val_renal)
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

sofa_resp <- function(pafi, vent, na_val) {
  fifelse(
    is_true(pafi < 100), 4L, fifelse(
      is_true(pafi < 200), 3L, fifelse(
        is_true(pafi < 300 & vent), 2L, fifelse(
          is_true(pafi < 400 & vent), 1L, 0L, na_val
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
