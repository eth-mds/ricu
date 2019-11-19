
sofa_window <- function(tbl,
                        pafi_win_fun   = min_or_na, vent_win_fun  = last_elem,
                        coag_win_fun   = min_or_na, bili_win_fun  = max_or_na,
                        map_win_fun    = min_or_na, dopa_win_fun  = max_or_na,
                        norepi_win_fun = max_or_na, dobu_win_fun  = max_or_na,
                        epi_win_fun    = max_or_na, gcs_win_fun   = min_or_na,
                        crea_win_fun   = max_or_na, urine_win_fun = min_or_na,
                        win_length = as.difftime(24L, units = "hours")) {

  need_cols <- c("pafi", "vent", "coag", "bili", "map", "dopa", "norepi",
                 "dobu", "epi", "gcs", "crea", "urine_24")

  assert_that(is_dt(tbl), has_cols(tbl, c("hadm_id", "hadm_time", need_cols)),
              is_regular(tbl),
              is_time(tbl[["hadm_time"]]),
              is_time(win_length, allow_neg = FALSE))

  message("computing worst values over window")

  tbl <- window_quo(tbl, substitute(
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
  ), full_window = FALSE, window_length = win_length)

  tbl <- data.table::set(tbl, j = need_cols, value = NULL)
  tbl <- data.table::setnames(tbl, paste0(need_cols, "_win"), need_cols)

  tbl
}

sofa_compute <- function(tbl, na_val = 0L, na_val_resp = na_val,
                         na_val_coag = na_val, na_val_liver = na_val,
                         na_val_cardio = na_val, na_val_cns = na_val,
                         na_val_renal = na_val, impute_fun = NULL) {

  need_cols <- c("pafi", "vent", "coag", "bili", "map", "dopa", "norepi",
                 "dobu", "epi", "gcs", "crea", "urine_24")

  assert_that(is_dt(tbl), has_cols(tbl, c("hadm_id", "hadm_time", need_cols)),
              is_regular(tbl),
              is_time(tbl[["hadm_time"]]))

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

  tbl <- data.table::set(tbl, j = need_cols, value = NULL)

  if (!is.null(impute_fun)) {
    tbl <- tbl[, c(sofa_cols) := lapply(.SD, impute_fun), .SDcols = sofa_cols,
               by = "hadm_id"]
  }

  tbl <- tbl[, sofa_score := sofa_resp + sofa_coag + sofa_liver +
                             sofa_cardio + sofa_cns + sofa_renal]

  tbl
}

sofa_resp <- function(pafi, vent, na_val) {
  ifelse(
    is_true(pafi < 100), 4L, ifelse(
      is_true(pafi < 200), 3L, ifelse(
        is_true(pafi < 300 & vent), 2L, ifelse(
          is_true(pafi < 400 & vent), 1L, ifelse(
            is.na(pafi), na_val, 0L
          )
        )
      )
    )
  )
}

sofa_coag <- function(x, na_val) {
  ifelse(is.na(x), na_val, 4L - findInterval(x, c(20, 50, 100, 150)))
}

sofa_liver <- function(x, na_val) {
  ifelse(is.na(x), na_val, findInterval(x, c(1.2, 2, 6, 12)))
}

sofa_cardio <- function(map, dopa, norepi, dobu, epi, na_val) {
  ifelse(
    is_true(dopa > 15 | epi > 0.1 | norepi > 0.1), 4L, ifelse(
      is_true(dopa > 5 | epi <= 0.1 | norepi <= 0.1), 3L, ifelse(
        is_true(dopa <= 5 | !is.na(dobu)), 2L, ifelse(
          is_true(map < 70), 1L, ifelse(
            is.na(map), na_val, 0L
          )
        )
      )
    )
  )
}

sofa_cns <- function(x, na_val) {
  ifelse(is.na(x), na_val, 4L - findInterval(x, c(6, 10, 13, 15)))
}

sofa_renal <- function(cre, uri, na_val) {
  ifelse(
    is_true(cre >= 5 | uri < 200), 4L, ifelse(
      is_true((cre >= 3.5 & cre < 5) | uri < 500), 3L, ifelse(
        is_true(cre >= 2 & cre < 3.5), 2L, ifelse(
          is_true(cre >= 1.2 & cre < 2), 1L, ifelse(
            is.na(cre) & is.na(uri), na_val, 0L
          )
        )
      )
    )
  )
}
