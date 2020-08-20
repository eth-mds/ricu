
#' SOFA score label
#'
#' The SOFA (Sequential Organ Failure Assessment) score is a commonly used
#' assessment tool used to track a patient's well-being in an ICU.
#'
#' @param pa_fi,vent_ind,platelet_count,bilirubin_total,map,norepinephrine,epinephrine,dopamine,dobutamine,gcs,creatinine,urine_24 Data input used for
#' sofa score evaluation (`ts_tbl` objects, as produced by [load_concepts()])
#' @param ... Passed to `sofa_window()` or `sofa_compute()`
#' @param interval Time series interval (only used for checking consistency
#' of input data)
#'
#' @encoding UTF-8
#'
#' @details The SOFA score (Vincent et. al.) is evaluated as follows:
#'
#' | **SOFA score**                                                          | 1                 | 2                                      | 3                                                         | 4                                                          |
#' | ----------------------------------------------------------------------- | ----------------- | -------------------------------------- | --------------------------------------------------------- | ---------------------------------------------------------- |
#' | **Respiration** PaO₂/FiO₂ \[mmHg\]                                      | < 400             | < 300                                  | < 200 (with mech. vent.)                                  | < 100 (with mech. vent.)                                   |
#' | **Coagulation** Platelets \[⨉10³/mm³\]                                  | < 150             | < 100                                  | < 50                                                      | < 20                                                       |
#' | **Liver** Bilirubin \[mg/dl\] (\[μmol/μl\])                             | 1.2-1.9 (20-32)   | 2.0-5.9 (33-101)                       | 6.0-11.9 (102-204)                                        | > 12.0 (> 204)                                             |
#' | **Cardiovascular** Hypotension                                          | MAP < 70 mmHg     | Dopamine ≤ 5 or dobutamine (any dose)ª | Dopamine > 5 or epinephrine ≤ 0.1 or norepinephrine ≤ 0.1 | Dopamine > 15 or epinephrine > 0.1 or norepinephrine > 0.1 |
#' | **Central nervous system** Glasgow Coma Score                           | 13-14             | 10-12                                  | 6-9                                                       | < 6                                                        |
#' | **Renal** Creatinine \[mg/dl\] (\[μmol/μl\]) or urine output \[ml/day\] | 1.2-1.9 (110-170) | 2.0-3.4 (171-299)                      | 3.5-4.9 (300-440) < 500                                   | > 5.0 (< 440) < 200                                        |
#'
#' ªAdrenergic agents administered for at least 1h (doses given are in \[μg/kg ⋅ min\])
#'
#' For each component, the worst value over a window of length
#' `worst_win_length` (default `hours(24L)`) is taken.
#'
#' The respiratory component of the SOFA score requires the evaluation of the
#' PaO₂/FiO₂ ratio. Often, however, both measures are not available at the
#' same time point. Therefore, PaO₂ and FiO₂ measurements are matched within a
#' window of size `pafi_win_length` (default `hours(2L)`).
#'
#' The renal component of the SOFA score assigns scores of 3 and 4 in case of
#' a very low urine output. Since the information on urine output is not very
#' reliable at the start of the ICU stay, we do not evaluate urine outputs
#' before a time window of `urine_min_win` (default `hours(12L)`).
#'
#' Mechanical ventilation is also part of the SOFA score. In some datasets,
#' ventilation start events are given, which cannot be matched to ventilation
#' end events specifically. In these cases, the duration of the ventilation
#' window is taken to be `vent_win_length` (default `hours(6L)`).
#'
#' @references
#' Vincent, J.-L., Moreno, R., Takala, J. et al. The SOFA (Sepsis-related Organ
#' Failure Assessment) score to describe organ dysfunction/failure. Intensive
#' Care Med 22, 707–710 (1996). https://doi.org/10.1007/BF01709751
#'
#' @rdname label_sofa
#' @export
#'
sofa_score <- function(pa_fi, vent_ind, platelet_count, bilirubin_total,
                       map, norepinephrine, epinephrine, dopamine,
                       dobutamine, gcs, creatinine, urine_24, ...,
                       interval = ricu::interval(pa_fi)) {

  args <- list(...)

  assert_that(!has_name(args, c("tbl", "by_ref")))

  dat <- reduce(merge,
    list(pa_fi, vent_ind, platelet_count, bilirubin_total, map,
         norepinephrine, epinephrine, dopamine, dobutamine, gcs, creatinine,
         urine_24),
    all = TRUE)

  assert_that(has_interval(dat, interval))

  dat <- dat[is_true(get("pa_fi") < 200) & !is_true(get("vent_ind")),
             c("pa_fi") := 200]

  dat <- rm_cols(dat, "vent_ind", by_ref = TRUE)

  rename <- c(
    platelet_count = "coag", bilirubin_total = "bili", map = "map",
    dopamine = "dopa", norepinephrine = "norepi", dobutamine = "dobu",
    epinephrine = "epi", creatinine = "crea", urine_24 = "urine",
    pa_fi = "pafi"
  )

  dat <- rename_cols(dat, rename, names(rename), by_ref = TRUE)

  com_args <- names(args)[names(args) %in% names(formals(sofa_compute))]
  win_args <- names(args)[names(args) %in% names(formals(sofa_window))]

  dat <- do.call(sofa_window, c(list(dat), args[win_args]))
  dat <- do.call(sofa_compute, c(list(dat), args[com_args],
                                 list(by_ref = TRUE)))

  dat
}

#' @param pa_o2,fi_o2 Data input used for PaO₂/FiO₂ evaluation
#' @param win_length Time-span during which matching of PaO₂ and FiO₂
#' values is allowed
#' @param mode Method for matching PaO₂ and FiO₂ values
#' @param fix_na_fio2 Logical flag indicating whether to impute missing FiO₂
#' values with 21
#'
#' @rdname label_sofa
#' @export
#'
sofa_pafi <- function(pa_o2, fi_o2, win_length = hours(2L),
                      mode = c("match_vals", "extreme_vals", "fill_gaps"),
                      fix_na_fio2 = TRUE, interval = ricu::interval(pa_o2)) {

  mode <- match.arg(mode)

  assert_that(
    has_interval(pa_o2, interval), has_interval(fi_o2, interval),
    is_interval(win_length), win_length > interval, is.flag(fix_na_fio2),
    has_cols(pa_o2, "pa_o2"), has_cols(fi_o2, "fi_o2")
  )

  if (identical(mode, "match_vals")) {

    res <- rbind(
      fi_o2[pa_o2, on = paste(meta_vars(fi_o2), "==", meta_vars(pa_o2)),
           roll = win_length],
      pa_o2[fi_o2, on = paste(meta_vars(pa_o2), "==", meta_vars(fi_o2)),
           roll = win_length]
    )
    res <- unique(res)

  } else {

    res <- merge(pa_o2, fi_o2, all = TRUE)

    if (identical(mode, "fill_gaps")) {
      res <- fill_gaps(res)
    }

    win_expr <- substitute(
      list(min_pa = min_fun(get("pa_o2")), max_fi = max_fun(get("fi_o2"))),
      list(min_fun = min_or_na, max_fun = max_or_na)
    )
    res <- slide(res, !!win_expr, before = win_length, full_window = FALSE)

    rename_cols(res, c("pa_o2", "fi_o2"), c("min_pa", "max_fi"), by_ref = TRUE)
  }

  if (fix_na_fio2) {
    res <- res[is.na(get("fi_o2")), c("fi_o2") := 21]
  }

  res <- res[!is.na(get("pa_o2")) & !is.na(get("fi_o2")) & get("fi_o2") != 0, ]
  res <- res[, c("pa_fi") := 100 * get("pa_o2") / get("fi_o2")]
  res <- res[, c("pa_o2", "fi_o2") := NULL]

  res
}

#' @param vent_start,vent_end Data input used for vent evaluation
#' @param win_length Default ventilation window if no stop time can be
#' matched to a start time
#' @param min_length Minimal time span between a ventilation start and end
#' time
#'
#' @rdname label_sofa
#' @export
#'
sofa_vent <- function(vent_start, vent_end, win_length = hours(6L),
                      min_length = mins(10L),
                      interval = ricu::interval(vent_start)) {

  final_units <- function(x) {
    units(x) <- units(interval)
    round_to(x, as.double(interval))
  }

  assert_that(
    is_interval(win_length), is_interval(min_length), min_length < win_length,
    interval(vent_start) < min_length
  )

  units(win_length) <- time_unit(vent_start)
  units(min_length) <- time_unit(vent_start)

  if (is.null(vent_end)) {

    ind <- index_var(vent_start)

    merged <- copy(vent_start)
    merged <- merged[,
      c("start_time", "stop_time") := list(get(ind), get(ind) + win_length)
    ]

  } else {

    assert_that(same_time(interval(vent_start), interval(vent_end)))

    ind_start <- index_var(vent_start)
    ind_end   <- index_var(vent_end)

    vent_start[, c("start_time") := get(ind_start)]
    vent_end[ , c("stop_time")  := get(ind_end)]

    on.exit({
      rm_cols(vent_start, "start_time", by_ref = TRUE)
      rm_cols(vent_end,  "stop_time", by_ref = TRUE)
    })

    join   <- paste(meta_vars(vent_end), "==", meta_vars(vent_start))
    merged <- vent_end[vent_start, roll = -win_length, on = join]
    merged <- merged[is.na(get("stop_time")),
                     c("stop_time") := get("start_time") + win_length]
  }

  merged <- merged[get("stop_time") - get("start_time") >= min_length, ]

  merged <- merged[, c("start_time", "stop_time") := list(
    final_units(get("start_time")), final_units(get("stop_time"))
  )]

  res <- unique(
    expand(merged, min_col = "start_time", max_col = "stop_time")
  )
  res <- res[, c("vent_ind") := TRUE]

  res
}

#' @param gcs_eye,gcs_motor,gcs_verbal,gcs_total,tracheostomy,rass_scale Data
#' input used for gcs evaluation
#' @param win_length Maximal time window for which a GCS value is valid
#' if no newer measurement is available
#' @param set_sed_max Logical flag for considering sedation
#' @param set_na_max Logical flag controlling imputation of missing GCS values
#' with the respective maximum values
#'
#' @rdname label_sofa
#' @export
#'
sofa_gcs <- function(gcs_eye, gcs_motor, gcs_verbal, gcs_total, tracheostomy,
                     rass_scale, win_length = hours(6L), set_sed_max = TRUE,
                     set_na_max = TRUE, interval = ricu::interval(gcs_eye)) {

  assert_that(
    has_interval(gcs_eye, interval), has_interval(gcs_motor, interval),
    has_interval(gcs_verbal, interval), has_interval(gcs_total, interval),
    has_interval(tracheostomy, interval), has_interval(rass_scale, interval),
    is_interval(win_length), win_length > interval, is.flag(set_sed_max),
    is.flag(set_na_max)
  )

  tra_var <- data_var(tracheostomy)
  ras_var <- data_var(rass_scale)

  sed <- merge(tracheostomy, rass_scale, all = TRUE)
  sed <- sed[, c("is_sed", tra_var, ras_var) := list(
    get(tra_var) | get(ras_var) <= -2, NULL, NULL)
  ]

  dat <- reduce(merge, list(gcs_eye, gcs_motor, gcs_verbal, gcs_total, sed),
                all = TRUE)

  if (set_sed_max) {
    dat <- dat[is_true(get("is_sed")),
      c("gcs_eye", "gcs_verbal", "gcs_motor", "gcs_total") := list(4, 5, 6, 15)
    ]
  }

  expr <- substitute(
    list(gcs_eye = fun(gcs_eye), gcs_verbal = fun(gcs_verbal),
         gcs_motor = fun(gcs_motor), gcs_total = fun(gcs_total)),
    list(fun = locf)
  )

  dat <- slide(dat, !!expr, before = win_length)

  if (set_na_max) {
    dat <- dat[, c("gcs_eye", "gcs_verbal", "gcs_motor") := list(
      replace_na(get("gcs_eye"), 4), replace_na(get("gcs_verbal"), 5),
      replace_na(get("gcs_motor"), 6)
    )]
  }

  dat <- dat[is.na(get("gcs_total")), c("gcs_total") := list(
    get("gcs_eye") + get("gcs_verbal") + get("gcs_motor")
  )]

  if (set_na_max) {
    dat <- dat[, c("gcs_total") := list(replace_na(get("gcs_total"), 15))]
  }

  dat <- rename_cols(dat, "gcs", "gcs_total")
  dat <- rm_cols(dat, c("gcs_eye", "gcs_verbal", "gcs_motor"))

  dat
}

#' @param urine_out Data input used for urine/24h evaluation
#' @param min_win Minimal time span required for calculation of urine/24h
#'
#' @rdname label_sofa
#' @export
#'
sofa_urine <- function(urine_out, min_win = hours(12L),
                       interval = ricu::interval(urine_out)) {

  convert_dt <- function(x) as.double(x, units(interval))

  urine_sum <- local({

    min_steps <- ceiling(convert_dt(min_win) / as.double(interval))
    step_factor <- convert_dt(hours(24L)) / as.double(interval)

    function(x) {
      if (length(x) < min_steps) return(NA_real_)
      else sum(x, na.rm = TRUE) * step_factor / length(x)
    }
  })

  assert_that(
    has_interval(urine_out, interval), is_interval(min_win),
    min_win > interval, min_win <= hours(24L)
  )

  res <- fill_gaps(urine_out)

  expr <- substitute(list(urine_24 = win_agg_fun(urine_out)),
                     list(win_agg_fun = urine_sum))

  slide(res, !!expr, hours(24L))
}

#' @param tbl Table holding SOFA covariates
#' @param pafi_win_fun,coag_win_fun,bili_win_fun,map_win_fun,dopa_win_fun,norepi_win_fun,dobu_win_fun,epi_win_fun,gcs_win_fun,crea_win_fun,urine_win_fun
#' functions used to calculate worst values over windows
#' @param explicit_wins The default `FALSE` iterates over all time steps,
#' `TRUE` uses only the last time step per patient and a vector of times will
#' iterate over these explicit time points
#' @param worst_win_length Time-frame to look back and apply the `*_win_fun`s
#'
#' @rdname label_sofa
#' @export
#'
sofa_window <- function(tbl,
                        pafi_win_fun  = min_or_na, coag_win_fun   = min_or_na,
                        bili_win_fun  = max_or_na, map_win_fun    = min_or_na,
                        dopa_win_fun  = max_or_na, norepi_win_fun = max_or_na,
                        dobu_win_fun  = max_or_na, epi_win_fun    = max_or_na,
                        gcs_win_fun   = min_or_na, crea_win_fun   = max_or_na,
                        urine_win_fun = min_or_na, explicit_wins  = FALSE,
                        worst_win_length = hours(24L)) {

  need_cols <- c("pafi", "coag", "bili", "map", "dopa", "norepi", "dobu",
                 "epi", "gcs", "crea", "urine")

  assert_that(is_ts_tbl(tbl), has_cols(tbl, need_cols))

  expr <- substitute(
    list(
      pafi_win  = pafi_wf(pafi), coag_win   = coag_wf(coag),
      bili_win  = bili_wf(bili), map_win    = map_wf(map),
      dopa_win  = dopa_wf(dopa), norepi_win = norepi_wf(norepi),
      dobu_win  = dobu_wf(dobu), epi_win    = epi_wf(epi),
      gcs_win   = gcs_wf(gcs),   crea_win   = crea_wf(crea),
      urine_win = urine_wf(urine)
    ), list(
      pafi_wf   = pafi_win_fun,  coag_wf    = coag_win_fun,
      bili_wf   = bili_win_fun,  map_wf     = map_win_fun,
      dopa_wf   = dopa_win_fun,  norepi_wf  = norepi_win_fun,
      dobu_wf   = dobu_win_fun,  epi_wf     = epi_win_fun,
      gcs_wf    = gcs_win_fun,   crea_wf    = crea_win_fun,
      urine_wf  = urine_win_fun
    )
  )

  if (isFALSE(explicit_wins)) {

    res <- fill_gaps(tbl)
    res <- slide(res, !!expr, before = worst_win_length,
                     full_window = FALSE)

  } else {

    if (isTRUE(explicit_wins)) {

      assert_that(is_scalar(worst_win_length), is_interval(worst_win_length))

      ind <- index_var(tbl)

      win <- tbl[, list(max_time = max(get(ind))), by = c(id_vars(tbl))]
      win <- win[, c("min_time") := get("max_time") - worst_win_length]

      res <- hop(tbl, !!expr, win)

    } else {

      res <- slide_index(tbl, !!expr, explicit_wins,
                             before = worst_win_length, full_window = FALSE)
    }
  }

  rename_cols(res, need_cols, paste0(need_cols, "_win"), by_ref = TRUE)
}

#' @param na_val Value to use for missing data
#' @param na_val_resp,na_val_coag,na_val_liver,na_val_cardio,na_val_cns,na_val_renal Feature-specific values to use in case of missing data; default is `na_val`
#' @param impute_fun Function used to impute missing data; default is NULL
#' @param by_ref Logical flag indicating whether to perform the operation by
#' reference
#'
#' @rdname label_sofa
#' @export
#'
sofa_compute <- function(tbl, na_val = 0L, na_val_resp = na_val,
                         na_val_coag = na_val, na_val_liver = na_val,
                         na_val_cardio = na_val, na_val_cns = na_val,
                         na_val_renal = na_val, impute_fun = NULL,
                         by_ref = FALSE) {

  need_cols <- c("pafi", "coag", "bili", "map", "dopa", "norepi", "dobu",
                 "epi", "gcs", "crea", "urine")

  assert_that(is_id_tbl(tbl), has_cols(tbl, need_cols), is.flag(by_ref))

  if (!by_ref) {
    tbl <- copy(tbl)
  }

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

  tbl <- rm_cols(tbl, need_cols, by_ref = TRUE)

  if (!is.null(impute_fun)) {
    tbl <- tbl[, c("sofa_cols") := lapply(.SD, impute_fun),
               .SDcols = sofa_cols, by = c(id_vars(tbl))]
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
