
#' SOFA score label
#'
#' The SOFA (Sequential Organ Failure Assessment) score is a commonly used
#' assessment tool used to track a patient's well-being in an ICU.
#'
#' @param ... Data and further arguments passed to `sofa_window()` or
#' `sofa_compute()`
#' @param interval Time series interval (only used for checking consistency
#' of input data, `NULL` will use the interval of the first data object)
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
#' | **Cardiovascular** Hypotension                                          | MAP < 70 mmHg     | Dopa ≤ 5 or dobu (any dose)ª | Dopa > 5 or epi ≤ 0.1 or norepi ≤ 0.1 | Dopa > 15 or epi > 0.1 or norepi > 0.1 |
#' | **Central nervous system** Glasgow Coma Score                           | 13-14             | 10-12                                  | 6-9                                                       | < 6                                                        |
#' | **Renal** Creatinine \[mg/dl\] (\[μmol/μl\]) or urine output \[ml/day\] | 1.2-1.9 (110-170) | 2.0-3.4 (171-299)                      | 3.5-4.9 (300-440) < 500                                   | > 5.0 (< 440) < 200                                        |
#'
#' ªAdrenergic agents administered for at least 1h (doses given are in \[μg/kg
#' ⋅ min\]; dopa: dopamine, dobu: dobutamine, epi: epinephrine, norepi:
#' norepinephrine)
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
sofa_score <- function(..., interval = NULL) {

  cnc <- c("pafi", "vent", "plt", "bili", "map", "norepi", "epi", "dopa",
           "dobu", "gcs", "crea", "urine24")

  sofa_data <- capture_fun(cnc, interval)
  form_args <- c(formals(sofa_data), formals(sofa_window),
                 formals(sofa_compute)[-1L])

  res <- capture_eval(sofa_data, ..., .formal_args = form_args)

  interval <- attr(res, "interval")

  res <- reduce(merge, res, all = TRUE)

  res <- res[is_true(get("pafi") < 200) & !is_true(get("vent")),
             c("pafi") := 200]

  res <- rm_cols(res, "vent", by_ref = TRUE)

  res <- capture_eval(sofa_window, tbl = res, ..., .formal_args = form_args)
  res <- capture_eval(sofa_compute, tbl = res, ..., .formal_args = form_args)

  res
}

#' @param tbl Table holding SOFA covariates
#' @param pafi_win_fun,plt_win_fun,bili_win_fun,map_win_fun,dopa_win_fun,norepi_win_fun,dobu_win_fun,epi_win_fun,gcs_win_fun,crea_win_fun,urine_win_fun
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
                        pafi_win_fun  = min_or_na, plt_win_fun    = min_or_na,
                        bili_win_fun  = max_or_na, map_win_fun    = min_or_na,
                        dopa_win_fun  = max_or_na, norepi_win_fun = max_or_na,
                        dobu_win_fun  = max_or_na, epi_win_fun    = max_or_na,
                        gcs_win_fun   = min_or_na, crea_win_fun   = max_or_na,
                        urine_win_fun = min_or_na, explicit_wins  = FALSE,
                        worst_win_length = hours(24L)) {

  need_cols <- c("pafi", "plt", "bili", "map", "dopa", "norepi", "dobu",
                 "epi", "gcs", "crea", "urine24")

  assert_that(is_ts_tbl(tbl), has_cols(tbl, need_cols))

  expr <- substitute(
    list(
      pafi_win    = pafi_wf(pafi), plt_win    = plt_wf(plt),
      bili_win    = bili_wf(bili), map_win    = map_wf(map),
      dopa_win    = dopa_wf(dopa), norepi_win = norepi_wf(norepi),
      dobu_win    = dobu_wf(dobu), epi_win    = epi_wf(epi),
      gcs_win     = gcs_wf(gcs),   crea_win   = crea_wf(crea),
      urine24_win = urine_wf(urine24)
    ), list(
      pafi_wf  = pafi_win_fun,  plt_wf    = plt_win_fun,
      bili_wf  = bili_win_fun,  map_wf    = map_win_fun,
      dopa_wf  = dopa_win_fun,  norepi_wf = norepi_win_fun,
      dobu_wf  = dobu_win_fun,  epi_wf    = epi_win_fun,
      gcs_wf   = gcs_win_fun,   crea_wf   = crea_win_fun,
      urine_wf = urine_win_fun
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
#' @param na_val_resp,na_val_plt,na_val_liver,na_val_cardio,na_val_cns,na_val_renal Feature-specific values to use in case of missing data; default is `na_val`
#' @param impute_fun Function used to impute missing data; default is NULL
#' @param keep_components Logical flag indicating whether to return individual
#' SOFA components as columns
#' @param by_ref Logical flag indicating whether to perform the operation by
#' reference
#'
#' @rdname label_sofa
#' @export
#'
sofa_compute <- function(tbl, na_val = 0L, na_val_resp = na_val,
                         na_val_plt = na_val, na_val_liver = na_val,
                         na_val_cardio = na_val, na_val_cns = na_val,
                         na_val_renal = na_val, impute_fun = NULL,
                         keep_components = FALSE, by_ref = TRUE) {

  need_cols <- c("pafi", "plt", "bili", "map", "dopa", "norepi", "dobu",
                 "epi", "gcs", "crea", "urine24")

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
      sofa_coag(get("plt"), na_val_plt),
      sofa_liver(get("bili"), na_val_liver),
      sofa_cardio(get("map"), get("dopa"), get("norepi"), get("dobu"),
                  get("epi"), na_val_cardio),
      sofa_cns(get("gcs"), na_val_cns),
      sofa_renal(get("crea"), get("urine24"), na_val_renal)
    )
  ]

  tbl <- rm_cols(tbl, need_cols, by_ref = TRUE)

  if (!is.null(impute_fun)) {
    tbl <- tbl[, c(sofa_cols) := lapply(.SD, impute_fun),
               .SDcols = sofa_cols, by = c(id_vars(tbl))]
  }

  tbl <- tbl[, c("sofa") := rowSums(.SD), .SDcols = sofa_cols]

  if (!isTRUE(keep_components)) {
    tbl <- rm_cols(tbl, sofa_cols, by_ref = TRUE)
  }

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
