
#' SOFA score label
#'
#' The SOFA (Sequential Organ Failure Assessment) score is a commonly used
#' assessment tool for tracking a patient's status during a stay at an ICU.
#' Organ function is quantified by aggregating 6 individual scores,
#' representing respiratory, cardiovascular, hepatic, coagulation, renal and
#' neurological systems. The function `sofa_score()` is used as callback
#' function to the `sofa` concept but is exported as there are a few arguments
#' that can used to modify some aspects of the presented SOFA implementation.
#' Internally, `sofa_score()` calls first `sofa_window()`, followed by
#' `sofa_compute()` and arguments passed as `...` will be forwarded to the
#' respective internally called function.
#'
#' @param ... Data and further arguments passed to `sofa_window()` or
#' `sofa_compute()`
#' @param interval Time series interval (only used for checking consistency
#' of input data, `NULL` will use the interval of the first data object)
#'
#' @details
#' The function `sofa_window()` calculates, for each covariate, the worst
#' value over a moving window as specified by `worst_win_length`, using the
#' respective function passed as `*_wf` (e.g. `pafi_wf` for the
#' `pafi` component. The default functions `min_or_na()` and `max_or_na()`
#' return `NA` instead of `-Inf/Inf` in the case where no measurement is
#' available over an entire window.
#'
#' Using data imputed by `sofa_window()`, the function `sofa_compute()`
#' performs the actual score calculation. First, for each time step and
#' component, measurements are converted to a component score using the
#' definition by Vincent et. al.:
#'
#' | **SOFA score**              |     1     |     2     |   3   |   4   |
#' | --------------------------- | --------- | --------- | ----- | ----- |
#' | **Respiration**             |           |           |       |       |
#' | PaO\ifelse{latex}{\out{\textsubscript{2}}}{\ifelse{html
#'   }{\out{<sub>2</sub>}}{2}}/FiO\ifelse{latex
#'   }{\out{\textsubscript{2}}}{\ifelse{html
#'   }{\out{<sub>2</sub>}}{2}} \[mmHg\] | < 400 | < 300  | < 200 | < 100 |
#' | and mechanical ventilation  |           |           |  yes  |  yes  |
#' | **Coagulation**             |           |           |       |       |
#' | Platelets \[\ifelse{latex}{\out{$\times$}}{\ifelse{html
#'   }{\out{&times;}}{x}}10\ifelse{latex}{\out{\textsuperscript{3}}}{\ifelse{
#'   html}{\out{<sup>3</sup>}}{3}}/mm\ifelse{latex
#'   }{\out{\textsuperscript{3}}}{\ifelse{html
#'   }{\out{<sup>3</sup>}}{3}}\] | < 150     |   < 100   | < 50  | < 20  |
#' | **Liver**                   |           |           |       |       |
#' | Bilirubin \[mg/dl\] (\[\ifelse{latex}{\out{$\mu$}}{\ifelse{html
#'   }{\out{&mu;}}{u}}mol/\ifelse{latex}{\out{$\mu$}}{\ifelse{html
#'   }{\out{&mu;}}{u}}l\])       | 1.2-1.9 (20-32) | 2.0-5.9 (33-101) | 6.0-11.9 (102-204) | > 12.0 (> 204) |
#' | **Cardiovascular**\ifelse{latex}{\out{\textsuperscript{a}}}{\ifelse{html
#'   }{\out{<sup>a</sup>}}{a}}   |           |           |       |       |
#' | MAP                         | < 70 mmHg |           |       |       |
#' | or dopamine                 |           | \ifelse{latex}{\out{$\le$}}{\ifelse{html
#'   }{\out{&leq;}}{<=}}                           5     |  > 5  |  > 15 |
#' | or dobutamine               |           |  any dose |       |       |
#' | or epinephrine              |           |           | \ifelse{latex}{\out{$\le$}}{\ifelse{html
#'   }{\out{&leq;}}{<=}}                                    0.1  | > 0.1 |
#' | or norepinephrine           |           |           | \ifelse{latex}{\out{$\le$}}{\ifelse{html
#'   }{\out{&leq;}}{<=}}                                    0.1  | > 0.1 |
#' | **Central nervous system**  |           |           |       |       |
#' | Glasgow Coma Score          | 13-14     |   10-12   |  6-9  |  < 6  |
#' | **Renal**                   |           |           |       |       |
#' | Creatinine \[mg/dl\] (\[\ifelse{latex}{\out{$\mu$}}{\ifelse{html
#'   }{\out{&mu;}}{u}}mol/\ifelse{latex}{\out{$\mu$}}{\ifelse{html
#'   }{\out{&mu;}}{u}}l\])       | 1.2-1.9 (110-170) | 2.0-3.4 (171-299) | 3.5-4.9 (300-440) | > 5.0 (< 440) |
#' | or urine output \[ml/day\]  |           |           | < 500 | < 200 |
#'
#' \ifelse{latex}{\out{\textsuperscript{a}}}{\ifelse{html}{
#' \out{<sup>a</sup>}}{a}}Adrenergic agents administered for at least 1h (doses
#' given are in \[\ifelse{latex}{\out{$\mu$}}{\ifelse{html}{\out{&mu;}}{u}}g/kg
#' \ifelse{latex}{\out{$\cdot$}}{\ifelse{html}{\out{&middot;}}{.}} min\]
#'
#' In case, for a given time step and component, no measurement is available,
#' the corresponding `na_*` value is used (e.g. `na_resp` for the
#' respiratory component). At default, this is 0 (the lowest possible score
#' for a SOFA component). It is possible to retain missingness by passing `NA`
#' as `na_*` value and using a function passed as `impute_fun()` in order
#' to perform an additional imputation step.
#'
#' @references
#' Vincent, J.-L., Moreno, R., Takala, J. et al. The SOFA (Sepsis-related Organ
#' Failure Assessment) score to describe organ dysfunction/failure. Intensive
#' Care Med 22, 707–710 (1996). https://doi.org/10.1007/BF01709751
#'
#' @encoding UTF-8
#' @rdname callback_sofa
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
#' @param pafi_wf,plt_wf,bili_wf,map_wf,dopa_wf,norepi_wf,dobu_wf,epi_wf,gcs_wf,crea_wf,urine_wf
#' functions used to calculate worst values over windows
#' @param explicit_wins The default `FALSE` iterates over all time steps,
#' `TRUE` uses only the last time step per patient and a vector of times will
#' iterate over these explicit time points
#' @param worst_win_length Time-frame to look back and apply the `*_wf`s
#'
#' @rdname callback_sofa
#' @export
#'
sofa_window <- function(tbl,
                        pafi_wf  = min_or_na, plt_wf    = min_or_na,
                        bili_wf  = max_or_na, map_wf    = min_or_na,
                        dopa_wf  = max_or_na, norepi_wf = max_or_na,
                        dobu_wf  = max_or_na, epi_wf    = max_or_na,
                        gcs_wf   = min_or_na, crea_wf   = max_or_na,
                        urine_wf = min_or_na, explicit_wins  = FALSE,
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
      pafi_wf  = pafi_wf,  plt_wf    = plt_wf,
      bili_wf  = bili_wf,  map_wf    = map_wf,
      dopa_wf  = dopa_wf,  norepi_wf = norepi_wf,
      dobu_wf  = dobu_wf,  epi_wf    = epi_wf,
      gcs_wf   = gcs_wf,   crea_wf   = crea_wf,
      urine_wf = urine_wf
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
#' @param na_resp,na_plt,na_liver,na_cardio,na_cns,na_renal Feature-specific values to use in case of missing data; default is `na_val`
#' @param impute_fun Function used to impute missing data; default is NULL
#' @param keep_components Logical flag indicating whether to return individual
#' SOFA components as columns
#' @param by_ref Logical flag indicating whether to perform the operation by
#' reference
#'
#' @rdname callback_sofa
#' @export
#'
sofa_compute <- function(tbl, na_val = 0L, na_resp = na_val,
                         na_plt = na_val, na_liver = na_val,
                         na_cardio = na_val, na_cns = na_val,
                         na_renal = na_val, impute_fun = NULL,
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
      sofa_resp(get("pafi"), na_resp),
      sofa_coag(get("plt"), na_plt),
      sofa_liver(get("bili"), na_liver),
      sofa_cardio(get("map"), get("dopa"), get("norepi"), get("dobu"),
                  get("epi"), na_cardio),
      sofa_cns(get("gcs"), na_cns),
      sofa_renal(get("crea"), get("urine24"), na_renal)
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
