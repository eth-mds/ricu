
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
#' @param ... Concept data, either passed as list or individual argument
#' @param worst_val_fun functions used to calculate worst values over windows
#' @param explicit_wins The default `FALSE` iterates over all time steps,
#' `TRUE` uses only the last time step per patient and a vector of times will
#' iterate over these explicit time points
#' @param win_length Time-frame to look back and apply the `worst_val_fun`
#' @param keep_components Logical flag indicating whether to return the
#' individual components alongside the aggregated score (with a suffix `_comp`
#' added to their names)
#' @param interval Time series interval (only used for checking consistency
#' of input data, `NULL` will use the interval of the first data object)
#'
#' @details
#' The function `sofa_score()` calculates, for each component, the worst value
#' over a moving window as specified by `win_length`, using the function
#' passed as `worst_val_fun`. The default functions `max_or_na()` return `NA`
#' instead of `-Inf/Inf` in the case where no measurement is available over an
#' entire window. When calculating the overall score by summing up components
#' per time-step, a `NA` value is treated as 0.
#'
#' Building on separate concepts, measurements for each component are
#' converted to a component score using the definition by Vincent et. al.:
#'
#' | **SOFA score**              |     1     |     2     |     3    |    4   |
#' | --------------------------- | --------- | --------- | -------- | ------ |
#' | **Respiration**             |           |           |          |        |
#' | PaO\ifelse{latex}{\out{\textsubscript{2}}}{\ifelse{html
#'   }{\out{<sub>2</sub>}}{2}}/FiO\ifelse{latex
#'   }{\out{\textsubscript{2}}}{\ifelse{html
#'   }{\out{<sub>2</sub>}}{2}} \[mmHg\] | < 400 | < 300  |   < 200  |  < 100 |
#' | and mechanical ventilation  |           |           |    yes   |   yes  |
#' | **Coagulation**             |           |           |          |        |
#' | Platelets \[\ifelse{latex}{\out{$\times$}}{\ifelse{html
#'   }{\out{&times;}}{x}}10\ifelse{latex}{\out{\textsuperscript{3}}}{\ifelse{
#'   html}{\out{<sup>3</sup>}}{3}}/mm\ifelse{latex
#'   }{\out{\textsuperscript{3}}}{\ifelse{html
#'   }{\out{<sup>3</sup>}}{3}}\] | < 150     |   < 100   |   < 50   |  < 20  |
#' | **Liver**                   |           |           |          |        |
#' | Bilirubin \[mg/dl\]         |  1.2-1.9  |  2.0-5.9  | 6.0-11.9 | > 12.0 |
#' | **Cardiovascular**\ifelse{latex}{\out{\textsuperscript{a}}}{\ifelse{html
#'   }{\out{<sup>a</sup>}}{a}}   |           |           |          |        |
#' | MAP                         | < 70 mmHg |           |          |        |
#' | or dopamine                 |           | \ifelse{latex}{\out{$\le$}}{\ifelse{html
#'   }{\out{&leq;}}{<=}}                           5     |    > 5   |   > 15 |
#' | or dobutamine               |           |  any dose |          |        |
#' | or epinephrine              |           |           | \ifelse{latex}{\out{$\le$}}{\ifelse{html
#'   }{\out{&leq;}}{<=}}                                      0.1   |  > 0.1 |
#' | or norepinephrine           |           |           | \ifelse{latex}{\out{$\le$}}{\ifelse{html
#'   }{\out{&leq;}}{<=}}                                      0.1   |  > 0.1 |
#' | **Central nervous system**  |           |           |          |        |
#' | Glasgow Coma Score          | 13-14     |   10-12   |    6-9   |   < 6  |
#' | **Renal**                   |           |           |          |        |
#' | Creatinine \[mg/dl\]        |  1.2-1.9  |  2.0-3.4  |  3.5-4.9 |  > 5.0 |
#' | or urine output \[ml/day\]  |           |           |   < 500  |  < 200 |
#'
#' \ifelse{latex}{\out{\textsuperscript{a}}}{\ifelse{html
#' }{\out{<sup>a</sup>}}{a}}Adrenergic agents administered for at least 1h
#' (doses given are in \[\ifelse{latex}{\out{$\mu$}}{\ifelse{html
#' }{\out{&mu;}}{u}}g/kg \ifelse{latex}{\out{$\cdot$}}{\ifelse{html
#' }{\out{&middot;}}{.}} min\]
#'
#' At default, for each patient, a score is calculated for every time step,
#' from the first available measurement to the last. In instead of a regularly
#' evaluated score, only certain time points are of interest, this can be
#' specified using the `explicit_wins` argument: passing for example
#' `hours(24, 48)` will yield for every patient a score at hours 24 and 48
#' relative to the origin of the current ID system (for example ICU stay).
#'
#' @return A `ts_tbl` object.
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
sofa_score <- function(..., worst_val_fun = max_or_na, explicit_wins = FALSE,
                       win_length = hours(24L), keep_components = FALSE,
                       interval = NULL) {

  cnc <- c("sofa_resp", "sofa_coag", "sofa_liver", "sofa_cardio",
           "sofa_cns", "sofa_renal")
  dat <- collect_dots(cnc, interval, ..., merge_dat = TRUE)

  expr <- substitute(lapply(.SD, fun), list(fun = worst_val_fun))

  if (isFALSE(explicit_wins)) {

    res <- fill_gaps(dat)
    res <- slide(res, !!expr, before = win_length, full_window = FALSE,
                 .SDcols = cnc)

  } else {

    if (isTRUE(explicit_wins)) {

      assert_that(is_scalar(win_length), is_interval(win_length))

      ind <- index_var(dat)

      win <- dat[, list(max_time = max(get(ind))), by = c(id_vars(dat))]
      win <- win[, c("min_time") := get("max_time") - win_length]

      res <- hop(dat, !!expr, win, .SDcols = cnc)

    } else {

      res <- slide_index(dat, !!expr, explicit_wins, before = win_length,
                         full_window = FALSE, .SDcols = cnc)
    }
  }

  res <- res[, c("sofa") := rowSums(.SD, na.rm = TRUE), .SDcols = cnc]

  if (isTRUE(keep_components)) {
    res <- rename_cols(res, paste0(cnc, "_comp"), cnc, by_ref = TRUE)
  } else {
    res <- rm_cols(res, cnc, by_ref = TRUE)
  }

  res
}

#' @rdname callback_sofa
#' @export
sofa_resp <- function(..., interval = NULL) {

  score_calc <- function(x) {
    fifelse(
      is_true(x < 100), 4L, fifelse(
        is_true(x < 200), 3L, fifelse(
          is_true(x < 300), 2L, fifelse(
            is_true(x < 400), 1L, 0L
          )
        )
      )
    )
  }

  vent_var <- "vent_ind"
  pafi_var <- "pafi"

  cnc <- c(pafi_var, vent_var)
  dat <- collect_dots(cnc, interval, ...)
  dat <- merge(dat[[pafi_var]], expand(dat[[vent_var]], aggregate = "any"),
               all = TRUE)

  dat <- dat[is_true(get(pafi_var) < 200) & !is_true(get(vent_var)),
             c(pafi_var) := 200]
  dat <- dat[, c("sofa_resp") := score_calc(get(pafi_var))]

  dat <- rm_cols(dat, cnc, by_ref = TRUE)

  dat
}

sofa_single <- function(cnc, nme, fun) {

  assert_that(is.string(cnc), is.string(nme), is.function(fun))

  function(..., interval = NULL) {

    dat <- collect_dots(cnc, interval, ...)
    dat <- dat[, c(nme) := fun(get(cnc))]
    dat <- rm_cols(dat, cnc, by_ref = TRUE)

    dat
  }
}

#' @rdname callback_sofa
#' @export
sofa_coag <- sofa_single(
  "plt", "sofa_coag", function(x) 4L - findInterval(x, c(20, 50, 100, 150))
)

#' @rdname callback_sofa
#' @export
sofa_liver <- sofa_single(
  "bili", "sofa_liver", function(x) findInterval(x, c(1.2, 2, 6, 12))
)

#' @rdname callback_sofa
#' @export
sofa_cardio <- function(..., interval = NULL) {

  score_calc <- function(map, dopa, norepi, dobu, epi) {
    fifelse(
      is_true(dopa > 15 | epi > 0.1 | norepi > 0.1), 4L, fifelse(
        is_true(dopa > 5 | (epi > 0 &    epi <= 0.1) |
                        (norepi > 0 & norepi <= 0.1)), 3L, fifelse(
          is_true((dopa > 0 & dopa <= 5) | dobu > 0), 2L, fifelse(
            is_true(map < 70), 1L, 0L
          )
        )
      )
    )
  }

  cnc <- c("map", "dopa60", "norepi60", "dobu60", "epi60")
  dat <- collect_dots(cnc, interval, ..., merge_dat = TRUE)

  dat <- dat[, c("sofa_cardio") := score_calc(
    get("map"), get("dopa60"), get("norepi60"), get("dobu60"), get("epi60")
  )]
  dat <- rm_cols(dat, cnc, by_ref = TRUE)

  dat
}

#' @rdname callback_sofa
#' @export
sofa_cns <- sofa_single(
  "gcs", "sofa_cns", function(x) 4L - findInterval(x, c(6, 10, 13, 15))
)

#' @rdname callback_sofa
#' @export
sofa_renal <- function(..., interval = NULL) {

  score_calc <- function(cre, uri) {
    fifelse(
      is_true(cre >= 5 | uri < 200), 4L, fifelse(
        is_true((cre >= 3.5 & cre < 5) | uri < 500), 3L, fifelse(
          is_true(cre >= 2 & cre < 3.5), 2L, fifelse(
            is_true(cre >= 1.2 & cre < 2), 1L, 0L
          )
        )
      )
    )
  }

  cnc <- c("crea", "urine24")
  dat <- collect_dots(cnc, interval, ..., merge_dat = TRUE)

  dat <- dat[, c("sofa_renal") := score_calc(get("crea"), get("urine24"))]
  dat <- rm_cols(dat, cnc, by_ref = TRUE)

  dat
}
