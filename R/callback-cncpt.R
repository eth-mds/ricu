
capture_eval <- function(fun, ..., .formal_args = NULL) {

  if (is.null(.formal_args)) {
    return(fun(...))
  }

  body <- quote({

    call <- match.call(expand.dots = FALSE)
    keep <- match(names(formals(fun)), names(call), 0L)
    call <- call[c(1L, keep)]

    call[[1L]] <- substitute(x, list(x = fun))

    eval(call)
  })

  rlang::new_function(.formal_args, body)(...)
}

capture_fun <- function(concepts, interval) {

  assert_that(is.character(concepts), has_length(concepts))

  check_interval <- function(x, ival) !is_ts_tbl(x) || has_interval(x, ival)

  body <- quote({

    call <- match.call(expand.dots = FALSE)

    call[[1L]] <- quote(list)

    res <- eval(call, caller_env())

    if (length(res) == 1L && length(concepts) > 1L) {
      res <- res[[1L]]
    }

    assert_that(same_length(res, concepts), all_fun(res, is_id_tbl))

    ivl <- NULL

    for (x in res) {
      if (is_ts_tbl(x)) {
        ivl <- interval(x)
        break
      }
    }

    assert_that(all_fun(res, check_interval, ivl),
                setequal(concepts, names(res)),
                all_map(has_cols, res[concepts], concepts),
                null_or(interval, all_equal, ivl))

    attr(res, "interval") <- ivl

    res
  })

  arg <- rep(list(bquote()), length(concepts))
  names(arg) <- concepts

  rlang::new_function(arg, body)
}

capture_data <- function(concepts, interval, ...) {
  res <- capture_fun(concepts, interval)(...)
  assign("interval", attr(res, "interval"), envir = caller_env())
  res
}

#' Concept callback functions
#'
#' Owing to increased complexity and more diverse applications, recursive
#' concepts (class [`rec_cncpt`][new_cncpt()]) may specify callback functions
#' to be called on corresponding data objects and perform post-processing
#' steps.
#'
#' @details
#' Several concept callback functions are exported, mainly for documenting
#' their arguments, as default values oftentimes represent somewhat arbitrary
#' choices and passing non-default values might be of interest for
#' investigating stability with respect to such choices. Furthermore, default
#' values might not be ideal for some datasets and/or analysis tasks.
#'
#' ## `pafi`
#' In order to calculate the PaO\ifelse{latex
#' }{\out{\textsubscript{2}}}{\ifelse{html}{\out{<sub>2</sub>}}{2}}/FiO\ifelse{
#' latex}{\out{\textsubscript{2}}}{\ifelse{html}{\out{<sub>2</sub>}}{2}} (or
#' Horowitz index), for a given time point, both a PaO\ifelse{
#' latex}{\out{\textsubscript{2}}}{\ifelse{html}{\out{<sub>2</sub>}}{2}} and a
#' FiO\ifelse{latex}{\out{\textsubscript{2}}}{\ifelse{html
#' }{\out{<sub>2</sub>}}{2}} measurement is required. As the two are often
#' not measured at the same time, some form of imputation or matching
#' procedure is required. Several options are available:
#'
#' * `match_vals` allows for a time difference of maximally `match_win`
#'   between two measurements for calculating their ratio
#' * `extreme_vals` uses the worst PaO\ifelse{latex
#'   }{\out{\textsubscript{2}}}{\ifelse{html}{\out{<sub>2</sub>}}{2}} and a
#'   FiO\ifelse{latex}{\out{\textsubscript{2}}}{\ifelse{html
#'   }{\out{<sub>2</sub>}}{2}} values within the time window spanned by
#'   `match_win`
#' * `fill_gaps` represents a variation of `extreme_vals`, where ratios are
#'   evaluated at every time-point as specified by `interval`as opposed to
#'   only the time points where either a PaO\ifelse{latex
#'   }{\out{\textsubscript{2}}}{\ifelse{html}{\out{<sub>2</sub>}}{2}} or a
#'   FiO\ifelse{latex}{\out{\textsubscript{2}}}{\ifelse{html
#'   }{\out{<sub>2</sub>}}{2}} measurement is available
#'
#' Finally, `fix_na_fio2` imputes all remaining missing FiO\ifelse{latex
#' }{\out{\textsubscript{2}}}{\ifelse{html}{\out{<sub>2</sub>}}{2}} with 21,
#' the percentage (by volume) of oxygen in (tropospheric) air.
#'
#' ## `vent`
#' Building on the atomic concepts `vent_start` and `vent_end`, an binary
#' indicator for ventilation status is constructed by combining start and end
#' events that are separated by at most `match_win` and at least `min_length`.
#' Time-points (as determined by `interval`) that fall into such ventilation
#' windows are set to `TRUE`, while missingness (`NA`) or `FALSE` indicate no
#' mechanical ventilation. Currently, no clear distinction between invasive
#' an non-invasive ventilation is made.
#'
#' ## `sed`
#' In order to construct an indicator for patient sedation, information from
#' the two concepts `trach` and `rass` is pooled: A patient is considered
#' sedated if intubated or has less or equal to -2 on the Richmond
#' Agitation-Sedation Scale.
#'
#' ## `gcs`
#' Aggregating components of the Glasgow Coma Scale into a total score
#' (whenever the total score `tgcs` is not already available) requires
#' coinciding availability of an eye (`egcs`), verbal (`vgcs`) and motor
#' (`mgcs`) score. In order to match values, a last observation carry forward
#' imputation scheme over the time span specified by `valid_win` is performed.
#' Furthermore passing `TRUE` as `set_sed_max` will assume maximal points for
#' time steps where the patient is sedated (as indicated by `sed`) and passing
#' `TRUE` as `set_na_max` will assume maximal points for missing values (after
#' matching and potentially applying `set_sed_max`).
#'
#' ## `urine24`
#' Single urine output events are aggregated into a 24 hour moving window sum.
#' At default value of `limits = NULL`, moving window evaluation begins with
#' the first and ends with the last available measurement. This can however be
#' extended by passing an `id_tbl` object, such as for example returned by
#' [stay_windows()] to full stay windows. In order to provide data earlier
#' than 24 hours before the evaluation start point, `min_win` specifies the
#' minimally required data window and the evaluation scheme is adjusted for
#' shorter than 24 hour windows.
#'
#' @param ... Data input used for concept calculation
#' @param match_win Time-span during which matching of values is allowed
#' @param mode Method for matching PaO\ifelse{latex
#' }{\out{\textsubscript{2}}}{\ifelse{html}{\out{<sub>2</sub>}}{2}} and
#' FiO\ifelse{latex}{\out{\textsubscript{2}}}{\ifelse{html
#' }{\out{<sub>2</sub>}}{2}} values
#' @param fix_na_fio2 Logical flag indicating whether to impute missing
#' FiO\ifelse{latex}{\out{\textsubscript{2}}}{\ifelse{html
#' }{\out{<sub>2</sub>}}{2}} values with 21
#' @param interval Expected time series step size (determined from data if
#' `NULL`)
#'
#' @encoding UTF-8
#' @rdname callback_cncpt
#' @export
#'
pafi <- function(..., match_win = hours(2L),
                 mode = c("match_vals", "extreme_vals", "fill_gaps"),
                 fix_na_fio2 = TRUE, interval = NULL) {

  mode <- match.arg(mode)

  cnc <- c("po2", "fio2")
  res <- capture_data(cnc, interval, ...)

  assert_that(is_interval(match_win), match_win > interval,
              is.flag(fix_na_fio2))

  if (identical(mode, "match_vals")) {

    on12 <- paste(meta_vars(res[[1L]]), "==", meta_vars(res[[2L]]))
    on21 <- paste(meta_vars(res[[2L]]), "==", meta_vars(res[[1L]]))

    res <- rbind(
      res[[1L]][res[[2L]], on = on12, roll = match_win],
      res[[2L]][res[[1L]], on = on21, roll = match_win]
    )
    res <- unique(res)

  } else {

    res <- reduce(merge, res, all = TRUE)

    if (identical(mode, "fill_gaps")) {
      res <- fill_gaps(res)
    }

    win_expr <- substitute(
      list(po2 = min_fun(get(cnc[1L])), fio2 = max_fun(get(cnc[2L]))),
      list(min_fun = min_or_na, max_fun = max_or_na)
    )

    res <- slide(res, !!win_expr, before = match_win, full_window = FALSE)
  }

  if (fix_na_fio2) {
    res <- res[is.na(get(cnc[2L])), c(cnc[2L]) := 21]
  }

  res <- res[!is.na(get(cnc[1L])) & !is.na(get(cnc[2L])) & get(cnc[2L]) != 0, ]
  res <- res[, c("pafi") := 100 * get(cnc[1L]) / get(cnc[2L])]
  res <- rm_cols(res, cnc)

  res
}

#' @param min_length Minimal time span between a ventilation start and end
#' time
#'
#' @rdname callback_cncpt
#' @export
#'
vent <- function(..., match_win = hours(6L), min_length = mins(10L),
                 interval = NULL) {

  subset_true <- function(x, col) x[is_true(get(col))]
  copy_time <- function(x, new, old) x[, c(new) := get(old)]

  final_int <- interval

  cnc <- c("vent_start", "vent_end")
  res <- capture_data(cnc, NULL, ...)

  if (is.null(final_int)) {
    final_int <- interval
  }

  assert_that(
    is_interval(final_int), is_interval(match_win), is_interval(min_length),
    min_length < match_win, interval < min_length
  )

  units(match_win) <- units(interval)
  units(min_length) <- units(interval)

  res <- Map(subset_true, res, cnc)
  sst <- c("start_time", "stop_time")

  if (has_rows(res[[2L]])) {

    res <- Map(copy_time, res, sst, chr_ply(res, index_var))
    jon <- unlist(
      do.call(map, c(paste, rev(lapply(res, meta_vars)), sep = " == "))
    )

    res <- res[[2L]][res[[1L]], roll = -match_win, on = jon]
    res <- res[is.na(get(sst[2L])), c(sst[2L]) := get(sst[1L]) + match_win]

  } else {

    ind <- index_var(res[[1L]])

    res <- copy(res[[1L]])
    res <- res[, c(sst) := list(get(ind), get(ind) + match_win)]
  }

  res <- res[get(sst[2L]) - get(sst[1L]) >= min_length, ]
  res <- change_interval(res, final_int, by_ref = TRUE)

  res <- unique(expand(res, start_var = sst[1L], end_var = sst[2L]))
  res <- res[, c("vent") := TRUE]

  res
}

#' @rdname callback_cncpt
#' @export
#'
sed <- function(..., interval = NULL) {

  cnc <- c("trach", "rass")
  res <- capture_data(cnc, interval, ...)

  res <- reduce(merge, res, all = TRUE)

  res <- res[, c("sed", cnc) := list(
    get(cnc[1L]) | get(cnc[2L]) <= -2, NULL, NULL)
  ]

  res
}

#' @param valid_win Maximal time window for which a GCS value is valid
#' if no newer measurement is available
#' @param set_sed_max Logical flag for considering sedation
#' @param set_na_max Logical flag controlling imputation of missing GCS values
#' with the respective maximum values
#'
#' @rdname callback_cncpt
#' @export
#'
gcs <- function(..., valid_win = hours(6L), set_sed_max = TRUE,
                set_na_max = TRUE, interval = NULL) {


  cnc <- c("egcs", "vgcs", "mgcs", "tgcs", "sed")
  res <- capture_data(cnc, interval, ...)

  assert_that(is_interval(valid_win), valid_win > interval,
              is.flag(set_sed_max), is.flag(set_na_max))

  res <- reduce(merge, res, all = TRUE)

  if (set_sed_max) {
    res <- res[is_true(get(cnc[5L])), c(cnc[-5L]) := list(4, 5, 6, 15)]
  }

  expr <- substitute(list(egcs = fun(egcs), vgcs = fun(vgcs),
                          mgcs = fun(mgcs), tgcs = fun(tgcs)),
                     list(fun = locf))

  res <- slide(res, !!expr, before = valid_win)

  if (set_na_max) {
    res <- res[, c(cnc[1L:3L]) := Map(replace_na, .SD, c(4, 5, 6)),
               .SDcols = cnc[1L:3L]]
  }

  res <- res[is.na(get(cnc[4L])), c(cnc[4L]) := rowSums(.SD),
             .SDcols = cnc[1L:3L]]

  if (set_na_max) {
    res <- res[, c(cnc[4L]) := list(replace_na(get(cnc[4L]), 15))]
  }

  res <- rename_cols(res, "gcs", cnc[4L])
  res <- rm_cols(res, cnc[1L:3L])

  res
}

#' @param min_win Minimal time span required for calculation of urine/24h
#' @param limits Passed to [fill_gaps()] in order to expand the time series
#' beyond first and last measurements
#' @param start_var,end_var Passed to [fill_gaps()]
#'
#' @rdname callback_cncpt
#' @export
#'
urine24 <- function(..., min_win = hours(12L), limits = NULL,
                    start_var = "start", end_var = "end", interval = NULL) {

  convert_dt <- function(x) as.double(x, units(interval))

  urine_sum <- function(x) {
    if (length(x) < min_steps) return(NA_real_)
    else sum(x, na.rm = TRUE) * step_factor / length(x)
  }

  res <- list(...)[[1L]]

  if (is.null(interval)) {
    interval <- interval(res)
  }

  assert_that(has_interval(res, interval), has_cols(res, "urine"),
              is_interval(min_win), min_win > interval, min_win <= hours(24L))

  min_steps   <- ceiling(convert_dt(min_win) / as.double(interval))
  step_factor <- convert_dt(hours(24L)) / as.double(interval)

  if (is.null(limits)) {
    limits <- collapse(res)
  }

  res <- fill_gaps(res, limits = limits)

  expr <- substitute(list(urine24 = win_agg_fun(urine)),
                     list(win_agg_fun = urine_sum))

  slide(res, !!expr, hours(24L))
}
