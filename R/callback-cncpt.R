
collect_dots <- function(concepts, interval, ..., merge_dat = FALSE) {

  assert_that(is.character(concepts))

  dots <- list(...)

  if (length(concepts) == 1L) {

    assert_that(identical(length(dots), 1L))

    res <- dots[[1L]]

    if (is_ts_tbl(res)) {
      ival <- coalesce(interval, interval(res))
      assert_that(has_interval(res, ival))
    } else {
      assert_that(is_df(res))
    }

    return(res)
  }

  if (length(dots) == 1L) {
    dots <- dots[[1L]]
  }

  if (is.null(names(dots))) {
    names(dots) <- concepts
  }

  if (not_null(names(concepts))) {
    concepts <- chr_ply(concepts, grep, names(dots), value = TRUE,
                        use_names = TRUE)
  }

  assert_that(setequal(names(dots), concepts))

  res <- dots[concepts]

  assert_that(all_map(has_col, res, concepts))

  if (not_null(names(concepts))) {
    names(res) <- names(concepts)
  }

  ival <- check_interval(res, interval)

  if (merge_dat) {
    res <- reduce(merge, res, all = TRUE)
  } else {
    attr(res, "ival_checked") <- ival
  }

  res
}

check_interval <- function(dat, ival = NULL) {

  check_ival <- function(x, iv) {
    is_df(x) && (!is_ts_tbl(x) || has_interval(x, iv))
  }

  if (has_attr(dat, "ival_checked")) {

    ival <- attr(dat, "ival_checked")

  } else if (is_ts_tbl(dat)) {

    if (is.null(ival)) {
      ival <- interval(dat)
    } else {
      assert_that(has_interval(dat, ival))
    }

  } else if (is_df(dat) || all_fun(dat, Negate(is_ts_tbl))) {

    ival <- NULL

  } else {

    if (is.null(ival)) {
      for (x in dat) {
        if (is_ts_tbl(x)) {
          ival <- interval(x)
          break
        }
      }
    }

    assert_that(all_fun(dat, check_ival, ival))
  }

  invisible(ival)
}

rename_data_var <- function(new_name, old_name = NULL) {
  function(...) rename_cols(..1, new_name, coalesce(old_name, data_var(..1)))
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
#' ## `vent_dur` and `vent_ind`
#' Building on the atomic concepts `vent_start` and `vent_end`, `vent_dur`
#' determines time windows during which patients are mechanically ventilated
#' by combining start and end events that are separated by at most `match_win`
#' and at least `min_length`. Durations can be converted into an indicator
#' varaible represented by `vent_ind`, where time-points (as determined by
#' `interval`) that fall into such ventilation windows are set to `TRUE`,
#' while missingness (`NA`) or `FALSE` indicate no mechanical ventilation.
#' Currently, no clear distinction between invasive an non-invasive
#' ventilation is made.
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
#' ## `vaso60`
#' Building on concepts for drug administration rate and drug administration
#' durations, administration events are filtered if they do not fall into
#' administrations windows of at least 1h. The `max_gap` argument can be used
#' to control how far apart windows can be in order to be merged (negative
#' times are possible as well, meaning that even overlapping windows can be
#' considered as individual windows).
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
#' @return Either an `id_tbl` or `ts_tbl` depending on the type of concept.
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
  res <- collect_dots(cnc, interval, ...)

  assert_that(is_interval(match_win), match_win > check_interval(res),
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
vent_dur <- function(..., match_win = hours(6L), min_length = mins(30L),
                     interval = NULL) {

  subset_true <- function(x, col) x[is_true(get(col))]
  calc_dur <- function(x, y) fifelse(is.na(y), x + match_win, y - x)

  final_int <- interval

  cnc <- c("vent_start", "vent_end")
  res <- collect_dots(cnc, NULL, ...)

  interval <- check_interval(res)

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
  var <- "vent_dur"

  if (has_rows(res[[2L]])) {

    idx_vars  <- chr_ply(res, index_var)
    res[[2L]] <- res[[2L]][, c(var, idx_vars[2L]) := list(
      get(idx_vars[2L]), get(idx_vars[2L]) - mins(1L))]

    jon <- chr_ply(do.call(map, c("c", lapply(rev(res), meta_vars))), paste,
                   collapse = " == ")


    res <- res[[2L]][res[[1L]], roll = -match_win, on = jon]
    res <- res[, c(var, cnc) := list(
      calc_dur(get(idx_vars[2L]), get(var)), NULL, NULL)]
    res <- res[get(var) >= min_length, ]

  } else {

    res <- res[[1L]][, c(var, "vent_start") := list(match_win, NULL)]
  }

  res <- change_interval(res, final_int, by_ref = TRUE)

  aggregate(res, "max")
}

#' @rdname callback_cncpt
#' @export
#'
vent_ind <- function(..., interval = NULL) {

  cnc <- "vent_dur"
  res <- collect_dots(cnc, interval, ...)
  idx <- index_var(res)
  res <- res[, c(cnc) := get(idx) + get(cnc)]

  res <- expand(res, idx, cnc)
  res <- unique(res)
  res <- res[, c("vent_ind") := TRUE]

  res
}

#' @rdname callback_cncpt
#' @export
#'
sed <- function(..., interval = NULL) {

  cnc <- c("trach", "rass")
  res <- collect_dots(cnc, interval, ..., merge_dat = TRUE)

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
  res <- collect_dots(cnc, interval, ..., merge_dat = TRUE)

  assert_that(is_interval(valid_win), valid_win > check_interval(res),
              is.flag(set_sed_max), is.flag(set_na_max))

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

  res      <- collect_dots("urine", interval, ...)
  interval <- check_interval(res)

  assert_that(is_interval(min_win), min_win > interval, min_win <= hours(24L))

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

#' @param max_gap Maximum time gap between administration windows that are
#' merged (can be negative).
#'
#' @rdname callback_cncpt
#' @export
#'
vaso60 <- function(..., max_gap = mins(5L), interval = NULL) {

  final_int <- interval

  dat <- collect_dots(c(rate = "_rate$", dur = "_dur$"), NULL, ...)

  if (any(int_ply(dat, nrow) == 0L)) {
    res <- dat[["rate"]]
    res <- rename_cols(res[0L], sub, data_vars(res), pattern = "_rate$",
                       replacement = "60")
    return(res)
  }

  interval <- check_interval(dat)

  if (is.null(final_int)) {
    final_int <- interval
  }

  assert_that(is_interval(final_int))

  dur <- dat[["dur"]]
  dva <- data_vars(dur)
  idx <- index_var(dur)
  dur <- dur[get(dva) > 0, ]

  dur <- dur[, c(dva) := get(idx) + get(dva)]
  dur <- merge_ranges(dur, idx, dva, max_gap = max_gap, by_ref = TRUE)
  dur <- dur[get(dva) - get(idx) >= hours(1L), ]

  rate <- dat[["rate"]]
  temp <- new_names(c(colnames(dur), colnames(rate)), 2L)
  rate <- rate[, c(temp) := get(index_var(rate))]
  on.exit(rm_cols(rate, temp, by_ref = TRUE))

  join <- c(
    paste(id_vars(rate), id_vars(dur), sep = " == "),
    paste(temp, c(">=", "<="), c(index_var(dur), data_vars(dur)))
  )

  res <- rate[dur, on = join, nomatch = NULL]
  res <- rm_cols(res, temp, by_ref = TRUE)
  res <- rename_cols(res, sub, data_vars(res), by_ref = TRUE,
                     pattern = "_rate$", replacement = "60")
  res <- change_interval(res, final_int, by_ref = TRUE)

  if (max_gap < 0L) {
    res <- unique(res)
  }

  aggregate(res, "max")
}

#' @rdname callback_cncpt
#' @export
vaso_ind <- function(..., interval = NULL) {

  cnc <- c("dopa_dur", "norepi_dur", "dobu_dur", "epi_dur")
  res <- collect_dots(cnc, interval, ..., merge_dat = TRUE)
  unt <- time_unit(res)

  res <- res[, c(cnc) := lapply(.SD, as.difftime, units = unt), .SDcols = cnc]
  res <- res[, c("vaso_ind", cnc) := list(pmax(
    get("dopa_dur"), get("norepi_dur"), get("dobu_dur"), get("epi_dur"),
    na.rm = TRUE), NULL, NULL, NULL, NULL)
  ]

  res <- expand(res, index_var(res), "vaso_ind")
  res <- unique(res)
  res <- res[, c("vaso_ind") := TRUE]

  res
}

#' @rdname callback_cncpt
#' @export
supp_o2 <- function(..., interval = NULL) {

  cnc <- c("vent_ind", "fio2")
  res <- collect_dots(cnc, interval, ..., merge_dat = TRUE)

  res <- res[, c("supp_o2", "vent_ind", "fio2") := list(
    get("vent_ind") | get("fio2") > 21, NULL, NULL
  )]

  res
}

map_vals <- function(pts, vals) {
  function(x) pts[findInterval(x, vals, left.open = TRUE) + 1]
}

#' @rdname callback_cncpt
#' @export
avpu <- function(..., interval = NULL) {

  avpu_map <- map_vals(c(NA, "U", "P", "V", "A", NA), c(2, 3, 9, 13, 15))

  res <- collect_dots("gcs", interval, ...)
  res <- res[, c("avpu", "gcs") := list(avpu_map(get("gcs")), NULL)]

  res
}

#' @rdname callback_cncpt
#' @export
bmi <- function(..., interval = NULL) {

  cnc <- c("weight", "height")
  res <- collect_dots(cnc, interval, ..., merge_dat = TRUE)
  res <- res[, c("bmi", cnc) := get("weight") / get("height") ^ 2]

  res
}

#' @rdname callback_cncpt
#' @export
norepi_equiv <- function(..., interval = NULL) {

  multiply_rename <- function(x, fact, col) {
    x <- x[, c(col) := get(col) * fact]
    x <- rename_cols(x, "norepi_equiv", col)
    x
  }

  cnc <- c("epi_rate", "norepi_rate", "dopa_rate", "adh_rate", "phn_rate")
  res <- collect_dots(cnc, interval, ...)
  res <- map(multiply_rename, res, 1 / c(1, 1, 150, 0.4, 10), cnc)

  res <- rbind_lst(res, fill = TRUE)

  aggregate(res)
}
