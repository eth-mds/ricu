
check_cncpt_cb_args <- function(names, in_val, ...) {

  res <- list(...)

  if (is.null(in_val)) {
    for (x in res) {
      if (is_ts_tbl(x)) {
        in_val <- interval(x)
        break
      }
    }
  }

  if (is.null(names(res))) {
    names(res) <- names
  }

  assert_that(all_fun(res, has_interval, in_val),
              setequal(names, names(res)),
              all_map(has_cols, res[names], names))

  assign("interval", in_val, envir = caller_env())

  res
}

#' Concept callback functions
#'
#' Owing to increased complexity and more diverse applications, recursive
#' concepts (class [`rec_cncpt`][new_cncpt()]) may specify callback functions
#' to be called on corresponding data objects and perform post-processing
#' steps.
#'
#' @param ... Data input used for concept calculation
#' @param match_win Time-span during which matching of PaO₂ and FiO₂
#' values is allowed
#' @param mode Method for matching PaO₂ and FiO₂ values
#' @param fix_na_fio2 Logical flag indicating whether to impute missing FiO₂
#' values with 21
#' @param interval Expected time series step size (determined from data if
#' `NULL`)
#'
#' @rdname callback_cncpt
#' @export
#'
cb_pafi <- function(..., match_win = hours(2L),
                    mode = c("match_vals", "extreme_vals", "fill_gaps"),
                    fix_na_fio2 = TRUE, interval = NULL) {

  mode <- match.arg(mode)

  cnc <- c("po2", "fio2")
  res <- check_cncpt_cb_args(cnc, interval, ...)

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

#' @param match_win Default ventilation window if no stop time can be
#' matched to a start time
#' @param min_length Minimal time span between a ventilation start and end
#' time
#'
#' @rdname callback_cncpt
#' @export
#'
cb_vent <- function(..., match_win = hours(6L),
                    min_length = mins(10L), interval = NULL) {

  subset_true <- function(x, col) x[is_true(get(col))]
  copy_time <- function(x, new, old) x[, c(new) := get(old)]

  final_int <- interval

  cnc <- c("vent_start", "vent_end")
  res <- check_cncpt_cb_args(cnc, NULL, ...)

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

  res <- unique(expand(res, min_col = sst[1L], max_col = sst[2L]))
  res <- res[, c("vent") := TRUE]

  res
}

#' @rdname callback_cncpt
#' @export
#'
cb_sed <- function(..., interval = NULL) {

  cnc <- c("trach", "rass")
  res <- check_cncpt_cb_args(cnc, interval, ...)

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
cb_gcs <- function(..., valid_win = hours(6L), set_sed_max = TRUE,
                   set_na_max = TRUE, interval = NULL) {


  cnc <- c("egcs", "vgcs", "mgcs", "tgcs", "sed")
  res <- check_cncpt_cb_args(cnc, interval, ...)

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
#'
#' @rdname callback_cncpt
#' @export
#'
cb_urine24 <- function(..., min_win = hours(12L), interval = NULL) {

  convert_dt <- function(x) as.double(x, units(interval))

  urine_sum <- local({

    min_steps <- ceiling(convert_dt(min_win) / as.double(interval))
    step_factor <- convert_dt(hours(24L)) / as.double(interval)

    function(x) {
      if (length(x) < min_steps) return(NA_real_)
      else sum(x, na.rm = TRUE) * step_factor / length(x)
    }
  })

  res <- check_cncpt_cb_args("urine", interval, ...)[[1L]]

  assert_that(is_interval(min_win), min_win > interval, min_win <= hours(24L))

  res <- fill_gaps(res)

  expr <- substitute(list(urine24 = win_agg_fun(urine)),
                     list(win_agg_fun = urine_sum))

  slide(res, !!expr, hours(24L))
}
