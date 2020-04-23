
#' Time series utility functions
#'
#' @param x `ts_tbl` object to use
#' @param min_col,max_col Name of the columns that represent lower and upper
#' windows bounds
#' @param step_size Controls the step size used to interpolate between
#' `min_col` and `max_col`
#' @param id_cols Names of the id columns
#' @param new_col Name of the new index column
#'
#' @rdname ts_utils
#' @export
#'
expand_limits <- function(x, min_col = "min", max_col = "max", step_size = 1L,
                          id_cols = NULL, new_col = "hadm_time") {

  seq_time <- function(min, max, step, unit) {
    do_seq <- function(min, max) seq(min, max, step)
    as.difftime(unlist(Map(do_seq, min, max)), units = unit)
  }

  assert_that(is_dt(x), has_time_col(x, min_col), has_time_col(x, max_col),
              same_time_unit(x[[min_col]], x[[max_col]]))

  unit <- units(x[[min_col]])

  x <- na.omit(x, c(id_cols, min_col, max_col))
  x <- x[get(min_col) <= get(max_col), ]

  res <- x[, lapply(.SD, as.double), .SDcols = c(min_col, max_col),
           by = id_cols]
  res <- res[, seq_time(get(min_col), get(max_col), step_size, unit),
           by = id_cols]

  setnames(res, c(id_cols, new_col))

  as_ts_tbl(res, id_cols, index = new_col,
            interval = as.difftime(step_size, units = unit))
}

#' @rdname ts_utils
#' @export
#'
has_no_gaps <- function(x) {

  check_time_col <- function(time, step) {
    len <- length(time)
    if (len < 2L) TRUE
    else identical(len - 1, as.numeric((time[len] - time[1L]) / step))
  }

  assert_that(is_ts_tbl(x), is_unique(x),
              identical(data.table::key(x), meta_cols(x)))

  id_cols <- id(x)

  res <- x[, check_time_col(get(index(x)), time_step(x)),
           by = c(id_cols)]

  all(res[[setdiff(colnames(res), id_cols)]])
}

#' @rdname ts_utils
#' @export
#'
has_gaps <- Negate(has_no_gaps)

#' @param limits A table with columns for lower and upper window bounds
#'
#' @rdname ts_utils
#' @export
#'
fill_gaps <- function(x, limits = NULL, ...) {

  assert_that(is_unique(x))

  time_col <- index(x)

  if (is.null(limits)) {

    limits <- x[, list(min = min(get(time_col)), max = max(get(time_col))),
                by = c(id(x))]

    join <- expand_limits(limits, "min", "max", time_step(x), id(x), time_col)

  } else {

    id <- if (is_icu_tbl(limits)) id(limits) else id(x)

    join <- expand_limits(limits, ..., step_size = time_step(x),
                          id_cols = id, new_col = time_col)
  }

  x[unique(join), on = paste(meta_cols(x), "==", meta_cols(join))]
}

#' @param expr Expression (quoted for `*_quo` and unquoted otherwise) to be
#' evaluated over each window
#'
#' @rdname ts_utils
#' @export
#'
slide <- function(x, expr, ...) slide_quo(x, substitute(expr), ...)

#' @param before,after Time span to look back/forward
#' @param ... Passed to the `*_quo` version and from there to `hop_quo()`
#'
#' @rdname ts_utils
#' @export
#'
slide_quo <- function(x, expr, before, after = hours(0L), ...) {

  assert_that(is_time(before, allow_neg = FALSE),
              is_time(after, allow_neg = FALSE))

  id_cols <- id(x)
  ind_col <- index(x)

  join <- x[,
    c(mget(id_cols), list(min_time = get(ind_col) - before,
                          max_time = get(ind_col) + after))
  ]

  hop_quo(x, expr, join, ..., lwr_col = "min_time", upr_col = "max_time")
}

#' @rdname ts_utils
#' @export
#'
slide_index <- function(x, expr, ...) {
  slide_index_quo(x, substitute(expr), ...)
}

#' @param index A vector of times around which windows are spanned (relative
#' to the index)
#'
#' @rdname ts_utils
#' @export
#'
slide_index_quo <- function(x, expr, index, before, after = hours(0L), ...) {

  assert_that(is_time_vec(index), is_time(before, allow_neg = FALSE),
              is_time(after, allow_neg = FALSE))

  join <- x[, list(min_time = index - before,
                   max_time = index + after), by = c(id(x))]

  hop_quo(x, expr, join, ..., lwr_col = "min_time", upr_col = "max_time")
}

#' @rdname ts_utils
#' @export
#'
hop <- function(x, expr, ...) hop_quo(x, substitute(expr), ...)

#' @param windows An `icu_tbl` defining the windows to span
#' @param full_window Logical flag controlling how the situation is handled
#' where the sliding window extends beyond available data
#' @param lwr_col,upr_col Names of columns (in `windows`) of lower/upper
#' window bounds
#'
#' @rdname ts_utils
#' @export
#'
hop_quo <- function(x, expr, windows, full_window = FALSE,
                    lwr_col = "min_time", upr_col = "max_time") {

  assert_that(is_ts_tbl(x), is_unique(x), is.flag(full_window),
              is_icu_tbl(windows), has_name(windows, c(lwr_col, upr_col)))

  win_id <- id(windows)
  tbl_id <- id(x)

  if (full_window) {

    extremes <- x[, list(grp_min = min(get(index(x))),
                         grp_max = max(get(index(x)))),
                  by = tbl_id]

    join <- c(paste(tbl_id, "==", win_id), paste("grp_min <=", lwr_col),
                                           paste("grp_max >=", upr_col))

    windows <- extremes[windows, on = join, nomatch = NULL]
    windows <- as_id_tbl(rename_cols(windows, c(win_id, lwr_col, upr_col)),
                         id = win_id)
  }

  tbl_ind <- index(x)

  tmp_col <- new_names(x)
  set(x, j = tmp_col, value = x[[tbl_ind]])
  on.exit(set(x, j = tmp_col, value = NULL))

  join <- c(paste(tbl_id, "==", win_id), paste(tbl_ind, "<=", upr_col),
                                         paste(tmp_col, ">=", lwr_col))

  res <- x[windows, eval(expr), on = join, by = .EACHI, nomatch = NULL]

  assert_that(is_unique(res))

  set(res, j = tmp_col, value = NULL)

  res
}

