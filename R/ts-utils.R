
#' Time series utility functions
#'
#' @param x `ts_tbl` object to use
#' @param min_col,max_col Name of the columns that represent lower and upper
#' windows bounds
#' @param step_size Controls the step size used to interpolate between
#' `min_col` and `max_col`
#' @param new_index Name of the new index column
#' @param keep_vars Names of the columns to hold onto
#'
#' @rdname ts_utils
#' @export
#'
expand <- function(x, min_col, max_col, step_size = time_step(x),
                   new_index = index_var(x), keep_vars = id_vars(x)) {

  do_seq <- function(min, max) seq(min, max, step_size)

  seq_expand <- function(min, max, unit, extra) {
    lst <- Map(do_seq, min, max)
    ind <- rep(seq_along(lst), lengths(lst))
    set(extra[ind, ], j = new_index,
        value = as.difftime(unlist(lst), units = unit))
  }

  assert_that(is_id_tbl(x), is.string(min_col), is.string(max_col),
              has_time_cols(x, c(min_col, max_col)),
              same_unit(x[[min_col]], x[[max_col]]))

  unit <- units(x[[min_col]])

  x <- rm_na(x, c(min_col, max_col), "any")
  x <- x[get(min_col) <= get(max_col), ]

  res <- x[, c(keep_vars, min_col, max_col), with = FALSE]
  res <- res[, c(min_col, max_col) := lapply(.SD, as.double),
             .SDcols = c(min_col, max_col)]

  res <- res[, seq_expand(get(min_col), get(max_col), unit, .SD),
             .SDcols = keep_vars]

  as_ts_tbl(res, index_var = new_index,
            interval = as.difftime(step_size, units = unit), by_ref = TRUE)
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
              identical(data.table::key(x), meta_vars(x)))

  id_cols <- id_vars(x)

  res <- x[, check_time_col(get(index_var(x)), time_step(x)),
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

  time_col <- index_var(x)

  if (is.null(limits)) {

    limits <- x[, list(min = min(get(time_col)), max = max(get(time_col))),
                by = c(id_vars(x))]

    join <- expand(limits, "min", "max", time_step(x), new_index = time_col)

  } else {

    id <- if (is_id_tbl(limits)) id_vars(limits) else id_vars(x)

    join <- expand(limits, ..., step_size = time_step(x),
                   new_index = time_col, keep_vars = id)
  }

  x[unique(join), on = paste(meta_vars(x), "==", meta_vars(join))]
}

#' @param expr Expression (quoted for `*_quo` and unquoted otherwise) to be
#' evaluated over each window
#' @param before,after Time span to look back/forward
#' @param ... Passed to `hop_quo()`
#'
#' @rdname ts_utils
#' @export
#'
slide <- function(x, expr, before, after = hours(0L), ...) {

  assert_that(is_scalar(before), is_interval(before),
              is_scalar(after), is_interval(after))

  id_cols <- id_vars(x)
  ind_col <- index_var(x)
  interva <- interval(x)
  time_ut <- time_unit(x)

  units(before) <- time_ut
  units(after)  <- time_ut

  join <- x[,
    c(mget(id_cols), list(min_time = get(ind_col) - before,
                          max_time = get(ind_col) + after))
  ]

  if (before == 0) {
    msg <- NULL
  } else {
    msg <- "Using 'by reference' syntax in expressions passed to `slide()`
            might yield undesired results if `before` > 0."
  }

  res <- hopper(x, {{ expr }}, join, lwr_col = "min_time",
                upr_col = "max_time", ..., nomatch = NA, warn_msg = msg)

  if (!is_ts_tbl(res)) {
    res <- set(res, j = ind_col, value = res[["min_time"]] + before)
    res <- as_ts_tbl(res, index_var = ind_col, interval = interva,
                     by_ref = TRUE)
  }

  res <- rm_cols(res, c("min_time", "max_time"), skip_absent = TRUE,
                 by_ref = TRUE)

  res
}

#' @param index A vector of times around which windows are spanned (relative
#' to the index)
#'
#' @rdname ts_utils
#' @export
#'
slide_index <- function(x, expr, index, before, after = hours(0L), ...) {

  assert_that(is_difftime(index), has_length(index), is_unique(index),
              is_scalar(before), is_interval(before),
              is_scalar(after),  is_interval(after))

  id_cols <- id_vars(x)
  ind_col <- index_var(x)
  interva <- interval(x)
  time_ut <- time_unit(x)

  units(before) <- time_ut
  units(after)  <- time_ut

  join <- x[, list(min_time = index - before,
                   max_time = index + after), by = c(id_cols)]

  res <- hopper(x, {{ expr }}, join, lwr_col = "min_time",
                upr_col = "max_time", ..., warn_msg = "
                Using 'by reference' syntax in expressions passed to
                `slide_index()` most likely will not yield the desired
                results.")

  if (!is_ts_tbl(res)) {
    res <- set(res, j = ind_col, value = res[["min_time"]] + before)
    res <- as_ts_tbl(res, index_var = ind_col, interval = interva,
                     by_ref = TRUE)
  }

  res <- rm_cols(res, c("min_time", "max_time"), skip_absent = TRUE,
                 by_ref = TRUE)

  res
}

#' @param windows An `icu_tbl` defining the windows to span
#' @param full_window Logical flag controlling how the situation is handled
#' where the sliding window extends beyond available data
#' @param lwr_col,upr_col Names of columns (in `windows`) of lower/upper
#' window bounds
#' @param left_closed,right_closed Logical flag indicating whether intervals
#' are closed (default) or open.
#' @param nomatch Forwarded to [`data.table::[()`][data.table]
#' @param eval_env Environment in which `expr` is substituted; `NULL` resolves
#' to the environment in which `expr` was created
#'
#' @importFrom rlang enexpr quo_get_expr quo_get_env caller_env is_quosure
#'
#' @rdname ts_utils
#' @export
#'
hop <- function(x, expr, windows, full_window = FALSE,
                lwr_col = "min_time", upr_col = "max_time",
                left_closed = TRUE, right_closed = TRUE,
                nomatch = NULL, eval_env = NULL) {

  msg <- "Using 'by reference' syntax in expressions passed to `hop()` most
          likely will not yield the desired results."

  hopper(x, {{ expr }}, windows, full_window, lwr_col, upr_col,
         left_closed, right_closed, nomatch, eval_env, msg)
}

hopper <- function(x, expr, windows, full_window = FALSE,
                   lwr_col = "min_time", upr_col = "max_time",
                   left_closed = TRUE, right_closed = TRUE,
                   nomatch = NULL, eval_env = NULL, warn_msg = FALSE) {

  assert_that(is_ts_tbl(x), is_unique(x), is.flag(full_window),
              is_id_tbl(windows), has_name(windows, c(lwr_col, upr_col)))

  win_id <- id_vars(windows)
  tbl_id <- id_vars(x)

  orig_unit <- time_unit(x)
  win_cols  <- c(lwr_col, upr_col)

  windows <- windows[, c(win_cols) := lapply(.SD, `units<-`, orig_unit),
                     .SDcols = win_cols]

  if (full_window) {

    extremes <- x[, list(grp_min = min(get(index_var(x))),
                         grp_max = max(get(index_var(x)))),
                  by = tbl_id]

    join <- c(paste(tbl_id, "==", win_id), paste("grp_min <=", lwr_col),
                                           paste("grp_max >=", upr_col))

    windows <- extremes[windows, on = join, nomatch = NULL]
    windows <- rename_cols(windows, c(win_id, lwr_col, upr_col), by_ref = TRUE)
    windows <- as_id_tbl(windows, win_id, by_ref = TRUE)
  }

  tbl_ind <- index_var(x)

  tmp_col <- new_names(x, n = 2L)
  x <- x[, c(tmp_col) := list(get(tbl_ind), get(tbl_ind))]
  on.exit(rm_cols(x, tmp_col, skip_absent = TRUE, by_ref = TRUE))

  join <- c(paste(tbl_id, "==", win_id),
            paste(tmp_col[2L], if (right_closed) "<=" else "<", upr_col),
            paste(tmp_col[1L], if (left_closed)  ">=" else ">", lwr_col))

  quo <- enexpr(expr)

  if (is_quosure(quo)) {
    env <- coalesce(eval_env, quo_get_env(quo))
    exp <- quo_get_expr(quo)
  } else {
    env <- coalesce(eval_env, caller_env(2))
    exp <- quo
  }

  if (not_null(warn_msg) && identical(exp[[1L]], as.symbol(":="))) {
    warn_ricu(warn_msg, class = "by_ref_slide")
  }

  .x_ <- .win_ <- .expr_ <- .join_ <- .nomatch_ <- NULL

  res <- local({
    if (isTRUE(is.na(.nomatch_))) {
      .x_[.win_, eval(.expr_), on = .join_, by = .EACHI]
    } else {
      .x_[.win_, eval(.expr_), on = .join_, by = .EACHI, nomatch = .nomatch_]
    }
  }, envir = list2env(
    list(.x_ = x, .win_ = windows, .expr_ = exp, .join_ = join,
         .nomatch_ = nomatch),
    parent = env
  ))

  rename_cols(res, win_cols, tmp_col, by_ref = TRUE)
}

locf <- function(x) {

  res <- last_elem(x)

  if (is.na(res)) {

    no_na <- !is.na(x)

    if (any(no_na)) {
      res <- last_elem(x[no_na])
    }
  }

  res
}
