
#' Time series utility functions
#'
#' ICU data as handled by `ricu` is mostly comprised of time series data and as
#' such, several utility functions are available for working with time series
#' data in addition to a class dedicated to representing time series data (see
#' [ts_tbl()]). Some terminology to begin with: a time series is considered
#' to have gaps if, per (combination of) ID variable value(s), some time steps
#' are missing. Expanding and collapsing mean to change between
#' representations where time steps are explicit or encoded as interval with
#' start and end times. For sliding window-type operations, `slide()` means to
#' iterate over time-windows, `slide_index()` means to iterate over certain
#' time-windows, selected relative to the index and `hop()` means to iterate
#' over time-windows selected in absolute terms.
#'
#' @details
#' A gap in a `ts_tbl` object is a missing time step, i.e. a missing entry in
#' the sequence `seq(min(index), max(index), by = interval)` in at least one
#' group (as defined by [id_vars()], where the extrema are calculated per
#' group. In this case, `has_gaps()` will return `TRUE`. The function
#' `is_regular()` checks whether the time series has no gaps, in addition to
#' the object being sorted and unique (see [is_sorted()] and [is_unique()]).
#' In order to transform a time series containing gaps into a regular time
#' series, `fill_gaps()` will fill missing time steps with `NA` values in all
#' [data_vars()] columns, while `remove_gaps()` provides the inverse operation
#' of removing time steps that consist of `NA` values in [data_vars()] columns.
#'
#' An `expand()` operation performed on an object inheriting from `data.table`
#' yields a `ts_tbl` where time-steps encoded by columns `start_var` and
#' `end_var` are made explicit with values in `keep_vars` being appropriately
#' repeated. The inverse operation is available as `collapse()`, which groups
#' by `id_vars`, represents `index_var` as group-wise extrema in two new
#' columns `start_var` and `end_var` and allows for further data summary using
#' `...`.
#'
#' Sliding-window type operations are available as `slide()`, `slide_index()`
#' and `hop()` (function naming is inspired by the CRAN package `slider`). The
#' most flexible of the three, `hop` takes as input a `ts_tbl` object `x`
#' containing the data, an `id_tbl` object `windows`, containing for each ID
#' the desired windows represented by two columns `lwr_col` and `upr_col`, as
#' well as an expression `expr` to be evaluated per window. At the other end
#' of the spectrum, `slide()` spans windows for every ID and available
#' time-step using the arguments `before` and `after`, while `slide_index()`
#' can be seen as a compromise between the two, where windows are spanned for
#' certain time-points, specified by `index`.
#'
#' @param x `ts_tbl` object to use
#' @param start_var,end_var Name of the columns that represent lower and upper
#' windows bounds
#' @param step_size Controls the step size used to interpolate between
#' `start_var` and `end_var`
#' @param new_index Name of the new index column
#' @param keep_vars Names of the columns to hold onto
#'
#' @examples
#' tbl <- ts_tbl(x = 1:5, y = hours(1:5), z = hours(2:6), val = rnorm(5),
#'               index_var = "y")
#' exp <- expand(tbl, "y", "z", step_size = 1L, new_index = "y",
#'               keep_vars = c("x", "val"))
#' col <- collapse(exp, start_var = "y", end_var = "z", val = unique(val))
#' all.equal(tbl, col, check.attributes = FALSE)
#'
#' tbl <- ts_tbl(x = rep(1:5, 1:5), y = hours(sequence(1:5)), z = 1:15)
#'
#' win <- id_tbl(x = c(3, 4), a = hours(c(2, 1)), b = hours(c(3, 4)))
#' hop(tbl, list(z = sum(z)), win, lwr_col = "a", upr_col = "b")
#' slide_index(tbl, list(z = sum(z)), hours(c(4, 5)), before = hours(2))
#' slide(tbl, list(z = sum(z)), before = hours(2))
#'
#' tbl <- ts_tbl(x = rep(3:4, 3:4), y = hours(sequence(3:4)), z = 1:7)
#' has_no_gaps(tbl)
#' is_regular(tbl)
#'
#' tbl[1, 2] <- hours(2)
#' has_no_gaps(tbl)
#' is_regular(tbl)
#'
#' tbl[6, 2] <- hours(2)
#' has_no_gaps(tbl)
#' is_regular(tbl)
#'
#' @rdname ts_utils
#' @export
#'
expand <- function(x, start_var = "start", end_var = "end", step_size = NULL,
                   new_index = NULL, keep_vars = id_vars(x)) {

  do_seq <- function(min, max) seq(min, max, step_size)

  seq_expand <- function(min, max, unit, extra) {
    lst <- Map(do_seq, min, max)
    ind <- rep(seq_along(lst), lengths(lst))
    set(extra[ind, ], j = new_index,
        value = as.difftime(unlist(lst), units = unit))
  }

  assert_that(
    is_dt(x), has_time_cols(x, c(start_var, end_var), 2L),
    same_unit(x[[start_var]], x[[end_var]])
  )

  if (identical(nrow(x), 0L)) {
    return(x)
  }

  step_size <- coalesce(step_size, time_step(x))
  new_index <- coalesce(new_index, index_var(x))

  assert_that(is.string(new_index), is_scalar(step_size),
              is.numeric(step_size))

  unit <- units(x[[start_var]])

  x <- rm_na(x, c(start_var, end_var), "any")
  x <- x[get(start_var) <= get(end_var), ]

  res <- x[, c(keep_vars, start_var, end_var), with = FALSE]
  res <- res[, c(start_var, end_var) := lapply(.SD, as.double),
             .SDcols = c(start_var, end_var)]

  res <- res[, seq_expand(get(start_var), get(end_var), unit, .SD),
             .SDcols = keep_vars]

  as_ts_tbl(res, index_var = new_index,
            interval = as.difftime(step_size, units = unit), by_ref = TRUE)
}

#' @param id_vars,index_var ID and index variables
#' @param env Environment used as parent to the environment used to evaluate
#' expressions passes as `...`
#'
#' @rdname ts_utils
#' @export
#'
collapse <- function(x, id_vars = NULL, index_var = NULL, start_var = "start",
                     end_var = "end", env = NULL, ...) {

  id_vars   <- coalesce(id_vars,   id_vars(x))
  index_var <- coalesce(index_var, index_var(x))

  if (is.null(env)) {
    env <- caller_env()
  }

  assert_that(is_dt(x), has_cols(x, id_vars), has_time_cols(x, index_var, 1L),
              is.string(start_var), is.string(end_var), is.environment(env))

  expr <- substitute(list(min(get(ind)), max(get(ind)), ...),
                     env = list(ind = index_var))

  names(expr)[c(2L, 3L)] <- c(start_var, end_var)

  expr <- do.call(substitute, list(substitute(expr)))

  .x_ <- .expr_ <- .by_ <- NULL

  local({
    .x_[, eval(.expr_), by = .by_]
  }, envir = list2env(
    list(.x_ = x, .expr_ = expr, .by_ = id_vars), parent = env)
  )
}

#' @rdname ts_utils
#' @export
#'
has_no_gaps <- function(x) {

  check_time_col <- function(time, step) {
    ext <- range(time, na.rm = TRUE)
    setequal(time, seq(ext[1L], ext[2L], by = step))
  }

  assert_that(is_ts_tbl(x))

  id_cols <- id_vars(x)

  expr <- substitute(
    check_time_col(as.double(get(idx)), step),
    env = list(idx = index_var(x), step = time_step(x))
  )

  res <- x[, eval(expr), by = c(id_cols)]

  all(res[[setdiff(colnames(res), id_cols)]])
}

#' @rdname ts_utils
#' @export
#'
has_gaps <- Negate(has_no_gaps)

#' @rdname ts_utils
#' @export
#'
is_regular <- function(x) {

  check_time_col <- function(time, step) {
    len <- length(time)
    if (len < 2L) TRUE
    else identical(len - 1, as.numeric((time[len] - time[1L]) / step))
  }

  assert_that(is_ts_tbl(x))

  if (!is_unique(x) || !is_sorted(x)) {
    return(FALSE)
  }

  id_cols <- id_vars(x)

  expr <- substitute(
    check_time_col(get(idx), step),
    env = list(idx = index_var(x), step = time_step(x))
  )

  res <- x[, eval(expr), by = c(id_cols)]

  all(res[[setdiff(colnames(res), id_cols)]])
}

#' @param limits A table with columns for lower and upper window bounds
#'
#' @rdname ts_utils
#' @export
#'
fill_gaps <- function(x, limits = collapse(x), start_var = "start",
                      end_var = "end") {

  assert_that(is_unique(x))

  if (is_id_tbl(limits)) {
    id_vars <- id_vars(limits)
  } else {
    id_vars <- id_vars(x)
  }

  join <- expand(limits, start_var, end_var, step_size = time_step(x),
                 new_index = index_var(x), keep_vars = id_vars)

  x[unique(join), on = paste(meta_vars(x), "==", meta_vars(join))]
}

#' @rdname ts_utils
#' @export
remove_gaps <- function(x) rm_na(x, data_vars(x), "all")

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

  if (identical(nrow(x), 0L)) {
    return(x)
  }

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
