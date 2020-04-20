
#' @export
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

#' @export
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

#' @export
has_gaps <- Negate(has_no_gaps)

#' @export
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

#' @export
slide <- function(tbl, expr, ...) slide_quo(tbl, substitute(expr), ...)

#' @export
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

#' @export
slide_index <- function(tbl, expr, ...) {
  slide_index_quo(tbl, substitute(expr), ...)
}

#' @export
slide_index_quo <- function(x, expr, index, before, after = hours(0L), ...) {

  assert_that(is_time_vec(index), is_time(before, allow_neg = FALSE),
              is_time(after, allow_neg = FALSE))

  join <- x[, list(min_time = index - before,
                   max_time = index + after), by = c(id(x))]

  hop_quo(x, expr, join, ..., lwr_col = "min_time", upr_col = "max_time")
}

#' @export
hop <- function(tbl, expr, ...) hop_quo(tbl, substitute(expr), ...)

#' @export
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

#' @export
make_unique <- function(x, expr, fun, ...) {
  if (missing(fun)) {
    make_unique_quo(x, substitute(expr), ...)
  } else {
    make_unique_quo(x, fun, ...)
  }
}

#' @export
make_unique_quo <- function(x, expr, by = meta_cols(x),
                            cols = setdiff(colnames(x), by), ...) {

  assert_that(is_ts_tbl(x))

  if (nrow(x) == 0) return(x)
  if (length(cols) == 0L) return(unique(x))

  if (is.function(expr)) {
    x[, lapply(.SD, expr, ...), .SDcols = cols, by = by]
  } else {
    x[, eval(expr), by = by]
  }
}

#' @export
dt_gforce <- function(x,
                      fun = c("mean", "median", "min", "max", "sum", "prod",
                              "var", "sd", "first", "last"),
                      by = meta_cols(x), cols = data_cols(x),
                      na.rm = !fun %in% c("first", "last")) {

  fun <- match.arg(fun)

  if (fun %in% c("first", "last") && isTRUE(na.rm)) {
    warning("The argument `na.rm` is ignored for `first()` and `last()`")
  }

  assert_that(is.flag(na.rm), all(c(cols, by) %in% colnames(x)))

  switch(fun,
    mean   = x[, lapply(.SD, mean, na.rm = na.rm),   by = by, .SDcols = cols],
    median = x[, lapply(.SD, median, na.rm = na.rm), by = by, .SDcols = cols],
    min    = x[, lapply(.SD, min, na.rm = na.rm),    by = by, .SDcols = cols],
    max    = x[, lapply(.SD, max, na.rm = na.rm),    by = by, .SDcols = cols],
    sum    = x[, lapply(.SD, sum, na.rm = na.rm),    by = by, .SDcols = cols],
    prod   = x[, lapply(.SD, prod, na.rm = na.rm),   by = by, .SDcols = cols],
    var    = x[, lapply(.SD, var, na.rm = na.rm),    by = by, .SDcols = cols],
    sd     = x[, lapply(.SD, sd, na.rm = na.rm),     by = by, .SDcols = cols],
    first  = x[, lapply(.SD, data.table::first),     by = by, .SDcols = cols],
    last   = x[, lapply(.SD, data.table::last),      by = by, .SDcols = cols]
  )
}

#' @export
rm_na <- function(x, cols = data_cols(x), mode = c("all", "any")) {

  mode <- match.arg(mode)

  assert_that(has_cols(x, cols))

  if (identical(mode, "any")) {
    return(na.omit(x, cols))
  }

  if (length(cols) == 1L) {
    drop <- is.na(x[[cols]])
  } else {
    drop <- Reduce(`&`, lapply(x[, cols, with = FALSE], is.na))
  }

  x[!drop, ]
}

#' @export
unmerge <- function(x, var_cols = data_cols(x), fixed_cols = meta_cols(x),
                    na_rm = TRUE) {

  assert_that(all(c(var_cols, fixed_cols) %in% colnames(x)), is.flag(na_rm))

  extract_col <- function(col, x) {

    y <- x[, c(fixed_cols, col), with = FALSE]

    if (na_rm) {
      y <- rm_na(y, col)
    }

    y
  }

  res <- lapply(var_cols, extract_col, x)
  names(res) <- var_cols

  res
}
