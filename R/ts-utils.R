
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

  res <- x[, lapply(.SD, as.numeric), .SDcols = c(min_col, max_col),
           by = id_cols]
  res <- res[, seq_time(get(min_col), get(max_col), step_size, unit),
           by = id_cols]

  setnames(res, c(id_cols, new_col))

  as_ts_tbl(res, id_cols, new_col, step_size)
}

#' @export
has_no_gaps <- function(x) {

  check_time_col <- function(time, step) {
    len <- length(time)
    if (len < 2L) TRUE
    else identical(len - 1, as.numeric((time[len] - time[1L]) / step))
  }

  assert_that(is_ts_tbl(x), is_unique(x),
              identical(data.table::key(x), id_cols(x)))

  key_cols <- key(x)

  res <- x[, check_time_col(get(index(x)), time_step(x)),
           by = c(key_cols)]

  all(res[[setdiff(colnames(res), key_cols)]])
}

#' @export
has_gaps <- Negate(has_no_gaps)

#' @export
fill_gaps <- function(x, limits = NULL, ...) {

  assert_that(is_unique(x))

  time_col <- index(x)

  if (is.null(limits)) {

    limits <- x[, list(min = min(get(time_col)), max = max(get(time_col))),
                by = c(key(x))]

    join <- expand_limits(limits, "min", "max", time_step(x), key(x), time_col)

  } else {

    join <- expand_limits(limits, ..., step_size = time_step(x),
                          id_cols = key(x), new_col = time_col)
  }

  assert_that(has_no_gaps(join))

  x[join, on = id_cols(x)]
}

#' @export
slide <- function(tbl, expr, ...) slide_quo(tbl, substitute(expr), ...)

#' @export
slide_quo <- function(x, expr, before, after = hours(0L),
                      full_window = FALSE) {

  assert_that(is_ts_tbl(x), is_unique(x), is.flag(full_window),
              is_time(before, allow_neg = FALSE),
              is_time(after, allow_neg = FALSE))

  time_col <- index(x)
  id_cols  <- key(x)
  time_unit <- time_unit(x)

  units(before) <- time_unit
  units(after)  <- time_unit

  join <- x[,
    c(mget(id_cols), list(min_time = get(time_col) - before,
                          max_time = get(time_col) + after))
  ]

  if (full_window) {

    extremes <- x[, list(grp_min = min(get(time_col)),
                         grp_max = max(get(time_col))),
                  by = id_cols]

    join <- extremes[join, on = c(id_cols, "grp_min <= min_time",
                                           "grp_max >= max_time"),
                     nomatch = NULL]

    setnames(join, c(id_cols, "min_time", "max_time"))
  }

  set(x, j = "extra_time", value = x[[time_col]])
  on.exit(set(x, j = "extra_time", value = NULL))

  on_clauses <- c(
    id_cols, paste(time_col, "<= max_time"), "extra_time >= min_time"
  )

  res <- x[join, eval(expr), on = on_clauses, by = .EACHI, nomatch = NULL]
  set(res, j = "extra_time", value = NULL)

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
make_unique_quo <- function(x, expr, by = id_cols(x),
                            cols = setdiff(colnames(x), by), ...) {

  assert_that(is_ts_tbl(x))

  if (nrow(x) == 0) return(x)
  if (length(cols) == 0L) return(unique(x))

  if (is.function(expr)) {
    x <- x[, lapply(.SD, expr, ...), .SDcols = cols, by = by]
  } else {
    x <- x[, eval(expr), by = by]
  }

  assert_that(is_unique(x, by = by))

  x
}

#' @export
is_unique.ts_tbl <- function(x, by = id_cols(x), ...) {
  identical(anyDuplicated(x, by = by, ...), 0L)
}

#' @export
is_unique.default <- function(x, ...) identical(anyDuplicated(x, ...), 0L)

#' @export
any_date <- function(x, col = index(x)) is_any_date_helper(x, col, 1L)

#' @export
is_date <- function(x, col = index(x)) is_any_date_helper(x, col, nrow(x))

is_any_date_helper <- function(x, col, length) {

  aux_col <- aux_names(x, "ts_date", col, FALSE)

  if (is.null(aux_col)) rep(FALSE, length)
  else if (is.na(aux_col)) rep(TRUE, length)
  else if (length == 1L) any(x[[aux_col]])
  else x[[aux_col]]
}

compact_unit <- function(x, col, handler = NULL, expected = NULL) {

  unit <- aux_names(x, "ts_unit", col, FALSE)

  assert_that(is.string(unit))

  unit <- ts_meta(x, "ts_unit")
  hits <- col == meta_names(unit)

  assert_that(!is.null(units), sum(hits) == 1L)

}
