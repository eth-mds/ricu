
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

has_no_gaps <- function(x) {

  check_time_col <- function(time, step) {
    len <- length(time)
    if (len < 2L) TRUE
    else identical(len - 1, as.numeric((time[len] - time[1L]) / step))
  }

  assert_that(is_ts_tbl(x), is_unique(x),
              identical(data.table::key(x), ts_id_cols(x)))

  key_cols <- ts_key(x)

  res <- x[, check_time_col(get(ts_index(x)), ts_time_step(x)),
           by = c(key_cols)]

  all(res[[setdiff(colnames(res), key_cols)]])
}

has_gaps <- Negate(has_no_gaps)

fill_gaps <- function(x, limits = NULL, ...) {

  assert_that(is_unique(x))

  time_col <- ts_index(x)

  if (is.null(limits)) {

    limits <- x[, list(min = min(get(time_col)), max = max(get(time_col))),
                by = c(ts_key(x))]

    join <- expand_limits(limits, "min", "max", ts_time_step(x), ts_key(x),
                          time_col)

  } else {

    join <- expand_limits(limits, ..., step_size = ts_time_step(x),
                          id_cols = ts_key(x), new_col = time_col)
  }

  assert_that(has_no_gaps(join))

  x[join, on = ts_id_cols(x)]
}

slide_expr <- function(tbl, expr, ...) slide_quo(tbl, substitute(expr), ...)

slide_quo <- function(x, expr, before, after = hours(0L),
                      full_window = FALSE) {

  assert_that(is_ts_tbl(x), is_unique(x), is.flag(full_window),
              is_time(before, allow_neg = FALSE),
              is_time(after, allow_neg = FALSE))

  time_col <- ts_index(x)
  id_cols  <- ts_key(x)
  time_unit <- ts_time_unit(x)

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

is_unique <- function(x, by = ts_id_cols(x)) {
  identical(anyDuplicated(x, by = by), 0L)
}

make_unique <- function(x, fun = mean, ...) {

  assert_that(is_ts_tbl(x), is.function(fun))

  if (nrow(x) == 0) return(x)

  cols <- setdiff(colnames(x), ts_id_cols(x))

  units <- lapply(cols, function(col) attr(x[[col]], "units"))

  x <- x[, lapply(.SD, fun, ...), .SDcols = cols, by = c(ts_id_cols(x))]

  Map(function(col, unit) setattr(x[[col]], "units", unit), cols, units)

  x
}

#' @export
secs <- function(x) as.difftime(x, units = "secs")

#' @export
mins <- function(x) as.difftime(x, units = "mins")

#' @export
hours <- function(x) as.difftime(x, units = "hours")

#' @export
days <- function(x) as.difftime(x, units = "days")

#' @export
weeks <- function(x) as.difftime(x, units = "weeks")

