
expand_limits <- function(x, min_col = "min", max_col = "max", step_size = 1L,
                          id_cols = NULL, new_col = "hadm_time") {

  seq_time <- function(min, max, step, unit) {
    as.difftime(seq(min, max, step_size), units = unit)
  }

  assert_that(is_dt(x), has_time_col(x, min_col), has_time_col(x, max_col),
              same_time_unit(x[[min_col]], x[[max_col]]))

  unit <- units(x[[min_col]])

  res <- x[, lapply(.SD, as.numeric), .SDcols = c(min_col, max_col),
           by = id_cols]
  res <- res[, seq_time(get(min_col), get(max_col), step_size, unit),
           by = id_cols]

  setnames(res, c(id_cols, new_col))

  new_ts_tbl(res, id_cols, new_col, step_size)
}

has_gaps <- function(x) {

  check_time_col <- function(time, step) {
    len <- length(time)
    if (len < 2L) TRUE
    else identical(len - 1, as.numeric((time[len] - time[1L]) / step))
  }

  assert_that(is_ts_tbl(x), is_unique(x),
              identical(data.table::key(x), by_cols(x)))

  res <- x[, check_time_col(get(ts_index(x)), ts_step(x)), by = c(ts_key(x))]

  all(res[[setdiff(colnames(res), ts_key(x))]])
}

fill_gaps <- function(x, limits = NULL, ...) {

  assert_that(is_unique(x))

  time_col <- ts_index(x)

  if (is.null(limits)) {

    limits <- x[, list(min = min(get(time_col)), max = max(get(time_col))),
                by = c(ts_key(x))]

    join <- expand_limits(limits, "min", "max", ts_step(x), ts_key(x),
                          time_col)

  } else {

    join <- expand_limits(limits, ..., step_size = ts_step(x),
                          id_cols = ts_key(x), new_col = time_col)
  }

  assert_that(has_gaps(join))

  x[join, on = by_cols(x), nomatch = NULL]
}

slide_expr <- function(tbl, expr, ...) slide_quo(tbl, substitute(expr), ...)

slide_quo <- function(x, expr, before, after = hours(0L),
                      full_window = FALSE) {

  assert_that(is_ts_tbl(x), is_unique(x), is.flag(full_window),
              is_difftime(before, allow_neg = FALSE),
              is_difftime(after, allow_neg = FALSE))

  time_col <- ts_index(x)
  id_cols  <- ts_key(x)

  units(before) <- time_unit(x)
  units(after)  <- time_unit(x)

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
