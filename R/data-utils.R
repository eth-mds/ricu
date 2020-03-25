
#' @export
data_ts <- function(source, table, row_expr, ...) {
  data_ts_quo(source, table, null_or_subs(row_expr), ...)
}

#' @export
data_ts_quo <- function(source, table, row_quo = NULL, cols = NULL,
                        id_col = default_id_col(config = cfg),
                        time_col = default_time_col(table, config = cfg),
                        interval = hours(1L),
                        cfg = get_col_config(source, "all")) {

  res <- data_tbl_quo(source, table, row_quo, cols, interval,
                      get_col_config(NULL, "data_fun", cfg))
  res <- as_ts_tbl(res, id_col, time_col, interval)

  res
}

#' @export
data_id <- function(source, table, row_expr, ...) {
  data_id_quo(source, table, null_or_subs(row_expr), ...)
}

#' @export
data_id_quo <- function(source, table, row_quo = NULL, cols = NULL,
                        id_col = default_id_col(source, cfg),
                        interval = hours(1L),
                        cfg = get_col_config(source, "all")) {

  if (!is.null(cols)) {
    cols <- c(id_col, cols)
  }

  res <- data_tbl_quo(source, table, row_quo, cols, interval,
                      get_col_config(NULL, "data_fun", cfg))
  res <- as_id_tbl(res, id_col)

  res
}

#' @export
data_tbl <- function(source, table, row_expr, ...) {
  data_tbl_quo(source, table, null_or_subs(row_expr), ...)
}

#' @export
data_tbl_quo <- function(source, table, row_quo = NULL, cols = NULL,
                         interval = hours(1L),
                         data_fun = get_col_config(source, "data_fun")) {

  assert_that(is.string(source), is.string(table),
              null_or(row_quo, is.language),
              null_or(cols, is.character),
              is_time(interval, allow_neg = FALSE),
              is.function(data_fun))

  res <- data_fun(table = table, row_quo = row_quo, cols = cols,
                  interval = interval, source = source)

  assert_that(is_dt(res))

  if (!is.null(cols)) {
    assert_that(has_cols(res, cols))
  }

  res[]
}

#' @export
get_col_config <- function(source = NULL,
                           what = c("tables", "id_cols", "data_fun", "all"),
                           config = get_config("default-cols"), ...) {

  what <- match.arg(what)

  if (!is.null(source)) {
    assert_that(is.string(source), has_name(config, source))
    config <- config[[source]]
  }

  if (identical(what, "all")) {

    config

  } else {

    assert_that(has_name(config, what))

    switch(
      what,
      tables   = get_default_cols(config[[what]], ...),
      id_cols  = get_id_col(config[[what]], ...),
      data_fun = get_data_fun(config[[what]], ...),
    )
  }
}

get_data_fun <- function(cfg) {

  if (is.null(cfg)) {
    default_data_fun
  } else {
    get(cfg, mode = "function")
  }
}

get_id_col <- function(cfg, type = "icustay") {

  opts <- c("icustay", "hadm", "patient")

  type <- match.arg(type, c(opts, "all"))

  if (identical(type, "all")) {

    res <- unlist(Filter(Negate(is.na), cfg[opts]))

    assert_that(length(res) > 0L)

  } else {

    assert_that(type %in% names(cfg))

    res <- cfg[[type]]

    if (is.null(res)) {
      stop("The selected data source does not allow for ", type, " ids to be ",
           "used.")
    }
  }

  res
}

get_default_cols <- function(cfg, table = NULL) {

  get_one <- function(x) {

    cfg_names <- c("time_col", "val_col", "unit_col")

    if (is.null(names(x))) {
      assert_that(identical(x, list()))
    }

    assert_that(all(names(x) %in% cfg_names))
    res <- setNames(x[cfg_names], cfg_names)
    assert_that(all(lgl_ply(res, is.string) | lgl_ply(res, is.null)))
    res
  }

  if (is.null(table)) {
    lapply(cfg, get_one)
  } else {
    assert_that(is.string(table), has_name(cfg, table))
    get_one(cfg[[table]])
  }
}


get_default_col <- function(name) {

  assert_that(is.string(name))

  function(table, source = NULL, ...) {
    assert_that(is.string(source), is.string(table))
    get_col_config(source, "tables", ..., table = table)[[name]]
  }
}

#' @export
default_time_col <- get_default_col("time_col")

#' @export
default_val_col <- get_default_col("val_col")

#' @export
default_unit_col <- get_default_col("unit_col")

default_id_col <- function(source, ...) {
  assert_that(is.string(source))
  get_col_config(source, "id_cols", ...)
}

