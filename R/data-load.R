
#' Low level functions for loading data
#'
#' Depending on the desired return type (`ts_tbl`, `id_tbl` or plain
#' `data.table`) and on whether the row-subsetting argument is a quoted
#' expression (suffix `_quo`), this set of functions provides the basic
#' mechanisms for loading data.
#'
#' @param source String specifying the data source
#' @param table String specifying the table from which to load data
#' @param row_expr Expression to be quoted and used to define a row subsetting;
#' `NULL` corresponds to no row subsetting
#' @param ... Forwarded to the corresponding function that takes a quoted
#' expression
#'
#' @rdname load_data
#'
#' @export
#'
data_ts <- function(source, table, row_expr, ...) {
  data_ts_quo(source, table, null_or_subs(row_expr), ...)
}

#' @param row_quo Quoted expression used for defining a row subsetting; `NULL`
#' corresponds to no row subsetting
#' @param cols Character vector, specifying the column to return (in addition
#' to potential columns specified as `id_col` and `time_col`); `NULL`
#' corresponds to all columns
#' @param id_col The column defining the id of `ts_tbl` and `id_tbl` objects
#' @param time_col The column defining the index of `ts_tbl` objects
#' @param interval The time interval used to discretize time stamps with,
#' specified as [base::difftime()] object
#' @param cfg Configuration list used for determining default options for
#' `id_col` and `time_col` arguments, id options as well as the `data_fun`
#' argument
#'
#' @rdname load_data
#'
#' @export
#'
data_ts_quo <- function(source, table, row_quo = NULL, cols = NULL,
                        id_col = default_id_col(table, cfg),
                        time_col = default_time_col(table, cfg),
                        interval = hours(1L),
                        cfg = get_src_config(source)) {

  tbl <- get_tbl(table, source)
  ids <- get_id_cols(cfg)

  if (!id_col %in% colnames(tbl)) {
    aux_id <- tail(ids[ids %in% colnames(tbl)], n = 1L)
  } else {
    aux_id <- id_col
  }

  id_col <- ids[ids == id_col]

  if (!is.null(cols)) {
    cols <- c(aux_id, time_col, cols)
  }

  res <- data_tbl_quo(source, table, row_quo, cols, interval,
                      get_data_fun(cfg))
  res <- as_ts_tbl(res, aux_id, ids, time_col, interval)

  change_id(res, source, to = names(id_col), interval = interval)
}

#' @rdname load_data
#'
#' @export
#'
data_id <- function(source, table, row_expr, ...) {
  data_id_quo(source, table, null_or_subs(row_expr), ...)
}

#' @rdname load_data
#'
#' @export
#'
data_id_quo <- function(source, table, row_quo = NULL, cols = NULL,
                        id_col = default_id_col(table, cfg),
                        interval = hours(1L),
                        cfg = get_src_config(source)) {

  tbl <- get_tbl(table, source)
  ids <- get_id_cols(cfg)

  if (!id_col %in% colnames(tbl)) {
    aux_id <- tail(ids[ids %in% colnames(tbl)], n = 1L)
  } else {
    aux_id <- id_col
  }

  id_col <- ids[ids == id_col]

  if (!is.null(cols)) {
    cols <- c(aux_id, cols)
  }

  res <- data_tbl_quo(source, table, row_quo, cols, interval,
                      get_data_fun(cfg))
  res <- as_id_tbl(res, aux_id, ids)

  change_id(res, source, to = names(id_col), interval = interval)
}

#' @rdname load_data
#'
#' @export
#'
data_tbl <- function(source, table, row_expr, ...) {
  data_tbl_quo(source, table, null_or_subs(row_expr), ...)
}

#' @param data_fun Function used for loading the data; the default uses
#' [prt::subset_quo()] and for non-default values, a function with arguments
#' `table`, `row_quo`, `cols`, and `source` is expected
#'
#' @rdname load_data
#'
#' @export
#'
data_tbl_quo <- function(source, table, row_quo = NULL, cols = NULL,
                         interval = hours(1L),
                         data_fun = get_data_fun(source)) {

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
