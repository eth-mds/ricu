
#' @export
load_src <- function(x, rows = NULL, cols = NULL) UseMethod("load_src", x)

#' @export
load_src.data_src <- function(x, rows = NULL, cols = NULL) {
  subset(x, {{ rows }}, cols)
}

#' @export
load_difftime <- function(x, rows = NULL, cols = NULL,
                          id_hint = default_col(x, "id")) {

  UseMethod("load_difftime", x)
}

#' @export
load_difftime.mimic_src <- function(x, rows = NULL, cols = NULL,
                                    id_hint = default_col(x, "id")) {

  load_mihi(x, {{ rows }}, cols, id_hint)
}

#' @export
load_difftime.mimic_demo_src <- function(x, rows = NULL, cols = NULL,
                                         id_hint = default_col(x, "id")) {

  load_mihi(x, {{ rows }}, cols, id_hint)
}

#' @export
load_difftime.eicu_src <- function(x, rows = NULL, cols = NULL,
                                   id_hint = default_col(x, "id")) {

  load_eicu(x, {{ rows }}, cols)
}

#' @export
load_difftime.eicu_demo_src <- function(x, rows = NULL, cols = NULL,
                                        id_hint = default_col(x, "id")) {

  load_eicu(x, {{ rows }}, cols)
}

#' @export
load_difftime.hirid_src <- function(x, rows = NULL, cols = NULL,
                                    id_hint = default_col(x, "id")) {

  load_mihi(x, {{ rows }}, cols, id_hint)
}

load_mihi <- function(x, rows, cols, id_hint) {

  assert_that(is.string(id_hint))

  opts <- id_opts(x)

  if (id_hint %in% colnames(x)) {
    id_col <- id_hint
  } else {
    id_col <- tail(intersect(opts, colnames(x)), n = 1L)
  }

  assert_that(is.string(id_col))

  if (not_null(cols) && !id_col %in% cols) {
    cols <- c(id_col, cols)
  }

  dat <- load_src(x, {{ rows }}, cols)

  date_cols <- colnames(dat)[vapply(dat, inherits, logical(1L), "POSIXt")]

  if (length(date_cols)) {

    dat <- merge(dat, id_origin(x, id_col), by = id_col)
    dat <- dat[,
      c(date_cols) := lapply(.SD, difftime, get("origin"), units = "mins"),
      .SDcols = date_cols
    ]
    dat <- dat[, c("origin") := NULL]
  }

  as_id_tbl(dat, id = id_col)
}

load_eicu <- function(x, rows, cols) {

  id_col <- "patientunitstayid"

  if (not_null(cols) && !id_col %in% cols) {
    cols <- c(id_col, cols)
  }

  dat <- load_src(x, {{ rows }}, cols)

  date_cols <- colnames(dat)[
    lgl_ply(dat, is.integer) & grepl("offset$", colnames(dat))
  ]

  if (length(date_cols)) {

    assert_that(id_col %in% colnames(dat),
      msg = paste("In order to return relative times, a single ID column",
                  paste0("`", id_col, "`"), "is required.")
    )

    dat <- dat[, c(date_cols) := lapply(.SD, as.difftime, units = "mins"),
               .SDcols = date_cols]
  }

  as_id_tbl(dat, id = id_col)
}

#' @export
load_id <- function(x, rows = NULL, cols = NULL,
                    id_col = default_col(x, "id"),
                    interval = hours(1L)) {

  UseMethod("load_id", x)
}

#' @export
load_id.data_src <- function(x, rows = NULL, cols = NULL,
                             id_col = default_col(x, "id"),
                             interval = hours(1L)) {

  res <- load_difftime(x, {{ rows }}, cols, id_col)

  if (!identical(id_col, id(x))) {

  }

  res
}

#' @export
load_ts <- function(x, ...) UseMethod("load_ts", x)

#' @export
load_ts.data_src <- function(x, rows = NULL, cols = NULL,
                             id_col = default_col(x, "id"),
                             index_col = default_col(x, "index"),
                             interval = hours(1L)) {

  NULL
}

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
