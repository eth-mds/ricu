
#' Low level functions for loading data
#'
#' Depending on the desired return type (`ts_tbl`, `id_tbl` or plain
#' `data.table`) and on whether the row-subsetting argument is a quoted
#' expression (suffix `_quo`), this set of functions provides the basic
#' mechanisms for loading data.
#'
#' @param x Object for which to load data
#' @param rows Expression used for row subsetting (NSE)
#' @param cols Character vector of column names
#'
#' @rdname load_src
#' @export
#'
load_src <- function(x, rows, cols = colnames(x)) {

  assert_that(is.character(cols), length(cols) > 0L)

  UseMethod("load_src", x)
}

#' @rdname load_src
#' @export
load_src.data_src <- function(x, rows, cols = colnames(x)) {
  subset(x, {{ rows }}, cols)
}

#' @param id_hint String valued id column selection (not necessarily honored)
#'
#' @rdname load_src
#' @export
load_difftime <- function(x, rows, cols = colnames(x),
                          id_hint = default_col(x, "id")) {

  assert_that(is.string(id_hint))

  UseMethod("load_difftime", x)
}

#' @rdname load_src
#' @export
load_difftime.mimic_src <- function(x, rows, cols = colnames(x),
                                    id_hint = default_col(x, "id")) {

  load_mihi(x, {{ rows }}, cols, id_hint)
}

#' @rdname load_src
#' @export
load_difftime.mimic_demo_src <- function(x, rows, cols = colnames(x),
                                         id_hint = default_col(x, "id")) {

  load_mihi(x, {{ rows }}, cols, id_hint)
}

#' @rdname load_src
#' @export
load_difftime.eicu_src <- function(x, rows, cols = colnames(x),
                                   id_hint = default_col(x, "id")) {

  load_eicu(x, {{ rows }}, cols)
}

#' @rdname load_src
#' @export
load_difftime.eicu_demo_src <- function(x, rows, cols = colnames(x),
                                        id_hint = default_col(x, "id")) {

  load_eicu(x, {{ rows }}, cols)
}

#' @rdname load_src
#' @export
load_difftime.hirid_src <- function(x, rows, cols = colnames(x),
                                    id_hint = default_col(x, "id")) {

  load_mihi(x, {{ rows }}, cols, id_hint)
}

load_mihi <- function(x, rows, cols, id_hint) {

  dt_round_min <- function(x, y) round_to(difftime(x, y, units = "mins"))

  opts <- id_types(x)

  if (id_hint %in% colnames(x)) {
    id_col <- id_hint
  } else {
    id_col <- tail(intersect(opts, colnames(x)), n = 1L)
  }

  assert_that(is.string(id_col), id_col %in% colnames(x))

  if (!id_col %in% cols) {
    cols <- c(cols, id_col)
  }

  dat <- load_src(x, {{ rows }}, cols)

  date_cols <- colnames(dat)[vapply(dat, inherits, logical(1L), "POSIXt")]

  if (length(date_cols)) {

    dat <- merge(dat, id_origin(x, id_col), by = id_col)
    dat <- dat[,
      c(date_cols) := lapply(.SD, dt_round_min, get("origin")),
      .SDcols = date_cols
    ]
    dat <- dat[, c("origin") := NULL]
  }

  as_id_tbl(dat, id = id_col)
}

load_eicu <- function(x, rows, cols) {

  id_col <- "patientunitstayid"

  assert_that(id_col %in% colnames(x))

  if (!id_col %in% cols) {
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

#' @param id_col The column defining the id of `ts_tbl` and `id_tbl` objects
#' @param interval The time interval used to discretize time stamps with,
#' specified as [base::difftime()] object
#'
#' @rdname load_src
#' @export
load_id <- function(x, rows, cols = colnames(x), id_col = default_col(x, "id"),
                    interval = hours(1L)) {

  assert_that(is.string(id_col))

  UseMethod("load_id", x)
}

#' @rdname load_src
#' @export
load_id.data_src <- function(x, rows, cols = colnames(x),
                             id_col = default_col(x, "id"),
                             interval = hours(1L)) {

  res <- load_difftime(x, {{ rows }}, cols, id_col)

  tim <- time_cols(res)
  res <- change_id(res, id_col, get_id_cols(x), cols = tim)

  if (length(tim) && !is_one_min(interval)) {
    res <- set_interval(res, interval, tim)
  }

  res
}

#' @param index_col The column defining the index of `ts_tbl` objects
#'
#' @rdname load_src
#' @export
load_ts <- function(x, rows, cols = colnames(x),
                    id_col = default_col(x, "id"),
                    index_col = default_col(x, "index"),
                    interval = hours(1L)) {

  assert_that(is.string(index_col))

  UseMethod("load_ts", x)
}

#' @rdname load_src
#' @export
load_ts.data_src <- function(x, rows, cols = colnames(x),
                             id_col = default_col(x, "id"),
                             index_col = default_col(x, "index"),
                             interval = hours(1L)) {

  if (!index_col %in% cols) {
    cols <- c(cols, index_col)
  }

  res <- load_difftime(x, {{ rows }}, cols, id_col)
  res <- as_ts_tbl(res, id(res), index = index_col, interval = mins(1L))

  tim <- time_cols(res)
  res <- change_id(res, id_col, get_id_cols(x), cols = tim)

  if (!is_one_min(interval)) {
    res <- set_interval(res, interval, tim)
  }

  res
}

#' @param source String specifying the data source
#' @param table String specifying the table from which to load data
#' @param row_expr Expression to be quoted and used to define a row subsetting;
#' `NULL` corresponds to no row subsetting
#' @param ... Forwarded to the corresponding function (`load_id()` or
#' `load_ts()`)
#'
#' @rdname load_src
#' @export
#'
data_ts <- function(source, table, row_expr, ...) {
  message("`data_ts()` is deprecated, please use `load_ts()` instead.")
  load_ts(get(table, envir = get_src_env(source)), {{ row_expr }}, ...)
}

#' @rdname load_src
#' @export
data_id <- function(source, table, row_expr, ...) {
  message("`data_id()` is deprecated, please use `load_id()` instead.")
  load_id(get(table, envir = get_src_env(source)), {{ row_expr }}, ...)
}
