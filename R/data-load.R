
#' Low level functions for loading data
#'
#' Depending on the desired return type (`ts_tbl`, `id_tbl` or plain
#' `data.table`) and on whether the row-subsetting argument is a quoted
#' expression (suffix `_quo`), this set of functions provides the basic
#' mechanisms for loading data.
#'
#' @param x Object for which to load data
#' @param ... Generic consistency
#'
#' @rdname load_src
#' @export
#'
load_src <- function(x, ...) UseMethod("load_src", x)

#' @param rows Expression used for row subsetting (NSE)
#' @param cols Character vector of column names
#'
#' @rdname load_src
#' @export
load_src.src_tbl <- function(x, rows, cols = colnames(x), ...) {

  assert_that(is.character(cols), length(cols) > 0L, ...length() == 0L)

  subset(x, {{ rows }}, unique(cols))
}

#' @param src Passed to [as_src_tbl()] in order to determine the data source
#'
#' @rdname load_src
#' @export
load_src.character <- function(x, src, ...) {
  load_src(as_src_tbl(x, src), ...)
}

#' @rdname load_src
#' @export
load_difftime <- function(x, ...) UseMethod("load_difftime", x)

#' @param id_hint String valued id column selection (not necessarily honored)
#'
#' @rdname load_src
#' @export
load_difftime.mimic_tbl <- function(x, rows, cols = colnames(x),
                                    id_hint = default_col(x, "id"), ...) {

  assert_that(...length() == 0L)

  load_mihi(x, {{ rows }}, cols, id_hint)
}

#' @rdname load_src
#' @export
load_difftime.mimic_demo_tbl <- function(x, rows, cols = colnames(x),
                                         id_hint = default_col(x, "id"), ...) {

  assert_that(...length() == 0L)

  load_mihi(x, {{ rows }}, cols, id_hint)
}

#' @rdname load_src
#' @export
load_difftime.eicu_tbl <- function(x, rows, cols = colnames(x), ...) {

  load_eicu(x, {{ rows }}, cols)
}

#' @rdname load_src
#' @export
load_difftime.eicu_demo_tbl <- function(x, rows, cols = colnames(x), ...) {

  load_eicu(x, {{ rows }}, cols)
}

#' @rdname load_src
#' @export
load_difftime.hirid_tbl <- function(x, rows, cols = colnames(x),
                                    id_hint = default_col(x, "id"), ...) {

  assert_that(...length() == 0L)

  load_mihi(x, {{ rows }}, cols, id_hint)
}

#' @rdname load_src
#' @export
load_difftime.character <- function(x, src, ...) {
  load_difftime(as_src_tbl(x, src), ...)
}

load_mihi <- function(x, rows, cols, id_hint) {

  dt_round_min <- function(x, y) round_to(difftime(x, y, units = "mins"))

  assert_that(is.string(id_hint))

  opts <- get_id_col(x)

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
load_id <- function(x, ...) UseMethod("load_id", x)

#' @rdname load_src
#' @export
load_id.src_tbl <- function(x, rows, cols = colnames(x),
                             id_col = default_col(x, "id"),
                             interval = hours(1L), ...) {

  assert_that(...length() == 0L)

  res <- load_difftime(x, {{ rows }}, cols, id_col)

  tim <- time_cols(res)
  res <- change_id(res, id_col, as_id_cfg(x), cols = tim)

  if (length(tim) && !is_one_min(interval)) {
    res <- set_interval(res, interval, tim)
  }

  res
}

#' @rdname load_src
#' @export
load_id.character <- function(x, src, ...) {
  load_id(as_src_tbl(x, src), ...)
}

#' @param index_col The column defining the index of `ts_tbl` objects
#'
#' @rdname load_src
#' @export
load_ts <- function(x, ...) UseMethod("load_ts", x)

#' @rdname load_src
#' @export
load_ts.src_tbl <- function(x, rows, cols = colnames(x),
                            id_col = default_col(x, "id"),
                            index_col = default_col(x, "index"),
                            interval = hours(1L), ...) {

  assert_that(is.string(index_col), ...length() == 0L)

  if (!index_col %in% cols) {
    cols <- c(cols, index_col)
  }

  res <- load_difftime(x, {{ rows }}, cols, id_col)
  res <- as_ts_tbl(res, id(res), index = index_col, interval = mins(1L))

  tim <- time_cols(res)
  res <- change_id(res, id_col, as_id_cfg(x), cols = tim)

  if (!is_one_min(interval)) {
    res <- set_interval(res, interval, tim)
  }

  res
}

#' @rdname load_src
#' @export
load_ts.character <- function(x, src, ...) {
  load_ts(as_src_tbl(x, src), ...)
}

#' @param source String specifying the data source
#' @param table String specifying the table from which to load data
#'
#' @rdname load_src
#' @export
data_id <- function(source, table, ...) {
  message("`data_id()` is deprecated, please use `load_id()` instead.")
  load_id(table, source, ...)
}

#' @rdname load_src
#' @export
data_ts <- function(source, table, ...) {
  message("`data_ts()` is deprecated, please use `load_ts()` instead.")
  load_ts(table, source, ...)
}
