
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

  warn_dots(...)

  assert_that(is.character(cols), length(cols) > 0L)

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
#' @param dftm_vars Character vector enumerating the columns to be treated as
#' timestamps and thus returned as `difftime` vectors
#'
#' @rdname load_src
#' @export
load_difftime.mimic_tbl <- function(x, rows, cols = colnames(x),
                                    id_hint = default_var(x, "id_var"),
                                    dftm_vars = time_vars(x), ...) {

  warn_dots(...)

  load_mihi(x, {{ rows }}, cols, id_hint, dftm_vars)
}

#' @rdname load_src
#' @export
load_difftime.mimic_demo_tbl <- function(x, rows, cols = colnames(x),
                                         id_hint = default_var(x, "id_var"),
                                         dftm_vars = time_vars(x), ...) {

  warn_dots(...)

  load_mihi(x, {{ rows }}, cols, id_hint, dftm_vars)
}

#' @rdname load_src
#' @export
load_difftime.eicu_tbl <- function(x, rows, cols = colnames(x),
                                   id_hint = default_var(x, "id_var"),
                                   dftm_vars = time_vars(x), ...) {

  warn_dots(...)

  load_eicu(x, {{ rows }}, cols, id_hint, dftm_vars)
}

#' @rdname load_src
#' @export
load_difftime.eicu_demo_tbl <- function(x, rows, cols = colnames(x),
                                        id_hint = default_var(x, "id_var"),
                                        dftm_vars = time_vars(x), ...) {

  warn_dots(...)

  load_eicu(x, {{ rows }}, cols, id_hint, dftm_vars)
}

#' @rdname load_src
#' @export
load_difftime.hirid_tbl <- function(x, rows, cols = colnames(x),
                                    id_hint = default_var(x, "id_var"),
                                    dftm_vars = time_vars(x), ...) {

  warn_dots(...)

  load_mihi(x, {{ rows }}, cols, id_hint, dftm_vars)
}

#' @rdname load_src
#' @export
load_difftime.character <- function(x, src, ...) {
  load_difftime(as_src_tbl(x, src), ...)
}

load_mihi <- function(x, rows, cols, id_hint, dftm_vars) {

  dt_round_min <- function(x, y) round_to(difftime(x, y, units = "mins"))

  assert_that(is.string(id_hint))

  opts <- get_id_var(x)

  if (id_hint %in% colnames(x)) {
    id_col <- id_hint
  } else {
    id_col <- tail(intersect(opts, colnames(x)), n = 1L)
  }

  assert_that(is.string(id_col), id_col %in% colnames(x))

  if (!id_col %in% cols) {
    cols <- c(cols, id_col)
  }

  dftm_vars <- intersect(dftm_vars, cols)

  dat <- load_src(x, {{ rows }}, cols)

  if (length(dftm_vars)) {

    dat <- merge(dat, id_origin(x, id_col), by = id_col)
    dat <- dat[,
      c(dftm_vars) := lapply(.SD, dt_round_min, get("origin")),
      .SDcols = dftm_vars
    ]
    dat <- dat[, c("origin") := NULL]
  }

  as_id_tbl(dat, id_vars = id_col, by_ref = TRUE)
}

load_eicu <- function(x, rows, cols, id_hint, dftm_vars) {

  if (id_hint %in% colnames(x)) {
    id_col <- id_hint
  } else {
    id_col <- "patientunitstayid"
  }

  if (!id_col %in% cols) {
    cols <- c(id_col, cols)
  }

  dftm_vars <- intersect(dftm_vars, cols)

  dat <- load_src(x, {{ rows }}, cols)

  if (length(dftm_vars)) {

    assert_that(id_col %in% colnames(dat),
      msg = paste("In order to return relative times, a single ID var",
                  paste0("`", id_col, "`"), "is required.")
    )

    dat <- dat[, c(dftm_vars) := lapply(.SD, as.difftime, units = "mins"),
               .SDcols = dftm_vars]
  }

  as_id_tbl(dat, id_vars = id_col, by_ref = TRUE)
}

#' @param id_var The column defining the id of `ts_tbl` and `id_tbl` objects
#' @param interval The time interval used to discretize time stamps with,
#' specified as [base::difftime()] object
#'
#' @rdname load_src
#' @export
load_id <- function(x, ...) UseMethod("load_id", x)

#' @rdname load_src
#' @export
load_id.src_tbl <- function(x, rows, cols = colnames(x),
                            id_var = default_var(x, "id_var"),
                            interval = hours(1L), dftm_vars = time_vars(x),
                            ...) {

  warn_dots(...)

  res <- load_difftime(x, {{ rows }}, cols, id_var, dftm_vars)

  dftm_vars <- intersect(dftm_vars, colnames(res))

  res <- change_id(res, id_var, as_id_cfg(x), cols = dftm_vars)

  if (!is_one_min(interval)) {
    res <- change_interval(res, interval, dftm_vars, by_ref = TRUE)
  }

  res
}

#' @rdname load_src
#' @export
load_id.character <- function(x, src, ...) {
  load_id(as_src_tbl(x, src), ...)
}

#' @param index_var The column defining the index of `ts_tbl` objects
#'
#' @rdname load_src
#' @export
load_ts <- function(x, ...) UseMethod("load_ts", x)

#' @rdname load_src
#' @export
load_ts.src_tbl <- function(x, rows, cols = colnames(x),
                            id_var = default_var(x, "id_var"),
                            index_var = default_var(x, "index_var"),
                            interval = hours(1L), dftm_vars = time_vars(x),
                            ...) {

  warn_dots(...)

  assert_that(is.string(index_var))

  if (!index_var %in% cols) {
    cols <- c(cols, index_var)
  }

  res <- load_difftime(x, {{ rows }}, cols, id_var, dftm_vars)
  res <- as_ts_tbl(res, id_vars(res), index_var, mins(1L), by_ref = TRUE)

  dftm_vars <- intersect(dftm_vars, colnames(res))

  res <- change_id(res, id_var, as_id_cfg(x), cols = dftm_vars)

  change_interval(res, interval, dftm_vars, by_ref = TRUE)
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
