
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

  cols <- unique(cols)

  subset(x, {{ rows }}, .env$cols, part_safe = TRUE)
}

#' @param src Passed to [as_src_tbl()] in order to determine the data source
#'
#' @rdname load_src
#' @export
load_src.character <- function(x, src, ...) {
  load_src(as_src_tbl(x, src), ...)
}

#' @export
load_src.default <- function(x, ...) stop_generic(x, .Generic)

#' @rdname load_src
#' @export
load_difftime <- function(x, ...) UseMethod("load_difftime", x)

#' @param id_hint String valued id column selection (not necessarily honored)
#' @param time_vars Character vector enumerating the columns to be treated as
#' timestamps and thus returned as [base::difftime()] vectors
#'
#' @rdname load_src
#' @export
load_difftime.mimic_tbl <- function(x, rows, cols = colnames(x),
                                    id_hint = id_vars(x),
                                    time_vars = ricu::time_vars(x), ...) {

  warn_dots(...)

  load_mihi(x, {{ rows }}, cols, id_hint, time_vars)
}

#' @rdname load_src
#' @export
load_difftime.eicu_tbl <- function(x, rows, cols = colnames(x),
                                   id_hint = id_vars(x),
                                   time_vars = ricu::time_vars(x), ...) {

  warn_dots(...)

  load_eicu(x, {{ rows }}, cols, id_hint, time_vars)
}

#' @rdname load_src
#' @export
load_difftime.hirid_tbl <- function(x, rows, cols = colnames(x),
                                    id_hint = id_vars(x),
                                    time_vars = ricu::time_vars(x), ...) {

  warn_dots(...)

  load_mihi(x, {{ rows }}, cols, id_hint, time_vars)
}

#' @rdname load_src
#' @export
load_difftime.character <- function(x, src, ...) {
  load_difftime(as_src_tbl(x, src), ...)
}

#' @export
load_difftime.default <- function(x, ...) stop_generic(x, .Generic)

resolve_id_hint <- function(tbl, hint) {

  assert_that(is.string(hint))

  if (hint %in% colnames(tbl)) {
    return(hint)
  }

  opts <- as_id_cfg(tbl)

  hits <- id_var_opts(opts) %in% colnames(tbl)

  assert_that(sum(hits) > 0L, msg = paste0("No overlap between configured id var options and available columns for table `", tbl_name(tbl), "` in ",
    src_name(tbl))
  )

  id_vars(opts[hits])
}

load_mihi <- function(x, rows, cols, id_hint, time_vars) {

  dt_round_min <- function(x, y) round_to(difftime(x, y, units = "mins"))

  id_col <- resolve_id_hint(x, id_hint)

  assert_that(is.string(id_col), id_col %in% colnames(x))

  if (!id_col %in% cols) {
    cols <- c(cols, id_col)
  }

  time_vars <- intersect(time_vars, cols)

  dat <- load_src(x, {{ rows }}, cols)

  if (length(time_vars)) {

    dat <- merge(dat, id_origin(x, id_col, origin_name = "origin"),
                 by = id_col)

    dat <- dat[,
      c(time_vars) := lapply(.SD, dt_round_min, get("origin")),
      .SDcols = time_vars
    ]
    dat <- dat[, c("origin") := NULL]
  }

  as_id_tbl(dat, id_vars = id_col, by_ref = TRUE)
}

load_eicu <- function(x, rows, cols, id_hint, time_vars) {

  id_col <- resolve_id_hint(x, id_hint)

  if (!id_col %in% cols) {
    cols <- c(id_col, cols)
  }

  time_vars <- intersect(time_vars, cols)

  dat <- load_src(x, {{ rows }}, cols)

  if (length(time_vars)) {

    assert_that(id_col %in% colnames(dat),
      msg = paste("In order to return relative times, a single ID var",
                  paste0("`", id_col, "`"), "is required.")
    )

    dat <- dat[, c(time_vars) := lapply(.SD, as.difftime, units = "mins"),
               .SDcols = time_vars]
  }

  as_id_tbl(dat, id_vars = id_col, by_ref = TRUE)
}

#' Load data as `id_tbl` or `ts_tbl` objects
#'
#' @inheritParams load_difftime
#'
#' @param id_var The column defining the id of `ts_tbl` and `id_tbl` objects
#' @param interval The time interval used to discretize time stamps with,
#' specified as [base::difftime()] object
#'
#' @rdname load_tbl
#' @export
load_id <- function(x, ...) UseMethod("load_id", x)

#' @rdname load_tbl
#' @export
load_id.src_tbl <- function(x, rows, cols = colnames(x),
                            id_var = id_vars(x), interval = hours(1L),
                            time_vars = ricu::time_vars(x), ...) {

  warn_dots(...)

  res <- load_difftime(x, {{ rows }}, cols, id_var, time_vars)

  time_vars <- intersect(time_vars, colnames(res))

  res <- change_id(res, id_var, x, cols = time_vars)

  if (!is_one_min(interval)) {
    res <- change_interval(res, interval, time_vars, by_ref = TRUE)
  }

  res
}

#' @rdname load_tbl
#' @export
load_id.character <- function(x, src, ...) {
  load_id(as_src_tbl(x, src), ...)
}

#' @rdname load_tbl
#' @export
load_id.default <- function(x, ...) load_id(as_src_tbl(x), ...)

#' @param index_var The column defining the index of `ts_tbl` objects
#'
#' @rdname load_tbl
#' @export
load_ts <- function(x, ...) UseMethod("load_ts", x)

#' @rdname load_tbl
#' @export
load_ts.src_tbl <- function(x, rows, cols = colnames(x), id_var = id_vars(x),
                            index_var = ricu::index_var(x),
                            interval = hours(1L),
                            time_vars = ricu::time_vars(x), ...) {

  warn_dots(...)

  assert_that(is.string(index_var))

  if (!index_var %in% cols) {
    cols <- c(cols, index_var)
  }

  res <- load_difftime(x, {{ rows }}, cols, id_var, time_vars)
  res <- as_ts_tbl(res, id_vars(res), index_var, mins(1L), by_ref = TRUE)

  time_vars <- intersect(time_vars, colnames(res))

  res <- change_id(res, id_var, x, cols = time_vars)

  change_interval(res, interval, time_vars, by_ref = TRUE)
}

#' @rdname load_tbl
#' @export
load_ts.character <- function(x, src, ...) {
  load_ts(as_src_tbl(x, src), ...)
}

#' @rdname load_tbl
#' @export
load_ts.default <- function(x, ...) load_ts(as_src_tbl(x), ...)
