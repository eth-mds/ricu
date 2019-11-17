
#' Methods for creating and inspecting ts_tbl objects
#'
#' @param tbl An object inheriting from `data.frame`.
#' @param ts_key Character or numeric vector of at least length 1, identifying
#' columns that combined with time stamps, uniquely identify rows.
#' @param ts_index Character or numeric vector of length 1, identifying the
#' column that defines temporal ordering.
#' @param ts_step Scalar value, defining time steps between rows (assuming
#' complete time series data).
#'
#' @rdname ts_tbl
#'
#' @export
#'
as_ts_tbl <- function(tbl, ts_key, ts_index = NULL, ts_step = 1L) {

  assert_that(inherits(tbl, "data.frame"))

  data.table::setDT(tbl)

  new_ts_tbl(tbl, ts_key, ts_index, ts_step)
}

#' @param ... Passed to [data.table::data.table()]/generic compatibility.
#'
#' @rdname ts_tbl
#'
#' @export
#'
ts_tbl <- function(..., ts_key, ts_index = NULL, ts_step = 1L) {
  new_ts_tbl(data.table::data.table(...), ts_key, ts_index, ts_step)
}

new_ts_tbl <- function(tbl, ts_key, ts_index, ts_step) {

  assert_that(is_dt(tbl))

  set_ts_attrs(tbl, ts_key, ts_index, ts_step)

  by_cols <-  c(ts_key, ts_index)

  setkeyv(tbl, by_cols)
  setcolorder(tbl, c(by_cols, setdiff(colnames(tbl), by_cols)))
  setattr(tbl, "class", unique(c("ts_tbl", class(tbl))))

  tbl
}

set_ts_key <- function(x, val) {

  if (is.numeric(val)) {
    val <- colnames(x)[val]
  }

  assert_that(has_cols(x, val))

  setattr(x, "ts_key", val)
  invisible(x)
}

set_ts_index <- function(x, val) {

  if (is.null(val)) {
    val <- which(vapply(x, is_difftime, logical(1L)))
  }

  if (is.numeric(val)) {
    val <- colnames(x)[val]
  }

  assert_that(length(val) == 1L, has_cols(x, val), is_difftime(x[[val]]))

  setattr(x, "ts_index", val)
  invisible(x)
}

set_ts_step <- function(x, val) {

  assert_that(is.numeric(val), length(val) == 1L, val > 0)

  setattr(x, "ts_step", val)
  invisible(x)
}

set_ts_attrs <- function(x, ts_key, ts_index, ts_step) {

  set_ts_key(x, ts_key)
  set_ts_index(x, ts_index)
  set_ts_step(x, ts_step)

  invisible(x)
}

get_ts_attrs <- function(x) {
  list(ts_key = ts_key(x), ts_index = ts_index(x), ts_step = ts_step(x))
}

get_ts_spec <- function(x) {
  c(get_ts_attrs(x), list(ts_unit = time_unit(x)))
}

unclass_ts_tbl <- function(x) {

  setattr(x, "class", setdiff(class(x), "ts_tbl"))

  setattr(x, "ts_key", NULL)
  setattr(x, "ts_index", NULL)
  setattr(x, "ts_step", NULL)

  invisible(x)
}

reclass_ts_tbl <- function(x, ts_key, ts_index, ts_step, ts_unit,
                           warn_on_fail = TRUE) {

  if (check_ts_tbl(x, ts_key, ts_index, ts_unit)) {

    new_ts_tbl(x, ts_key, ts_index, ts_step)

  } else {

    if (warn_on_fail) {
      warning("Cannot reclass as `ts_tbl` according to specification.")
    }

    unclass_ts_tbl(x)

    x
  }
}

#' @param x A `ts_tbl` object.
#'
#' @rdname ts_tbl
#'
#' @export
#'
is_ts_tbl <- function(x) inherits(x, "ts_tbl")

#' @rdname ts_tbl
#'
#' @export
#'
`[.ts_tbl` <- function(x, ...) {
  spec <- get_ts_spec(x)
  res <- NextMethod()
  do.call(reclass_ts_tbl, c(list(res), spec, list(warn_on_fail = FALSE)))
}

#' @rdname ts_tbl
#'
#' @export
#'
dimnames.ts_tbl <- function(x) list(NULL, colnames(x))

#' @rdname ts_tbl
#'
#' @export
#'
print.ts_tbl <- function(x, ..., n = NULL, width = NULL, n_extra = NULL) {
  cat_line(format(x, ..., n = n, width = width, n_extra = n_extra))
  invisible(x)
}

#' @rdname ts_tbl
#'
#' @export
#'
format.ts_tbl <- function(x, ..., n = NULL, width = NULL, n_extra = NULL) {
  mat <- prt::trunc_dt(x, n = n, width = width, n_extra = n_extra)
  format(mat)
}

#' @rdname ts_tbl
#'
#' @importFrom tibble tbl_sum
#'
#' @export
#'
tbl_sum.ts_tbl <- function(x) {
  c("A ts_tbl" = prt::dim_desc(x),
    "Index" = paste0(ts_key(x), collapse = ", "),
    "Key" = ts_index(x),
    "Interval" = format(step_time(x)))
}

#' @param object A `ts_tbl` object.
#'
#' @rdname ts_tbl
#'
#' @export
#'
str.ts_tbl <- function(object, ...) {
  invisible(prt::str_dt(object, ...))
}

#' @rdname ts_tbl
#'
#' @export
#'
ts_index <- function(x) {
  assert_that(is_ts_tbl(x), has_attr(x, "ts_index"))
  attr(x, "ts_index")
}

#' @rdname ts_tbl
#'
#' @export
#'
ts_key <- function(x) {
  assert_that(is_ts_tbl(x), has_attr(x, "ts_key"))
  attr(x, "ts_key")
}

#' @rdname ts_tbl
#'
#' @export
#'
by_cols <- function(x) c(ts_key(x), ts_index(x))

#' @rdname ts_tbl
#'
#' @export
#'
val_cols <- function(x) setdiff(colnames(x), c(ts_key(x), ts_index(x)))

#' @rdname ts_tbl
#'
#' @export
#'
ts_step <- function(x) {
  assert_that(is_ts_tbl(x), has_attr(x, "ts_step"))
  attr(x, "ts_step")
}

#' @rdname ts_tbl
#'
#' @export
#'
step_time <- function(x) as.difftime(ts_step(x), units = time_unit(x))

#' @rdname ts_tbl
#'
#' @export
#'
time_unit <- function(x) units(x[[ts_index(x)]])

#' @param value New time unit.
#'
#' @rdname ts_tbl
#'
#' @export
#'
`time_unit<-` <- function(x, value) units(x[[ts_index(x)]]) <- value

#' @rdname ts_tbl
#'
#' @importFrom data.table as.data.table
#' @method as.data.table ts_tbl
#'
#' @export
#'
as.data.table.ts_tbl <- function(x, ...) {

  if (...length() > 0L) warning("Ignoring further `...` arguments.")

  setattr(x, "ts_key", NULL)
  setattr(x, "ts_index", NULL)
  setattr(x, "ts_step", NULL)
  setattr(x, "class", setdiff(class(x), "ts_tbl"))

  x
}

#' @param row.names,optional Generic consistency: passing anything other than
#' the default value issues a warning.
#'
#' @rdname ts_tbl
#'
#' @method as.data.frame ts_tbl
#'
#' @export
#'
as.data.frame.ts_tbl <- function(x, row.names = NULL, optional = FALSE, ...) {

  if (!is.null(row.names)) warning("Ignoring `row.names` argument.")
  if (!isFALSE(optional)) warning("Ignoring `optional` argument.")
  if (...length() > 0L) warning("Ignoring further `...` arguments.")

  data.table::setDF(as.data.table(x))

  x
}
