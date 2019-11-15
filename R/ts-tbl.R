
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

  if (is.null(ts_index)) {
    ts_index <- which(vapply(tbl, is_difftime, logical(1L)))
  }

  if (is.numeric(ts_key))    ts_key  <- colnames(tbl)[ts_key]
  if (is.numeric(ts_index)) ts_index <- colnames(tbl)[ts_index]

  assert_that(is_dt(tbl), length(ts_key) > 0L, length(ts_index) == 1L,
              has_cols(tbl, c(ts_key, ts_index)),
              is_difftime(tbl[[ts_index]]),
              is.numeric(ts_step), length(ts_step) == 1L, ts_step > 0)

  by_cols <-  c(ts_key, ts_index)
  val_cols <- setdiff(colnames(tbl), by_cols)

  setkeyv(tbl, by_cols)
  setcolorder(tbl, c(by_cols, val_cols))

  setattr(tbl, "ts_key", ts_key)
  setattr(tbl, "ts_index", ts_index)
  setattr(tbl, "ts_step", ts_step)
  setattr(tbl, "class", c("ts_tbl", class(tbl)))

  tbl
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
  res <- NextMethod()
  new_ts_tbl(res, ts_key(x), ts_index(x), ts_step(x))
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
  assert_that(is_ts_tbl(x))
  attr(x, "ts_index")
}

#' @rdname ts_tbl
#'
#' @export
#'
ts_key <- function(x) {
  assert_that(is_ts_tbl(x))
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
  assert_that(is_ts_tbl(x))
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

#' @param y A `ts_tbl` object.
#' @rdname ts_tbl
#'
#' @export
#'
merge.ts_tbl <- function(x, y, by = by_cols(x), ...) {

  assert_that(is_ts_tbl(y), same_by_cols(x, y), same_time_unit(x, y))

  res <- NextMethod()

  new_ts_tbl(res, ts_key(x), ts_index(x), ts_step(x))
}

#' @inheritParams data.table::rbindlist
#'
#' @rdname ts_tbl
#'
#' @export
#'
rbind.ts_tbl <- function(x, ..., use.names = "check", fill = FALSE,
                         idcol = NULL) {

  dots <- list(...)

  if (length(dots) == 0L) return(x)

  assert_that(
    all(vapply(dots, same_by_cols, logical(1L), x)),
    all(vapply(dots, same_time_unit, logical(1L), x))
  )

  res <- data.table::rbindlist(c(list(x), dots), use.names, fill, idcol)

  new_ts_tbl(res, ts_key(x), ts_index(x), ts_step(x))
}

#' @rdname ts_tbl
#'
#' @export
#'
unique.ts_tbl <- function(x, ...) {
  new_ts_tbl(NextMethod(), ts_key(x), ts_index(x), ts_step(x))
}

#' @rdname ts_tbl
#'
#' @export
#'
is_unique <- function(x, by = by_cols(x)) {
  identical(anyDuplicated(x, by = by), 0L)
}
