
#' Methods for creating and inspecting ts_tbl objects
#'
#' @param tbl An object inheriting from `data.frame`.
#' @param id_cols Character or numeric vector of at least length 1, identifying
#' columns that combined with time stamps, uniquely identify rows.
#' @param time_col Character or numeric vector of length 1, identifying the
#' column that defines temporal ordering.
#' @param step_size Scalar value, defining time steps between rows (assuming
#' complete time series data).
#'
#' @rdname ts_tbl
#'
#' @export
#'
as_ts_tbl <- function(tbl, id_cols, time_col = NULL, step_size = 1L) {

  assert_that(inherits(tbl, "data.frame"))

  data.table::setDT(tbl)

  new_ts_tbl(tbl, id_cols, time_col, step_size)
}

#' @param ... Passed to [data.table::data.table()]/generic compatibility.
#'
#' @rdname ts_tbl
#'
#' @export
#'
ts_tbl <- function(..., id_cols, time_col = NULL, step_size = 1L) {
  new_ts_tbl(data.table::data.table(...), id_cols, time_col, step_size)
}

new_ts_tbl <- function(tbl, id_cols, time_col, step_size) {

  if (is.null(time_col)) {
    time_col <- which(vapply(tbl, is_difftime, logical(1L)))
  }

  if (is.numeric(id_cols))  id_cols  <- colnames(tbl)[id_cols]
  if (is.numeric(time_col)) time_col <- colnames(tbl)[time_col]

  assert_that(is_dt(tbl), length(id_cols) > 0L, length(time_col) == 1L,
              has_cols(tbl, c(id_cols, time_col)),
              is_difftime(tbl[[time_col]]),
              is.numeric(step_size), length(step_size) == 1L, step_size > 0)

  data.table::setkeyv(tbl, c(id_cols, time_col))

  structure(
    list(tbl = tbl, id_cols = id_cols, time_col = time_col,
         step_size = step_size),
    class = c("ts_tbl", class(tbl))
  )
}

tbl <- function(x) .subset2(x, "tbl")

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
`[[.ts_tbl` <- function(x, ...) `[[`(tbl(x), ...)

#' @rdname ts_tbl
#'
#' @export
#'
`[.ts_tbl` <- function(x, ...) {
  new_ts_tbl(`[`(tbl(x), ...), id_cols(x), time_col(x), step_size(x))
}

#' @rdname ts_tbl
#'
#' @export
#'
`$.ts_tbl` <- function(x, ...) `$`(tbl(x), ...)

#' @rdname ts_tbl
#'
#' @export
#'
dim.ts_tbl <- function(x) dim(tbl(x))

#' @rdname ts_tbl
#'
#' @export
#'
length.ts_tbl <- function(x) length(tbl(x))

#' @rdname ts_tbl
#'
#' @export
#'
dimnames.ts_tbl <- function(x) list(NULL, colnames(tbl(x)))

#' @rdname ts_tbl
#'
#' @export
#'
names.ts_tbl <- function(x) colnames(x)

#' @rdname ts_tbl
#'
#' @export
#'
time_col <- function(x) {
  assert_that(is_ts_tbl(x))
  .subset2(x, "time_col")
}

#' @rdname ts_tbl
#'
#' @export
#'
id_cols <- function(x) {
  assert_that(is_ts_tbl(x))
  .subset2(x, "id_cols")
}

#' @rdname ts_tbl
#'
#' @export
#'
val_cols <- function(x) setdiff(colnames(x), c(id_cols(x), time_col(x)))

#' @rdname ts_tbl
#'
#' @export
#'
step_size <- function(x) {
  assert_that(is_ts_tbl(x))
  .subset2(x, "step_size")
}

#' @rdname ts_tbl
#'
#' @export
#'
step_time <- function(x) as.difftime(step_size(x), units = time_unit(x))

#' @rdname ts_tbl
#'
#' @export
#'
time_unit <- function(x) units(x[[time_col(x)]])

#' @param value New time unit.
#'
#' @rdname ts_tbl
#'
#' @export
#'
`time_unit<-` <- function(x, value) units(x[[time_col(x)]]) <- value

#' @rdname ts_tbl
#'
#' @importFrom data.table as.data.table
#' @method as.data.table ts_tbl
#'
#' @export
#'
as.data.table.ts_tbl <- function(x, ...) {
  if (...length() > 0L) warning("Ignoring further `...` arguments.")
  tbl(x)
}
