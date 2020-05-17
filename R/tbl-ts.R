
#' @rdname icu_tbl
#' @export
#'
ts_tbl <- function(..., id, id_opts = NULL, index = NULL,
                   interval = hours(1L)) {

  as_ts_tbl(data.table::data.table(...), id, id_opts, index, interval)
}

#' @rdname icu_tbl
#' @export
#'
as_ts_tbl <- function(x, ...) UseMethod("as_ts_tbl", x)

#' @param index Name of index column
#' @param interval Time spacing between rows
#'
#' @rdname icu_tbl
#' @export
#'
as_ts_tbl.default <- function(x, id, id_opts = NULL, index = NULL,
                              interval = hours(1L), ...) {

  if (!is_dt(x)) data.table::setDT(x)

  if (is.numeric(id) || is.logical(id)) {
    id <- colnames(x)[id]
  }

  new_icu_tbl(x,
    new_ts_meta(new_tbl_id(id, id_opts),
                new_tbl_index(auto_index(x, index), interval))
  )
}

auto_index <- function(x, index = NULL) {

  if (is.null(index)) {

    hits <- vapply(x, is_time_vec, logical(1L))

    assert_that(sum(hits) == 1L,
      msg = paste("In order to automatically determine the index column,",
                  "exactly one `difftime` column is required.")
    )

    index <- colnames(x)[hits]

  } else if (is.numeric(index) || is.logical(index)) {

    index <- colnames(x)[index]
  }

  index
}

#' @rdname icu_tbl
#' @export
#'
is_ts_tbl <- function(x) inherits(x, "ts_tbl")

#' @rdname icu_tbl
#' @export
#'
as_id_tbl.ts_tbl <- function(x, ...) {
  new_icu_tbl(x, new_id_meta(tbl_id(x)))
}

#' @export
tbl_index.icu_tbl <- function(x) tbl_index(tbl_meta(x))

#' @export
set_index.ts_tbl <- function(x, value) {
  set_meta(x, set_index(tbl_meta(x), value))
}

#' @export
interval.ts_tbl <- function(x) {
  `units<-`(interval(tbl_meta(x)), time_unit(x))
}

#' @export
set_interval.ts_tbl <- function(x, value, cols = time_cols(x), ...) {

  change_time <- function(x) re_time(x, value)

  assert_that(...length() == 0L)

  if (interval(x) > value) {
    warning("Higher time resolution does not add missing time steps")
  }

  assert_that(index(x) %in% cols)

  meta <- set_interval(tbl_meta(x), value)

  x <- x[, c(cols) := lapply(.SD, change_time), .SDcols = cols]

  reclass_tbl(x, meta)
}

#' @export
time_unit.ts_tbl <- function(x) units(time_col(x))

#' @export
set_time_unit.ts_tbl <- function(x, value, cols = time_cols(x), ...) {

  change_units <- function(x) `units<-`(x, value)

  assert_that(...length() == 0L)

  assert_that(index(x) %in% cols)

  meta <- set_time_unit(tbl_meta(x), value)

  x <- x[, c(cols) := lapply(.SD, change_units), .SDcols = cols]

  reclass_tbl(x, meta)
}

#' @rdname meta_utils
#' @export
#'
time_step <- function(x) as.double(interval(x), units = time_unit(x))

#' @rdname meta_utils
#' @export
#'
time_col <- function(x) x[[index(x)]]

#' @importFrom tibble tbl_sum
#' @export
tbl_sum.ts_tbl <- function(x) {
  setNames(
    c(prt::dim_desc(x), format(tbl_id(x)), format(tbl_index(x))),
    c(paste0("A `", tbl_class(x), "`"), "Id", "Index")
  )
}
