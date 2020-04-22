
#' Tabular ICU data
#'
#' Two objects central to `ricu`, `id_tbl` and `ts_tbl` (both inheriting
#' from `icu_tbl`) are used to represent tabular data.
#'
#' @param ... forwarded to [data.table::data.table()] or generic consistency
#' @param id String valued column name used as `id` column
#' @param id_opts `NULL` or a named character vector indicating options to
#' which ids may be converted see [change_id()]
#'
#' @rdname icu_tbl
#' @export
#'
id_tbl <- function(..., id, id_opts = NULL) {
  as_id_tbl(data.table::data.table(...), id, id_opts)
}

#' @param x Object to test/coerce
#'
#' @rdname icu_tbl
#' @export
#'
as_id_tbl <- function(x, ...) UseMethod("as_id_tbl", x)

#' @rdname icu_tbl
#' @export
#'
as_id_tbl.id_tbl <- function(x, ...) x

#' @rdname icu_tbl
#' @export
#'
as_id_tbl.default <- function(x, id, id_opts = NULL, ...) {

  if (!is_dt(x)) data.table::setDT(x)

  if (is.numeric(id) || is.logical(id)) {
    id <- colnames(x)[id]
  }

  new_icu_tbl(x, new_id_meta(new_tbl_id(id, id_opts)))
}

#' @param index String valued column name used as index
#' @param interval Scalar `difftime` object representing the minimal time
#' difference between rows
#'
#' @rdname icu_tbl
#' @export
#'
as_ts_tbl.id_tbl <- function(x, index = NULL, interval = hours(1L), ...) {

  new_icu_tbl(x,
    new_ts_meta(tbl_id(x), new_tbl_index(auto_index(x, index), interval))
  )
}

#' @rdname icu_tbl
#' @export
#'
is_id_tbl <- function(x) inherits(x, "id_tbl")

#' @importFrom tibble tbl_sum
#' @export
tbl_sum.id_tbl <- function(x) {
  setNames(
    c(prt::dim_desc(x), format(tbl_id(x))),
    c(paste0("A `", tbl_class(x), "`"), "Id")
  )
}
