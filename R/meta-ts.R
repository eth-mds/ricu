
#' @param tbl_id,tbl_index Meta data objects to construct `tbl_meta` objects
#' with
#'
#' @rdname tbl_meta
#' @keywords internal
#' @export
#'
new_ts_meta <- function(tbl_id, tbl_index) {

  assert_that(is_tbl_id(tbl_id), is_tbl_index(tbl_index),
              id(tbl_id) != index(tbl_index))

  structure(list(tbl_id = tbl_id, tbl_index = tbl_index),
            class = c("ts_meta", "tbl_meta"))
}

#' @rdname tbl_meta
#' @keywords internal
#' @export
#'
is_ts_meta <- function(x) inherits(x, "ts_meta")

tbl_index.ts_meta <- function(x) x[["tbl_index"]]

#' @rdname tbl_utils
#' @export
#'
rename_cols.ts_meta <- function(x, new, old, ...) {
  do.call(new_ts_meta, lapply(x, rename_cols, new, old))
}

#' @rdname meta_utils
#' @export
#'
index.ts_meta <- function(x) index(tbl_index(x))

#' @rdname meta_utils
#' @export
#'
interval.ts_meta <- function(x) interval(tbl_index(x))

#' @rdname meta_utils
#' @export
#'
time_unit.ts_meta <- function(x) units(interval(x))

#' @rdname meta_utils
#' @export
#'
meta_cols.ts_meta <- function(x) c(id(x), index(x))

#' @rdname meta_utils
#' @export
#'
set_id.ts_meta <- function(x, value) {
  new_ts_meta(set_id(tbl_id(x), value), tbl_index(x))
}

#' @rdname meta_utils
#' @export
#'
set_index.ts_meta <- function(x, value) {
  new_ts_meta(tbl_id(x), set_index(tbl_index(x), value))
}

#' @rdname meta_utils
#' @export
#'
set_interval.ts_meta <- function(x, value) {
  new_ts_meta(tbl_id(x), set_interval(tbl_index(x), value))
}

#' @rdname meta_utils
#' @export
#'
set_time_unit.ts_meta <- function(x, value) {
  new_ts_meta(tbl_id(x), set_time_unit(tbl_index(x), value))
}

tbl_class.ts_meta <- function(x) "ts_tbl"

#' @rdname tbl_meta
#' @keywords internal
#' @export
#'
validate_meta.ts_meta <- function(x, meta) {

  ind <- index(meta)

  validate_that(
    has_col(x, id(meta)), has_col(x, ind),
    has_time_col(x, ind), has_interval(x, ind, interval(meta))
  )
}
