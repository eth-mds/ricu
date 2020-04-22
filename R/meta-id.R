
#' @rdname tbl_meta
#' @keywords internal
#' @export
#'
new_id_meta <- function(tbl_id) {

  assert_that(is_tbl_id(tbl_id))

  structure(list(tbl_id = tbl_id), class = c("id_meta", "tbl_meta"))
}

#' @rdname tbl_meta
#' @keywords internal
#' @export
#'
is_id_meta <- function(x) inherits(x, "id_meta")

#' @rdname tbl_meta
#' @keywords internal
#' @export
#'
as_id_meta <- function(x) UseMethod("as_id_meta", x)

#' @rdname tbl_meta
#' @keywords internal
#' @export
#'
as_id_meta.id_meta <- function(x) x

#' @rdname tbl_meta
#' @keywords internal
#' @export
#'
as_id_meta.ts_meta <- function(x) new_id_meta(tbl_id(x))

#' @rdname tbl_meta
#' @keywords internal
#' @export
#'
meta_cols.id_meta <- function(x) id(x)

#' @rdname tbl_utils
#' @export
#'
rename_cols.id_meta <- function(x, new, old, ...) {
  do.call(new_id_meta, lapply(x, rename_cols, new, old))
}

#' @param x Object to query/modify
#' @param value New value to set
#'
#' @rdname meta_utils
#' @export
#'
set_id.id_meta <- function(x, value) {
  new_id_meta(set_id(tbl_id(x), value))
}

#' @rdname tbl_meta
#' @keywords internal
#' @export
#'
tbl_class.id_meta <- function(x) "id_tbl"

#' @rdname tbl_meta
#' @keywords internal
#' @export
#'
validate_meta.id_meta <- function(x, meta) {
  validate_that(has_col(x, id(meta)))
}
