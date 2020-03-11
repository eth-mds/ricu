
new_id_meta <- function(tbl_id) {

  assert_that(is_tbl_id(tbl_id))

  structure(list(tbl_id = tbl_id), class = c("id_meta", "tbl_meta"))
}

#' @export
is_id_meta <- function(x) inherits(x, "id_meta")

#' @export
meta_cols.id_meta <- function(x) id(x)

#' @export
rename_cols.id_meta <- function(x, new, old, ...) {
  do.call(new_id_meta, lapply(x, rename_cols, new, old))
}

#' @export
set_id.id_meta <- function(x, value) {
  new_id_meta(set_id(tbl_id(x), value))
}

#' @export
format.id_meta <- function(x, ...) {
  format_one_meta(x, format(tbl_id(x)))
}

tbl_class.id_meta <- function(x) "id_tbl"

validate_meta.id_meta <- function(x, meta) {
  validate_that(has_col(x, id(meta)))
}
