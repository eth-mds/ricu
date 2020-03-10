
new_id_meta <- function(tbl_id) {

  assert_that(is_tbl_id(tbl_id))

  structure(list(tbl_id = tbl_id), class = "id_meta")
}

#' @export
is_id_meta <- function(x) inherits(x, "id_meta")

id_meta.id_meta <- function(x) x

tbl_index.id_meta <- function(x) NULL

tbl_id.id_meta <- function(x) x[["tbl_id"]]

#' @export
rename_cols.id_meta <- function(x, new, old, ...) {
  do.call(new_id_meta, lapply(x, rename_cols, new, old))
}

#' @export
index.id_meta <- function(x) NULL

#' @export
interval.id_meta <- function(x) NULL

#' @export
time_unit.id_meta <- function(x) NULL

#' @export
id.id_meta <- function(x) id(tbl_id(x))

#' @export
set_id.id_meta <- function(x, value) {
  new_id_meta(set_id(tbl_id(x), value))
}
