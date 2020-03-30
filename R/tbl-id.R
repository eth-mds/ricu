
#' @export
id_tbl <- function(..., id, id_opts = NULL) {
  as_id_tbl(data.table::data.table(...), id, id_opts)
}

#' @export
as_id_tbl <- function(tbl, id, id_opts = NULL) {

  if (!is_dt(tbl)) data.table::setDT(tbl)

  if (is.numeric(id) || is.logical(id)) {
    id <- colnames(tbl)[id]
  }

  new_icu_tbl(tbl, new_id_meta(new_tbl_id(id, id_opts)))
}

#' @export
is_id_tbl <- function(x) inherits(x, "id_tbl")

#' @importFrom tibble tbl_sum
#' @export
tbl_sum.id_tbl <- function(x) {
  setNames(
    c(prt::dim_desc(x), paste0("<`", id(x), "`>")),
    c(paste0("A `", tbl_class(x), "`"), "Id")
  )
}
