
#' @export
id_tbl <- function(..., id, id_opts = NULL) {
  as_id_tbl(data.table::data.table(...), id, id_opts)
}

#' @export
as_id_tbl.id_tbl <- function(x, ...) x

#' @export
as_id_tbl.default <- function(x, id, id_opts = NULL, ...) {

  if (!is_dt(x)) data.table::setDT(x)

  if (is.numeric(id) || is.logical(id)) {
    id <- colnames(x)[id]
  }

  new_icu_tbl(x, new_id_meta(new_tbl_id(id, id_opts)))
}

#' @export
as_ts_tbl.id_tbl <- function(x, index = NULL, interval = hours(1L), ...) {

  new_icu_tbl(x,
    new_ts_meta(tbl_id(x), new_tbl_index(auto_index(x, index), interval))
  )
}

#' @export
is_id_tbl <- function(x) inherits(x, "id_tbl")

#' @importFrom tibble tbl_sum
#' @export
tbl_sum.id_tbl <- function(x) {
  setNames(
    c(prt::dim_desc(x), format(tbl_id(x))),
    c(paste0("A `", tbl_class(x), "`"), "Id")
  )
}
