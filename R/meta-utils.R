
#' @export
is_tbl_meta <- function(x) inherits(x, "tbl_meta")

tbl_meta.tbl_meta <- function(x) x

tbl_id.tbl_meta <- function(x) x[["tbl_id"]]

#' @export
id.tbl_meta <- function(x) id(tbl_id(x))

#' @export
id_opts.tbl_meta <- function(x) id_opts(tbl_id(x))

#' @export
print.tbl_meta <- function(x, ...) cat_line(format(x, ...))
