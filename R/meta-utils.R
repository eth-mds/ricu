
#' Meta data utilities
#'
#' @rdname tbl_meta
#' @keywords internal
#' @export
#'
is_tbl_meta <- function(x) inherits(x, "tbl_meta")

#' @export
tbl_meta <- function(x) UseMethod("tbl_meta", x)

#' @rdname tbl_meta
#' @keywords internal
#' @export
#'
tbl_meta.tbl_meta <- function(x) x

#' @rdname tbl_meta
#' @keywords internal
#' @export
#'
tbl_id.tbl_meta <- function(x) x[["tbl_id"]]

#' @rdname meta_utils
#' @export
#'
id.tbl_meta <- function(x) id(tbl_id(x))

#' @rdname meta_utils
#' @export
#'
id_opts.tbl_meta <- function(x) id_opts(tbl_id(x))

#' @param meta Meta data object to validate
#'
#' @rdname tbl_meta
#' @keywords internal
#' @export
#'
validate_meta <- function(x, meta) UseMethod("validate_meta", meta)

#' @export
format.tbl_meta <- function(x, ...) {
  paste0("<", vapply(x, format, character(1L), ...), ">", collapse = ", ")
}

#' @export
print.tbl_meta <- function(x, ...) cat_line(format(x, ...))

