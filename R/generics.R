
#' @export
tbl_meta <- function(x) UseMethod("tbl_meta", x)

tbl_class <- function(x) UseMethod("tbl_class", x)

validate_meta <- function(x, meta) UseMethod("validate_meta", meta)

#' @export
as_id_tbl <- function(x, ...) UseMethod("as_id_tbl", x)

#' @export
as_ts_tbl <- function(x, ...) UseMethod("as_ts_tbl", x)

#' @export
tbl_index <- function(x) UseMethod("tbl_index", x)

#' @export
tbl_id <- function(x) UseMethod("tbl_id", x)

#' @export
index <- function(x) UseMethod("index", x)

#' @export
id <- function(x) UseMethod("id", x)

#' @export
id_opts <- function(x) UseMethod("id_opts", x)

#' @export
interval <- function(x) UseMethod("interval", x)

#' @export
time_unit <- function(x) UseMethod("time_unit", x)

#' @export
meta_cols <- function(x) UseMethod("meta_cols", x)

#' @export
set_index <- function(x, value) UseMethod("set_index", x)

#' @export
set_interval <- function(x, value) {

  assert_that(is_time(value, allow_neg = FALSE))

  UseMethod("set_interval", x)
}

#' @export
set_id <- function(x, value) UseMethod("set_id", x)

#' @export
set_id_opts <- function(x, value) UseMethod("set_id_opts", x)

#' @export
set_time_unit <- function(x, value) UseMethod("set_time_unit", x)

#' @export
rm_cols <- function(x, cols, ...) {

  if (length(intersect(cols, colnames(x))) == 0L) {
    return(x)
  }

  UseMethod("rm_cols", x)
}

#' @method rm_cols data.table
#' @export
#'
rm_cols.data.table <- function(x, cols, ...) {
  x <- set(x, j = unique(intersect(cols, colnames(x))), value = NULL)
  x
}

#' @export
rename_cols <- function(x, new, old = colnames(x), ...) {

  assert_that(is_unique(new), is_unique(old), same_length(new, old))

  UseMethod("rename_cols", x)
}

#' @method rename_cols data.table
#' @export
#'
rename_cols.data.table <- function(x, new, old = colnames(x), ...) {
  x <- data.table::setnames(x, old, new)
  x
}

#' @export
is_unique <- function(x, ...) UseMethod("is_unique", x)

#' @export
is_unique.default <- function(x, ...) identical(anyDuplicated(x, ...), 0L)

#' @export
upgrade_id <- function(x, source, to = next_id(x), from = id_name(x), ...) {
  UseMethod("upgrade_id", x)
}

#' @export
downgrade_id <- function(x, source, to = prev_id(x), from = id_name(x), ...) {
  UseMethod("downgrade_id", x)
}

#' @export
get_source <- function(x) UseMethod("get_source", x)
