
#' @export
ts_meta <- function(x) UseMethod("ts_meta", x)

#' @export
tbl_index <- function(x) UseMethod("tbl_index", x)

#' @export
tbl_id <- function(x) UseMethod("tbl_id", x)

#' @export
index <- function(x) UseMethod("index", x)

#' @export
id <- function(x) UseMethod("id", x)

#' @export
interval <- function(x) UseMethod("interval", x)

#' @export
time_unit <- function(x) UseMethod("time_unit", x)

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
rename_cols <- function(x, new, old, ...) {

  assert_that(is_unique(new), is_unique(old), same_length(new, old))

  UseMethod("rename_cols", x)
}

#' @method rename_cols data.table
#' @export
#'
rename_cols.data.table <- function(x, new, old, ...) {
  x <- data.table::setnames(x, old, new)
  x
}

#' @export
is_unique <- function(x, ...) UseMethod("is_unique", x)

#' @export
is_unique.default <- function(x, ...) identical(anyDuplicated(x, ...), 0L)
