
#' @export
ts_meta <- function(x) UseMethod("ts_meta", x)

#' @export
ts_index <- function(x) UseMethod("ts_index", x)

#' @export
ts_key <- function(x) UseMethod("ts_key", x)

#' @export
index <- function(x) UseMethod("index", x)

#' @export
key <- function(x) UseMethod("key", x)

#' @method key data.table
#' @export
#'
key.data.table <- data.table::key

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
set_key <- function(x, value) UseMethod("set_key", x)

#' @export
set_time_unit <- function(x, value) UseMethod("set_time_unit", x)

#' @export
rm_cols <- function(x, cols, ...) {

  if (length(cols) == 0L) {
    return(x)
  }

  assert_that(has_cols(x, cols), is_unique(cols))

  UseMethod("rm_cols", x)
}

#' @method rm_cols data.table
#' @export
#'
rm_cols.data.table <- function(x, cols, ...) {
  x <- set(x, j = cols, value = NULL)
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
