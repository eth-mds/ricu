
#' @export
is_ts_meta <- function(x) inherits(x, "ts_meta")

#' @export
meta_cols <- function(x, ...) UseMethod("meta_cols", x)

#' @export
meta_cols.ts_meta <- function(x, ...) x[["meta_cols"]]

#' @export
interval <- function(x, ...) UseMethod("interval", x)

#' @export
new_ts_index <- function(col, interval) {

  assert_that(is.string(col), is_time(interval))

  structure(list(meta_cols = col, interval = interval),
            class = c("ts_index", "ts_meta"))
}

#' @export
is_ts_index <- function(x) inherits(x, "ts_index")

#' @export
interval.ts_index <- function(x, ...) x[["interval"]]

#' @export
new_ts_key <- function(cols) {

  assert_that(is.character(cols))

  structure(list(meta_cols = cols), class = c("ts_key", "ts_meta"))
}

#' @export
is_ts_key <- function(x) inherits(x, "ts_key")
