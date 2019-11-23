
#' @export
is_ts_meta <- function(x) inherits(x, "ts_meta")

#' @export
meta_cols <- function(x, ...) UseMethod("meta_cols", x)

#' @export
`meta_cols<-` <- function(x, ...) UseMethod("meta_cols", x)

#' @export
meta_cols.ts_meta <- function(x, ...) x[["meta_cols"]]

#' @export
`meta_cols<-.ts_meta` <- function(x, value) x[["meta_cols"]] <- value

#' @export
interval <- function(x, ...) UseMethod("interval", x)

#' @export
check <- function(x, tbl, ...) UseMethod("check", x)

#' @export
is_required <- function(x, ...) UseMethod("check", x)

#' @export
is_required.ts_meta <- function(x, ...) FALSE

#' @export
print.ts_meta <- function(x, ...) {
  cat_line(format(x, ...))
}

#' @export
format.ts_meta <- function(x, ...) {
  paste0(sub("^ts_", "", class(x)[1L]), "<",
         paste(meta_cols(x), collapse = ", "), ">")
}

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
check.ts_index <- function(x, tbl, ...) {

  col <- meta_cols(x)
  interv <- interval(x)

  is.string(col) && col %in% colnames(tbl) &&
    identical(col, last_elem(data.table::key(tbl))) &&
  is_time(interv, allow_neg = FALSE) && length(interv) == 1L &&
    all(as.double(tbl[[col]]) %% ts_time_step(tbl) == 0)
}

#' @export
is_required.ts_index <- function(x, ...) TRUE

#' @export
format.ts_index <- function(x, ...) {
  paste0("index<", meta_cols(x), ", ", format(interval(x), ...), ">")
}

#' @export
new_ts_key <- function(cols) {

  assert_that(is.character(cols))

  structure(list(meta_cols = cols), class = c("ts_key", "ts_meta"))
}

#' @export
is_ts_key <- function(x) inherits(x, "ts_key")

#' @export
check.ts_key <- function(x, tbl, ...) {

  cols <- meta_cols(x)

  is.character(cols) && length(cols) > 0L &&
    identical(length(cols), length(unique(cols))) &&
    all(cols %in% colnames(tbl)) &&
    identical(cols, head(data.table::key(tbl), n = length(cols)))
}

#' @export
is_required.ts_key <- function(x, ...) TRUE
