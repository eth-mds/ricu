
new_ts_index <- function(index, interval) {

  assert_that(
    is.string(index), not_na(index),
    is_time(interval, allow_neg = FALSE), not_na(interval), is.scalar(interval)
  )

  structure(list(col_name = index, interval = interval), class = "ts_index")
}

new_ts_key <- function(key) {

  assert_that(is.string(key), not_na(key))

  structure(list(col_name = key), class = "ts_key")
}

new_ts_meta <- function(ts_key, ts_index) {

  assert_that(is_ts_key(ts_key), is_ts_index(ts_index),
              key(ts_key) != index(ts_index))

  structure(list(ts_key = ts_key, ts_index = ts_index), class = "ts_meta")
}

#' @export
is_ts_index <- function(x) inherits(x, "ts_index")

#' @export
is_ts_key <- function(x) inherits(x, "ts_key")

#' @export
is_ts_meta <- function(x) inherits(x, "ts_meta")

#' @export
is_ts_tbl <- function(x) inherits(x, "ts_tbl")

ts_meta.ts_meta <- function(x) x

ts_index.ts_meta <- function(x) x[["ts_index"]]

ts_key.ts_meta <- function(x) x[["ts_key"]]

ts_index.ts_index <- function(x) x

ts_key.ts_key <- function(x) x

#' @export
rename_cols.ts_index <- function(x, new, old, ...) {

  hit <- index(x) == old

  if (any(hit)) {
    assert_that(sum(hit) == 1L)
    x <- new_ts_index(new[hit], interval(x))
  }

  x
}

#' @export
rename_cols.ts_key <- function(x, new, old, ...)  {

  hit <- key(x) == old

  if (any(hit)) {
    assert_that(sum(hit) == 1L)
    x <- new_ts_key(new[hit])
  }

  x
}

#' @export
rename_cols.ts_meta <- function(x, new, old, ...) {
  do.call(new_ts_meta, lapply(x, rename_cols, new, old))
}

#' @export
index.ts_index <- function(x) x[["col_name"]]

#' @export
index.ts_meta <- function(x) index(ts_index(x))

#' @export
interval.ts_index <- function(x) x[["interval"]]

#' @export
interval.ts_meta <- function(x) interval(ts_index(x))

#' @export
time_unit.ts_index <- function(x) units(interval(x))

#' @export
time_unit.ts_meta <- function(x) units(interval(x))

#' @export
key.ts_key <- function(x) x[["col_name"]]

#' @export
key.ts_meta <- function(x) key(ts_key(x))

#' @export
set_key.ts_key <- function(x, value) new_ts_key(value)

#' @export
set_key.ts_meta <- function(x, value) {
  new_ts_meta(set_key(ts_key(x), value), ts_index(x))
}

#' @export
set_index.ts_index <- function(x, value) new_ts_index(value, interval(x))

#' @export
set_index.ts_meta <- function(x, value) {
  new_ts_meta(ts_key(x), set_index(ts_index(x), value))
}

#' @export
set_interval.ts_index <- function(x, value) new_ts_index(index(x), value)

#' @export
set_interval.ts_meta <- function(x, value) {
  new_ts_meta(ts_key(x), set_interval(ts_index(x), value))
}

#' @export
set_time_unit.ts_index <- function(x, value) {
  new_ts_index(index(x), `units<-`(interval(x), value))
}

#' @export
set_time_unit.ts_meta <- function(x, value) {
  new_ts_meta(ts_key(x), set_time_unit(ts_index(x), value))
}
