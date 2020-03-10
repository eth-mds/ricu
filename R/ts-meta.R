
new_tbl_index <- function(index, interval) {

  assert_that(
    is.string(index), not_na(index),
    is_time(interval, allow_neg = FALSE), not_na(interval), is.scalar(interval)
  )

  structure(list(col_name = index, interval = interval), class = "tbl_index")
}

new_tbl_id <- function(id) {

  assert_that(is.string(id), not_na(id))

  structure(list(col_name = id), class = "tbl_id")
}

new_ts_meta <- function(tbl_id, tbl_index) {

  assert_that(is_tbl_id(tbl_id), is_tbl_index(tbl_index),
              id(tbl_id) != index(tbl_index))

  structure(list(tbl_id = tbl_id, tbl_index = tbl_index), class = "ts_meta")
}

#' @export
is_tbl_index <- function(x) inherits(x, "tbl_index")

#' @export
is_tbl_id <- function(x) inherits(x, "tbl_id")

#' @export
is_ts_meta <- function(x) inherits(x, "ts_meta")

ts_meta.ts_meta <- function(x) x

tbl_index.ts_meta <- function(x) x[["tbl_index"]]

tbl_id.ts_meta <- function(x) x[["tbl_id"]]

tbl_index.tbl_index <- function(x) x

tbl_id.tbl_id <- function(x) x

#' @export
rename_cols.tbl_index <- function(x, new, old, ...) {

  hit <- index(x) == old

  if (any(hit)) {
    assert_that(sum(hit) == 1L)
    x <- new_tbl_index(new[hit], interval(x))
  }

  x
}

#' @export
rename_cols.tbl_id <- function(x, new, old, ...)  {

  hit <- id(x) == old

  if (any(hit)) {
    assert_that(sum(hit) == 1L)
    x <- new_tbl_id(new[hit])
  }

  x
}

#' @export
rename_cols.ts_meta <- function(x, new, old, ...) {
  do.call(new_ts_meta, lapply(x, rename_cols, new, old))
}

#' @export
index.tbl_index <- function(x) x[["col_name"]]

#' @export
index.ts_meta <- function(x) index(tbl_index(x))

#' @export
interval.tbl_index <- function(x) x[["interval"]]

#' @export
interval.ts_meta <- function(x) interval(tbl_index(x))

#' @export
time_unit.tbl_index <- function(x) units(interval(x))

#' @export
time_unit.ts_meta <- function(x) units(interval(x))

#' @export
id.tbl_id <- function(x) x[["col_name"]]

#' @export
id.ts_meta <- function(x) id(tbl_id(x))

#' @export
set_id.tbl_id <- function(x, value) new_tbl_id(value)

#' @export
set_id.ts_meta <- function(x, value) {
  new_ts_meta(set_id(tbl_id(x), value), tbl_index(x))
}

#' @export
set_index.tbl_index <- function(x, value) new_tbl_index(value, interval(x))

#' @export
set_index.ts_meta <- function(x, value) {
  new_ts_meta(tbl_id(x), set_index(tbl_index(x), value))
}

#' @export
set_interval.tbl_index <- function(x, value) new_tbl_index(index(x), value)

#' @export
set_interval.ts_meta <- function(x, value) {
  new_ts_meta(tbl_id(x), set_interval(tbl_index(x), value))
}

#' @export
set_time_unit.tbl_index <- function(x, value) {
  new_tbl_index(index(x), `units<-`(interval(x), value))
}

#' @export
set_time_unit.ts_meta <- function(x, value) {
  new_ts_meta(tbl_id(x), set_time_unit(tbl_index(x), value))
}
