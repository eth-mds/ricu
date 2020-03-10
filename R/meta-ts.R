
new_ts_meta <- function(tbl_id, tbl_index) {

  assert_that(is_tbl_id(tbl_id), is_tbl_index(tbl_index),
              id(tbl_id) != index(tbl_index))

  structure(list(tbl_id = tbl_id, tbl_index = tbl_index), class = "ts_meta")
}

#' @export
is_ts_meta <- function(x) inherits(x, "ts_meta")

ts_meta.ts_meta <- function(x) x

tbl_index.ts_meta <- function(x) x[["tbl_index"]]

tbl_id.ts_meta <- function(x) x[["tbl_id"]]

#' @export
rename_cols.ts_meta <- function(x, new, old, ...) {
  do.call(new_ts_meta, lapply(x, rename_cols, new, old))
}

#' @export
index.ts_meta <- function(x) index(tbl_index(x))

#' @export
interval.ts_meta <- function(x) interval(tbl_index(x))

#' @export
time_unit.ts_meta <- function(x) units(interval(x))

#' @export
id.ts_meta <- function(x) id(tbl_id(x))

#' @export
set_id.ts_meta <- function(x, value) {
  new_ts_meta(set_id(tbl_id(x), value), tbl_index(x))
}

#' @export
set_index.ts_meta <- function(x, value) {
  new_ts_meta(tbl_id(x), set_index(tbl_index(x), value))
}

#' @export
set_interval.ts_meta <- function(x, value) {
  new_ts_meta(tbl_id(x), set_interval(tbl_index(x), value))
}

#' @export
set_time_unit.ts_meta <- function(x, value) {
  new_ts_meta(tbl_id(x), set_time_unit(tbl_index(x), value))
}

#' @export
print.ts_meta <- function(x, ...) cat_line(format(x, ...))

#' @export
format.ts_meta <- function(x, ...) {
  format_one_meta(x, format(tbl_id(x)), format(tbl_index(x)))
}
