
new_tbl_index <- function(index, interval) {

  assert_that(
    is.string(index), not_na(index),
    is_time(interval, allow_neg = FALSE), not_na(interval), is.scalar(interval)
  )

  structure(list(col_name = unname(index), interval = interval),
            class = "tbl_index")
}

new_tbl_id <- function(id, opts = NULL) {

  assert_that(is.string(id), not_na(id))

  if (!is.null(opts)) {

    assert_that(is.character(opts), !is.null(names(opts)), length(opts) > 0L)

    if (!id %in% opts) {

      warning("id `", id, "` is incompatible with id options ",
              paste0("`", opts, "`", collapse = ", "))

      opts <- NULL
    }
  }

  structure(list(col_name = unname(id), id_opts = opts), class = "tbl_id")
}

#' @export
is_tbl_index <- function(x) inherits(x, "tbl_index")

#' @export
is_tbl_id <- function(x) inherits(x, "tbl_id")

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
    x <- new_tbl_id(new[hit], id_opts(x))
  }

  x
}

#' @export
index.tbl_index <- function(x) x[["col_name"]]

#' @export
interval.tbl_index <- function(x) x[["interval"]]

#' @export
time_unit.tbl_index <- function(x) units(interval(x))

#' @export
id.tbl_id <- function(x) x[["col_name"]]

#' @export
id_opts.tbl_id <- function(x) x[["id_opts"]]

#' @export
set_id.tbl_id <- function(x, value) new_tbl_id(value, id_opts(x))

#' @export
set_id_opts.tbl_id <- function(x, value) new_tbl_id(id(x), value)

#' @export
set_index.tbl_index <- function(x, value) new_tbl_index(value, interval(x))

#' @export
set_interval.tbl_index <- function(x, value) new_tbl_index(index(x), value)

#' @export
set_time_unit.tbl_index <- function(x, value) {
  new_tbl_index(index(x), `units<-`(interval(x), value))
}

#' @export
print.tbl_id <- function(x, ...) cat_line(format(x, ...))

#' @export
print.tbl_index <- function(x, ...) cat_line(format(x, ...))

#' @export
format.tbl_id <- function(x, ...) {
  format_one_meta(x, paste0("`", id(x), "`"))
}

#' @export
format.tbl_index <- function(x, ...) {
  format_one_meta(x, paste0("`", index(x), "`"), format(interval(x)))
}
