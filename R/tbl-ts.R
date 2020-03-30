
#' @export
ts_tbl <- function(..., id, id_opts = NULL, index = NULL,
                   interval = hours(1L)) {

  as_ts_tbl(data.table::data.table(...), id, id_opts, index, interval)
}

#' @export
as_ts_tbl <- function(tbl, id, id_opts = NULL, index = NULL,
                      interval = hours(1L)) {

  if (!is_dt(tbl)) data.table::setDT(tbl)

  if (is.numeric(id) || is.logical(id)) {
    id <- colnames(tbl)[id]
  }

  if (is.null(index)) {

    hits <- vapply(tbl, is_time_vec, logical(1L))

    assert_that(sum(hits) == 1L,
      msg = paste("In order to automatically determine the index column,",
                  "exactly one `difftime` column is required.")
    )

    index <- colnames(tbl)[hits]

  } else if (is.numeric(index) || is.logical(index)) {

    index <- colnames(tbl)[index]
  }

  new_icu_tbl(tbl,
    new_ts_meta(new_tbl_id(id, id_opts), new_tbl_index(index, interval))
  )
}

#' @export
is_ts_tbl <- function(x) inherits(x, "ts_tbl")

#' @export
set_index.ts_tbl <- function(x, value) {
  set_meta(x, set_index(tbl_meta(x), value))
}

#' @export
interval.ts_tbl <- function(x) {
  `units<-`(interval(tbl_meta(x)), time_unit(x))
}

#' @export
set_interval.ts_tbl <- function(x, value) {

  if (interval(x) > value) {
    warning("Higher time resolution does not add missing time steps")
  }

  set(x, j = index(x),
      value = round_to(time_col(x), as.double(value, time_unit(x))))

  set_meta(x, set_interval(tbl_meta(x), value))

  if (!identical(units(value), time_unit(x))) {
    set_time_unit(x, units(value))
  }

  x
}

#' @export
time_unit.ts_tbl <- function(x) units(time_col(x))

#' @export
set_time_unit.ts_tbl <- function(x, value) {

  set(x, j = index(x), value = `units<-`(time_col(x), value))

  set_meta(x, set_time_unit(tbl_meta(x), value))
}

#' @export
time_step <- function(x) as.double(interval(x), units = time_unit(x))

#' @export
time_col <- function(x) x[[index(x)]]

#' @importFrom tibble tbl_sum
#' @export
tbl_sum.ts_tbl <- function(x) {
  setNames(
    c(prt::dim_desc(x), paste0("<`", id(x), "`>"),
      paste0("<`", index(x), "`, ", format(interval(x)), ">")),
    c(paste0("A `", tbl_class(x), "`"), "Id", "Index")
  )
}
