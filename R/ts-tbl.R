
#' Methods for creating and inspecting ts_tbl objects
#'
#' @param tbl An object inheriting from `data.frame`.
#' @param ts_key Character or numeric vector of at least length 1, identifying
#' columns that combined with time stamps, uniquely identify rows.
#' @param ts_index Character or numeric vector of length 1, identifying the
#' column that defines temporal ordering.
#' @param ts_step Scalar value, defining time steps between rows (assuming
#' complete time series data).
#'
#' @rdname ts_tbl
#'
#' @export
#'
as_ts_tbl <- function(tbl, ts_key, ts_index = NULL, ts_step = 1L) {

  assert_that(inherits(tbl, "data.frame"))

  data.table::setDT(tbl)

  new_ts_tbl(tbl, ts_key, ts_index, ts_step)
}

#' @param ... Passed to [data.table::data.table()]/generic compatibility.
#'
#' @rdname ts_tbl
#'
#' @export
#'
ts_tbl <- function(..., ts_key, ts_index = NULL, ts_step = 1L) {
  new_ts_tbl(data.table::data.table(...), ts_key, ts_index, ts_step)
}

new_ts_tbl <- function(tbl, ts_key, ts_index, ts_step) {

  assert_that(is_dt(tbl))

  set_ts_attrs(tbl, ts_key, ts_index, ts_step)

  by_cols <-  c(ts_key, ts_index)

  setkeyv(tbl, by_cols)
  setcolorder(tbl, c(by_cols, setdiff(colnames(tbl), by_cols)))
  setattr(tbl, "class", unique(c("ts_tbl", class(tbl))))

  tbl
}

#' @param x A `ts_tbl` object.
#'
#' @rdname ts_tbl
#'
#' @export
#'
is_ts_tbl <- function(x) inherits(x, "ts_tbl")

#' @export
get_ts_meta <- function(x, is_fun = NULL) {

  assert_that(is_ts_tbl(x), has_attr(x, "ts_meta"))

  res <- attr(x, "ts_meta")

  assert_that(is.list(res), all(vapply(res, is_ts_meta, logical(1L))))

  if (is.null(is_fun)) {
    res
  } else {
    hit <- vapply(res, is_fun, logical(1L))
    assert_that(sum(hit) == 1L)
    res[[which(hit)]]
  }
}

#' @rdname ts_tbl
#'
#' @export
#'
ts_index <- function(x) meta_cols(get_ts_meta(x, is_ts_index))

#' @rdname ts_tbl
#'
#' @export
#'
ts_key <- function(x) meta_cols(get_ts_meta(x, is_ts_key))

#' @rdname ts_tbl
#'
#' @export
#'
ts_meta_cols <- function(x) unique(unlist(lapply(get_ts_meta(x), meta_cols)))

#' @rdname ts_tbl
#'
#' @export
#'
ts_data_cols <- function(x) setdiff(colnames(x), ts_meta(x))

#' @rdname ts_tbl
#'
#' @export
#'
ts_id_cols <- function(x) c(ts_key(x), ts_index(x))

#' @rdname ts_tbl
#'
#' @export
#'
ts_interval <- function(x) interval(get_ts_meta(x, is_ts_index))

#' @rdname ts_tbl
#'
#' @export
#'
ts_time_unit <- function(x) units(x[[ts_index(x)]])

#' @rdname ts_tbl
#'
#' @export
#'
ts_time_step <- function(x) as.double(ts_interval(x), units = ts_time_unit(x))

#' @export
set_ts_meta <- function(x, meta) {

  if (is_ts_meta(meta)) meta <- list(meta)

  assert_that(is_ts_tbl(x), all(vapply(meta, is_ts_meta, logical(1L))))

  old <- attr(x, "ts_meta")
}

#' @param value New time unit.
#'
#' @rdname ts_tbl
#'
#' @export
#'
`time_unit<-` <- function(x, value) units(x[[ts_index(x)]]) <- value


set_ts_key <- function(x, val) {

  if (is.numeric(val)) {
    val <- colnames(x)[val]
  }

  assert_that(has_cols(x, val))

  setattr(x, "ts_key", val)
  invisible(x)
}

set_ts_index <- function(x, val) {

  if (is.null(val)) {
    val <- which(vapply(x, is_time, logical(1L)))
  }

  if (is.numeric(val)) {
    val <- colnames(x)[val]
  }

  assert_that(length(val) == 1L, has_cols(x, val), is_time(x[[val]]))

  setattr(x, "ts_index", val)
  invisible(x)
}

set_ts_step <- function(x, val) {

  assert_that(is.numeric(val), length(val) == 1L, val > 0)

  setattr(x, "ts_step", val)
  invisible(x)
}

set_ts_attrs <- function(x, ts_key, ts_index, ts_step) {

  set_ts_key(x, ts_key)
  set_ts_index(x, ts_index)
  set_ts_step(x, ts_step)

  invisible(x)
}

get_ts_attrs <- function(x) {
  list(ts_key = ts_key(x), ts_index = ts_index(x), ts_step = ts_step(x))
}

get_ts_spec <- function(x) {
  c(get_ts_attrs(x), list(ts_unit = time_unit(x)))
}

unclass_ts_tbl <- function(x) {

  setattr(x, "class", setdiff(class(x), "ts_tbl"))

  setattr(x, "ts_key", NULL)
  setattr(x, "ts_index", NULL)
  setattr(x, "ts_step", NULL)

  invisible(x)
}

reclass_ts_tbl <- function(x, spec, warn_on_fail = TRUE) {

  check <- check_ts_tbl(x, spec[["ts_key"]], spec[["ts_index"]],
                        spec[["ts_unit"]])

  if (check) {

    new_ts_tbl(x, spec[["ts_key"]], spec[["ts_index"]], spec[["ts_step"]])

  } else {

    if (warn_on_fail) {
      warning("Cannot reclass as `ts_tbl` according to specification.")
    }

    unclass_ts_tbl(x)

    x
  }
}

rm_ts_cols <- function(x, cols) {

  assert_that(is_ts_tbl(x))
  index <- ts_index(x)

  if (is.numeric(cols)) cols <- colnames(x)[cols]

  assert_that(has_cols(x, cols), !index %in% cols)

  old_keys <- ts_key(x)

  set(x, j = cols, value = NULL)

  if (any(cols %in% old_keys)) {

    new_keys <- setdiff(old_keys, cols)

    assert_that(length(new_keys) > 0L)

    set_ts_key(x, new_keys)
    setkeyv(x, c(new_keys, index))
  }

  invisible(x)
}

set_names <- function(x, old, new) {

  assert_that(is_ts_tbl(x))

  if (missing(new)) {
    names(x) <- old
  } else {
    names(x)[match(old, colnames(x))] <- new
  }

  invisible(x)
}
