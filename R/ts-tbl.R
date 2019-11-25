
#' Methods for creating and inspecting ts_tbl objects
#'
#' @param tbl An object inheriting from `data.frame`.
#' @param key Character or numeric vector of at least length 1, identifying
#' columns that combined with time stamps, uniquely identify rows.
#' @param index Character or numeric vector of length 1, identifying the
#' column that defines temporal ordering.
#' @param interval Scalar value, defining time steps between rows (assuming
#' complete time series data).
#'
#' @rdname ts_tbl
#'
#' @export
#'
ts_tbl <- function(..., key, index = NULL, interval = hours(1L)) {
  as_ts_tbl(data.table::data.table(...), key, index, interval)
}

#' @param ... Passed to [data.table::data.table()]/generic compatibility.
#'
#' @rdname ts_tbl
#'
#' @export
#'
as_ts_tbl <- function(tbl, key, index = NULL, interval = hours(1L), ...) {

  assert_that(inherits(tbl, "data.frame"))

  if (!is_dt(tbl)) data.table::setDT(tbl)

  meta <- c(ts_tbl_key(x, key), ts_tbl_ind(x, index, interval), ...)

  new_ts_tbl(tbl, meta)
}

new_ts_tbl <- function(tbl, meta) {

  assert_that(is_dt(tbl), is_ts_def(meta))

  meta <- as_ts_def(meta)
  id_cols <- ts_id_cols(meta)

  tbl <- stats::na.omit(tbl, id_cols)

  setkeyv(tbl, id_cols)
  setcolorder(tbl, c(id_cols, setdiff(colnames(tbl), id_cols)))

  set_ts_meta(tbl, meta)
  setattr(tbl, "class", unique(c("ts_tbl", class(tbl))))

  tbl
}

ts_tbl_key <- function(tbl, key) {

  if (is.numeric(key) || is.logical(key)) {
    key <- colnames(tbl)[key]
  }

  new_ts_key(key)
}

ts_tbl_ind <- function(tbl, index, interval) {

  if (is.null(index)) {

    hits <- vapply(tbl, is_time, logical(1L))

    assert_that(sum(hits) == 1L,
      msg = paste("In order to automatically determine the index column,",
                  "exactly one `difftime` column is required.")
    )

    index <- colnames(tbl)[hits]

  } else if (is.numeric(index) || is.logical(index)) {

    index <- colnames(tbl)[index]
  }

  new_ts_index(index, interval)
}

#' @param x A `ts_tbl` object.
#'
#' @rdname ts_tbl
#'
#' @export
#'
is_ts_tbl <- function(x) inherits(x, "ts_tbl")

#' @export
set_ts_meta <- function(x, meta, warn_opt = TRUE) {

  find_hits <- function(new_elem, old_set) {
    which(vapply(old_set, same_class, logical(1L), new_elem))
  }

  assert_that(is_ts_tbl(x), is.flag(warn_opt))

  meta <- as_ts_def(meta)

  if (has_attr(x, "ts_meta")) {

    old <- get_ts_meta(x)

    assert_that(is_ts_def(old))

    hits <- flapply(meta, find_hits, old)

    if (length(hits) > 0L) old <- old[-hits]

    new <- c(old, meta)
  }

  is_valid <- validate_meta(new, x, stop_req = TRUE, warn_opt = warn_opt)

  setattr(x, "ts_meta", new[is_valid])

  invisible(x)
}

#' @export
validate_meta <- function(x, ..., stop_req = TRUE, warn_opt = TRUE) {
  assert_that(is.flag(stop_req), is.flag(warn_opt))
  UseMethod()
}

#' @export
validate_meta.ts_tbl <- function(x, stop_req, warn_opt, ...) {
  validate_meta(get_ts_meta(x), x, stop_req, warn_opt)
}

#' @export
validate_meta.ts_def <- function(x, tbl, stop_req, warn_opt, ...) {
  vapply(x, validate_meta, logical(1L), tbl, stop_req, warn_opt)
}

#' @export
get_ts_meta <- function(x, class = NULL, drop = FALSE) {
  assert_that(is.string(class), is.flag(drop))
  UseMethod()
}

#' @export
get_ts_meta.ts_tbl <- function(x, class, drop) {
  get_ts_meta(attr(x, "ts_meta"), class, drop)
}

#' @export
get_ts_meta.ts_def <- function(x, class, drop) {

  if (!is.null(class)) {
    x <- x[vapply(x, inherits, logical(1L), class)]
  }

  if (drop && length(x) == 1L) x <- x[[1L]]

  x
}

#' @export
unclass_ts_tbl <- function(x) {

  setattr(x, "ts_meta", NULL)
  setattr(x, "class", setdiff(class(x), "ts_tbl"))

  x
}

#' @export
reclass_ts_tbl <- function(x, meta, warn_opt = TRUE) {

  is_valid <- validate_meta(meta, x, stop_req = FALSE, warn_opt = warn_opt)

  if (anyNA(is_valid)) {
    x <- unclass_ts_tbl(x)
  } else {
    x <- new_ts_tbl(x, meta[is_valid])
  }

  x
}

#' @export
rm_cols <- function(x, cols, ...) UseMethod()

#' @export
rm_cols.ts_tbl <- function(x, cols, ...) {

  assert_that(has_cols(x, cols))

  meta <- rm_cols(get_ts_meta(x), cols, ...)
  x <- set(x, j = cols, value = NULL)

  reclass_ts_tbl(x, meta)
}

#' @export
rm_cols.ts_def <- function(x, cols, ...) {
  new_ts_def(lapply(x, rm_cols, cols, ...))
}

#' @export
rename_cols <- function(x, old, new) {

  assert_that(is_ts_tbl(x))

  if (missing(new) && is.function(old)) {
    names(x) <- old(names(x))
  } else if (missing(new)) {
    names(x) <- old
  } else if (is.function(new)) {
    names(x) <- new(old)
  } else {
    names(x)[match(old, colnames(x))] <- new
  }

  invisible(x)
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
`ts_index<-` <- function(x, value) {
  set_ts_meta(x, ts_tbl_ind(x, value, ts_interval(x)))
}

#' @rdname ts_tbl
#'
#' @export
#'
ts_key <- function(x) meta_cols(get_ts_meta(x, is_ts_key))

#' @rdname ts_tbl
#'
#' @export
#'
`ts_key<-` <- function(x, value) set_ts_meta(x, ts_tbl_key(x, value))

#' @rdname ts_tbl
#'
#' @export
#'
ts_meta_cols <- function(x) flapply(get_ts_meta(x), meta_cols)

#' @rdname ts_tbl
#'
#' @export
#'
ts_data_cols <- function(x) setdiff(colnames(x), ts_meta_cols(x))

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
`ts_interval<-` <- function(x, value) {
  set_ts_meta(x, ts_tbl_ind(x, ts_index(x), value))
}

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
