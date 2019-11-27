
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

  meta <- c(new_tbl_key(x, key), new_tbl_index(x, index, interval), ...)

  new_ts_tbl(tbl, meta)
}

new_ts_tbl <- function(tbl, meta) {

  assert_that(is_dt(tbl))

  meta <- as_ts_def(meta)
  cols <- id_cols(meta)

  tbl <- stats::na.omit(tbl, cols)

  setkeyv(tbl, cols)
  setcolorder(tbl, c(cols, setdiff(colnames(tbl), cols)))

  ts_def(tbl) <- meta
  setattr(tbl, "class", unique(c("ts_tbl", class(tbl))))

  tbl
}

new_tbl_key <- function(tbl, key) {

  if (is.numeric(key) || is.logical(key)) {
    key <- colnames(tbl)[key]
  }

  new_ts_key(key)
}

new_tbl_index <- function(tbl, index, interval) {

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
ts_def <- function(x) UseMethod("ts_def", x)

#' @export
ts_def.data.table <- function(x) NULL

#' @export
ts_def.ts_tbl <- function(x) attr(x, "ts_def")

#' @export
ts_def.ts_def <- function(x) x

#' @export
`ts_def<-` <- function(x, value) {

  find_hits <- function(new_elem, old_set) {
    which(vapply(old_set, same_class, logical(1L), new_elem))
  }

  value <- as_ts_def(value)
  old <- ts_def(x)

  if (!is.null(old)) {

    assert_that(is_ts_def(old))

    hits <- flapply(value, find_hits, old)

    if (length(hits) > 0L) old <- old[-hits]
    if (length(old) > 0L) value <- c(old, value)
  }

  is_valid <- validate_def(value, x)

  setattr(x, "ts_def", value[is_valid])

  invisible(x)
}

#' @export
validate_def <- function(x, ..., stop_req = TRUE, warn_opt = TRUE) {
  UseMethod("validate_def", x)
}

#' @export
validate_def.ts_tbl <- function(x, stop_req = TRUE, warn_opt = TRUE, ...) {
  validate_def(ts_def(x), x, stop_req, warn_opt)
}

#' @export
unclass_ts_tbl <- function(x) {

  setattr(x, "ts_def", NULL)
  setattr(x, "class", setdiff(class(x), "ts_tbl"))

  x
}

#' @export
reclass_ts_tbl <- function(x, meta, warn_opt = TRUE) {

  is_valid <- validate_def(meta, x, stop_req = FALSE, warn_opt = warn_opt)

  if (anyNA(is_valid)) {
    x <- unclass_ts_tbl(x)
  } else {
    x <- new_ts_tbl(x, meta[is_valid])
  }

  x
}

#' @export
rm_cols <- function(x, cols, ...) UseMethod("rm_cols", x)

#' @export
rm_cols.ts_tbl <- function(x, cols, ...) {

  assert_that(has_cols(x, cols))

  meta <- rm_cols(ts_def(x), cols)
  x <- set(x, j = cols, value = NULL)

  reclass_ts_tbl(x, meta)
}

#' @export
rm_cols.ts_def <- function(x, cols, ...) {
  new_ts_def(lapply(x, rm_cols, cols))
}

#' @export
rename_cols <- function(x, new, old, ...) UseMethod("rename_cols", x)

#' @export
rename_cols.ts_tbl <- function(x, new, old = colnames(x), ...) {

  meta <- rename_cols(ts_def(x), new, old)
  x <- setnames(x, old, new)

  reclass_ts_tbl(x, meta)
}

#' @export
rename_cols.ts_def <- function(x, new, old, ...) {
  new_ts_def(lapply(x, rename_cols, new, old))
}

#' @export
index <- function(x) UseMethod("index", x)

#' @export
index.ts_tbl <- function(x) index(ts_def(x))

#' @export
`index<-` <- function(x, value) {
  ts_def(x) <- new_tbl_index(x, value, interval(x))
}

#' @export
key <- function(x) UseMethod("key", x)

#' @export
key.data.table <- data.table::key

#' @export
key.ts_tbl <- function(x) key(ts_def(x))

#' @export
key.ts_def <- function(x) key(x[["ts_key"]])

#' @export
`key<-` <- function(x, value) {
  ts_def(x) <- new_tbl_key(x, value)
}

#' @export
meta_cols <- function(x) flapply(ts_def(x), col_names)

#' @export
data_cols <- function(x) setdiff(colnames(x), meta_cols(x))

#' @export
id_cols <- function(x) c(key(x), index(x))

#' @export
interval <- function(x) UseMethod("interval", x)

#' @export
interval.ts_tbl <- function(x) interval(ts_def(x))

#' @export
`interval<-` <- function(x, value) {
  ts_def(x) <- new_tbl_index(x, index(x), value)
}

#' @export
time_unit <- function(x) units(x[[index(x)]])

#' @export
time_step <- function(x) as.double(interval(x), units = time_unit(x))
