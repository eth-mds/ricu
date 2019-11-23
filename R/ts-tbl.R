
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
as_ts_tbl <- function(tbl, key, index = NULL, interval = hours(1L), ...) {

  assert_that(inherits(tbl, "data.frame"))

  if (!is_dt(tbl)) data.table::setDT(tbl)

  new_ts_tbl(tbl, c(ts_tbl_key_ind(tbl, key, index, interval), list(...)))
}

#' @param ... Passed to [data.table::data.table()]/generic compatibility.
#'
#' @rdname ts_tbl
#'
#' @export
#'
ts_tbl <- function(..., key, index = NULL, interval = hours(1L)) {
  as_ts_tbl(data.table::data.table(...), key, index, interval)
}

new_ts_tbl <- function(tbl, meta) {

  assert_that(is_dt(tbl), is.list(meta),
              all(vapply(meta, is_ts_meta, logical(1L))))

  id_cols <- ts_id_cols(meta)

  setkeyv(tbl, id_cols)
  setcolorder(tbl, c(id_cols, setdiff(cols, id_cols)))

  set_ts_meta_lst(tbl, meta)
  setattr(tbl, "class", unique(c("ts_tbl", class(tbl))))

  tbl
}

ts_tbl_key_ind <- function(x, key, index, interval) {
  list(ts_tbl_key(x, key), ts_tbl_ind(x, index, interval))
}

ts_tbl_key <- function(x, key) {

  if (is.numeric(key) || is.logical(key)) {
    key <- colnames(x)[key]
  }

  new_ts_key(key)
}

ts_tbl_ind <- function(x, index, interval) {

  if (is.null(index)) {

    hits <- vapply(x, is_time, logical(1L))

    assert_that(sum(hits) == 1L,
      msg = paste("In order to automatically determine the index column,",
                  "exactly one `difftime` column is required.")
    )

    index <- colnames(x)[hits]

  } else if (is.numeric(index) || is.logical(index)) {

    index <- colnames(x)[index]
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
set_ts_meta <- function(x, ..., warn_opt = TRUE) {
  set_ts_meta_lst(x, list(...), warn_opt)
}

#' @export
set_ts_meta_lst <- function(x, meta, warn_opt = TRUE) {

  find_hits <- function(new_elem, old_set) {
    which(vapply(old_set, same_class, logical(1L), new_elem))
  }

  old <- attr(x, "ts_meta")

  assert_that(is_ts_tbl(x), all_fun(meta, is_ts_meta),
              is.null(old) || all_fun(old,  is_ts_meta))

  hits <- flapply(meta, find_hits, old)

  if (length(hits) > 0L) old <- old[-hits]

  new <- c(old, meta)
  new <- check_ts_meta(new, x, stop_req = TRUE, warn_opt = warn_opt)

  setattr(x, "ts_meta", new)

  invisible(x)
}

check_ts_meta <- function(objs, tbl, stop_req, warn_opt) {

  msg <- function(prob, fun) {
    fun("Some ts_meta requirements cannot be satisfied:\n  ",
        paste(vapply(prob, format, character(1L)), collapse = ", "))
  }

  needed <- vapply(objs, is_required,   logical(1L))
  failed <- vapply(x,    Negate(check), logical(1L), tbl)

  if (any(needed & failed)) {
    if (stop_req) msg(objs[needed & failed], stop)
    return(NULL)
  }

  if (any(!needed & failed)) {
    if (warn_opt) msg(objs[failed], warning)
    objs <- objs[!failed]
  }

  objs
}

#' @export
get_ts_meta <- function(x, is_fun = NULL) {

  if (is_ts_tbl(x)) {
    assert_that(has_attr(x, "ts_meta"))
    meta <- attr(x, "ts_meta")
  } else {
    assert_that(is.list(x))
    meta <- x
  }

  res <- subset_meta(meta, is_fun, drop = TRUE)

  assert_that(is_ts_meta(res))

  res
}

subset_meta <- function(meta, is_fun = NULL, drop = FALSE) {

  assert_that(is.list(meta), all(vapply(meta, is_ts_meta, logical(1L))))

  if (!is.null(is_fun)) {
    meta <- meta[vapply(meta, is_fun, logical(1L))]
  }

  if (drop && length(meta) == 1L) meta <- meta[[1L]]

  meta
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

unclass_ts_tbl <- function(x) {

  setattr(x, "ts_meta", NULL)
  setattr(x, "class", setdiff(class(x), "ts_tbl"))

  invisible(x)
}

reclass_ts_tbl <- function(x, meta, warn_opt = TRUE) {

  meta <- check_ts_meta(meta, x, stop_req = FALSE, warn_opt = warn_opt)

  if (is.null(meta)) {
    x <- unclass_ts_tbl(x)
  } else {
    x <- new_ts_tbl(x, meta)
  }

  x
}

rm_cols <- function(x, cols) {

  assert_that(is_ts_tbl(x))

  meta <- get_ts_meta(x)

  old_cols <- lapply(meta, meta_cols)
  new_cols <- lapply(old_cols, setdiff, cols)

  Map(`meta_cols<-`, meta, new_cols)

  x <- set(x, j = cols, value = NULL)
  x <- reclass_ts_tbl(x, meta)

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
