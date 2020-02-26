
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

  set_ts_def(tbl, meta)
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
ts_def.ts_tbl <- function(x) attr(x, "ts_def")

#' @export
ts_def.ts_def <- function(x) x

#' @export
update_ts_def <- function(x, new, ...) {

  find_hits <- function(new_elem, old_set) {
    which(vapply(old_set, same_class, logical(1L), new_elem))
  }

  new <- as_ts_def(new)
  old <- ts_def(x)

  if (!is.null(old)) {

    assert_that(is_ts_def(old))

    hits <- flapply(new, find_hits, old)

    if (length(hits) > 0L) old <- old[-hits]
    if (length(old) > 0L) new <- c(old, new)
  }

  set_ts_def(x, new, ...)
}

make_valid <- function(meta, tbl, stop_req = TRUE, warn_opt = TRUE) {

  msg_fun <- function(fun, issues) {
    fun("Error validating `", deparse(meta), "`:\n  -",
        paste(issues, collapse = "\n  -"))
  }

  assert_that(is_ts_def(meta), all_fun(meta, is_ts_meta))

  res <- lapply(meta, validate, tbl)
  req <- vapply(meta, is_required, logical(1L))
  ok  <- vapply(res, isTRUE, logical(1L))

  req_not_ok <- req & !ok

  if (any(req_not_ok)) {
    if (stop_req) msg_fun(stop, res[req_not_ok])
    return(NULL)
  }

  opt_not_ok <- !req & !ok

  if (any(opt_not_ok) && warn_opt) {
    msg_fun(warning, res[opt_not_ok])
  }

  meta[ok]
}

#' @export
set_ts_def <- function(x, new, warn_opt = TRUE) {

  new <- as_ts_def(new)
  new <- make_valid(new, x, warn_opt = warn_opt)

  setattr(x, "ts_def", new)

  x
}

#' @export
validate.ts_tbl <- function(x, ...) validate(ts_def(x), x, ...)

#' @export
unclass_ts_tbl <- function(x) {

  setattr(x, "ts_def", NULL)
  setattr(x, "class", setdiff(class(x), "ts_tbl"))

  x
}

#' @export
reclass_ts_tbl <- function(x, meta, warn_opt = TRUE) {

  if (!is_dt(x)) return(x)

  ids <- id_cols(meta)

  if (has_cols(x, ids) && !identical(data.table::key(x), ids)) {
    x <- data.table::setkeyv(x, ids)
  }

  meta <- make_valid(meta, x, stop_req = FALSE, warn_opt = warn_opt)

  if (is.null(meta)) {
    x <- unclass_ts_tbl(x)
  } else {
    x <- new_ts_tbl(x, meta)
  }

  x
}

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
index.ts_tbl <- function(x) index(ts_def(x))

#' @export
set_index <- function(x, new) {
  update_ts_def(x, new_tbl_index(x, new, interval(x)))
}

#' @export
key.ts_tbl <- function(x) key(ts_def(x))

#' @export
set_key <- function(x, new) update_ts_def(x, new_tbl_key(x, new))

#' @export
meta_cols <- function(x) flapply(ts_def(x), aux_names)

#' @export
data_cols <- function(x) setdiff(colnames(x), c(id_cols(x), meta_cols(x)))

#' @export
id_cols <- function(x) c(key(x), index(x))

#' @export
interval.ts_tbl <- function(x) interval(ts_def(x))

#' @export
set_interval <- function(x, new) {

  if (is_ts_tbl(x)) {
    set(x, j = index(x),
        value = round_to(x[[index(x)]], as.double(new, time_unit(x))))
    setkeyv(x, id_cols(x))
  }

  update_ts_def(x, new_tbl_index(x, index(x), new))
}

#' @export
time_unit <- function(x) {
  if (is_ts_tbl(x)) units(x[[index(x)]]) else units(interval(x))
}

#' @export
set_time_unit <- function(x, new) {

  if (is_ts_tbl(x)) {
    set(x, j = index(x), value = `units<-`(x[[index(x)]], new))
  }

  update_ts_def(x, new_tbl_index(x, index(x), `units<-`(interval(x), new)))
}

#' @export
time_step <- function(x) as.double(interval(x), units = time_unit(x))

#' @export
ts_meta.ts_tbl <- function(x, ...) ts_meta(ts_def(x), ...)

#' @export
has_aux_names.ts_tbl <- function(x) has_aux_names(ts_def(x))

#' @export
aux_names.ts_tbl <- function(x, ...) aux_names(ts_def(x), ...)

#' @export
aux_data.ts_tbl <- function(x, ...) aux_data(ts_def(x), ...)

#' @export
make_compatible <- function(x, def, key = TRUE, index = TRUE) {

  def <- ts_def(def)

  assert_that(is_ts_tbl(x), is_ts_def(def))

  if (key && !setequal(key(x), key(def))) {
    assert_that(same_length(key(x), key(def)))
    x <- rename_cols(x, key(def), key(x))
  }

  if (index && !setequal(index(x), index(def))) {
    assert_that(same_length(key(x), key(def)))
    x <- rename_cols(x, index(def), index(x))
  }

  if (index && !identical(time_unit(x), time_unit(def))) {
    x <- set_time_unit(x, time_unit(def))
  }

  if (index && !all.equal(interval(x), interval(def))) {
    x <- set_interval(x, interval(def))
  }

  x
}

#' @export
rbind_lst <- function(lst, use.names = TRUE, fill = FALSE, idcol = NULL) {

  cond_as <- function(x) {
    if (is.list(x)) x else data.table::as.data.table(x)
  }

  dt_rbl <- function(x, use.names, fill, idcol) {
    data.table::rbindlist(lapply(x, cond_as), use.names, fill, idcol)
  }

  ts_tbls <- vapply(lst, is_ts_tbl, logical(1L))
  first <- which(ts_tbls)[1L]
  meta <- ts_def(lst[[first]])

  lst[ts_tbls] <- lapply(lst[ts_tbls], make_compatible, meta)

  res <- dt_rbl(lst, use.names, fill, idcol)

  reclass_ts_tbl(res, meta)
}
