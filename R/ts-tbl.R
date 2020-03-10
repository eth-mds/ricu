
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
as_ts_tbl <- function(tbl, key, index = NULL, interval = hours(1L)) {

  assert_that(inherits(tbl, "data.frame"), is_unique(colnames(tbl)))

  if (!is_dt(tbl)) data.table::setDT(tbl)

  if (is.numeric(key) || is.logical(key)) {
    key <- colnames(tbl)[key]
  }

  key <- new_ts_key(key)

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

  index <- new_ts_index(index, interval)

  tbl <- set_ts_meta(tbl, new_ts_meta(key, index), stop_on_fail = TRUE)
  tbl <- set_ts_class(tbl)

  tbl
}

ts_meta.ts_tbl <- function(x) attr(x, "ts_meta")

set_ts_meta <- function(x, meta, stop_on_fail = TRUE) {

  assert_that(is.flag(stop_on_fail))

  if (!stop_on_fail && is.null(meta)) return(x)

  assert_that(is_ts_meta(meta))

  key <- key(meta)
  index <- index(meta)

  id_cols <- c(key, index)

  check <- validate_that(
    is_dt(x), has_col(x, key), has_col(x, index),
    has_time_col(x, index), has_interval(x, index, interval(meta))
  )

  if (!isTRUE(check)) {
    if (stop_on_fail) stop(check)
    else return(unclass_ts_tbl(x))
  }

  x <- na.omit(x, id_cols)

  setkeyv(x, id_cols)
  setcolorder(x, c(id_cols, setdiff(colnames(x), id_cols)))

  setattr(x, "ts_meta", meta)

  x
}

set_ts_class <- function(x) {
  setattr(x, "class", unique(c("ts_tbl", class(x))))
}

unclass_ts_tbl <- function(x) {

  setattr(x, "ts_meta", NULL)
  setattr(x, "class", setdiff(class(x), "ts_tbl"))

  x
}

reclass_ts_tbl <- function(x, meta) {

  x <- set_ts_meta(x, meta, stop_on_fail = FALSE)

  if (has_attr(x, "ts_meta")) {
    x <- set_ts_class(x)
  }

  x
}

#' @export
rm_cols.ts_tbl <- function(x, cols, ...) {

  cols <- unique(intersect(cols, colnames(x)))

  assert_that(has_cols(x, cols))

  if (any(id_cols(x) %in% cols)) {
    x <- unclass_ts_tbl(x)
  }

  x <- set(x, j = cols, value = NULL)

  x
}

#' @export
rename_cols.ts_tbl <- function(x, new, old = colnames(x), ...) {

  hits <- old %in% colnames(x)

  new <- new[hits]
  old <- old[hits]

  assert_that(is_unique(replace(colnames(x), match(old, colnames(x)), new)))

  meta <- rename_cols(ts_meta(x), new, old)
  x <- setnames(x, old, new)

  reclass_ts_tbl(x, meta)
}

#' @export
index.ts_tbl <- function(x) index(ts_meta(x))

#' @export
set_index.ts_tbl <- function(x, value) {
  set_ts_meta(x, set_index(ts_meta(x), value))
}

#' @export
key.ts_tbl <- function(x) key(ts_meta(x))

#' @export
set_key.ts_tbl <- function(x, value) {
  set_ts_meta(x, set_key(ts_meta(x), value))
}

#' @export
data_cols <- function(x) setdiff(colnames(x), id_cols(x))

#' @export
id_cols <- function(x) c(key(x), index(x))

#' @export
interval.ts_tbl <- function(x) {
  `units<-`(interval(ts_meta(x)), time_unit(x))
}

#' @export
set_interval.ts_tbl <- function(x, value) {

  if (interval(x) > value) {
    warning("Higher time resolution does not add missing time steps")
  }

  set(x, j = index(x),
      value = round_to(time_col(x), as.double(value, time_unit(x))))

  set_ts_meta(x, set_interval(ts_meta(x), value))

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

  set_ts_meta(x, set_time_unit(ts_meta(x), value))
}

#' @export
time_step <- function(x) as.double(interval(x), units = time_unit(x))

#' @export
time_col <- function(x) x[[index(x)]]

#' @export
data_unit <- function(x) {

  get_unit <- function(col) {
    if (has_attr(x[[col]], "units")) attr(x[[col]], "units")
    else NA_character_
  }

  chr_ply(data_cols(x), get_unit, use_names = TRUE)
}

#' @export
units.ts_tbl <- function(x) {
  setNames(c(time_unit(x), data_unit(x)), c(time_col(x), data_cols(x)))
}

#' @export
make_compatible <- function(x, def, key = TRUE, index = TRUE) {

  def <- ts_meta(def)

  assert_that(is_ts_tbl(x), is_ts_meta(def))

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
  meta <- ts_meta(lst[[first]])

  lst[ts_tbls] <- lapply(lst[ts_tbls], make_compatible, meta)

  res <- dt_rbl(lst, use.names, fill, idcol)

  reclass_ts_tbl(res, meta)
}
