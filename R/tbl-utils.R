
set_meta <- function(x, meta, stop_on_fail = TRUE) {

  assert_that(is.flag(stop_on_fail))

  if (!stop_on_fail && is.null(meta)) return(x)

  assert_that(is_tbl_meta(meta))

  check_meta <- validate_meta(x, meta)
  check_data <- validate_that(is_dt(x), is_unique(colnames(x)))

  if (!isTRUE(check_data) || !isTRUE(check_meta)) {
    if (stop_on_fail && !isTRUE(check_data)) stop(check_data)
    if (stop_on_fail && !isTRUE(check_meta)) stop(check_meta)
    return(unclass_tbl(x))
  }

  cols <- meta_cols(meta)

  x <- na.omit(x, cols)

  setkeyv(x, cols)
  setcolorder(x, c(cols, setdiff(colnames(x), cols)))

  setattr(x, "tbl_meta", meta)

  x
}

set_class <- function(x, meta) {
  setattr(x, "class", unique(c(tbl_class(meta), "icu_tbl", strip_class(x))))
}

strip_class <- function(x) {
  setdiff(class(x), c("id_tbl", "ts_tbl", "icu_tbl"))
}

unclass_tbl <- function(x) {

  setattr(x, "tbl_meta", NULL)
  setattr(x, "class", strip_class(x))

  x
}

reclass_tbl <- function(x, meta) {

  x <- set_meta(x, meta, stop_on_fail = FALSE)

  if (has_attr(x, "tbl_meta")) {
    return(set_class(x, meta))
  }

  if (is_ts_meta(meta)) {
    meta <- as_id_meta(meta)
    x <- set_meta(x, meta, stop_on_fail = FALSE)
  }

  if (has_attr(x, "tbl_meta")) {
    set_class(x, meta)
  }

  x
}

#' @rdname tbl_utils
#' @export
#'
rbind_lst <- function(x, ...) {

  cond_as <- function(x) {
    if (is.list(x)) x else data.table::as.data.table(x)
  }

  dt_rbl <- function(x, ...) {
    data.table::rbindlist(lapply(x, cond_as), ...)
  }

  rename <- function(x, new) {

    if (is_ts_tbl(x) && is_ts_meta(new)) {
      fun <- function(y) c(id(y), index(y))
    } else {
      fun <- function(y) id(y)
    }

    rename_cols(x, fun(new), fun(x))
  }

  id_tbl <- lgl_ply(x, is_id_tbl)
  ts_tbl <- lgl_ply(x, is_ts_tbl)

  if (any(id_tbl)) {

    meta <- tbl_meta(x[[which(id_tbl)[1L]]])

  } else if (any(ts_tbl)) {

    meta <- tbl_meta(x[[which(ts_tbl)[1L]]])

    assert_that(
      all(lgl_ply(lapply(x[ts_tbl], interval), all.equal, interval(meta))),
      msg = "cannot mix interval types when row-binding"
    )

  } else {

    meta <- NULL
  }

  if (!is.null(meta)) {

    icu_tbl <- lgl_ply(x, is_icu_tbl)
    old_met <- lapply(x[icu_tbl], tbl_meta)

    x[icu_tbl] <- lapply(x[icu_tbl], rename, meta)

    on.exit(Map(rename, x[icu_tbl], old_met))
  }

  res <- dt_rbl(x, ...)

  if (is.null(meta)) {
    res
  } else {
    reclass_tbl(res, meta)
  }

}

#' @param cols Column names of columns to consider
#' @param mode Switch between `all` where all entries of a row have to be
#' missing (for the selected columns) or `any`, where a single missing entry
#' suffices
#'
#' @rdname tbl_utils
#' @export
#'
rm_na <- function(x, cols = data_cols(x), mode = c("all", "any")) {

  mode <- match.arg(mode)

  assert_that(has_cols(x, cols))

  if (identical(mode, "any")) {
    return(na.omit(x, cols))
  }

  if (length(cols) == 1L) {
    drop <- is.na(x[[cols]])
  } else {
    drop <- Reduce(`&`, lapply(x[, cols, with = FALSE], is.na))
  }

  x[!drop, ]
}

#' @param col_groups A list of character vectors defining the grouping of
#' non-by columns
#' @param by Columns that will be present in every one of the resulting tables
#' @param na_rm Logical flag indicating whether to remove rows that have all
#' missing entries in the respective `col_groups` group
#'
#' @rdname tbl_utils
#' @export
#'
unmerge <- function(x, col_groups = as.list(data_cols(x)), by = meta_cols(x),
                    na_rm = TRUE) {

  name_has <- function(name, x) has_name(x, name)

  assert_that(has_name(x, by), all_is(col_groups, name_has, x), is.flag(na_rm))

  extract_col <- function(col, x) {

    y <- x[, c(by, col), with = FALSE]

    if (na_rm) {
      y <- rm_na(y, col)
    }

    y
  }

  lapply(col_groups, extract_col, x)
}

#' @param fun Function name (as string) to apply over groups
#'
#' @rdname tbl_utils
#' @export
#'
dt_gforce <- function(x,
                      fun = c("mean", "median", "min", "max", "sum", "prod",
                              "var", "sd", "first", "last"),
                      by = meta_cols(x), cols = data_cols(x),
                      na_rm = !fun %in% c("first", "last")) {

  fun <- match.arg(fun)

  if (fun %in% c("first", "last") && isTRUE(na_rm)) {
    warning("The argument `na_rm` is ignored for `first()` and `last()`")
  }

  assert_that(is.flag(na_rm), all(c(cols, by) %in% colnames(x)))

  switch(fun,
    mean   = x[, lapply(.SD, mean, na.rm = na_rm),   by = by, .SDcols = cols],
    median = x[, lapply(.SD, median, na.rm = na_rm), by = by, .SDcols = cols],
    min    = x[, lapply(.SD, min, na.rm = na_rm),    by = by, .SDcols = cols],
    max    = x[, lapply(.SD, max, na.rm = na_rm),    by = by, .SDcols = cols],
    sum    = x[, lapply(.SD, sum, na.rm = na_rm),    by = by, .SDcols = cols],
    prod   = x[, lapply(.SD, prod, na.rm = na_rm),   by = by, .SDcols = cols],
    var    = x[, lapply(.SD, var, na.rm = na_rm),    by = by, .SDcols = cols],
    sd     = x[, lapply(.SD, sd, na.rm = na_rm),     by = by, .SDcols = cols],
    first  = x[, lapply(.SD, data.table::first),     by = by, .SDcols = cols],
    last   = x[, lapply(.SD, data.table::last),      by = by, .SDcols = cols]
  )
}

#' @rdname tbl_utils
#' @export
#'
is_unique <- function(x, ...) UseMethod("is_unique", x)

#' @rdname tbl_utils
#' @export
#'
is_unique.default <- function(x, ...) identical(anyDuplicated(x, ...), 0L)

#' @export
is_unique.icu_tbl <- function(x, by = meta_cols(x), ...) {
  identical(anyDuplicated(x, by = by, ...), 0L)
}

#' @param expr Expression to apply over groups
#'
#' @rdname tbl_utils
#' @export
#'
make_unique <- function(x, expr, fun, ...) {

  if (missing(fun)) {

    make_unique_quo(x, substitute(expr), ...)

  } else {

    make_unique_quo(x, fun, ...)
  }
}

#' @rdname tbl_utils
#' @export
#'
make_unique_quo <- function(x, expr, by = meta_cols(x), cols = data_cols(x),
                            ...) {

  assert_that(is_icu_tbl(x))

  if (nrow(x) == 0) return(x)
  if (length(cols) == 0L) return(unique(x))

  if (is.function(expr)) {

    x[, lapply(.SD, expr, ...), .SDcols = cols, by = by]

  } else if (!is.language(expr) && is.null(expr)) {

    assert_that(is_unique(x, by = by))
    x

  } else if (is.character(expr)) {

    assert_that(is.string(expr))

    if (is.na(expr)) {
      if (is.numeric(x[[data_cols(x)]])) {
        expr <- "median"
      } else {
        expr <- "first"
      }
    }

    dt_gforce(x, expr, by = by, cols = cols, ...)

  } else {

    x[, eval(expr), by = by]
  }
}
