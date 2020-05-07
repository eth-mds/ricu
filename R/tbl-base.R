
#' @export
`[.icu_tbl` <- function(x, ...) {
  reclass_tbl(NextMethod(), tbl_meta(x))
}

#' @export
dimnames.icu_tbl <- function(x) list(NULL, colnames(x))

#' @export
print.icu_tbl <- function(x, ..., n = NULL, width = NULL, n_extra = NULL) {
  cat_line(format(x, ..., n = n, width = width, n_extra = n_extra))
  invisible(x)
}

#' @export
format.icu_tbl <- function(x, ..., n = NULL, width = NULL, n_extra = NULL) {
  mat <- prt::trunc_dt(x, n = n, width = width, n_extra = n_extra)
  format(mat)
}

#' @export
str.icu_tbl <- function(object, ...) {
  invisible(prt::str_dt(object, ...))
}

#' @importFrom data.table as.data.table
#' @method as.data.table icu_tbl
#'
#' @export
#'
as.data.table.icu_tbl <- function(x, ...) {

  if (...length() > 0L) warning("Ignoring further `...` arguments.")

  unclass_tbl(x)

  x
}

#' @method as.data.frame icu_tbl
#'
#' @export
#'
as.data.frame.icu_tbl <- function(x, row.names = NULL, optional = FALSE, ...) {

  if (!is.null(row.names)) warning("Ignoring `row.names` argument.")
  if (!isFALSE(optional)) warning("Ignoring `optional` argument.")
  if (...length() > 0L) warning("Ignoring further `...` arguments.")

  data.table::setDF(as.data.table(x))

  x
}

#' @export
.cbind.icu_tbl <- function(..., keep.rownames = FALSE, check.names = FALSE,
                          key = NULL, stringsAsFactors = FALSE) {

  lst <- list(...)
  check <- vapply(lst, is_icu_tbl, logical(1L))

  if (sum(check) == 1L) {
    hit <- which(check)
    lst <- c(lst[hit], lst[-hit])
    meta <- tbl_meta(lst[[hit]])
  } else {
    meta <- NULL
  }

  res <- do.call(data.table::data.table,
    c(lst, list(keep.rownames = keep.rownames, check.names = check.names,
                key = key, stringsAsFactors = stringsAsFactors))
  )

  reclass_tbl(res, meta)
}

#' @export
.rbind.icu_tbl <- function(..., use.names = TRUE, fill = FALSE, idcol = NULL) {
  rbind_lst(list(...), use.names = use.names, fill = fill, idcol = idcol)
}

#' @rawNamespace if (getRversion() >= "4.0.0") { S3method(cbind, icu_tbl) }
cbind.icu_tbl <- .cbind.icu_tbl

#' @rawNamespace if (getRversion() >= "4.0.0") { S3method(rbind, icu_tbl) }
rbind.icu_tbl <- .rbind.icu_tbl

#' @export
split.icu_tbl <- function(x, ...) {
  lapply(NextMethod(), reclass_tbl, tbl_meta(x))
}

#' @export
merge.icu_tbl <- function(x, y, by = NULL, by.x = NULL, by.y = NULL, ...) {

  if (is_ts_tbl(x) && is_ts_tbl(y)) {
    assert_that(same_interval(x, y))
  }

  targ <- NULL

  if (is_icu_tbl(y)) {

    if (is_ts_tbl(x) && is_ts_tbl(y)) {

      if (same_meta_cols(x, y)) {
        if (is.null(by))   by   <- meta_cols(x)
      } else {
        if (is.null(by.x)) by.x <- meta_cols(x)
        if (is.null(by.y)) by.y <- meta_cols(y)
      }

      targ <- tbl_meta(x)

    } else {

      if (same_id(x, y)) {
        if (is.null(by))   by   <- id(x)
      } else {
        if (is.null(by.x)) by.x <- id(x)
        if (is.null(by.y)) by.y <- id(y)
      }

      if (is_ts_tbl(y)) {
        targ <- set_id(tbl_meta(y), id(x))
      } else {
        targ <- tbl_meta(x)
      }
    }

  } else {

    if (has_cols(y, meta_cols(x)) && is.null(by)) {
      by <- meta_cols(x)
    }

    targ <- tbl_meta(x)
  }

  if (is.null(by)) {

    if (is.null(by.x) && is.null(by.y)) {
      res <- data.table::merge.data.table(x, y, ...)
    } else if (is.null(by.x)) {
      res <- data.table::merge.data.table(x, y, by.y = by.y, ...)
    } else {
      res <- data.table::merge.data.table(x, y, by.x = by.x, by.y = by.y, ...)
    }

  } else {

    res <- data.table::merge.data.table(x, y, by, ...)
  }

  reclass_tbl(res, targ)
}
