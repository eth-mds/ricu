
#' @rdname ts_tbl
#'
#' @export
#'
`[.ts_tbl` <- function(x, ...) {
  reclass_ts_tbl(NextMethod(), ts_meta(x))
}

#' @rdname ts_tbl
#'
#' @export
#'
dimnames.ts_tbl <- function(x) list(NULL, colnames(x))

#' @rdname ts_tbl
#'
#' @export
#'
print.ts_tbl <- function(x, ..., n = NULL, width = NULL, n_extra = NULL) {
  cat_line(format(x, ..., n = n, width = width, n_extra = n_extra))
  invisible(x)
}

#' @rdname ts_tbl
#'
#' @export
#'
format.ts_tbl <- function(x, ..., n = NULL, width = NULL, n_extra = NULL) {
  mat <- prt::trunc_dt(x, n = n, width = width, n_extra = n_extra)
  format(mat)
}

#' @rdname ts_tbl
#'
#' @importFrom tibble tbl_sum
#'
#' @export
#'
tbl_sum.ts_tbl <- function(x) {
  c("A `ts_tbl`" = prt::dim_desc(x),
    "Id" = paste0("<`", id(x), "`>"),
    "Index" = paste0("<`", index(x), "`, ", format(interval(x)), ">"))
}

#' @param object A `ts_tbl` object.
#'
#' @rdname ts_tbl
#'
#' @export
#'
str.ts_tbl <- function(object, ...) {
  invisible(prt::str_dt(object, ...))
}

#' @rdname ts_tbl
#'
#' @importFrom data.table as.data.table
#' @method as.data.table ts_tbl
#'
#' @export
#'
as.data.table.ts_tbl <- function(x, ...) {

  if (...length() > 0L) warning("Ignoring further `...` arguments.")

  unclass_ts_tbl(x)

  x
}

#' @param row.names,optional Generic consistency: passing anything other than
#' the default value issues a warning.
#'
#' @rdname ts_tbl
#'
#' @method as.data.frame ts_tbl
#'
#' @export
#'
as.data.frame.ts_tbl <- function(x, row.names = NULL, optional = FALSE, ...) {

  if (!is.null(row.names)) warning("Ignoring `row.names` argument.")
  if (!isFALSE(optional)) warning("Ignoring `optional` argument.")
  if (...length() > 0L) warning("Ignoring further `...` arguments.")

  data.table::setDF(as.data.table(x))

  x
}

#' @export
.cbind.ts_tbl <- function(..., keep.rownames = FALSE, check.names = FALSE,
                          key = NULL, stringsAsFactors = FALSE) {

  lst <- list(...)
  check <- vapply(lst, is_ts_tbl, logical(1L))

  if (sum(check) == 1L) {
    hit <- which(check)
    lst <- c(lst[hit], lst[-hit])
    meta <- ts_meta(lst[[hit]])
  } else {
    meta <- NULL
  }

  res <- do.call(data.table::data.table,
    c(lst, list(keep.rownames = keep.rownames, check.names = check.names,
                key = key, stringsAsFactors = stringsAsFactors))
  )

  if (!is.null(meta)) {
    reclass_ts_tbl(res, meta)
  } else {
    res
  }
}

#' @export
.rbind.ts_tbl <- function(..., use.names = TRUE, fill = FALSE, idcol = NULL) {
  rbind_lst(list(...), use.names = use.names, fill = fill, idcol = idcol)
}

#' @rawNamespace if (getRversion() >= "4.0.0") { S3method(cbind, ts_tbl) }
cbind.ts_tbl <- .cbind.ts_tbl

#' @rawNamespace if (getRversion() >= "4.0.0") { S3method(rbind, ts_tbl) }
rbind.ts_tbl <- .rbind.ts_tbl

#' @export
split.ts_tbl <- function(x, ...) {
  lapply(NextMethod(), reclass_ts_tbl, ts_meta(x))
}

#' @export
print.ts_meta <- function(x, ...) cat_line(format(x, ...))

#' @export
print.tbl_id <- function(x, ...) cat_line(format(x, ...))

#' @export
print.tbl_index <- function(x, ...) cat_line(format(x, ...))

#' @export
format.ts_meta <- function(x, ...) {
  format_one_meta(x, format(tbl_id(x)), format(tbl_index(x)))
}

#' @export
format.tbl_id <- function(x, ...) {
  format_one_meta(x, paste0("`", id(x), "`"))
}

#' @export
format.tbl_index <- function(x, ...) {
  format_one_meta(x, paste0("`", index(x), "`"), format(interval(x)))
}

format_one_meta <- function(x, ...) {
  paste0("<", class(x), "[", paste(..., sep = ", "), "]>")
}


#' @export
merge.ts_tbl <- function(x, y, by = meta_cols(x), ...) {

  if (is_ts_tbl(y)) {
    y <- data.table::copy(y)
    y <- make_compatible(y, x, id(x) %in% by, index(x) %in% by)
  }

  reclass_ts_tbl(NextMethod(), ts_meta(x))
}
