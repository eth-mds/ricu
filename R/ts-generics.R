
#' @rdname ts_tbl
#'
#' @export
#'
`[.ts_tbl` <- function(x, ...) {
  reclass_ts_tbl(NextMethod(), get_ts_spec(x), warn_on_fail = FALSE)
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
  c("A ts_tbl" = prt::dim_desc(x),
    "Index" = paste0(ts_key(x), collapse = ", "),
    "Key" = ts_index(x),
    "Interval" = format(step_time(x)))
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
    spec <- get_ts_spec(lst[[hit]])
  } else {
    spec <- NULL
  }

  res <- do.call(data.table::data.table,
    c(lst, list(keep.rownames = keep.rownames, check.names = check.names,
                key = key, stringsAsFactors = stringsAsFactors))
  )

  if (!is.null(spec)) {
    reclass_ts_tbl(res, spec, warn_on_fail = TRUE)
  } else {
    res
  }
}

#' @export
.rbind.ts_tbl <- function(..., use.names = TRUE, fill = FALSE, idcol = NULL) {

  cond_as <- function(x) {
    if (is.list(x)) x else data.table::as.data.table(x)
  }

  dt_rbl <- function(x, use.names, fill, idcol) {
    data.table::rbindlist(lapply(x, cond_as), use.names, fill, idcol)
  }

  lst <- list(...)

  hit <- which(vapply(lst, is_ts_tbl, logical(1L)))[1L]
  spec <- get_ts_spec(lst[[hit]])

  reclass_ts_tbl(
    dt_rbl(lst, use.names, fill, idcol),
    get_ts_spec(lst[[hit]]),
    warn_on_fail = TRUE
  )
}

#' @rawNamespace if (getRversion() >= "3.6.2") { S3method(cbind, ts_tbl) }
cbind.ts_tbl <- .cbind.ts_tbl

#' @rawNamespace if (getRversion() >= "3.6.2") { S3method(rbind, ts_tbl) }
rbind.ts_tbl <- .rbind.ts_tbl
