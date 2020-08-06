
#' @export
`[.id_tbl` <- function(x, ...) {

  ptyp <- as_ptype(x)
  res  <- NextMethod()

  if (is_dt(res)) {
    reclass_tbl(res, ptyp, FALSE)
  } else {
    res
  }
}

#' @export
dimnames.id_tbl <- function(x) list(NULL, colnames(x))

#' @export
print.id_tbl <- function(x, ..., n = NULL, width = NULL, n_extra = NULL) {
  cat_line(format(x, ..., n = n, width = width, n_extra = n_extra))
  invisible(x)
}

#' @export
format.id_tbl <- function(x, ..., n = NULL, width = NULL, n_extra = NULL) {
  mat <- prt::trunc_dt(x, n = n, width = width, n_extra = n_extra)
  format(mat)
}

#' @importFrom tibble tbl_sum
#' @export
tbl_sum.ts_tbl <- function(x) {
  ids <- id_vars(x)
  setNames(
    c(dim_desc(x), concat(quote_bt(ids)),
      paste0(quote_bt(index_var(x)), " (", format(interval(x)), ")")),
    c("A `ts_tbl`", paste0("Id var", if (length(ids) > 1L) "s"), "Index var")
  )
}

#' @importFrom tibble tbl_sum
#' @export
tbl_sum.id_tbl <- function(x) {
  ids <- id_vars(x)
  setNames(c(dim_desc(x), concat(quote_bt(ids))),
           c("An `id_tbl`", paste0("Id var", if (length(ids) > 1L) "s")))
}

#' @export
str.id_tbl <- function(object, ...) invisible(prt::str_dt(object, ...))

#' @method as.data.table id_tbl
#' @export
as.data.table.id_tbl <- function(x, ...) warn_dot_ident(unclass_tbl(x), ...)

#' @method as.data.frame id_tbl
#' @export
as.data.frame.id_tbl <- function(x, row.names = NULL, optional = FALSE, ...) {

  if (!is.null(row.names)) {
    warn_arg("row.names")
  }

  if (!isFALSE(optional)) {
    warn_arg("optional")
  }

  setDF(as.data.table(x, ...))
}

#' @export
.cbind.id_tbl <- function(..., keep.rownames = FALSE, check.names = FALSE,
                          key = NULL, stringsAsFactors = FALSE) {

  lst <- list(...)
  check <- lgl_ply(lst, is_id_tbl)

  if (sum(check) == 1L) {
    hit <- which(check)
    lst <- c(lst[hit], lst[-hit])
    ptyp <- as_ptype(lst[[hit]])
  } else {
    ptyp <- NULL
  }

  res <- do.call(data.table::data.table,
    c(lst, list(keep.rownames = keep.rownames, check.names = check.names,
                key = key, stringsAsFactors = stringsAsFactors))
  )

  reclass_tbl(res, ptyp)
}

#' @export
.rbind.id_tbl <- function(..., use.names = TRUE, fill = FALSE, idcol = NULL) {
  rbind_lst(list(...), use.names = use.names, fill = fill, idcol = idcol)
}

#' @rawNamespace if (getRversion() >= "4.0.0") { S3method(cbind, id_tbl) }
cbind.id_tbl <- .cbind.id_tbl

#' @rawNamespace if (getRversion() >= "4.0.0") { S3method(rbind, id_tbl) }
rbind.id_tbl <- .rbind.id_tbl

#' @export
split.id_tbl <- function(x, ...) lapply(NextMethod(), reclass_tbl, x)

#' @export
merge.id_tbl <- function(x, y, by = NULL, by.x = NULL, by.y = NULL, ...) {

  targ <- NULL

  if (is_id_tbl(y)) {

    if (is_ts_tbl(x) && is_ts_tbl(y)) {

      assert_that(same_time(interval(x), interval(y)))

      if (setequal(meta_vars(x), meta_vars(y))) {
        if (is.null(by))   by   <- meta_vars(x)
      } else {
        if (is.null(by.x)) by.x <- meta_vars(x)
        if (is.null(by.y)) by.y <- meta_vars(y)
      }

      targ <- as_ptype(x)

    } else {

      if (setequal(id_vars(x), id_vars(y))) {
        if (is.null(by))   by   <- id_vars(x)
      } else {
        if (is.null(by.x)) by.x <- id_vars(x)
        if (is.null(by.y)) by.y <- id_vars(y)
      }

      if (is_ts_tbl(y)) {
        targ <- new_ts_tbl(list(), id_vars(x), index_var(y), interval(y))
      } else {
        targ <- as_ptype(x)
      }
    }

  } else {

    if (has_cols(y, meta_vars(x)) && is.null(by)) {
      by <- meta_vars(x)
    }

    targ <- as_ptype(x)
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
