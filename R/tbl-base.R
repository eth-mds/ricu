
#' @export
`[.id_tbl` <- function(x, ...) wrap_ptype(as_ptype(x), NextMethod())

#' @export
`[<-.id_tbl` <- function(x, ..., value) wrap_ptype(as_ptype(x), NextMethod())

#' @export
`[[<-.id_tbl` <- function(x, ..., value) wrap_ptype(as_ptype(x), NextMethod())

#' @export
`$<-.id_tbl` <- function(x, ..., value) wrap_ptype(as_ptype(x), NextMethod())

wrap_ptype <- function(ptyp, res) {

  ptyp <- force(ptyp)

  if (is_dt(res)) {
    reclass_tbl(res, ptyp, FALSE)
  } else {
    res
  }
}

#' @method row.names id_tbl
#' @export
row.names.id_tbl <- function(x) NULL

#' @method row.names<- id_tbl
#' @export
`row.names<-.id_tbl` <- function(x, value) {
  warn_arg("value")
  x
}

#' @export
`names<-.id_tbl` <- function(x, value) rename_cols(x, value)

#' @export
`dimnames<-.id_tbl` <- function(x, value) {

  assert_that(length(value) == 2L)

  if (not_null(value[[1L]])) {
    warn_arg("value[[1]]")
  }

  rename_cols(x, value[[2L]])
}

#' @export
print.id_tbl <- function(x, ..., n = NULL, width = NULL, n_extra = NULL) {
  cat_line(format(x, ..., n = n, width = width, n_extra = n_extra))
  invisible(x)
}

#' @export
format.id_tbl <- function(x, ..., n = NULL, width = NULL, n_extra = NULL) {
  format(prt::trunc_dt(x, n = n, width = width, n_extra = n_extra))
}

#' @importFrom tibble tbl_sum
#' @export
tbl_sum.win_tbl <- function(x) {
  c(NextMethod(), `Duration var` = quote_bt(dur_var(x)))
}

#' @export
tbl_sum.ts_tbl <- function(x) {
  idx <- paste0(quote_bt(index_var(x)), " (", format(interval(x)), ")")
  c(NextMethod(), `Index var` = idx)
}

#' @export
tbl_sum.id_tbl <- function(x) {

  get_unit <- function(x) {
    if (inherits(x, "difftime")) ""
    else if (has_attr(x, "units")) attr(x, "units")
    else ""
  }

  ids <- id_vars(x)
  cls <- class(x)[1L]
  cls <- paste0(approx_art(cls), " `", cls, "`")

  res <- setNames(
    c(dim_desc(x), concat(quote_bt(ids))),
    c(cls, paste0("Id var", if (length(ids) > 1L) "s"))
  )

  unt <- chr_ply(x, get_unit, use_names = TRUE)
  unt <- unt[nzchar(unt)]

  if (has_length(unt)) {
    res <- c(res, Units = concat("`", names(unt), "` [", unt, "]"))
  }

  res
}

approx_art <- function(x) {
  if (substr(x, 1L, 1L) %in% c("a", "e", "i", "o", "u")) "An" else "A"
}

#' @export
str.id_tbl <- function(object, ...) invisible(prt::str_dt(object, ...))

#' ICU class data reshaping
#'
#' Utilities for reshaping `id_tbl` and `ts_tbl` objects.
#'
#' @param ... Objects to combine
#' @param keep.rownames,check.names,key,stringsAsFactors Forwarded to
#' [data.table::data.table]
#'
#' @return Either `id_tbl` or `ts_tbl` objects (depending on inputs) or lists
#' thereof in case of `split()` and `unmerge()`.
#'
#' @rdname tbl_reshape
#' @keywords internal
#'
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

#' @param use.names,fill,idcol Forwarded to [data.table::rbindlist]
#'
#' @rdname tbl_reshape
#'
#' @export
.rbind.id_tbl <- function(..., use.names = TRUE, fill = FALSE, idcol = NULL) {
  rbind_lst(list(...), use.names = use.names, fill = fill, idcol = idcol)
}

#' @rawNamespace if (getRversion() >= "4.0.0") { S3method(cbind, id_tbl) }
cbind.id_tbl <- .cbind.id_tbl

#' @rawNamespace if (getRversion() >= "4.0.0") { S3method(rbind, id_tbl) }
rbind.id_tbl <- .rbind.id_tbl

#' @param x,y Objects to combine
#' @param by,by.x,by.y Column names used for combining data
#'
#' @rdname tbl_reshape
#' @export
merge.id_tbl <- function(x, y, by = NULL, by.x = NULL, by.y = NULL, ...) {

  targ <- NULL

  if (is_win_tbl(x) || is_win_tbl(y)) {
    stop_ricu("`win_tbl` objects should be converted to `ts_tbl` objects using
               `expand()` before merging", class = "merge_win_tbl")
  }

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
        targ <- as_ptype(y)
        targ <- rename_cols(targ, id_vars(x), id_vars(targ))
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

  res <- reclass_tbl(res, targ)

  sort(res, by_ref = TRUE)
}

#' @rdname tbl_reshape
#' @export
split.id_tbl <- function(x, ...) lapply(NextMethod(), try_reclass, x)

#' @rdname tbl_reshape
#' @export
#'
rbind_lst <- function(x, ...) {

  cond_as <- function(x) if (is.list(x)) x else as.data.table(x)

  dt_rbl <- function(x, ...) rbindlist(lapply(x, cond_as), ...)

  do_rename <- function(x, new) {
    fun <- if (is_ts_tbl(x) && is_ts_tbl(new)) meta_vars else id_vars
    rename_cols(x, fun(new), fun(x), by_ref = TRUE)
  }

  win_tbl <- lgl_ply(x, is_win_tbl)

  id_tbl <- lgl_ply(x, is_id_tbl)
  ts_tbl <- lgl_ply(x, is_ts_tbl) & !win_tbl
  id_tbl <- id_tbl & !ts_tbl

  if (any(id_tbl)) {

    ptyp <- as_ptype(x[[which(id_tbl)[1L]]])

  } else if (any(ts_tbl)) {

    ptyp <- as_ptype(x[[which(ts_tbl)[1L]]])

    assert_that(
      all_fun(lapply(x[ts_tbl], interval), all_equal, interval(ptyp)),
      msg = "cannot mix interval lengths when row-binding"
    )

  } else {

    ptyp <- NULL
  }

  if (sum(win_tbl) >= 2L) {

    units <- chr_ply(x[win_tbl], dur_unit)
    targ  <- min_time_unit(units)

    todo <- rep_along(FALSE, x)
    todo[win_tbl] <- units != targ

    if (any(todo)) {
      x[todo] <- lapply(x[todo], change_dur_unit, targ)
    }
  }

  if (not_null(ptyp)) {

    id_tbls <- lgl_ply(x, is_id_tbl)
    old_ptp <- lapply(x[id_tbls], as_ptype)

    x[id_tbls] <- lapply(x[id_tbls], do_rename, ptyp)

    on.exit(Map(do_rename, x[id_tbls], old_ptp))
  }

  res <- reclass_tbl(dt_rbl(x, ...), ptyp)

  sort(res, by_ref = TRUE)
}

#' @rdname tbl_reshape
#' @export
#'
merge_lst <- function(x) {

  assert_that(is.list(x), all_fun(x, is_id_tbl))

  ts <- lgl_ply(x, is_ts_tbl)
  id <- c(which(ts), which(!ts))
  ft <- unlist(lapply(x, data_vars))

  x <- reduce(merge, x[id], all = TRUE)
  x <- setcolorder(x, c(meta_vars(x), ft))

  x
}

#' @param col_groups A list of character vectors defining the grouping of
#' non-by columns
#' @param na_rm Logical flag indicating whether to remove rows that have all
#' missing entries in the respective `col_groups` group
#'
#' @rdname tbl_reshape
#' @export
#'
unmerge <- function(x, col_groups = as.list(data_vars(x)), by = meta_vars(x),
                    na_rm = TRUE) {

  name_has <- function(name, x) has_name(x, name)

  assert_that(has_name(x, by), all_fun(col_groups, name_has, x),
              is.flag(na_rm))

  extract_col <- function(col, x) {

    y <- x[, c(by, col), with = FALSE]

    if (na_rm) {
      y <- rm_na(y, col)
    }

    y
  }

  lapply(col_groups, extract_col, x)
}
