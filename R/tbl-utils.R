
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
    x <- set_class(x, meta)
  }

  x
}

#' @export
data_cols <- function(x) setdiff(colnames(x), meta_cols(x))

#' @export
data_unit <- function(x) {

  get_unit <- function(col) {
    if (has_attr(x[[col]], "units")) attr(x[[col]], "units")
    else NA_character_
  }

  chr_ply(data_cols(x), get_unit, use_names = TRUE)
}

#' @export
rbind_lst <- function(lst, use.names = TRUE, fill = FALSE, idcol = NULL) {

  cond_as <- function(x) {
    if (is.list(x)) x else data.table::as.data.table(x)
  }

  dt_rbl <- function(x, use.names, fill, idcol) {
    data.table::rbindlist(lapply(x, cond_as), use.names, fill, idcol)
  }

  rename <- function(x, new) {

    if (is_ts_tbl(x) && is_ts_meta(new)) {
      fun <- function(y) c(id(y), index(y))
    } else {
      fun <- function(y) id(y)
    }

    rename_cols(x, fun(new), fun(x))
  }

  id_tbl <- lgl_ply(lst, is_id_tbl)
  ts_tbl <- lgl_ply(lst, is_ts_tbl)

  if (any(id_tbl)) {

    meta <- tbl_meta(lst[[which(id_tbl)[1L]]])

  } else if (any(ts_tbl)) {

    meta <- tbl_meta(lst[[which(ts_tbl)[1L]]])

    assert_that(
      all(lgl_ply(lapply(lst[ts_tbl], interval), all.equal, interval(meta))),
      msg = "cannot mix interval types when row-binding"
    )

  } else {

    meta <- NULL
  }

  if (!is.null(meta)) {

    icu_tbl <- lgl_ply(lst, is_icu_tbl)
    old_met <- lapply(lst[icu_tbl], tbl_meta)

    lst[icu_tbl] <- lapply(lst[icu_tbl], rename, meta)

    on.exit(Map(rename, lst[icu_tbl], old_met))
  }

  res <- dt_rbl(lst, use.names, fill, idcol)

  if (is.null(meta)) {
    res
  } else {
    reclass_tbl(res, meta)
  }

}
