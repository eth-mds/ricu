
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
  setattr(x, "class", unique(c(tbl_class(meta), "icu_tbl", class(x))))
}

unclass_tbl <- function(x) {

  setattr(x, "tbl_meta", NULL)
  setattr(x, "class", setdiff(class(x), c("id_tbl", "ts_tbl", "icu_tbl")))

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
make_compatible <- function(x, def, id = TRUE, index = TRUE) {

  if (is.null(def)) return(x)

  def <- tbl_meta(def)

  assert_that(is_icu_tbl(x), is_tbl_meta(def))

  if (id && !setequal(id(x), id(def))) {
    assert_that(same_length(id(x), id(def)))
    x <- rename_cols(x, id(def), id(x))
  }

  do_ind <- index && is_ts_tbl(x)

  if (do_ind && !setequal(index(x), index(def))) {
    assert_that(same_length(id(x), id(def)))
    x <- rename_cols(x, index(def), index(x))
  }

  if (do_ind && !identical(time_unit(x), time_unit(def))) {
    x <- set_time_unit(x, time_unit(def))
  }

  if (do_ind && !all.equal(interval(x), interval(def))) {
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
  meta <- tbl_meta(lst[[first]])

  lst[ts_tbls] <- lapply(lst[ts_tbls], make_compatible, meta)

  res <- dt_rbl(lst, use.names, fill, idcol)

  reclass_tbl(res, meta)
}
