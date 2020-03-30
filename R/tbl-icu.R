
new_icu_tbl <- function(x, meta) {

  x <- set_meta(x, meta, stop_on_fail = TRUE)
  x <- set_class(x, meta)

  x
}

#' @export
is_icu_tbl <- function(x) inherits(x, "icu_tbl")

#' @export
tbl_meta.icu_tbl <- function(x) attr(x, "tbl_meta")

#' @export
rm_cols.icu_tbl <- function(x, cols, ...) {

  cols <- unique(intersect(cols, colnames(x)))

  assert_that(has_cols(x, cols))

  if (any(meta_cols(x) %in% cols)) {
    x <- unclass_tbl(x)
  }

  x <- set(x, j = cols, value = NULL)

  x
}

#' @export
rename_cols.icu_tbl <- function(x, new, old = colnames(x), skip_absent = FALSE,
                                ...) {

  assert_that(is.flag(skip_absent))

  if (skip_absent) {

    hits <- old %in% colnames(x)

    if (sum(hits) == 0L) return(x)

    new <- new[hits]
    old <- old[hits]
  }

  assert_that(is_unique(replace(colnames(x), match(old, colnames(x)), new)),
              has_cols(x, old))

  meta <- rename_cols(tbl_meta(x), new, old)
  x <- setnames(x, old, new, skip_absent)

  reclass_tbl(x, meta)
}

#' @export
id.icu_tbl <- function(x) id(tbl_meta(x))

#' @export
id_opts.icu_tbl <- function(x) id_opts(tbl_meta(x))

#' @export
set_id.icu_tbl <- function(x, value) set_meta(x, set_id(tbl_meta(x), value))

#' @export
set_id_opts.icu_tbl <- function(x, value) {
  set_meta(x, set_id_opts(tbl_meta(x), value))
}

#' @export
index.icu_tbl <- function(x) index(tbl_meta(x))

#' @export
units.icu_tbl <- function(x) {
  setNames(c(time_unit(x), data_unit(x)), c(time_col(x), data_cols(x)))
}

#' @export
is_unique.icu_tbl <- function(x, by = meta_cols(x), ...) {
  identical(anyDuplicated(x, by = by, ...), 0L)
}

tbl_class.icu_tbl <- function(x) tbl_class(tbl_meta(x))

#' @export
meta_cols.icu_tbl <- function(x) meta_cols(tbl_meta(x))
