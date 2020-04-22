
#' Table meta data
#'
#' Tabular ICU data as used by `ricu` frequently contains a column that defines
#' a grouping structure (e.g. patient IDs, ICU stay IDs, etc.) and a column
#' holding time information.
#'
#' @param index Column name of the index column
#' @param interval Minimal time interval between rows
#'
#' @rdname tbl_meta
#' @keywords internal
#' @export
#'
new_tbl_index <- function(index, interval) {

  assert_that(
    is.string(index), not_na(index),
    is_time(interval, allow_neg = FALSE), not_na(interval), is.scalar(interval)
  )

  structure(list(col_name = unname(index), interval = interval),
            class = "tbl_index")
}

#' @param id Column name of the ID column
#' @param opts Alternative ID systems
#'
#' @rdname tbl_meta
#' @keywords internal
#' @export
#'
new_tbl_id <- function(id, opts = NULL) {

  assert_that(is.string(id), not_na(id))

  if (!is.null(opts)) {

    assert_that(is.character(opts), length(opts) > 0L, is_unique(opts))

    if (!is.null(names(opts))) {
      assert_that(length(intersect(opts, names(opts))) == 0L)
    }

    if (!id %in% opts) {

      warning("id `", id, "` is incompatible with id options ",
              paste0("`", opts, "`", collapse = ", "))

      opts <- NULL
    }
  }

  structure(list(col_name = unname(id), id_opts = opts), class = "tbl_id")
}

#' @param x Object to query/coerce
#'
#' @rdname tbl_meta
#' @keywords internal
#' @export
#'
is_tbl_index <- function(x) inherits(x, "tbl_index")

#' @rdname tbl_meta
#' @keywords internal
#' @export
#'
is_tbl_id <- function(x) inherits(x, "tbl_id")

#' @rdname tbl_meta
#' @keywords internal
#' @export
#'
tbl_index.tbl_index <- function(x) x

#' @rdname tbl_meta
#' @keywords internal
#' @export
#'
tbl_id.tbl_id <- function(x) x

#' @rdname tbl_utils
#' @export
#'
rename_cols.tbl_index <- function(x, new, old, ...) {

  hit <- index(x) == old

  if (any(hit)) {
    assert_that(sum(hit) == 1L)
    x <- new_tbl_index(new[hit], interval(x))
  }

  x
}

#' @rdname tbl_utils
#' @export
#'
rename_cols.tbl_id <- function(x, new, old, ...)  {

  old_id <- id(x)
  id_hit <- old_id == old

  old_opt <- id_opts(x)
  opt_hit <- match(old, old_opt)

  if (any(id_hit)) {
    assert_that(sum(id_hit) == 1L)
    new_id <- new[id_hit]
  } else {
    new_id <- old_id
  }

  no_hit <- is.na(opt_hit)

  if (!all(no_hit)) {
    new_opt <- replace(old_opt, opt_hit[!no_hit], new[!no_hit])
  } else {
    new_opt <- old_opt
  }

  new_tbl_id(new_id, new_opt)
}

#'
#'
#' @rdname meta_utils
#' @export
#'
index.tbl_index <- function(x) x[["col_name"]]

#' @rdname meta_utils
#' @export
#'
interval.tbl_index <- function(x) x[["interval"]]

#' @rdname meta_utils
#' @export
#'
time_unit.tbl_index <- function(x) units(interval(x))

#' @rdname meta_utils
#' @export
#'
id.tbl_id <- function(x) x[["col_name"]]

#' @rdname meta_utils
#' @export
#'
id_opts.tbl_id <- function(x) x[["id_opts"]]

#' @rdname meta_utils
#' @export
#'
set_id.tbl_id <- function(x, value) new_tbl_id(value, id_opts(x))

#' @rdname meta_utils
#' @export
#'
set_id_opts.tbl_id <- function(x, value) new_tbl_id(id(x), value)

#' @rdname meta_utils
#' @export
#'
set_index.tbl_index <- function(x, value) new_tbl_index(value, interval(x))

#' @rdname meta_utils
#' @export
#'
set_interval.tbl_index <- function(x, value) new_tbl_index(index(x), value)

#' @rdname meta_utils
#' @export
#'
set_time_unit.tbl_index <- function(x, value) {
  new_tbl_index(index(x), `units<-`(interval(x), value))
}

#' @export
print.tbl_id <- function(x, ...) cat_line(format(x, ...))

#' @export
print.tbl_index <- function(x, ...) cat_line(format(x, ...))

#' @export
format.tbl_id <- function(x, fancy = l10n_info()$`UTF-8`, ...) {

  code <- function(hits, code) {
    ifelse(hits, if (fancy) paste0("\u001b[", code, "m") else "*", "")
  }

  opts <- id_opts(x)

  if (is.null(opts)) {
    opts <- ""
  } else {
    if (is.null(names(opts))) {
      opts <- paste0("`", opts, "`")
    } else {
      mark <- id(x) == opts
      opts <- paste0(code(mark, 4), names(opts), code(mark, 24))
    }
    opts <- paste0(" (", paste(opts, collapse = " < "), ")")
  }

  paste0("`", id(x), "`", opts)
}

#' @export
format.tbl_index <- function(x, ...) {
  paste0("`", index(x), "` (", format(interval(x)), ")")
}
