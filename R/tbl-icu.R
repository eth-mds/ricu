
new_icu_tbl <- function(x, meta) {

  x <- set_meta(x, meta, stop_on_fail = TRUE)
  x <- set_class(x, meta)

  x
}

#' @rdname icu_tbl
#' @export
#'
is_icu_tbl <- function(x) inherits(x, "icu_tbl")

#' @rdname tbl_meta
#' @keywords internal
#' @export
#'
tbl_meta.icu_tbl <- function(x) attr(x, "tbl_meta")

#' @rdname tbl_meta
#' @keywords internal
#' @export
#'
tbl_id.icu_tbl <- function(x) tbl_id(tbl_meta(x))

#' Utilities for `icu_tbl` objects
#'
#' @param x Object to operate on
#' @param cols Column names of columns to remove
#' @param ... Generic consistency
#'
#' @rdname tbl_utils
#' @export
#'
rm_cols <- function(x, cols, ...) {

  if (length(intersect(cols, colnames(x))) == 0L) {
    return(x)
  }

  UseMethod("rm_cols", x)
}

#' @method rm_cols data.table
#' @rdname tbl_utils
#' @export
#'
rm_cols.data.table <- function(x, cols, ...) {
  x <- set(x, j = unique(intersect(cols, colnames(x))), value = NULL)
  x
}

#' @param new New column names
#' @param old Column names to be replaces by the ones specified as `new`
#'
#' @rdname tbl_utils
#' @export
#'
rename_cols <- function(x, new, old = colnames(x), ...) {

  assert_that(is_unique(new), is_unique(old), same_length(new, old))

  UseMethod("rename_cols", x)
}

#' @method rename_cols data.table
#' @rdname tbl_utils
#' @export
#'
rename_cols.data.table <- function(x, new, old = colnames(x), ...) {
  x <- data.table::setnames(x, old, new)
  x
}

#' @rdname tbl_utils
#' @export
#'
rm_cols.icu_tbl <- function(x, cols, ...) {

  cols <- unique(intersect(cols, colnames(x)))

  assert_that(has_cols(x, cols))

  if (any(meta_cols(x) %in% cols)) {
    x <- unclass_tbl(x)
  }

  x <- set(x, j = cols, value = NULL)

  x
}

#' @param skip_absent Silently ignore absent columns in `old` (along with
#' their corresponding replacements in `new`)
#'
#' @rdname tbl_utils
#' @export
#'
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

#' @rdname tbl_meta
#' @keywords internal
#' @export
#'
tbl_index <- function(x) UseMethod("tbl_index", x)

#' @rdname tbl_meta
#' @keywords internal
#' @export
#'
tbl_id <- function(x) UseMethod("tbl_id", x)

#' Utilities for querying and modifying table meta data
#'
#' @rdname meta_utils
#' @export
#'
index <- function(x) UseMethod("index", x)

#' @rdname meta_utils
#' @export
#'
id <- function(x) UseMethod("id", x)

#' @rdname meta_utils
#' @export
#'
id_opts <- function(x) UseMethod("id_opts", x)

#' @rdname meta_utils
#' @export
#'
interval <- function(x) UseMethod("interval", x)

#' @rdname meta_utils
#' @export
#'
time_unit <- function(x) UseMethod("time_unit", x)

#' @rdname meta_utils
#' @export
#'
meta_cols <- function(x) UseMethod("meta_cols", x)

#' @rdname meta_utils
#' @export
#'
set_index <- function(x, value) UseMethod("set_index", x)

#' @rdname meta_utils
#' @export
#'
set_interval <- function(x, value) {

  assert_that(is_time(value, allow_neg = FALSE))

  UseMethod("set_interval", x)
}

#' @rdname meta_utils
#' @export
#'
set_id <- function(x, value) UseMethod("set_id", x)

#' @rdname meta_utils
#' @export
#'
set_id_opts <- function(x, value) UseMethod("set_id_opts", x)

#' @rdname meta_utils
#' @export
#'
set_time_unit <- function(x, value) UseMethod("set_time_unit", x)

#' @rdname meta_utils
#' @export
#'
id.icu_tbl <- function(x) id(tbl_meta(x))

#' @rdname meta_utils
#' @export
#'
id_opts.icu_tbl <- function(x) id_opts(tbl_meta(x))

#' @rdname meta_utils
#' @export
#'
set_id.icu_tbl <- function(x, value) set_meta(x, set_id(tbl_meta(x), value))

#' @rdname meta_utils
#' @export
#'
set_id_opts.icu_tbl <- function(x, value) {
  set_meta(x, set_id_opts(tbl_meta(x), value))
}

#' @rdname meta_utils
#' @export
#'
index.icu_tbl <- function(x) index(tbl_meta(x))

#' @rdname meta_utils
#' @export
#'
units.icu_tbl <- function(x) {
  setNames(c(time_unit(x), data_unit(x)), c(time_col(x), data_cols(x)))
}

#' @rdname meta_utils
#' @export
#'
meta_cols.icu_tbl <- function(x) meta_cols(tbl_meta(x))

#' @rdname tbl_meta
#' @keywords internal
#' @export
#'
tbl_class <- function(x) UseMethod("tbl_class", x)

#' @rdname tbl_meta
#' @keywords internal
#' @export
#'
tbl_class.icu_tbl <- function(x) tbl_class(tbl_meta(x))

#' @rdname meta_utils
#' @export
#'
data_cols <- function(x) setdiff(colnames(x), meta_cols(x))

#' @rdname meta_utils
#' @export
#'
data_unit <- function(x) {

  get_unit <- function(col) {
    if (has_attr(x[[col]], "units")) attr(x[[col]], "units")
    else NA_character_
  }

  chr_ply(data_cols(x), get_unit, use_names = TRUE)
}

