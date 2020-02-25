
new_ts_meta <- function(x, subclass) {

  res <- structure(x, class = c(subclass, "ts_meta"))

  assert_that(is_valid(res))

  res
}

#' @export
validate.ts_meta <- function(x, ...) {
  paste0(
    "No `validate()` method specific to class `", class(x)[1L], "` found."
  )
}

#' @export
new_ts_index <- function(index, interval) {
  new_ts_meta(list(list(meta_col = index, aux_data = interval)), "ts_index")
}

#' @export
validate.ts_index <- function(x, tbl = NULL, ...) {

  index <- index(x)
  interval <- interval(x)

  assert_that(
    is.string(index), not_na(index),
    is_time(interval, allow_neg = FALSE), not_na(interval), is.scalar(interval)
  )

  if (is.null(tbl)) return(TRUE)

  time_col <- tbl[[index]]

  validate_that(
    has_time_col(tbl, index),
    identical(index, last_elem(data.table::key(tbl))),
    all_zero(
      as.double(time_col) %% as.double(interval, units = units(time_col))
    )
  )
}

#' @export
new_ts_key <- function(key) {
  new_ts_meta(Map(list, meta_col = as.list(key)), "ts_key")
}

#' @export
validate.ts_key <- function(x, tbl = NULL, ...) {

  key <- key(x)

  assert_that(
    is.character(key), length(key) > 0L, no_na(key), is_unique(key)
  )

  if (is.null(tbl)) return(TRUE)

  validate_that(
    has_cols(tbl, key),
    identical(key, head(data.table::key(tbl), n = length(key)))
  )
}

#' @export
new_ts_unit <- function(val_cols, unit_cols = NA_character_,
                        unit_vals = NA_character_) {

  new_ts_meta(Map(list, meta_col = as.list(val_cols), aux_col = unit_cols,
                        aux_data = unit_vals),
              "ts_unit")
}

#' @export
validate.ts_unit <- function(x, tbl = NULL, ...) {

  val_cols  <- extract_strings(x, "meta_col")
  unit_cols <- extract_strings(x, "aux_col")
  unit_vals <- extract_strings(x, "aux_data")

  assert_that(
    no_na(val_cols), is_unique(val_cols),
    is_unique(unit_cols, incomparables = NA),
    same_length(val_cols, unit_cols), same_length(val_cols, unit_vals),
    xor_na(unit_cols, unit_vals), is_disjoint(val_cols, unit_cols)
  )

  if (is.null(tbl)) return(TRUE)

  all_cols <- c(val_cols, unit_cols[!is.na(unit_cols)])

  validate_that(has_cols(tbl, all_cols))
}

#' @export
is_ts_meta <- function(x) inherits(x, "ts_meta")

#' @export
is_ts_index <- function(x) inherits(x, "ts_index")

#' @export
is_ts_key <- function(x) inherits(x, "ts_key")

#' @export
is_ts_unit <- function(x) inherits(x, "ts_unit")

#' @export
meta_names.ts_meta <- function(x) vapply(x, `[[`, character(1L), "meta_col")

`meta_names<-` <- function(x, value) {

  assert_that(is_ts_meta(x), !is_ts_def(x))

  res <- Map(function(x, value) `[[<-`(x, "meta_col", value), x, value)

  class(res) <- class(x)

  res
}

#' @export
is_required.ts_meta <- function(x, ...) FALSE

#' @export
is_required.ts_index <- function(x, ...) TRUE

#' @export
is_required.ts_key <- function(x, ...) TRUE

#' @export
has_aux_names.ts_meta <- function(x) !is.null(x[[1L]][["aux_col"]])

# TODO: write selection fun for aux_naes/aux_data & replacement funs

#' @export
aux_names.ts_meta <- function(x, meta_col = NULL, allow_multiple = FALSE,
                              ...) {

  if (!has_aux_names(x)) {
    res <- rep(NA_character_, length(x))
  } else {
    res <- vapply(x, `[[`, character(1L), "aux_col")
  }

  nms <- meta_names(x)

  if (is.null(meta_col)) {

    stats::setNames(res, nms)

  } else {

    assert_that(is.string(meta_col), is.flag(allow_multiple))

    hits <- meta_col == nms

    if (sum(hits) == 0L) {
      NULL
    } else if (sum(hits) == 1L) {
      res[[hits]]
    } else if (allow_multiple) {
      res[hits]
    } else {
      stop("If `!allow_multiple` a single entry is allowed per column.")
    }
  }
}

`aux_names<-` <- function(x, value) {

  assert_that(is_ts_meta(x), !is_ts_def(x), has_aux_names(x))

  res <- Map(function(x, value) `[[<-`(x, "aux_col", value), x, value)
  class(res) <- class(x)

  res
}

#' @export
has_aux_data.ts_meta <- function(x) !is.null(x[[1L]][["aux_data"]])

#' @export
aux_data.ts_meta <- function(x, meta_col = NULL, allow_multiple = FALSE, ...) {

  if (!has_aux_data(x)) {
    res <- rep(list(NULL), length(x))
  } else {
    res <- lapply(x, `[[`, "aux_data")
  }

  nms <- meta_names(x)

  if (is.null(meta_col)) {

    stats::setNames(res, nms)

  } else {

    assert_that(is.string(meta_col), is.flag(allow_multiple))

    hits <- meta_col == nms

    if (sum(hits) == 0L) {
      NULL
    } else if (sum(hits) == 1L) {
      res[[hits]]
    } else if (allow_multiple) {
      res[hits]
    } else {
      stop("If `!allow_multiple` a single entry is allowed per column.")
    }
  }
}

`aux_data<-` <- function(x, value) {

  assert_that(is_ts_meta(x), !is_ts_def(x), has_aux_data(x))

  Map(function(x, value) `[[<-`(x, "aux_data", value), x, value)
}

#' @export
rm_cols.ts_index <- function(x, cols, ...) {

  if (meta_names(x) %in% cols) {
    stop("Cannot remove the only column that defines an index.")
  }

  x
}

#' @export
rm_cols.ts_key <- function(x, cols, ...) {
  new_ts_key(setdiff(meta_names(x), cols))
}

#' @export
rm_cols.ts_meta <- function(x, cols, ...) {

  hits <- meta_names(x) %in% cols

  if (has_aux_names(x)) {
    hits <- hits | (aux_names(x) %in% cols)
  }

  if (all(hits)) return(NULL)
  else structure(x[!hits], class = class(x))
}

#' @export
rename_cols.ts_meta <- function(x, new, old, ...) {

  meta_names(x) <- replace_with(meta_names(x), old, new)

  if (has_aux_names(x)) {
    aux_names(x) <- replace_with(aux_names(x), old, new)
  }

  x
}

#' @export
index.ts_index <- meta_names

#' @export
key.ts_key <- meta_names

#' @export
interval.ts_index <- function(x) aux_data(x)[[1L]]
