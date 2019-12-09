
#' @export
new_ts_def <- function(x) {

  assert_that(is.list(x))

  null <- vapply(x, is.null, logical(1L))

  if(any(null)) x <- x[!null]

  assert_that(all_fun(x, is_ts_meta), all_fun(x, Negate(is_ts_def)))

  classes <- lapply(x, class)

  assert_that(length(classes) == length(unique(classes)))

  structure(x, class = c("ts_def", "ts_meta"))
}

#' @export
validate.ts_def <- function(x, tbl = NULL, ...) {

  res <- lapply(x, validate, tbl, ...)
  ok <- vapply(res, isTRUE, logical(1L))

  if(all(ok)) {
    TRUE
  } else {
    paste0("Validation errors:\n  -", paste(res[!ok], collapse = "\n  -"))
  }
}

#' @export
is_ts_def <- function(x) inherits(x, "ts_def")

#' @export
as_ts_def.list <- function(x) new_ts_def(x)

#' @export
as_ts_def.ts_def <- function(x) x

#' @export
as_ts_def.ts_meta <- function(x) new_ts_def(list(x))

#' @export
ts_meta.ts_def <- function(x, class, ...) {
  hits <- vapply(x, inherits, logical(1L), class)
  if (sum(hits) == 0L) NULL else x[[which(hits)]]
}

#' @export
meta_names.ts_def <- function(x) {

  res <- lapply(x, meta_names)

  names(res) <- vapply(x, format_class, character(1L))

  res
}

#' @export
has_aux_names.ts_def <- function(x) vapply(x, has_aux_names, logical(1L))

#' @export
aux_names.ts_def <- function(x, class = NULL, ...) {

  if (is.null(class)) {

    res <- lapply(x, aux_names, ...)
    names(res) <- vapply(x, format_class, character(1L))
    res

  } else {

    meta <- ts_meta(x, class)
    if (is.null(meta)) return(NULL)
    aux_names(meta, ...)
  }
}

#' @export
has_aux_data.ts_def <- function(x) vapply(x, has_aux_data, logical(1L))

#' @export
aux_data.ts_def <- function(x, class = NULL, ...) {

  if (is.null(class)) {

    res <- lapply(x, aux_data, ...)
    names(res) <- vapply(x, format_class, character(1L))
    res

  } else {

    meta <- ts_meta(x, class)
    if (is.null(meta)) return(NULL)
    aux_data(meta, ...)
  }
}

#' @export
index.ts_def <- function(x) index(ts_meta(x, "ts_index"))

#' @export
key.ts_def <- function(x) key(ts_meta(x, "ts_key"))

#' @export
interval.ts_def <- function(x) interval(ts_meta(x, "ts_index"))
