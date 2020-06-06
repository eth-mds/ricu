
#' Tabular ICU data
#'
#' Two objects central to `ricu`, `id_tbl` and `ts_tbl` (both inheriting
#' from `id_tbl`) are used to represent tabular data.
#'
#' @param ... forwarded to [data.table::data.table()] or generic consistency
#' @param id_vars Column name(s) to be used as `id` column(s)
#'
#' @rdname id_tbl
#' @export
id_tbl <- function(..., id_vars = 1L) {
  as_id_tbl(data.table(...), id_vars, by_ref = TRUE)
}

#' @param x Object to query/operate on
#'
#' @rdname id_tbl
#' @export
is_id_tbl <- function(x) inherits(x, "id_tbl")

#' @param by_ref Logical flag indicating whether to perform the operation by
#' reference
#'
#' @rdname id_tbl
#' @export
as_id_tbl <- function(x, id_vars = NULL, by_ref = FALSE) {
  UseMethod("as_id_tbl", x)
}

#' @rdname id_tbl
#' @export
as_id_tbl.ts_tbl <- function(x, id_vars = NULL, by_ref = FALSE) {

  if (is.null(id_vars)) {
    id_vars <- id_vars(x)
  }

  NextMethod()
}

#' @rdname id_tbl
#' @export
as_id_tbl.id_tbl <- function(x, id_vars = NULL, by_ref = FALSE) {

  if (is.null(id_vars)) {
    return(x)
  }

  NextMethod()
}

#' @rdname id_tbl
#' @method as_id_tbl data.table
#' @export
as_id_tbl.data.table <- function(x, id_vars = NULL, by_ref = FALSE) {

  validate_unclass(
    new_id_tbl(x, id_vars, by_ref = by_ref), x, by_ref
  )
}

#' @rdname id_tbl
#' @export
as_id_tbl.default <- function(x, id_vars = NULL, by_ref = FALSE) {

  if (by_ref) {
    setDT(x)
  } else {
    x <- as.data.table(x)
  }

  as_id_tbl(x, id_vars = id_vars, by_ref = TRUE)
}

new_id_tbl <- function(x, id_vars, ..., class = character()) {

  if (is.null(id_vars)) {
    if (data.table::haskey(x)) {
      id_vars <- data.table::key(x)
    } else {
      id_vars <- 1L
    }
  }

  if (is.numeric(id_vars) || is.logical(id_vars)) {
    id_vars <- colnames(x)[id_vars]
  }

  assert_that(is.character(id_vars), has_length(id_vars))

  new_tbl(x, id_vars = id_vars, ..., class = c(class, "id_tbl"))
}

#' @param index_var Column name of the index column
#' @param interval Time series interval length specified as scalar-valued
#' `difftime` object
#'
#' @rdname id_tbl
#' @export
ts_tbl <- function(..., id_vars = 1L, index_var = NULL, interval = NULL) {
  as_ts_tbl(data.table(...), id_vars, index_var, interval, by_ref = TRUE)
}

#' @rdname id_tbl
#' @export
is_ts_tbl <- function(x) inherits(x, "ts_tbl")

#' @rdname id_tbl
#' @export
as_ts_tbl <- function(x, id_vars = NULL, index_var = NULL, interval = NULL,
                      by_ref = FALSE) {

  UseMethod("as_ts_tbl", x)
}

#' @rdname id_tbl
#' @export
as_ts_tbl.ts_tbl <- function(x, id_vars = NULL, index_var = NULL,
                             interval = NULL, by_ref = FALSE) {

  repl_if_null <- function(x, how) if (is.null(x)) how else x

  if (is.null(id_vars) && is.null(index_var) && is.null(interval)) {
    return(x)
  }

  id_vars   <- repl_if_null(id_vars,   id_vars(x))
  index_var <- repl_if_null(index_var, index_var(x))
  interval  <- repl_if_null(interval,  interval(x))

  NextMethod()
}

#' @rdname id_tbl
#' @export
as_ts_tbl.id_tbl <- function(x, id_vars = NULL, index_var = NULL,
                             interval = NULL, by_ref = FALSE) {

  if (is.null(id_vars)) {
    id_vars <- id_vars(x)
  }

  NextMethod()
}

#' @rdname id_tbl
#' @method as_ts_tbl data.table
#' @export
as_ts_tbl.data.table <- function(x, id_vars = NULL, index_var = NULL,
                                 interval = NULL, by_ref = FALSE) {

  validate_unclass(
    new_ts_tbl(x, id_vars, index_var, interval, by_ref = by_ref), x, by_ref
  )
}

#' @rdname id_tbl
#' @export
as_ts_tbl.default <- function(x, id_vars = NULL, index_var = NULL,
                              interval = NULL, by_ref = FALSE) {

  if (by_ref) {
    x <- setDT(x)
  } else {
    x <- as.data.table(x)
  }

  as_ts_tbl(x, id_vars = id_vars, index_var = index_var, interval = interval,
            by_ref = TRUE)
}

new_ts_tbl <- function(x, id_vars, index_var = NULL, interval = NULL,
                       ..., class = character()) {

  if (is.null(index_var)) {

    index_var <- time_vars(x)

    assert_that(length(index_var) == 1L,
      msg = paste("In order to automatically determine the index column,",
                  "exactly one `difftime` column is required.")
    )

  } else if (is.numeric(index_var) || is.logical(index_var)) {

    index_var <- colnames(x)[index_var]
  }

  if (is.null(interval)) {
    assert_that(has_time_col(x, index_var))
    interval <- interval(x[[index_var]])
  }

  new_id_tbl(x, id_vars, index_var = index_var, interval = interval, ...,
             class = c(class, "ts_tbl"))
}

new_tbl <- function(x, ..., class, by_ref = TRUE) {

  assert_that(is.list(x), is.flag(by_ref))

  if (!by_ref) {
    x <- copy(x)
  }

  if (is_id_tbl(x)) {
    x <- unclass_tbl(x)
  }

  attrs <- c(list(...), list(class = c(class, class(x))))

  x <- set_attributes(x, attrs)

  cols <- unique(meta_vars(x))

  if (has_name(x, cols)) {

    x <- na.omit(x, cols)

    x <- setkeyv(x, cols)
    x <- setcolorder(x, c(cols, setdiff(colnames(x), cols)))
  }

  x
}

set_attributes <- function(x, attributes) {

  nms <- names(attributes)

  assert_that(is.list(attributes), has_length(attributes),
              not_null(nms), is_unique(nms))

  Map(setattr, list(x), nms, attributes)

  x
}

strip_class <- function(x, what) setdiff(class(x), what)

#' @rdname id_tbl
#' @export
unclass_tbl <- function(x) UseMethod("unclass_tbl", x)

#' @rdname id_tbl
#' @export
unclass_tbl.ts_tbl <- function(x) {
  unclass_tbl(
    set_attributes(x, list(index_var = NULL, interval = NULL,
                           class = strip_class(x, "ts_tbl")))
  )
}

#' @rdname id_tbl
#' @export
unclass_tbl.id_tbl <- function(x) {
  set_attributes(x, list(id_vars = NULL, class = strip_class(x, "id_tbl")))
}

#' @param template Object after which to model the object in question
#' @param stop_on_fail Logical flag indicating whether to consider failed
#' object validation as error
#'
#' @rdname id_tbl
#' @export
reclass_tbl <- function(x, template, stop_on_fail = TRUE)
  UseMethod("reclass_tbl", template)

#' @rdname id_tbl
#' @export
reclass_tbl.NULL <- function(x, template, stop_on_fail = TRUE) x

#' @rdname id_tbl
#' @export
reclass_tbl.id_tbl <- function(x, template, stop_on_fail = TRUE) {

  id_nms <- id_vars(template)

  check_valid(new_id_tbl(x, id_nms), stop_on_fail)
}

#' @rdname id_tbl
#' @export
reclass_tbl.ts_tbl <- function(x, template, stop_on_fail = TRUE) {

  id_nms <- id_vars(template)
  idx_nm <- index_var(template)
  intval <- interval(template)

  res <- new_ts_tbl(x, id_nms, idx_nm, intval)

  if (isTRUE(validate_tbl(res))) {
    return(res)
  }

  check_valid(new_id_tbl(x, id_nms), stop_on_fail)
}

try_reclass <- function(x, template) {
  reclass_tbl(x, template, stop_on_fail = FALSE)
}

#' @rdname id_tbl
#' @export
as_ptype <- function(x) UseMethod("as_ptype", x)

#' @rdname id_tbl
#' @export
as_ptype.id_tbl <- function(x) {
  new_id_tbl(list(), id_vars(x))
}

#' @rdname id_tbl
#' @export
as_ptype.ts_tbl <- function(x) {
  new_ts_tbl(list(), id_vars(x), index_var(x), interval(x))
}

#' @rdname id_tbl
#' @export
validate_tbl <- function(x) UseMethod("validate_tbl", x)

#' @rdname id_tbl
#' @export
validate_tbl.id_tbl <- function(x) {

  res <- validate_that(has_length(id_vars(x)))

  if (isTRUE(res)) NextMethod() else res
}

#' @rdname id_tbl
#' @export
validate_tbl.ts_tbl <- function(x) {

  index <- index_col(x)
  inval <- interval(x)

  res <- validate_that(
    is_disjoint(id_vars(x), index_var(x)),
    is_time_vec(index, allow_neg = TRUE), is_time(inval, allow_neg = FALSE),
    has_interval(index, inval)
  )

  if (isTRUE(res)) NextMethod() else res
}

#' @rdname id_tbl
#' @method validate_tbl data.table
#' @export
validate_tbl.data.table <- function(x) {
  validate_that(has_cols(x, meta_vars(x)), is_unique(colnames(x)))
}

validate_unclass <- function(new, old, by_ref) {

  chk <- validate_tbl(new)

  if (isTRUE(chk)) {
    return(new)
  }

  if (by_ref) {
    unclass_tbl(old)
  }

  stop(chk, call. = FALSE)
}

check_valid <- function(x, stop_on_fail = TRUE) {

  res <- validate_tbl(x)

  if (isTRUE(res)) {
    x
  } else if (isTRUE(stop_on_fail)) {
    stop(res, call. = FALSE)
  } else {
    unclass_tbl(x)
  }
}

set_id_vars <- function(x, new_id_vars) {

  assert_that(is_id_tbl(x))

  if (is.numeric(new_id_vars) || is.logical(new_id_vars)) {
    new_id_vars <- colnames(x)[new_id_vars]
  }

  assert_that(is.character(new_id_vars), has_length(new_id_vars))

  check_valid(
    set_attributes(x, list(id_vars = new_id_vars))
  )
}
