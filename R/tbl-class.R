
#' Tabular ICU data classes
#'
#' In order to simplify handling or tabular ICU data, `ricu` provides two
#' S3 classes, `id_tbl` and `ts_tbl`. The two classes essentially
#' consist of a `data.table` object, alongside some meta data and S3 dispatch
#' is used to enable more natural behavior for some data manipulation tasks.
#' For example, when merging two tables, a default for the `by` argument can
#' be chosen more sensibly if columns representing patient ID and timestamp
#' information can be identified.
#'
#' @details
#' The two classes are designed for two often encountered data scenarios:
#'
#' * `id_tbl` objects can be used to represent static (with repspect to
#'   relevant time scales) patient data such as patient age and such an object
#'   is simply a `data.table` combined with a non-zero length character vector
#'   valued attribute marking the columns tracking patient ID information
#'   ([`id_vars`][id_vars()]). All further columns are considered as
#'   [data_vars][data_vars()].
#' * `ts_tbl` objects are used for grouped time series data. A `data.table`
#'   object again is augmented by attributes, including a non-zero length
#'   character vector identifying patient ID columns ([id_vars][id_vars()]),
#'   a string, tracking the column holding time-stamps
#'   ([index_var][index_var()]) and a scalar `difftime` object determining
#'   the time-series step size [interval][interval()]. Again, all further
#'   columns are treated as [data_vars][data_vars()].
#'
#' Owing to the nested structure of required meta data, `ts_tbl` inherits from
#' `id_tbl`. Furthermore, both classes inherit from `data.table`. As such,
#' `data.table` [reference semantics][data.table::set()] are available for
#' some operations, indicated by presence of a `by_ref` argument. At default,
#' value, `by_ref` is set to `FALSE` as this is in line with base R behavior
#' at the cost of potentially incurring unnecessary data copies. Some care has
#' to be taken when passing `by_ref = TRUE` and enabling by reference
#' operations as this can have side effects (see examples).
#'
#' For instantiating `ts_tbl` objects, both `index_var` and `interval` can be
#' automatically determined if not specified. For the index column, the only
#' requirement is that a single [`difftime`][base::difftime()] column is
#' present, while for the time step, the minimal difference between two
#' consecutive observations is chosen (and all differences are therefore
#' required to be multiples of the minimum difference).
#'
#' Coercion between `id_tbl` and `ts_tbl` by default keeps intersecting
#' attributes fixed and new attributes are by default inferred as for class
#' instantiation. Each class comes with a class-specific implementation of the
#' S3 generic function `validate_tbl()` which returns `TRUE` if the ibject is
#' considered valid or a string outlining the type of validation failure that
#' was encountered. Validity requires
#'
#' 1. inheriting from `data.table` and unique column names
#' 1. for `id_tbl` that all columns specified by the non-zero length character
#'    vector holding onto the `id_vars` specification are available
#' 1. for `ts_tbl` that the string-valued `index_var` column is available and
#'    does not intersect with `id_vars` and that the index column obeys the
#'    specified interval.
#'
#' Finally, inheritance can be checked by calling `is_id_tbl()` and
#' `is_ts_tbl()`. Note that due to `ts_tbl` inheriting from `id_tbl`,
#' `is_id_tbl()` returns `TRUE` for both `id_tbl` and `ts_tbl` objects, while
#' `is_ts_tbl()` only returns `TRUE` for `ts_tbl` objects.
#'
#' @examples
#' tbl <- id_tbl(a = 1:10, b = rnorm(10))
#' is_id_tbl(tbl)
#' is_ts_tbl(tbl)
#'
#' dat <- data.frame(a = 1:10, b = hours(1:10), c = rnorm(10))
#' tbl <- as_ts_tbl(dat, "a")
#' is_id_tbl(tbl)
#' is_ts_tbl(tbl)
#'
#' tmp <- as_id_tbl(tbl)
#' is_ts_tbl(tbl)
#' is_ts_tbl(tmp)
#'
#' tmp <- as_id_tbl(tbl, by_ref = TRUE)
#' is_ts_tbl(tbl)
#' is_ts_tbl(tmp)
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
is_id_tbl <- is_type("id_tbl")

#' @param by_ref Logical flag indicating whether to perform the operation by
#' reference
#'
#' @rdname id_tbl
#' @export
as_id_tbl <- function(x, id_vars = NULL, by_ref = FALSE) {
  UseMethod("as_id_tbl", x)
}

#' @export
as_id_tbl.ts_tbl <- function(x, id_vars = NULL, by_ref = FALSE) {

  id_vars <- coalesce(id_vars, id_vars(x))

  as_id_tbl.id_tbl(x, id_vars, by_ref)
}

#' @export
as_id_tbl.id_tbl <- function(x, id_vars = NULL, by_ref = FALSE) {

  if (is.null(id_vars)) {
    return(x)
  }

  as_id_tbl.data.table(x, id_vars, by_ref)
}

#' @method as_id_tbl data.table
#' @export
as_id_tbl.data.table <- function(x, id_vars = NULL, by_ref = FALSE) {

  validate_unclass(
    new_id_tbl(x, id_vars, by_ref = by_ref), x, by_ref
  )
}

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

  new_tbl(x, id_vars = unname(id_vars), ..., class = c(class, "id_tbl"))
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
is_ts_tbl <- is_type("ts_tbl")

#' @rdname id_tbl
#' @export
as_ts_tbl <- function(x, id_vars = NULL, index_var = NULL, interval = NULL,
                      by_ref = FALSE) {

  UseMethod("as_ts_tbl", x)
}

#' @export
as_ts_tbl.ts_tbl <- function(x, id_vars = NULL, index_var = NULL,
                             interval = NULL, by_ref = FALSE) {

  if (is.null(id_vars) && is.null(index_var) && is.null(interval)) {
    return(x)
  }

  id_vars   <- coalesce(id_vars,   id_vars(x))
  index_var <- coalesce(index_var, index_var(x))
  interval  <- coalesce(interval,  interval(x))

  as_ts_tbl.id_tbl(x, id_vars, index_var, interval, by_ref)
}

#' @export
as_ts_tbl.id_tbl <- function(x, id_vars = NULL, index_var = NULL,
                             interval = NULL, by_ref = FALSE) {

  id_vars <- coalesce(id_vars, id_vars(x))

  as_ts_tbl.data.table(x, id_vars, index_var, interval, by_ref)
}

#' @method as_ts_tbl data.table
#' @export
as_ts_tbl.data.table <- function(x, id_vars = NULL, index_var = NULL,
                                 interval = NULL, by_ref = FALSE) {

  validate_unclass(
    new_ts_tbl(x, id_vars, index_var, interval, by_ref = by_ref), x, by_ref
  )
}

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
    assert_that(is.string(index_var), has_time_cols(x, index_var))
    interval <- interval(x[[index_var]])
  }

  new_id_tbl(x, id_vars, index_var = unname(index_var), interval = interval,
             ..., class = c(class, "ts_tbl"))
}

new_tbl <- function(x, ..., class, by_ref = TRUE) {

  assert_that(is.list(x), is.flag(by_ref), is_unique(names(x)))

  if (!by_ref) {
    x <- copy(x)
  }

  # dots need evaluating before stripping of class in case of by-ref operation

  dots <- list(...)

  if (is_id_tbl(x)) {
    x <- unclass_tbl(x)
  }

  attrs <- c(dots, list(class = c(class, class(x))))

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

#' ICU class data utilities
#'
#' Utilities for working with `id_tbl` and `ts_tbl` objects.
#'
#' @param x Object to modify/query
#'
#' @rdname tbl_utils
#' @export
unclass_tbl <- function(x) UseMethod("unclass_tbl", x)

#' @export
unclass_tbl.data.frame <- function(x) x

#' @export
unclass_tbl.ts_tbl <- function(x) {
  unclass_tbl(
    set_attributes(x, list(index_var = NULL, interval = NULL,
                           class = strip_class(x, "ts_tbl")))
  )
}

#' @export
unclass_tbl.id_tbl <- function(x) {
  set_attributes(x, list(id_vars = NULL, class = strip_class(x, "id_tbl")))
}

#' @param template Object after which to model the object in question
#' @param stop_on_fail Logical flag indicating whether to consider failed
#' object validation as error
#'
#' @rdname tbl_utils
#' @export
reclass_tbl <- function(x, template, stop_on_fail = TRUE)
  UseMethod("reclass_tbl", template)

#' @export
reclass_tbl.NULL <- function(x, template, stop_on_fail = TRUE) x

#' @export
reclass_tbl.id_tbl <- function(x, template, stop_on_fail = TRUE) {

  id_nms <- id_vars(template)

  check_valid(new_id_tbl(x, id_nms), stop_on_fail)
}

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

#' @rdname tbl_utils
#' @export
as_ptype <- function(x) UseMethod("as_ptype", x)

#' @export
as_ptype.id_tbl <- function(x) {
  new_id_tbl(list(), id_vars(x))
}

#' @export
as_ptype.ts_tbl <- function(x) {
  new_ts_tbl(list(), id_vars(x), index_var(x), interval(x))
}

#' @rdname id_tbl
#' @export
validate_tbl <- function(x) {

  res <- validate_that(is_dt(x), is_unique(colnames(x)))

  if (!isTRUE(res)) {
    return(res)
  }

  UseMethod("validate_tbl", x)
}

#' @export
validate_tbl.id_tbl <- function(x) {

  res <- validate_that(has_cols(x, id_vars(x)))

  if (isTRUE(res)) NextMethod() else res
}

#' @export
validate_tbl.ts_tbl <- function(x) {

  index <- index_col(x)
  inval <- interval(x)
  invar <- index_var(x)

  res <- validate_that(
    is.string(invar), has_cols(x, invar), is_disjoint(id_vars(x), invar),
    obeys_interval(index, inval)
  )

  if (isTRUE(res)) NextMethod() else res
}

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

  stop_ricu(chk, class = c("valid_unclass_fail", attr(chk, "assert_class")))
}

check_valid <- function(x, stop_on_fail = TRUE) {

  res <- validate_tbl(x)

  if (isTRUE(res)) {
    x
  } else if (isTRUE(stop_on_fail)) {
    stop_ricu(res, class = c("valid_check_fail", attr(res, "assert_class")))
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
