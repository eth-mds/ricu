
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
#' Upon instantiation, the data might be rearranged: columns are reordered
#' such that ID columns are moved to the front, followed by the index column
#' and a [data.table::key()] is set on meta columns, causing rows to be sorted
#' accordingly. Moving meta columns to the front is done for reasons of
#' convenience for printing, while setting a key on meta columns is done to
#' improve efficiency of subsequent transformations such as merging or grouped
#' operations. Furthermore, `NA` values in either ID or index columns are not
#' allowed and therefore corresponding rows are silently removed.
#'
#' Coercion between `id_tbl` and `ts_tbl` by default keeps intersecting
#' attributes fixed and new attributes are by default inferred as for class
#' instantiation. Each class comes with a class-specific implementation of the
#' S3 generic function `validate_tbl()` which returns `TRUE` if the object is
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
#' @section Relationship to `data.table`:
#' Both `id_tbl` and `ts_tbl` inherit from `data.table` and as such, functions
#' intended for use with `data.table` objects can be applied to `id_tbl` and
#' `ts_tbl` as well. But there are some caveats: Many functions introduced by
#' `data.table` are not S3 generic and therefore they would have to be masked
#' in order to retain control over how they operate on objects inheriting form
#' `data.table`. Take for example the function [data.table::setnames()], which
#' changes column names by reference. Using this function, the name of an
#' index column of an `id_tbl` object can me changed without updating the
#' attribute marking the column as such and thusly leaving the object in an
#' inconsistent state. Instead of masking the function `setnames()`, an
#' alternative is provided as [rename_cols()]. In places where it is possible
#' to seamlessly insert the appropriate function (such as
#' [base::names<-()] or [base::colnames<-()]) and the responsibility for not
#' using [data.table::setnames()] in a way that breaks the `id_tbl` object is
#' left to the user.
#'
#' Owing to `data.table` heritage, one of the functions that is often called
#' on `id_tbl` and `ts_tbl` objects is base S3 generic [base::`[`()]. As this
#' function is capable of modifying the object in a way that makes it
#' incompatible with attached meta data, an attempt is made at preserving as
#' much as possible and if all fails, a `data.table` object is returned
#' instead of an object inheriting form `id_tbl`. If for example the index
#' column is removed (or modified in a way that makes it incompatible with the
#' interval specification) from a `ts_tbl`, an `id_tbl` is returned. If
#' however the ID column is removed the only sensible thing to return is a
#' `data.table` (see examples).
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
#' tbl <- id_tbl(a = 1:10, b = rnorm(10))
#' names(tbl) <- c("c", "b")
#' tbl
#'
#' tbl <- id_tbl(a = 1:10, b = rnorm(10))
#' validate_tbl(data.table::setnames(tbl, c("c", "b")))
#'
#' tbl <- id_tbl(a = 1:10, b = rnorm(10))
#' validate_tbl(rename_cols(tbl, c("c", "b")))
#'
#' tbl <- ts_tbl(a = rep(1:2, each = 5), b = hours(rep(1:5, 2)), c = rnorm(10))
#' tbl[, c("a", "c"), with = FALSE]
#' tbl[, c("b", "c"), with = FALSE]
#' tbl[, list(a, b = as.double(b), c)]
#'
#' @param ... forwarded to [data.table::data.table()] or generic consistency
#' @param id_vars Column name(s) to be used as `id` column(s)
#'
#' @rdname id_tbl
#' @export
id_tbl <- function(..., id_vars = 1L) {
  as_id_tbl(list(...), id_vars, by_ref = TRUE)
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
as_id_tbl.id_tbl <- function(x, id_vars = NULL, ...) {
  as_id_tbl.data.table(x, coalesce(id_vars, id_vars(x)), ...)
}

#' @method as_id_tbl data.table
#' @export
as_id_tbl.data.table <- function(x, id_vars = NULL, by_ref = FALSE) {
  new_id_tbl(x, id_vars, by_ref = by_ref)
}

#' @export
as_id_tbl.default <- function(x, id_vars = NULL, by_ref = FALSE) {

  if (isTRUE(by_ref)) {
    setDT(x)
  } else {
    x <- as.data.table(x)
  }

  as_id_tbl(x, id_vars = id_vars, by_ref = by_ref)
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
  as_ts_tbl(list(...), id_vars, index_var, interval, by_ref = TRUE)
}

#' @rdname id_tbl
#' @export
is_ts_tbl <- is_type("ts_tbl")

#' @rdname id_tbl
#' @export
as_ts_tbl <- function(x, id_vars = NULL, index_var = NULL,
                      interval = NULL, by_ref = FALSE) {

  UseMethod("as_ts_tbl", x)
}

#' @export
as_ts_tbl.ts_tbl <- function(x, id_vars = NULL, index_var = NULL,
                             interval = NULL, ...) {

  as_ts_tbl.id_tbl(x, id_vars, coalesce(index_var, index_var(x)),
                   coalesce(interval, interval(x)), ...)
}

#' @export
as_ts_tbl.id_tbl <- function(x, id_vars = NULL, ...) {
  as_ts_tbl.data.table(x, coalesce(id_vars, id_vars(x)), ...)
}

#' @method as_ts_tbl data.table
#' @export
as_ts_tbl.data.table <- function(x, id_vars = NULL, index_var = NULL,
                                 interval = NULL, by_ref = FALSE) {

  new_ts_tbl(x, id_vars, index_var, interval, by_ref = by_ref)
}

#' @export
as_ts_tbl.default <- function(x, id_vars = NULL, index_var = NULL,
                              interval = NULL, by_ref = FALSE) {

  if (isTRUE(by_ref)) {
    x <- setDT(x)
  } else {
    x <- as.data.table(x)
  }

  as_ts_tbl(x, id_vars = id_vars, index_var = index_var, interval = interval,
            by_ref = by_ref)
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

  if (isTRUE(by_ref)) {
    ptyp <- as_ptype(x)
  } else {
    x <- copy(x)
  }

  # dots need evaluating before stripping of class in case of by-ref operation

  dots <- list(...)

  x <- unclass_tbl(x)

  attrs <- c(dots, list(class = c(class, class(x))))

  x <- do.call(set_attributes, c(list(x), attrs))

  chk <- validate_tbl(x)

  if (isTRUE(chk)) {

    mcols <- meta_vars(x)
    nrows <- nrow(x)

    x <- rm_na(x, mcols, "any")
    x <- sort(x, by = mcols, by_ref = TRUE)
    x <- setcolorder(x, c(mcols, setdiff(colnames(x), mcols)))

    return(x)
  }

  if (isTRUE(by_ref)) {
    reclass_tbl(x, ptyp, TRUE)
  }

  stop_ricu(chk, class = c("valid_unclass_fail", attr(chk, "assert_class")))
}

set_attributes <- function(x, ...) {

  dot <- list(...)
  nms <- names(dot)

  assert_that(is.list(dot), has_length(dot), not_null(nms), is_unique(nms))

  Map(setattr, list(x), nms, dot)

  x
}

strip_class <- function(x, what = class(x)[1L]) setdiff(class(x), what)

#' Internal utilities for ICU data objects
#'
#' In order to remove all `id_tbl`/`ts_tbl`-related attributes, as well as
#' extra class-labels, the exported but marked internal function
#' `unclass_tbl()` can be used. This function provides what one might expect
#' from an `id_tbl`/`ts_tbl`-specific implementation of the S3 generic
#' function [data.table::as.data.table()]. The inverse functionality if
#' provided by `reclass_tbl()` which attempts to add attributes as seen in
#' `template` to the object passed as `x`. The logical flag `stop_on_fail`
#' controls how to proceed if the attributes of `template` are incompatible
#' with the object `x`. Finally, in order to generate a template, `as_ptype()`
#' creates an empty object with the appropriate attributes.
#'
#' @param x Object to modify/query
#'
#' @rdname tbl_internal
#' @keywords internal
#' @export
unclass_tbl <- function(x) UseMethod("unclass_tbl", x)

#' @export
unclass_tbl.data.frame <- function(x) x

#' @export
unclass_tbl.ts_tbl <- function(x) {
  unclass_tbl(
    set_attributes(x, index_var = NULL, interval = NULL,
                   class = strip_class(x))
  )
}

#' @export
unclass_tbl.id_tbl <- function(x) {
  set_attributes(x, id_vars = NULL, class = strip_class(x))
}

#' @param template Object after which to model the object in question
#' @param stop_on_fail Logical flag indicating whether to consider failed
#' object validation as error
#'
#' @rdname tbl_internal
#' @keywords internal
#' @export
reclass_tbl <- function(x, template, stop_on_fail = TRUE) {
  UseMethod("reclass_tbl", template)
}

#' @export
reclass_tbl.NULL <- function(x, template, stop_on_fail = TRUE) x

#' @export
reclass_tbl.id_tbl <- function(x, template, stop_on_fail = TRUE) {

  x <- NextMethod(object = template)

  check_valid(set_attributes(x, id_vars = id_vars(template), class = .Class),
              stop_on_fail)
}

#' @export
reclass_tbl.ts_tbl <- function(x, template, stop_on_fail = TRUE) {

  x <- NextMethod(object = template)

  x <- set_attributes(x, index_var = index_var(template),
                      interval = interval(template), class = .Class)

  if (isTRUE(validate_tbl(x))) {
    return(x)
  }

  NextMethod(object = template)
}

#' @method reclass_tbl data.table
#' @export
reclass_tbl.data.table <- function(x, template, stop_on_fail = TRUE) {
  check_valid(unclass_tbl(x), stop_on_fail)
}

try_reclass <- function(x, template) {
  reclass_tbl(x, template, stop_on_fail = FALSE)
}

#' @rdname tbl_internal
#' @keywords internal
#' @export
as_ptype <- function(x) {
  reclass_tbl(as.data.table(lapply(x, `[`, 0L)[meta_vars(x)]), x)
}

#' @rdname id_tbl
#' @export
validate_tbl <- function(x) {

  res <- validate_that(
    is_dt(x), is_unique(colnames(x))
  )

  if (!isTRUE(res)) {
    return(res)
  }

  UseMethod("validate_tbl", x)
}

#' @export
validate_tbl.id_tbl <- function(x) {

  idv <- id_vars(x)
  res <- validate_that(has_cols(x, idv), is_unique(idv))

  if (isTRUE(res)) NextMethod() else res
}

#' @export
validate_tbl.ts_tbl <- function(x) {

  index <- index_col(x)
  inval <- interval(x)
  invar <- index_var(x)

  res <- validate_that(
    is.string(invar), has_cols(x, invar), is_disjoint(id_vars(x), invar),
    obeys_interval(index, inval), same_unit(index, inval)
  )

  if (isTRUE(res)) NextMethod() else res
}

#' @method validate_tbl data.table
#' @export
validate_tbl.data.table <- function(x) TRUE

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
    set_attributes(x, id_vars = unname(new_id_vars))
  )
}
