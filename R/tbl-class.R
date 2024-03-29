#' Tabular ICU data classes
#'
#' In order to simplify handling or tabular ICU data, `ricu` provides
#' S3 classes, `id_tbl`, `ts_tbl`, and `win_tbl`. These classes essentially
#' consist of a `data.table` object, alongside some meta data and S3 dispatch
#' is used to enable more natural behavior for some data manipulation tasks.
#' For example, when merging two tables, a default for the `by` argument can
#' be chosen more sensibly if columns representing patient ID and timestamp
#' information can be identified.
#'
#' @details
#' The introduced classes are designed for several often encountered data
#' scenarios:
#'
#' * `id_tbl` objects can be used to represent static (with respect to
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
#' * `win_tbl`: In addition to representing grouped time-series data as does
#'   a `ts_tbl`, `win_tbl` objects also encode a validity interval for each
#'   time-stamped measurement (as [dur_var][dur_var()]). This can for example
#'   be useful when a drug is administered at a certain infusion rate for a
#'   given time period.
#'
#' Owing to the nested structure of required meta data, `ts_tbl` inherits from
#' `id_tbl` and `win_tbl` from `ts_tbl`. Furthermore, both classes inherit from
#' `data.table`. As such, `data.table` [reference semantics][data.table::set()]
#' are available for some operations, indicated by presence of a `by_ref`
#' argument. At default, value, `by_ref` is set to `FALSE` as this is in line
#' with base R behavior at the cost of potentially incurring unnecessary data
#' copies. Some care has to be taken when passing `by_ref = TRUE` and enabling
#' by reference operations as this can have side effects (see examples).
#'
#' For instantiating `ts_tbl` objects, both `index_var` and `interval` can be
#' automatically determined if not specified. For the index column, the only
#' requirement is that a single [`difftime`][base::difftime()] column is
#' present, while for the time step, the minimal difference between two
#' consecutive observations is chosen (and all differences are therefore
#' required to be multiples of the minimum difference). Similarly, for a
#' `win_tbl`, exactly two [`difftime`][base::difftime()] columns are required
#' where the first is assumed to be corresponding to the `index_var` and the
#' second to the `dur_var`.
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
#' Coercion between `id_tbl` and `ts_tbl` (and `win_tbl`) by default keeps
#' intersecting attributes fixed and new attributes are by default inferred as
#' for class instantiation. Each class comes with a class-specific
#' implementation of the S3 generic function `validate_tbl()` which returns
#' `TRUE` if the object is considered valid or a string outlining the type of
#' validation failure that was encountered. Validity requires
#'
#' 1. inheriting from `data.table` and unique column names
#' 1. for `id_tbl` that all columns specified by the non-zero length character
#'    vector holding onto the `id_vars` specification are available
#' 1. for `ts_tbl` that the string-valued `index_var` column is available and
#'    does not intersect with `id_vars` and that the index column obeys the
#'    specified interval.
#' 1. for `win_tbl` that the string-valued `dur_var` corresponds to a
#'    `difftime` vector and is not among the columns marked as index or ID
#'    variables
#'
#' Finally, inheritance can be checked by calling `is_id_tbl()` and
#' `is_ts_tbl()`. Note that due to `ts_tbl` inheriting from `id_tbl`,
#' `is_id_tbl()` returns `TRUE` for both `id_tbl` and `ts_tbl` objects (and
#' similarly for `win_tbl`), while `is_ts_tbl()` only returns `TRUE` for
#' `ts_tbl` objects.
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
#' @return Constructors `id_tbl()`/`ts_tbl()`/`win_tbl()`, as well as coercion
#' functions `as_id_tbl()`/`as_ts_tbl()`/`as_win_tbl()` return
#' `id_tbl`/`ts_tbl`/`win_tbl` objects respectively,
#' while inheritance testers `is_id_tbl()`/`is_ts_tbl()`/`is_win_tbl()` return
#' logical flags and `validate_tbl()` returns either `TRUE` or a string
#' describing the validation failure.
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
as_ts_tbl <- function(x, id_vars = NULL, index_var = NULL, interval = NULL,
                      by_ref = FALSE) {

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

#' @param dur_var Column name of the duration column
#'
#' @rdname id_tbl
#' @export
win_tbl <- function(..., id_vars = NULL, index_var = NULL, interval = NULL,
                    dur_var = NULL) {
  as_win_tbl(list(...), id_vars, index_var, interval, dur_var, by_ref = TRUE)
}

#' @rdname id_tbl
#' @export
is_win_tbl <- is_type("win_tbl")

#' @rdname id_tbl
#' @export
as_win_tbl <- function(x, id_vars = NULL, index_var = NULL, interval = NULL,
                       dur_var = NULL, by_ref = FALSE) {

  UseMethod("as_win_tbl", x)
}

#' @export
as_win_tbl.win_tbl <- function(x, id_vars = NULL, index_var = NULL,
                               interval = NULL, dur_var = NULL, ...) {

  as_win_tbl.ts_tbl(x, id_vars, index_var, interval,
                    coalesce(dur_var, dur_var(x)), ...)
}

#' @export
as_win_tbl.ts_tbl <- function(x, id_vars = NULL, index_var = NULL,
                             interval = NULL, ...) {

  as_win_tbl.id_tbl(x, id_vars, coalesce(index_var, index_var(x)),
                   coalesce(interval, interval(x)), ...)
}

#' @export
as_win_tbl.id_tbl <- function(x, id_vars = NULL, ...) {
  as_win_tbl.data.table(x, coalesce(id_vars, id_vars(x)), ...)
}

#' @method as_win_tbl data.table
#' @export
as_win_tbl.data.table <- function(x, ...) new_win_tbl(x, ...)

#' @export
as_win_tbl.default <- function(x, ..., by_ref = FALSE) {

  if (isTRUE(by_ref)) {
    x <- setDT(x)
  } else {
    x <- as.data.table(x)
  }

  as_win_tbl(x, ..., by_ref = by_ref)
}

new_win_tbl <- function(x, id_vars = NULL, index_var = NULL, interval = NULL,
                        dur_var = NULL, ..., class = character()) {

  if (is.null(index_var) || is.null(dur_var)) {
    opts <- time_vars(x)
  }

  if (is.null(index_var) && is.null(dur_var)) {

    assert_that(length(opts) == 2L, msg = "In order to automatically determine
      both the index and duration columns, exactly two `difftime` columns are
      required.")

    index_var <- opts[1L]
    dur_var <- opts[2L]

  } else if (is.null(index_var)) {

    index_var <- setdiff(opts, dur_var)

    assert_that(length(index_var) == 1L, msg = "In order to automatically
      determine the index column, exactly one `difftime` column apart from the
      duration column is required.")

  } else if (is.null(dur_var)) {

    dur_var <- setdiff(opts, index_var)

    assert_that(length(dur_var) == 1L, msg = "In order to automatically
      determine the duration column, exactly one `difftime` column apart from
      the index column is required.")
  }

  new_ts_tbl(x, id_vars, index_var, interval, dur_var = dur_var, ...,
             class = c(class, "win_tbl"))
}

new_ts_tbl <- function(x, id_vars = NULL, index_var = NULL, interval = NULL,
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
    interval <- as.difftime(NA_real_, units = units(x[[index_var]]))
  }

  res <- new_id_tbl(x, id_vars, index_var = unname(index_var),
                    interval = interval, ..., class = c(class, "ts_tbl"))

  if (is.na(interval)) {
    res <- set_attributes(res, interval = interval(index_col(res)))
  }

  res
}

new_id_tbl <- function(x, id_vars = NULL, ..., class = character()) {

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

    return(x)
  }

  if (isTRUE(by_ref)) {
    reclass_tbl(x, ptyp)
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

#' @method as.data.table id_tbl
#' @inheritParams data.table::as.data.table
#' @rdname id_tbl
#' @export
as.data.table.id_tbl <- function(x, keep.rownames = FALSE, by_ref = FALSE,
                                 ...) {

  warn_dots(...)

  if (!isFALSE(keep.rownames)) {
    warn_arg("keep.rownames")
  }

  if (!by_ref) {
    x <- copy(x)
  }

  unclass_tbl(x)
}

#' @method as.data.frame id_tbl
#' @inheritParams base::as.data.frame
#' @rdname id_tbl
#' @export
as.data.frame.id_tbl <- function(x, row.names = NULL, optional = FALSE, ...) {

  if (!is.null(row.names)) {
    warn_arg("row.names")
  }

  if (!isFALSE(optional)) {
    warn_arg("optional")
  }

  x <- as.data.table(x, ...)
  x <- setDF(x)

  x
}

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
#' @return
#' * `unclass_tbl()`: a `data.table`
#' * `reclass_tbl()`: either an `id_tbl` or a `ts_tbl` depending on the type
#'    of the object passed as `template`
#' * `as_ptype()`: an object of the same type as `x`, but with on data
#'
#' @rdname tbl_internal
#' @keywords internal
#' @export
unclass_tbl <- function(x) UseMethod("unclass_tbl", x)

#' @export
unclass_tbl.data.frame <- function(x) x

#' @export
unclass_tbl.win_tbl <- function(x) {
  unclass_tbl(set_attributes(x, dur_var = NULL, class = strip_class(x)))
}

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

#' @export
unclass_tbl.default <- function(x) stop_generic(x, .Generic)

#' @param template Object after which to model the object in question
#' @param stop_on_fail Logical flag indicating whether to consider failed
#' object validation as error
#'
#' @rdname tbl_internal
#' @keywords internal
#' @export
reclass_tbl <- function(x, template, stop_on_fail = TRUE) {

  template <- force(template)

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

#' @export
reclass_tbl.win_tbl <- function(x, template, stop_on_fail = TRUE) {

  x <- NextMethod(object = template)

  x <- set_attributes(x, dur_var = dur_var(template), class = .Class)

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

#' @method reclass_tbl data.frame
#' @export
reclass_tbl.data.frame <- function(x, template, stop_on_fail = TRUE) {
  check_valid(setDF(unclass_tbl(x)), stop_on_fail)
}

#' @export
reclass_tbl.default <- function(x, template, ...) {
  stop_generic(template, .Generic)
}

try_reclass <- function(x, template) {

  if (is_df(x)) {
    reclass_tbl(x, template, stop_on_fail = FALSE)
  } else {
    x
  }
}

#' @rdname tbl_internal
#' @keywords internal
#' @export
as_ptype <- function(x) UseMethod("as_ptype", x)

#' @export
as_ptype.id_tbl <- function(x) {
  reclass_tbl(as.data.table(lapply(x, `[`, 0L)[meta_vars(x)]), x)
}

#' @method as_ptype data.table
#' @export
as_ptype.data.table <- function(x) data.table()

#' @method as_ptype data.frame
#' @export
as_ptype.data.frame <- function(x) data.frame()

#' @export
as_ptype.default <- function(x) stop_generic(x, .Generic)

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

#' @export
validate_tbl.win_tbl <- function(x) {

  dvar <- dur_var(x)

  res <- validate_that(
    is.string(dvar), has_cols(x, dvar), is_disjoint(id_vars(x), dvar),
    is_disjoint(dvar, index_var(x)), is_difftime(dur_col(x))
  )

  if (isTRUE(res)) NextMethod() else res
}

#' @method validate_tbl data.frame
#' @export
validate_tbl.data.frame <- function(x) TRUE

#' @export
validate_tbl.default <- function(x) stop_generic(x, .Generic)

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

  res <- set_attributes(x, id_vars = unname(new_id_vars))
  res <- sort(res, by_ref = TRUE)

  check_valid(res)
}
