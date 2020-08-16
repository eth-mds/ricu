
#' @importFrom assertthat see_if on_failure<- validate_that
#' @importFrom assertthat is.string is.flag is.dir is.count
#' @importFrom assertthat has_name has_attr are_equal is.number
#' @importFrom rlang as_label
NULL

fail_type <- function(arg_name, class) {

  assertthat::assert_that(is.string(arg_name), is.string(class))

  function(call, env) {
    format_assert(
      paste0("{as_label(call$", arg_name, ")} is not a `{class}` object"),
      paste0("is_", class, "_assert")
    )
  }
}

is_scalar <- function(x) is.atomic(x) && length(x) == 1L

on_failure(is_scalar) <- function(call, env) {
  format_assert("{as_label(call$x)} is not a scalar", "is_scalar_assert")
}

has_length <- function(x) length(x) > 0L

on_failure(has_length) <- function(call, env) {
  format_assert("{as_label(call$x)} has zero length", "has_length_assert")
}

are_in <- function(x, opts, na_rm = FALSE) {
  assert_that(
    has_length(x), has_length(opts), is.character(x), is.character(opts)
  ) && all(x %in% opts, na.rm = na_rm)
}

in_failure <- function(call, env) {

  x    <- eval(call$x, env)
  opts <- eval(call$opts, env)
  sug  <- suggest(x[!x %in% opts], opts)

  if (length(sug) == 1L) {
    format_assert(
      "{names(sug)} was not found among the provided options. Did you possibly
       mean {sug[[1L]]} instead?", "are_in_assert"
     )
  } else {
    format_assert({
      cli_text("None of the following were found among the provided options.
                Did you possibly mean:")
      cli_ul(paste0(concat("'", sug, "'"), " instead of '", names(sug), "'"))
    }, "are_in_assert")
  }
}

on_failure(are_in) <- in_failure

is_in <- function(x, opts, na_rm = FALSE) {
  assert_that(is.string(x)) && are_in(x, opts, na_rm)
}

on_failure(is_in) <- in_failure

has_cols <- function(x, cols) {
  assert_that(
    is.character(cols), has_length(cols), is_unique(cols)
  ) && length(setdiff(cols, colnames(x))) == 0L
}

on_failure(has_cols) <- function(call, env) {

  cols <- setdiff(eval(call$cols, env), colnames(eval(call$x, env)))

  format_assert(
    "{as_label(call$x)} does not contain {qty(length(cols))} column{?s}
     {quote_bt(cols)}", "has_cols_assert"
  )
}

has_interval <- function(x, interval) {
  assert_that(is_ts_tbl(x), is_interval(interval)) &&
    same_time(interval(x), interval)
}

on_failure(has_interval) <- function(call, env) {

  ival <- eval(call$interval, env)

  format_assert(
    "{as_label(call$x)} is not on the time scale of {format(ival)}",
    "has_interval_assert"
  )
}

is_difftime <- function(x) inherits(x, "difftime")

on_failure(is_difftime) <- fail_type("x", "difftime")

is_interval <- function(x) {
  assert_that(is_difftime(x), has_length(x)) && all(x >= 0)
}

on_failure(is_interval) <- function(call, env) {
  format_assert(
    "Not all of {as_label(call$x)} represent positive time intervals",
    "is_interval_assert"
  )
}

has_time_cols <- function(x, cols) {
  assert_that(has_cols(x, cols)) && all(col_ply(x, cols, is_difftime))
}

on_failure(has_time_cols) <- function(call, env) {

  cols <- eval(call$cols, env)
  cols <- cols[col_ply(eval(call$x, env), cols, Negate(is_difftime))]

  format_assert(
    "{qty(length(cols))} Column{?s} {quote_bt(cols)} of {as_label(call$x)}
     {qty(length(cols))} {?does/do} not represent time intervals",
    "has_time_cols_assert"
  )
}

obeys_interval <- function(x, interval, na_rm = TRUE, tolerance = secs(1e-3)) {
  assert_that(
    is_difftime(x), is_scalar(interval), is_interval(interval),
    is_scalar(tolerance), is_interval(tolerance)
  ) && all(
    as.double(x) %% as.double(interval, units = units(x)) <
      as.double(tolerance, units = units(x)), na.rm = na_rm
  )
}

on_failure(obeys_interval) <- function(call, env) {

  ival <- eval(call$interval, env)

  format_assert(
    "{as_label(call$x)} is not compatible with an interval of
     {format(ival)}", "obeys_interval_assert"
  )
}

same_unit <- function(x, y) identical(units(x), units(y))

on_failure(same_unit) <- function(call, env) {
  format_assert(
    "{as_label(call$x)} and {as_label(call$y)} are not measured in the
     same unit", "same_unit_assert"
  )
}

same_time <- function(x, y, tolerance = secs(1e-3)) {
  assert_that(same_unit(x, y)) &&
    all(abs(as.double(x - y)) < as.double(tolerance, units = units(x)))
}

on_failure(same_time) <- function(call, env) {
  format_assert(
    "{as_label(call$x)} and {as_label(call$y)} are not on the same time scale",
    "same_time_assert"
  )
}

all_fun <- function(x, fun, ..., na_rm = FALSE) {
  assert_that(is.function(fun)) && all(lgl_ply(x, fun, ...), na.rm = na_rm)
}

on_failure(all_fun) <- function(call, env) {
  format_assert(
    "some of {as_label(call$x)} do not satisfy function
    `{as_label(call$fun)}`", "all_fun_assert"
  )
}

all_null <- function(x) all_fun(x, is.null)

on_failure(all_null) <- function(call, env) {
  format_assert("some of {as_label(call$x)} are not NULL", "all_null_assert")
}

same_length <- function(x, y) identical(length(x), length(y))

on_failure(same_length) <- function(call, env) {
  format_assert(
    "{as_label(call$x)} does not have the same length as {as_label(call$y)}",
    "same_length_assert"
  )
}

is_disjoint <- function(x, y) length(intersect(x, y)) == 0L

on_failure(is_disjoint) <- function(call, env) {
  format_assert(
    "{as_label(call$x)} and {as_label(call$y)} have a nonempty intersection",
    "is_disjoint_assert"
  )
}

not_null <- function(x) !is.null(x)

on_failure(not_null) <- function(call, env) {
  format_assert("{as_label(call$x)} is NULL", "not_null_assert")
}

null_or <- function(x, what, ...) {
  is.null(x) || what(x, ...)
}

on_failure(null_or) <- function(call, env) {
  format_assert(
    "{as_label(call$x)} is neither NULL, nor {as_label(call$what)}",
    "null_or_assert"
  )
}

evals_to_fun <- function(x) {
  assert_that(is.string(x)) &&
    is.function(tryCatch(eval(parse(text = x)), error = function(e) NULL))
}

on_failure(evals_to_fun) <- function(call, env) {
  format_assert(
    "{as_label(call$x)} does not evaluate to a function",
    "evals_fun_assert"
  )
}

all_equal <- function(x, y, ...) isTRUE(all.equal(x, y, ...))

on_failure(all_equal) <- function(call, env) {
  format_assert(
    "{as_label(call$x)} and {as_label(call$y)} are not equal",
    "all_equal_assert"
  )
}
