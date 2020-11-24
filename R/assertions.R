
#' @importFrom assertthat see_if on_failure<- validate_that
#' @importFrom assertthat is.string is.flag is.dir is.count
#' @importFrom assertthat has_name has_attr are_equal is.number
#' @importFrom rlang as_label
NULL

assert_that <- function(..., env = parent.frame(), msg = NULL) {

  res <- see_if(..., env = env)

  if (isTRUE(res)) {
    return(TRUE)
  }

  if (is.null(msg)) {
    msg <- attr(res, "msg")
  } else {
    msg <- fmt_msg(msg, envir = env)
  }

  cls <- c(attr(msg, "assert_class"), "assertError", "ricu_err")

  rlang::abort(msg, class = cls)
}

fail_type <- function(arg_name, class) {

  assertthat::assert_that(is.string(arg_name), is.string(class))

  function(call, env) {
    msg <- paste0("{as_label(call$", arg_name, ")} is not a `", class,
                  "` object")
    format_assert(msg, paste0("is_", class, "_assert"))
  }
}

is_type <- function(type) {

  res <- function(x) inherits(x, type)

  on_failure(res) <- fail_type("x", type)

  res
}

is_scalar <- function(x) is.atomic(x) && length(x) == 1L

on_failure(is_scalar) <- function(call, env) {
  format_assert("{as_label(call$x)} is not a scalar", "is_scalar_assert")
}

is_number <- function(x) is_scalar(x) && is.numeric(x)

on_failure(is_number) <- function(call, env) {
  format_assert("{as_label(call$x)} is not a scalar number",
                "is_number_assert")
}

is_intish <- function(x) {
  is.integer(x) || (is.numeric(x) && all(x == trunc(x)) && !is.na(x))
}

on_failure(is_intish) <- function(call, env) {
  format_assert("{as_label(call$x)} integer-values", "is_intish_assert")
}

has_length <- function(x) length(x) > 0L

on_failure(has_length) <- function(call, env) {
  format_assert("{as_label(call$x)} has zero length", "has_length_assert")
}

has_rows <- function(x) nrow(x) > 0L

on_failure(has_rows) <- function(call, env) {
  format_assert("{as_label(call$x)} has zero rows", "has_rows_assert")
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
    format_assert(
      c("None of the following were found among the provided options.
         Did you possibly mean:",
         bullet(concat("'", sug, "'"), " instead of '", names(sug), "'")),
      "are_in_assert", exdent = c(0L, rep_along(2L, sug)))
  }
}

on_failure(are_in) <- in_failure

is_in <- function(x, opts, na_rm = FALSE) {
  assert_that(is.string(x)) && are_in(x, opts, na_rm)
}

on_failure(is_in) <- in_failure

has_col <- function(x, cols) has_cols(x, cols, 1L)

has_cols <- function(x, cols, length = NA) {
  if (is.na(length)) {
    len_check <- assert_that(has_length(cols))
  } else {
    len_check <- assert_that(is.count(length), all_equal(length(cols), length))
  }
  assert_that(is.character(cols), is_unique(cols)) && len_check &&
    length(setdiff(cols, colnames(x))) == 0L
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

is_interval <- function(x) {
  assert_that(is_difftime(x), has_length(x)) && all(x >= 0)
}

on_failure(is_interval) <- function(call, env) {
  format_assert(
    "Not all of {as_label(call$x)} represent positive time intervals",
    "is_interval_assert"
  )
}

has_time_cols <- function(x, cols, length = NA) {
  assert_that(has_cols(x, cols, length)) && all(col_ply(x, cols, is_difftime))
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

all_map <- function(fun, ...) {
  assert_that(is.function(fun)) && all(lgl_ply(map(fun, ...), isTRUE))
}

on_failure(all_map) <- function(call, env) {
  format_assert(
    "some invocations of `{as_label(call$fun)}` do not evaluate to `TRUE`",
    "all_map_assert"
  )
}

all_null <- function(x) all_fun(x, is.null)

on_failure(all_null) <- function(call, env) {
  format_assert("some of {as_label(call$x)} are not `NULL`", "all_null_assert")
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
  is.function(x) || (is.string(x) &&
    is.function(tryCatch(eval(parse(text = x)), error = function(e) NULL))
  )
}

on_failure(evals_to_fun) <- function(call, env) {
  format_assert(
    "{as_label(call$x)} is neither a function nor evaluates to a function",
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
