#' ICU class meta data utilities
#'
#' The two data classes `id_tbl` and `ts_tbl`, used by `ricu` to represent ICU
#' patient data, consist of a `data.table` alongside some meta data. This
#' includes marking columns that have special meaning and for data
#' representing measurements ordered in time, the step size. The following
#' utility functions can be used to extract columns and column names with
#' special meaning, as well as query a `ts_tbl` object regarding its time
#' series related meta data.
#'
#' @details
#' The following functions can be used to query an object for columns or
#' column names that represent a distinct aspect of the data:
#'
#' * `id_vars()`: ID variables are one or more column names with the
#'   interaction of corresponding columns identifying a grouping of the data.
#'   Most commonly this is some sort of patient identifier.
#' * `id_var()`: This function either fails or returns a string and can
#'   therefore be used in case only a single column provides grouping
#'   information.
#' * `id_col()`: Again, in case only a single column provides grouping
#'   information, this column can be extracted using this function.
#' * `index_var()`: Suitable for use as index variable is a column that encodes
#'   a temporal ordering of observations as [`difftime`][base::difftime()]
#'   vector. Only a single column can be marked as index variable and this
#'   function queries a `ts_tbl` object for its name.
#' * `index_col()`: similarly to `id_col()`, this function extracts the column
#'   with the given designation. As a `ts_tbl` object is required to have
#'   exactly one column marked as index, this function always returns for
#'   `ts_tbl` objects (and fails for `id_tbl` objects).
#' * `dur_var()`: For `win_tbl` objects, this returns the name of the column
#'   encoding the data validity interval.
#' * `dur_col()`: Similarly to `index_col()`, this returns the `difftime`
#'   vector corresponding to the `dur_var()`.
#' * `meta_vars()`: For `ts_tbl` objects, meta variables represent the union
#'   of ID and index variables (for `win_tbl`, this also includes the
#'   `dur_var()`), while for `id_tbl` objects meta variables consist pf ID
#'   variables.
#' * `data_vars()`: Data variables on the other hand are all columns that are
#'   not meta variables.
#' * `data_var()`: Similarly to `id_var()`, this function either returns the
#'   name of a single data variable or fails.
#' * `data_col()`: Building on `data_var()`, in situations where only a single
#'   data variable is present, it is returned or if multiple data column
#'   exists, an error is thrown.
#' * `time_vars()`: Time variables are all columns in an object inheriting
#'   from [`data.frame`][base::data.frame()] that are of type
#'   [`difftime`][base::difftime()]. Therefore in a `ts_tbl` object the index
#'   column is one of (potentially) several time variables. For a `win_tbl`,
#'   however the `dur_var()` is not among the `time_vars()`.
#' * `interval()`: The time series interval length is represented a scalar
#'   valued [`difftime`][base::difftime()] object.
#' * `time_unit()`: The time unit of the time series interval, represented by
#'   a string such as "hours" or "mins" (see [`difftime`][base::difftime()]).
#' * `time_step()`: The time series step size represented by a numeric value
#'   in the unit as returned by `time_unit()`.
#'
#' @param x Object to query
#'
#' @return Mostly column names as character vectors, in case of `id_var()`,
#' `index_var()`, `data_var()` and `time_unit()` of length 1, else of variable
#' length. Functions `id_col()`, `index_col()` and `data_col()` return table
#' columns as vectors, while `interval()` returns a scalar valued `difftime`
#' object and `time_step()` a number.
#'
#' @examples
#' tbl <- id_tbl(a = rep(1:2, each = 5), b = rep(1:5, 2), c = rnorm(10),
#'               id_vars = c("a", "b"))
#'
#' id_vars(tbl)
#' tryCatch(id_col(tbl), error = function(...) "no luck")
#' data_vars(tbl)
#' data_col(tbl)
#'
#' tmp <- as_id_tbl(tbl, id_vars = "a")
#' id_vars(tmp)
#' id_col(tmp)
#'
#' tbl <- ts_tbl(a = rep(1:2, each = 5), b = hours(rep(1:5, 2)), c = rnorm(10))
#' index_var(tbl)
#' index_col(tbl)
#'
#' identical(index_var(tbl), time_vars(tbl))
#'
#' interval(tbl)
#' time_unit(tbl)
#' time_step(tbl)
#'
#' @rdname tbl_meta
#' @export
id_vars <- function(x) UseMethod("id_vars", x)

#' @export
id_vars.id_tbl <- function(x) attr(x, "id_vars")

#' @export
id_vars.default <- function(x) stop_generic(x, .Generic)

#' @rdname tbl_meta
#' @export
id_var <- function(x) {
  res <- id_vars(x)
  assert_that(is.string(res))
  res
}

#' @rdname tbl_meta
#' @export
id_col <- function(x) x[[id_var(x)]]

#' @rdname tbl_meta
#' @export
index_var <- function(x) UseMethod("index_var", x)

#' @export
index_var.ts_tbl <- function(x) attr(x, "index_var")

#' @export
index_var.default <- function(x) stop_generic(x, .Generic)

#' @rdname tbl_meta
#' @export
index_col <- function(x) x[[index_var(x)]]

#' @rdname tbl_meta
#' @export
dur_var <- function(x) UseMethod("dur_var", x)

#' @export
dur_var.win_tbl <- function(x) attr(x, "dur_var")

#' @export
dur_var.default <- function(x) stop_generic(x, .Generic)

#' @rdname tbl_meta
#' @export
dur_col <- function(x) x[[dur_var(x)]]

#' @rdname tbl_meta
#' @export
dur_unit <- function(x) units(dur_col(x))

#' @rdname tbl_meta
#' @export
meta_vars <- function(x) UseMethod("meta_vars", x)

#' @export
meta_vars.id_tbl <- function(x) id_vars(x)

#' @export
meta_vars.ts_tbl <- function(x) c(id_vars(x), index_var(x))

#' @export
meta_vars.win_tbl <- function(x) c(id_vars(x), index_var(x), dur_var(x))

#' @export
meta_vars.default <- function(x) stop_generic(x, .Generic)

#' @rdname tbl_meta
#' @export
data_vars <- function(x) setdiff(colnames(x), meta_vars(x))

#' @rdname tbl_meta
#' @export
data_var <- function(x) {
  res <- data_vars(x)
  assert_that(is.string(res))
  res
}

#' @rdname tbl_meta
#' @export
data_col <- function(x) x[[data_var(x)]]

#' @rdname tbl_meta
#' @export
interval <- function(x) UseMethod("interval", x)

#' @export
interval.ts_tbl <- function(x) attr(x, "interval")

#' @export
interval.default <- function(x) stop_generic(x, .Generic)

#' @export
interval.difftime <- function(x) {

  dif <- diff(x)
  res <- min(dif[dif > 0], na.rm = TRUE)

  assert_that(obeys_interval(x, res))

  res
}

#' @rdname tbl_meta
#' @export
time_unit <- function(x) units(interval(x))

#' @rdname tbl_meta
#' @export
time_step <- function(x) as.double(interval(x))

#' @rdname tbl_meta
#' @export
time_vars <- function(x) UseMethod("time_vars", x)

#' @method time_vars data.frame
#' @export
time_vars.data.frame <- function(x) colnames(x)[lgl_ply(x, is_difftime)]

#' @export
time_vars.win_tbl <- function(x) setdiff(NextMethod(), dur_var(x))

#' @export
time_vars.default <- function(x) stop_generic(x, .Generic)

rename <- function(x, new, old) {
  hits <- match(old, x)
  replace(x, hits[!is.na(hits)], new[!is.na(hits)])
}

is_dt <- is_type("data.table")

is_df <- is_type("data.frame")

is_fst <- is_type("fst_table")

is_prt <- is_type("prt")

#' ICU class data utilities
#'
#' Several utility functions for working with `id_tbl` and `ts_tbl` objects
#' are available, including functions for changing column names, removing
#' columns, as well as aggregating or removing rows. An important thing to
#' note is that as `id_tbl` (and consequently `ts_tbl`) inherits from
#' `data.table`, there are several functions provided by the `data.table`
#' package that are capable of modifying `id_tbl` in a way that results in an
#' object with inconsistent state. An example for this is
#' [data.table::setnames()]: if an ID column or the index column name is
#' modified without updating the attribute marking the column as such, this
#' leads to an invalid object. As [data.table::setnames()] is not an S3
#' generic function, the only way to control its behavior with respect to
#' `id_tbl` objects is masking the function. As such an approach has its own
#' down-sides, a separate function, `rename_cols()` is provided, which is able
#' to handle column renaming correctly.
#'
#' @details
#' Apart from a function for renaming columns while respecting attributes
#' marking columns a index or ID columns, several other utility functions are
#' provided to make handling of `id_tbl` and `ts_tbl` objects more convenient.
#'
#' ## Sorting
#' An `id_tbl` or `ts_tbl` object is considered sorted when rows are in
#' ascending order according to columns as specified by [meta_vars()]. This
#' means that for an `id_tbl` object rows have to be ordered by [id_vars()]
#' and for a `ts_tbl` object rows have to be ordered first by [id_vars()],
#' followed by the [index_var()]. Calling the S3 generic function
#' [base::sort()] on an object that inherits form `id_tbl` using default
#' arguments yields an object that is considered sorted. For convenience
#' (mostly in printing), the column by which the table was sorted are moved to
#' the front (this can be disabled by passing `FALSE` as `reorder_cols`
#' argument). Internally, sorting is handled by either setting a
#' [data.table::key()] in case `decreasing = FALSE` or be calling
#' [data.table::setorder()] in case `decreasing = TRUE`.
#'
#' ## Uniqueness
#' On object inheriting form `id_tbl` is considered unique if it is unique in
#' terms of the columns as specified by [meta_vars()]. This means that for an
#' `id_tbl` object, either zero or a single row is allowed per combination of
#' values in columns [id_vars()] and consequently for `ts_tbl` objects a
#' maximum of one row is allowed per combination of time step and ID. In order
#' to create a unique `id_tbl` object from a non-unique `id_tbl` object,
#' `aggregate()` will combine observations that represent repeated
#' measurements within a group.
#'
#' ## Aggregating
#' In order to turn a non-unique `id_tbl` or `ts_tbl` object into an object
#' considered unique, the S3 generic function [stats::aggregate()] is
#' available. This applied the expression (or function specification) passed
#' as `expr` to each combination of grouping variables. The columns to be
#' aggregated can be controlled using the `vars` argument and the grouping
#' variables can be changed using the `by` argument. The argument `expr` is
#' fairly flexible: it can take an expression that will be evaluated in the
#' context of the `data.table` in a clean environment inheriting from `env`,
#' it can be a function, or it can be a string in which case `dt_gforce()` is
#' called. The default value `NULL` chooses a string dependent on data types,
#' where `numeric` resolves to `median`, `logical` to `sum` and `character` to
#' `first`.
#'
#' As aggregation is used in concept loading (see [load_concepts()]),
#' performance is important. For this reason, `dt_gforce()` allows for any of
#' the available functions to be applied using the `GForce` optimization of
#' `data.table` (see [data.table::datatable.optimize]).
#'
#' @examples
#' tbl <- id_tbl(a = rep(1:5, 4), b = rep(1:2, each = 10), c = rnorm(20),
#'               id_vars = c("a", "b"))
#' is_unique(tbl)
#' is_sorted(tbl)
#'
#' is_sorted(tbl[order(c)])
#'
#' identical(aggregate(tbl, list(c = sum(c))), aggregate(tbl, "sum"))
#'
#' tbl <- aggregate(tbl, "sum")
#' is_unique(tbl)
#' is_sorted(tbl)
#'
#' @param new,old Replacement names and existing column names for renaming
#' columns
#' @param skip_absent Logical flag for ignoring non-existent column names
#' @param by_ref Logical flag indicating whether to perform the operation by
#' reference
#' @param ... In case a function is passed as `new`, further arguments are
#' forwarded to that function
#'
#' @return Most of the utility functions return an object inheriting from
#' `id_tbl`, potentially modified by reference, depending on the type of the
#' object passed as `x`. The functions `is_sorted()`, `anyDuplicated()` and
#' `is_unique()` return logical flags, while `duplicated()` returns a logical
#' vector of the length `nrow(x)`.
#'
#' @rdname tbl_utils
#' @export
rename_cols <- function(x, new, old = colnames(x), skip_absent = FALSE,
                        by_ref = FALSE, ...) {

  if (is.function(new)) {
    new <- new(old, ...)
  } else {
    warn_dots(...)
  }

  assert_that(is_unique(new), is_unique(old), same_length(new, old),
              is.flag(skip_absent), is.flag(by_ref),
              is_unique(rename(colnames(x), new, old)))

  if (setequal(new, old)) {
    return(x)
  }

  col_renamer(x, new, old, skip_absent, by_ref)
}

#' Internal utilities for ICU data classes
#'
#' @inheritParams rename_cols
#'
#' @keywords internal
#' @export
col_renamer <- function(x, new, old = colnames(x), skip_absent = FALSE,
                        by_ref = FALSE) {

  UseMethod("col_renamer", x)
}

#' @keywords internal
#' @export
col_renamer.win_tbl <- function(x, new, old = colnames(x),
                                skip_absent = FALSE, by_ref = FALSE) {

  old_dur <- dur_var(x)

  if (old_dur %in% old) {

    new_dur <- new[old %in% old_dur]

    if (!by_ref) {
      x <- copy(x)
      by_ref <- TRUE
    }

    x <- set_attributes(x, dur_var = unname(new_dur))
  }

  col_renamer.ts_tbl(x, new, old, skip_absent, by_ref)
}

#' @keywords internal
#' @export
col_renamer.ts_tbl <- function(x, new, old = colnames(x),
                               skip_absent = FALSE, by_ref = FALSE) {

  old_ind <- index_var(x)
  intval  <- interval(x)

  if (old_ind %in% old) {

    new_ind <- new[old %in% old_ind]

    if (!by_ref) {
      x <- copy(x)
      by_ref <- TRUE
    }

    x <- set_attributes(x, index_var = unname(new_ind))
  }

  col_renamer.id_tbl(x, new, old, skip_absent, by_ref)
}

#' @keywords internal
#' @export
col_renamer.id_tbl <- function(x, new, old = colnames(x),
                              skip_absent = FALSE, by_ref = FALSE) {

  if (skip_absent) {

    hits <- old %in% colnames(x)

    if (sum(hits) == 0L) return(x)

    new <- new[hits]
    old <- old[hits]
  }

  old_id <- id_vars(x)

  if (any(old_id %in% old)) {

    new_id <- rename(old_id, new, old)

    if (!by_ref) {
      x <- copy(x)
      by_ref <- TRUE
    }

    x <- set_attributes(x, id_vars = unname(new_id))
  }

  col_renamer.data.table(x, new, old, skip_absent, by_ref)
}

#' @method col_renamer data.table
#' @keywords internal
#' @export
col_renamer.data.table <- function(x, new, old = colnames(x),
                                   skip_absent = FALSE, by_ref = FALSE) {

  if (!skip_absent) {
    assert_that(has_cols(x, old))
  }

  if (!by_ref) {
    x <- copy(x)
  }

  x <- setnames(x, old, new, skip_absent)

  check_valid(x)
}

#' @keywords internal
#' @export
col_renamer.default <- function(x, ...) stop_generic(x, .Generic)

#' @rdname tbl_utils
#' @export
rm_cols <- function(x, cols, skip_absent = FALSE, by_ref = FALSE) {

  assert_that(is.flag(skip_absent), is.flag(by_ref))

  if (skip_absent) {
    cols <- intersect(cols, colnames(x))
  } else {
    cols <- unique(cols)
  }

  if (length(cols) == 0L) {
    return(x)
  }

  assert_that(has_cols(x, cols))

  if (!by_ref) {
    x <- copy(x)
  }

  if (is_id_tbl(x) && any(cols %in% meta_vars(x))) {
    ptyp <- as_ptype(x)
  } else {
    ptyp <- NULL
  }

  x <- set(x, j = cols, value = NULL)

  try_reclass(x, ptyp)
}

#' @param new_interval Replacement interval length specified as scalar-valued
#' `difftime` object
#' @param ... Ignored
#'
#' @rdname tbl_utils
#' @export
change_interval <- function(x, new_interval, cols = time_vars(x),
                            by_ref = FALSE) {

  assert_that(is_scalar(new_interval), is_interval(new_interval),
              is.flag(by_ref))

  if (!length(cols) ||
      (is_ts_tbl(x) && all_equal(interval(x), new_interval))) {

    return(x)
  }

  UseMethod("change_interval", x)
}

#' @export
change_interval.ts_tbl <- function(x, new_interval, cols = time_vars(x),
                                   by_ref = FALSE) {

  id_nms <- id_vars(x)
  idx_nm <- index_var(x)

  if (!idx_nm %in% cols) {
    warn_ricu("when changing the `ts_tbl` interval the index variable is
               automatically included")
    cols <- unique(c(idx_nm, cols))
  }

  if (!by_ref) {
    x <- copy(x)
    by_ref <- TRUE
  }

  x <- set_attributes(x, interval = new_interval)

  change_interval.data.table(x, new_interval, cols, by_ref)
}

#' @method change_interval data.table
#' @export
change_interval.data.table <- function(x, new_interval, cols = time_vars(x),
                                       by_ref = FALSE) {

  if (!by_ref) {
    x <- copy(x)
  }

  for (col in cols) {
    set(x, j = col, value = re_time(x[[col]], new_interval))
  }

  check_valid(x)
}

#' @export
change_interval.default <- function(x, ...) stop_generic(x, .Generic)

#' @param new_unit New `difftime` unit for the `dur_var` column
#' @rdname tbl_utils
#' @export
change_dur_unit <- function(x, new_unit, by_ref = FALSE) {

  assert_that(is_win_tbl(x), is.string(new_unit), is.flag(by_ref))

  dura_var <- dur_var(x)

  if (by_ref) {
    x <- x[, c(dura_var) := `units<-`(get(dura_var), new_unit)]
  } else {
    x[[dura_var]] <- `units<-`(dur_col(x), new_unit)
  }

  x
}

#' @param cols Column names of columns to consider
#' @param mode Switch between `all` where all entries of a row have to be
#' missing (for the selected columns) or `any`, where a single missing entry
#' suffices
#'
#' @rdname tbl_utils
#' @export
#'
rm_na <- function(x, cols = data_vars(x), mode = c("all", "any")) {

  mode <- match.arg(mode)

  if (length(cols) == 0L) {
    return(x)
  }

  assert_that(has_cols(x, cols))

  if (identical(mode, "any") || length(cols) == 1L) {
    return(na.omit(x, cols))
  }

  drop <- Reduce(`&`, lapply(x[, cols, with = FALSE], is.na))

  x[!drop, ]
}

#' @param decreasing Logical flag indicating the sort order
#' @param reorder_cols Logical flag indicating whether to move the `by`
#' columns to the front.
#'
#' @rdname tbl_utils
#' @export
sort.id_tbl <- function(x, decreasing = FALSE, by = meta_vars(x),
                        reorder_cols = TRUE, by_ref = FALSE, ...) {

  warn_dots(...)

  assert_that(has_cols(x, by), is.flag(decreasing), is.flag(by_ref),
              is.flag(reorder_cols))

  if (!by_ref) {
    x <- copy(x)
  }

  if (decreasing) {
    x <- data.table::setorderv(x, by, order = -1L)
  } else {
    x <- data.table::setkeyv(x, by, physical = TRUE)
  }

  if (reorder_cols) {
    x <- setcolorder(x, c(by, setdiff(colnames(x), by)))
  }

  x
}

#' @rdname tbl_utils
#' @export
is_sorted <- function(x) {

  meta <- meta_vars(x)

  identical(head(data.table::key(x), n = length(meta)), meta)
}

on_failure(is_sorted) <- function(call, env) {
  cols <- meta_vars(eval(call$x, env))
  format_assert("{as_label(call$x)} is not sored by {quote_bt(cols)} in
                 increasing order", "is_sorted_assert")
}

temp_unclass <- function(x, expr) {
  ptyp <- as_ptype(x)
  unclass_tbl(x)
  on.exit(reclass_tbl(x, ptyp))
  expr
}

#' @param x Object to query
#' @param incomparables Not used. Here for S3 method consistency
#' @param by Character vector indicating which combinations of columns from
#' `x` to use for uniqueness checks
#'
#' @rdname tbl_utils
#' @export
duplicated.id_tbl <- function(x, incomparables = FALSE,
                              by = meta_vars(x), ...) {

  by <- force(by)

  temp_unclass(x,
    duplicated(x, incomparables = incomparables, by = by, ...)
  )
}

#' @rdname tbl_utils
#' @export
anyDuplicated.id_tbl <- function(x, incomparables = FALSE,
                                 by = meta_vars(x), ...) {

  by <- force(by)

  temp_unclass(x,
    anyDuplicated(x, incomparables = incomparables, by = by, ...)
  )
}

#' @rdname tbl_utils
#' @export
unique.id_tbl <- function(x, incomparables = FALSE, by = meta_vars(x), ...) {

  by <- force(by)

  res <- temp_unclass(x,
    unique(x, incomparables = incomparables, by = by, ...)
  )

  reclass_tbl(res, as_ptype(x))
}

#' @rdname tbl_utils
#' @export
#'
is_unique <- function(x, ...) identical(anyDuplicated(x, ...), 0L)

on_failure(is_unique) <- function(call, env) {
  format_assert("{as_label(call$x)} contains duplicate elements",
                "is_unique_assert")
}

#' @param expr Expression to apply over groups
#' @param env Environment to look up names in `expr`
#'
#' @importFrom rlang is_symbol
#'
#' @rdname tbl_utils
#' @export
#'
aggregate.id_tbl <- function(x, expr = NULL, by = meta_vars(x),
                             vars = data_vars(x), env = NULL, ...) {

  is_type <- function(col, ...) {
    Reduce(`|`, lapply(list(...), function(fun) fun(x[[col]])))
  }

  if (is.null(env)) {
    env <- caller_env()
  }

  assert_that(is.environment(env))

  if (nrow(x) == 0) {
    return(x)
  }

  if (length(vars) == 0L) {
    return(unique(x))
  }

  how <- enexpr(expr)

  if (is_symbol(how)) {
    how <- get(as.character(substitute(expr)), envir = env)
  }

  if (is.null(how) || isTRUE(how)) {

    if (all(lgl_ply(vars, is_type, is.numeric, is_difftime))) {

      fun <- "median"

    } else if (all(lgl_ply(vars, is_type, is.logical))) {

      fun <- "any"

    } else if (all(lgl_ply(vars, is_type, is.character))) {

      fun <- "first"

    } else {

      stop_ricu("when automatically determining an aggregation function,
                 {quote_bt(vars)} are required to be of the same type",
                "auto_agg_fun")
    }

    dt_gforce(x, fun, by = by, vars = vars, ...)

  } else if (is.string(how)) {

    dt_gforce(x, how, by = by, vars = vars, ...)

  } else if (is.function(how)) {

    x[, lapply(.SD, how, ...), .SDcols = vars, by = c(by)]

  } else {

    do.call(`[`, list(x, substitute(), substitute(how), by = by), envir = env)
  }
}

#' @param fun Function name (as string) to apply over groups
#' @param vars Column names to apply the function to
#' @param na_rm Logical flag indicating how to treat `NA` values
#'
#' @rdname tbl_utils
#' @export
#'
dt_gforce <- function(x,
                      fun = c("mean", "median", "min", "max", "sum", "prod",
                              "var", "sd", "first", "last", "any", "all"),
                      by = meta_vars(x), vars = data_vars(x),
                      na_rm = !fun %in% c("first", "last")) {

  col_is_lgl <- function(col, tbl) is.logical(tbl[[col]])

  .N <- NULL

  if (getOption("datatable.optimize") < 2L) {
    warn_ricu("the setting `datatable.optimize` prevents GForce optimizations
               from being applied.", "gforce_disabled")
  }

  fun <- match.arg(fun)

  if (fun %in% c("first", "last") && isTRUE(na_rm)) {
    warn_arg("na_rm")
  }

  assert_that(is.flag(na_rm), all(c(vars, by) %in% colnames(x)))

  if (fun %in% c("any", "all")) {
    assert_that(lgl_ply(vars, col_is_lgl, x), !"N" %in% c(by, vars))
  }

  switch(fun,
    mean   = x[, lapply(.SD, mean, na.rm = na_rm),   by = by, .SDcols = vars],
    median = x[, lapply(.SD, median, na.rm = na_rm), by = by, .SDcols = vars],
    min    = x[, lapply(.SD, min, na.rm = na_rm),    by = by, .SDcols = vars],
    max    = x[, lapply(.SD, max, na.rm = na_rm),    by = by, .SDcols = vars],
    sum    = x[, lapply(.SD, sum, na.rm = na_rm),    by = by, .SDcols = vars],
    prod   = x[, lapply(.SD, prod, na.rm = na_rm),   by = by, .SDcols = vars],
    var    = x[, lapply(.SD, var, na.rm = na_rm),    by = by, .SDcols = vars],
    sd     = x[, lapply(.SD, sd, na.rm = na_rm),     by = by, .SDcols = vars],
    first  = x[, first(.SD),                         by = by, .SDcols = vars],
    last   = x[, last(.SD),                          by = by, .SDcols = vars],
    any = {
      x <- x[, lapply(.SD, sum, na.rm = na_rm), by = by, .SDcols = vars]
      x <- x[, c(vars) := lapply(.SD, as.logical), .SDcols = vars]
      x
    },
    all = {
      x <- x[, c(lapply(.SD, sum, na.rm = na_rm), .N), by = by, .SDcols = vars]
      x <- x[, c(vars) := lapply(.SD, `==`, get("N")), .SDcols = vars]
      x <- x[, c("N") := NULL]
      x
    }
  )
}

#' @inheritParams data.table::nafill
#' @param val Replacement value (if `type` is `"const"`)
#' @rdname tbl_utils
#' @export
replace_na <- function(x, val, type = "const", ...) UseMethod("replace_na", x)

#' @export
replace_na.numeric <- function(x, val, type = "const", ...) {

  if (identical(type, "const")) {
    data.table::nafill(x, type, val, ...)
  } else {
    data.table::nafill(x, type, ...)
  }
}

#' @export
replace_na.logical <- function(x, val, type = "const", ...) {

  if (identical(type, "const")) {
    res <- NextMethod()
  } else {
    res <- replace_na(as.integer(x), type = type, ...)
  }

  as.logical(res)
}

#' @export
replace_na.default <- function(x, val, type = "const", ...) {

  warn_dots(...)

  assert_that(identical(type, "const"), msg = "currently only \"const\"
    replacement is possible (data.table#3992)"
  )

  replace(x, is.na(x), val)
}

#' @export
replace_na.data.table <- function(x, val, type = "const", by_ref = FALSE,
                                  vars = colnames(x), by = NULL, ...) {

  assert_that(is.flag(by_ref))

  if (isFALSE(by_ref)) {
    x <- copy(x)
  }

  if (missing(val) && !any(type == "const")) {
    val <- NA
  }

  x <- x[, c(vars) := Map(replace_na, .SD, val, type, MoreArgs = list(...)),
         .SDcols = vars, by = by]
  x
}

