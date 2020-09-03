
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
#' * `meta_vars()`: For `ts_tbl` objects, meta variables represent the union
#'   of ID and index variables, while for `id_tbl` objects meta variables
#'   consist pf ID variables.
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
#'   column is one of (potentially) several time variables.
#' * `interval()`: The time series interval length is represented a scalar
#'   valued [`difftime`][base::difftime()] object.
#' * `time_unit()`: The time unit of the time series interval, represented by
#'   a string such as "hours" or "mins" (see [`difftime`][base::difftime()]).
#' * `time_step()`: The time series step size represented by a nuemric value
#'   in the unit as returned by `time_unit()`.
#'
#' @param x Object to query
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

#' @rdname tbl_meta
#' @export
index_col <- function(x) x[[index_var(x)]]

#' @rdname tbl_meta
#' @export
meta_vars <- function(x) UseMethod("meta_vars", x)

#' @export
meta_vars.id_tbl <- function(x) id_vars(x)

#' @export
meta_vars.ts_tbl <- function(x) c(id_vars(x), index_var(x))

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

#' ICU class temporal utilities
#'
#' Utilities for retrieving/modifying temporal metadata from `ts_tbl` objects.
#'
#' @param x Object to query/operate on
#'
#' @rdname tbl_meta
#' @export
interval <- function(x) UseMethod("interval", x)

#' @export
interval.ts_tbl <- function(x) attr(x, "interval")

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

rename <- function(x, new, old) {
  hits <- match(old, x)
  replace(x, hits[!is.na(hits)], new[!is.na(hits)])
}

is_dt <- is_type("data.table")

#' @param new,old Replacement names and existing column names for renaming
#' columns
#' @param skip_absent Logical flag for ignoring non-existent column names
#' @param by_ref Logical flag indicating whether to perform the operation by
#' reference
#'
#' @rdname tbl_utils
#' @export
rename_cols <- function(x, new, old = colnames(x), skip_absent = FALSE,
                        by_ref = FALSE) {

  assert_that(is_unique(new), is_unique(old), same_length(new, old),
              is.flag(skip_absent), is.flag(by_ref),
              is_unique(rename(colnames(x), new, old)))

  UseMethod("rename_cols", x)
}

#' @export
rename_cols.ts_tbl <- function(x, new, old = colnames(x),
                               skip_absent = FALSE, ...) {

  new_ind <- index_var(x)
  intval  <- interval(x)

  if (new_ind %in% old) {
    new_ind <- new[old %in% new_ind]
  }

  res <- NextMethod()

  new_ts_tbl(res, id_vars(res), new_ind, intval)
}

#' @export
rename_cols.id_tbl <- function(x, new, old = colnames(x),
                               skip_absent = FALSE, ...) {

  if (skip_absent) {

    hits <- old %in% colnames(x)

    if (sum(hits) == 0L) return(x)

    new <- new[hits]
    old <- old[hits]
  }

  res <- NextMethod()

  new_id_tbl(res, rename(id_vars(x), new, old))
}

#' @method rename_cols data.table
#' @export
rename_cols.data.table <- function(x, new, old = colnames(x),
                                   skip_absent = FALSE, by_ref = FALSE) {

  if (!skip_absent) {
    assert_that(has_cols(x, old))
  }

  if (!by_ref) {
    x <- copy(x)
  }

  x <- setnames(x, old, new, skip_absent)

  x
}

#' @rdname tbl_utils
#' @export
rm_cols <- function(x, cols, skip_absent = FALSE, by_ref = FALSE) {

  assert_that(is.flag(skip_absent), is.flag(by_ref))

  if (!length(cols)) {
    return(x)
  }

  if (skip_absent) {
    cols <- intersect(cols, colnames(x))
  } else {
    assert_that(has_length(cols), has_cols(x, cols))
  }

  if (!by_ref) {
    x <- copy(x)
  }

  if (is_id_tbl(x) && any(cols %in% meta_vars(x))) {
    ptyp <- as_ptype(x)
  } else {
    ptyp <- NULL
  }

  if (length(cols)) {
    x <- set(x, j = unique(cols), value = NULL)
  }

  reclass_tbl(x, ptyp, FALSE)
}

#' @param new_interval Replacement interval length specified as scalar-valued
#' `difftime` object
#' @param ... Ignored
#'
#' @rdname tbl_utils
#' @export
change_interval <- function(x, new_interval, cols = time_vars(x),
                            by_ref = FALSE) {

  assert_that(is_scalar(new_interval), is_interval(new_interval))

  if (!length(cols)) {
    return(x)
  }

  UseMethod("change_interval", x)
}

#' @export
change_interval.ts_tbl <- function(x, new_interval, cols = time_vars(x), ...) {

  if (all_equal(interval(x), new_interval)) {
    return(x)
  }

  id_nms <- id_vars(x)
  idx_nm <- index_var(x)

  res <- NextMethod()

  new_ts_tbl(res, id_nms, idx_nm, new_interval, by_ref = TRUE)
}

#' @method change_interval data.table
#' @export
change_interval.data.table <- function(x, new_interval, cols = time_vars(x),
                                       by_ref = FALSE) {

  change_time <- function(x) re_time(x, new_interval)

  if (!by_ref) {
    x <- copy(x)
  }

  x[, c(cols) := lapply(.SD, change_time), .SDcols = cols]
}

#' @rdname tbl_utils
#' @export
#'
rbind_lst <- function(x, ...) {

  cond_as <- function(x) if (is.list(x)) x else as.data.table(x)

  dt_rbl <- function(x, ...) rbindlist(lapply(x, cond_as), ...)

  do_rename <- function(x, new) {
    fun <- if (is_ts_tbl(x) && is_ts_tbl(new)) meta_vars else id_vars
    rename_cols(x, fun(new), fun(x), by_ref = TRUE)
  }

  id_tbl <- lgl_ply(x, is_id_tbl)
  ts_tbl <- lgl_ply(x, is_ts_tbl)
  id_tbl <- id_tbl & !ts_tbl

  if (any(id_tbl)) {

    ptyp <- as_ptype(x[[which(id_tbl)[1L]]])

  } else if (any(ts_tbl)) {

    ptyp <- as_ptype(x[[which(ts_tbl)[1L]]])

    assert_that(
      all_fun(lapply(x[ts_tbl], interval), all_equal, interval(ptyp)),
      msg = "cannot mix interval lengths when row-binding"
    )

  } else {

    ptyp <- NULL
  }

  if (not_null(ptyp)) {

    id_tbls <- lgl_ply(x, is_id_tbl)
    old_ptp <- lapply(x[id_tbls], as_ptype)

    x[id_tbls] <- lapply(x[id_tbls], do_rename, ptyp)

    on.exit(Map(do_rename, x[id_tbls], old_ptp))
  }

  reclass_tbl(dt_rbl(x, ...), ptyp)
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

  assert_that(has_cols(x, cols))

  if (identical(mode, "any")) {
    return(na.omit(x, cols))
  }

  if (length(cols) == 1L) {
    drop <- is.na(x[[cols]])
  } else {
    drop <- Reduce(`&`, lapply(x[, cols, with = FALSE], is.na))
  }

  x[!drop, ]
}

#' @param col_groups A list of character vectors defining the grouping of
#' non-by columns
#' @param by Columns that will be present in every one of the resulting tables
#' @param na_rm Logical flag indicating whether to remove rows that have all
#' missing entries in the respective `col_groups` group
#'
#' @rdname tbl_utils
#' @export
#'
unmerge <- function(x, col_groups = as.list(data_vars(x)), by = meta_vars(x),
                    na_rm = TRUE) {

  name_has <- function(name, x) has_name(x, name)

  assert_that(has_name(x, by), all_fun(col_groups, name_has, x),
              is.flag(na_rm))

  extract_col <- function(col, x) {

    y <- x[, c(by, col), with = FALSE]

    if (na_rm) {
      y <- rm_na(y, col)
    }

    y
  }

  lapply(col_groups, extract_col, x)
}

#' @param fun Function name (as string) to apply over groups
#' @param vars Column names to apply the function to
#'
#' @rdname tbl_utils
#' @export
#'
dt_gforce <- function(x,
                      fun = c("mean", "median", "min", "max", "sum", "prod",
                              "var", "sd", "first", "last"),
                      by = meta_vars(x), vars = data_vars(x),
                      na_rm = !fun %in% c("first", "last")) {

  fun <- match.arg(fun)

  if (fun %in% c("first", "last") && isTRUE(na_rm)) {
    warn_arg("na_rm")
  }

  assert_that(is.flag(na_rm), all(c(vars, by) %in% colnames(x)))

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
    last   = x[, last(.SD),                          by = by, .SDcols = vars]
  )
}

#' @rdname tbl_utils
#' @export
#'
is_unique <- function(x, ...) UseMethod("is_unique", x)

#' @rdname tbl_utils
#' @export
#'
is_unique.default <- function(x, ...) identical(anyDuplicated(x, ...), 0L)

#' @export
is_unique.id_tbl <- function(x, by = meta_vars(x), ...) {
  identical(anyDuplicated(x, by = by, ...), 0L)
}

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
make_unique <- function(x, expr = NULL, by = meta_vars(x), vars = data_vars(x),
                        env = NULL, ...) {

  is_num <- function(col) is.numeric(x[[col]])

  if (is.null(env)) {
    env <- caller_env()
  }

  assert_that(is_id_tbl(x), is.environment(env))

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

  if (is.null(how)) {

    is_num_col <- lgl_ply(vars, is_num)

    if (any(is_num_col)) {

      assert_that(all(is_num_col), msg = paste("For automatically determining",
        "an aggregation function, either all of or none of vars",
        concat(quote_bt(vars)), "are expected to be of numeric type")
      )

      fun <- "median"

    } else {

      fun <- "first"
    }

    dt_gforce(x, fun, by = by, vars = vars, ...)

  } else if (is.string(how)) {

    dt_gforce(x, how, by = by, vars = vars, ...)

  } else if (is.function(how)) {

    x[, lapply(.SD, how, ...), .SDcols = vars, by = c(by)]

  } else {

    .x_ <- .expr_ <- .by_ <- NULL

    local({
      .x_[, eval(.expr_), by = .by_]
    },
      envir = list2env(list(.x_ = x, .expr_ = how, .by_ = by), parent = env)
    )
  }
}
