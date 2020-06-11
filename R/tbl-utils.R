
#' Utilities for working with id_tbl and ts_tbl objects
#'
#' @param x Object to query/operate on
#'
#' @rdname tbl_utils
#' @export
id_vars <- function(x) UseMethod("id_vars", x)

#' @rdname tbl_utils
#' @export
id_vars.id_tbl <- function(x) attr(x, "id_vars")

#' @rdname tbl_utils
#' @export
id_col <- function(x) {
  col <- id_vars(x)
  assert_that(length(col) == 1L)
  x[[col]]
}

#' @rdname tbl_utils
#' @export
index_var <- function(x) UseMethod("index_var", x)

#' @rdname tbl_utils
#' @export
index_var.ts_tbl <- function(x) attr(x, "index_var")

#' @rdname tbl_utils
#' @export
index_col <- function(x) x[[index_var(x)]]

#' @rdname tbl_utils
#' @export
meta_vars <- function(x) UseMethod("meta_vars", x)

#' @rdname tbl_utils
#' @export
meta_vars.id_tbl <- function(x) id_vars(x)

#' @rdname tbl_utils
#' @export
meta_vars.ts_tbl <- function(x) c(id_vars(x), index_var(x))

#' @rdname tbl_utils
#' @export
data_vars <- function(x) setdiff(colnames(x), meta_vars(x))

#' @rdname tbl_utils
#' @export
data_col <- function(x) {
  col <- data_vars(x)
  assert_that(length(col) == 1L)
  x[[col]]
}

#' @rdname tbl_utils
#' @export
interval <- function(x) UseMethod("interval", x)

#' @rdname tbl_utils
#' @export
interval.ts_tbl <- function(x) attr(x, "interval")

#' @rdname tbl_utils
#' @export
interval.difftime <- function(x) {

  dif <- diff(x)
  res <- min(dif[dif > 0], na.rm = TRUE)

  assert_that(has_interval(x, res), msg = paste(
    "failed to determine interval from data: not all time steps are a",
    "multiple of the minimal time step", format(res))
  )

  res
}

#' @rdname tbl_utils
#' @export
time_unit <- function(x) units(interval(x))

#' @rdname tbl_utils
#' @export
time_step <- function(x) as.double(interval(x))

#' @rdname tbl_utils
#' @export
time_vars <- function(x) UseMethod("time_vars", x)

#' @method time_vars data.table
#' @rdname tbl_utils
#' @export
time_vars.data.table <- function(x) colnames(x)[lgl_ply(x, is_difftime)]

rename <- function(x, new, old) {
  hits <- match(old, x)
  replace(x, hits[!is.na(hits)], new[!is.na(hits)])
}

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

#' @rdname tbl_utils
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

#' @rdname tbl_utils
#' @export
rename_cols.id_tbl <- function(x, new, old = colnames(x),
                               skip_absent = FALSE, ...) {

  assert_that(is.flag(skip_absent))

  if (skip_absent) {

    hits <- old %in% colnames(x)

    if (sum(hits) == 0L) return(x)

    new <- new[hits]
    old <- old[hits]
  }

  new_id_tbl(NextMethod(), rename(id_vars(x), new, old))
}

#' @method rename_cols data.table
#' @rdname tbl_utils
#' @export
rename_cols.data.table <- function(x, new, old = colnames(x),
                                   skip_absent = FALSE, by_ref = FALSE) {

  assert_that(has_cols(x, old))

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

  if (any(cols %in% meta_vars(x))) {
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

  assert_that(is_time(new_interval, allow_neg = FALSE))

  if (!length(cols)) {
    return(x)
  }

  UseMethod("change_interval", x)
}

#' @rdname tbl_utils
#' @export
change_interval.ts_tbl <- function(x, new_interval, cols = time_vars(x), ...) {

  if (all_equal(interval(x), new_interval)) {
    return(x)
  }

  id_nms <- id_vars(x)
  idx_nm <- index_var(x)

  new_ts_tbl(NextMethod(), id_nms, idx_nm, new_interval, by_ref = TRUE)
}

#' @method change_interval data.table
#' @rdname tbl_utils
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
    rename_cols(x, fun(new), fun(x))
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
#'
#' @rdname tbl_utils
#' @export
#'
dt_gforce <- function(x,
                      fun = c("mean", "median", "min", "max", "sum", "prod",
                              "var", "sd", "first", "last"),
                      by = meta_vars(x), cols = data_vars(x),
                      na_rm = !fun %in% c("first", "last")) {

  fun <- match.arg(fun)

  if (fun %in% c("first", "last") && isTRUE(na_rm)) {
    warning("The argument `na_rm` is ignored for `first()` and `last()`")
  }

  assert_that(is.flag(na_rm), all(c(cols, by) %in% colnames(x)))

  switch(fun,
    mean   = x[, lapply(.SD, mean, na.rm = na_rm),   by = by, .SDcols = cols],
    median = x[, lapply(.SD, median, na.rm = na_rm), by = by, .SDcols = cols],
    min    = x[, lapply(.SD, min, na.rm = na_rm),    by = by, .SDcols = cols],
    max    = x[, lapply(.SD, max, na.rm = na_rm),    by = by, .SDcols = cols],
    sum    = x[, lapply(.SD, sum, na.rm = na_rm),    by = by, .SDcols = cols],
    prod   = x[, lapply(.SD, prod, na.rm = na_rm),   by = by, .SDcols = cols],
    var    = x[, lapply(.SD, var, na.rm = na_rm),    by = by, .SDcols = cols],
    sd     = x[, lapply(.SD, sd, na.rm = na_rm),     by = by, .SDcols = cols],
    first  = x[, lapply(.SD, data.table::first),     by = by, .SDcols = cols],
    last   = x[, lapply(.SD, data.table::last),      by = by, .SDcols = cols]
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

#' @param expr Expression to apply over groups
#'
#' @rdname tbl_utils
#' @export
#'
make_unique <- function(x, expr, fun, ...) {

  if (missing(fun)) {

    make_unique_quo(x, substitute(expr), ...)

  } else {

    make_unique_quo(x, fun, ...)
  }
}

#' @rdname tbl_utils
#' @export
#'
make_unique_quo <- function(x, expr, by = meta_vars(x), cols = data_vars(x),
                            ...) {

  assert_that(is_id_tbl(x))

  if (nrow(x) == 0) return(x)
  if (length(cols) == 0L) return(unique(x))

  if (is.function(expr)) {

    x[, lapply(.SD, expr, ...), .SDcols = cols, by = c(by)]

  } else if (!is.language(expr) && is.null(expr)) {

    assert_that(is_unique(x, by = by))
    x

  } else if (is.character(expr)) {

    assert_that(is.string(expr))

    if (is.na(expr)) {
      if (is.numeric(data_col(x))) {
        expr <- "median"
      } else {
        expr <- "first"
      }
    }

    dt_gforce(x, expr, by = by, cols = cols, ...)

  } else {

    x[, eval(expr), by = by]
  }
}
