
#' @importFrom assertthat on_failure<- validate_that
NULL

is_dt <- function(x) data.table::is.data.table(x)

on_failure(is_dt) <- function(call, env) {
  paste0(deparse(call$x), " is not a `data.table`")
}

has_cols <- function(x, cols) {
  is.character(cols) && length(cols) > 0L &&
    length(cols) == unique(length(cols)) &&
    all_fun(cols, str_in_vec_once, colnames(x))
}

on_failure(has_cols) <- function(call, env) {
  out_names <- paste0("`", paste0(eval(call$cols, env), collapse = "`, `"),
                      "`")
  paste0(deparse(call$x), " does not contain exactly one of the following ",
         "(unique) column names: ", out_names)
}

has_col <- function(x, col) {
  is.string(col) && str_in_vec_once(col, colnames(x))
}

on_failure(has_col) <- function(call, env) {
  paste0(deparse(call$x), " does not contain exactly one column `",
         eval(call$col, env), "`.")
}

has_time_col <- function(x, col, ...) {
  has_col(x, col) && is_time_vec(x[[col]], ...)
}

on_failure(has_time_col) <- function(call, env) {
  paste0(deparse(call$x), " does not contain column `",
         eval(call$col, env), "` of class `difftime`.")
}

has_time_cols <- function(x, cols, ...) {
  all_fun(cols, function(col, ...) has_time_col(x, col, ...), ...)
}

on_failure(has_time_cols) <- function(call, env) {
  paste0("Not all of ", paste0(eval(call$col, env), collapse = ", "),
         " are contained in ", deparse(call$x), " as `difftime` objects.")
}

is_time <- function(x, allow_neg = TRUE) {
  inherits(x, "difftime") && length(x) == 1L && (allow_neg || all(x >= 0))
}

on_failure(is_time) <- function(call, env) {
  pos <- !eval(call$allow_neg, env)
  paste0(deparse(call$x), " is not a",
         if (pos) " strictly positive " else " ",
         "`difftime` object of length 1.")
}

is_time_vec <- function(x, allow_neg = TRUE) {
  inherits(x, "difftime") && (allow_neg || all(x >= 0))
}

on_failure(is_time_vec) <- function(call, env) {
  pos <- !eval(call$allow_neg, env)
  paste0(deparse(call$x), " is not a",
         if (pos) " strictly positive " else " ",
         "`difftime` object.")
}

same_time_unit <- function(x, y)
 is_time_vec(x) && is_time_vec(y) && identical(units(x), units(y))

on_failure(same_time_unit) <- function(call, env) {
  paste0("`", deparse(call$x), "` and `", deparse(call$y),
         "` are not on the same time scale.")
}

same_id_cols <- function(x, y) setequal(ts_id_cols(x), ts_id_cols(y))

on_failure(same_id_cols) <- function(call, env) {
  paste0(deparse(call$x), " and ", deparse(call$y),
         " do not share the same `by` columns.")
}

same_time_cols <- function(x, y) {
  identical(ts_index(x), ts_index(y)) &&
    identical(ts_time_unit(x), ts_time_unit(y)) &&
    identical(ts_time_step(x), ts_time_step(y))
}

on_failure(same_time_cols) <- function(call, env) {
  paste0(deparse(call$x), " and ", deparse(call$y),
         " do not share the same `time` columns and scale.")
}

has_unit <- function(x, col, unit) {
  is_dt(x) && if (nrow(x) > 0L && !all(is.na(x[[col]]))) {
    identical(attr(x[[col]], "unit"), unit)
  } else TRUE
}

on_failure(has_unit) <- function(call, env) {
  paste0("column `", eval(call$col, env), "` of ", deparse(call$x),
         " does not have unit `", eval(call$unit, env), "`.")
}

all_fun <- function(x, fun, ...) all(vapply(x, fun, logical(1L), ...))

on_failure(all_fun) <- function(call, env) {
  paste0("some of ", deparse(call$x), " do not satisfy `",
         deparse(call$fun), "`.")
}

same_length <- function(x, y) identical(length(x), length(y))

on_failure(same_length) <- function(call, env) {
  paste0(deparse(call$x), " does not have the same length as ",
         deparse(call$y), ".")
}

has_ts_meta <- function(x, class) {
  is_ts_tbl(x) && !is.null(ts_def(x)[[class]])
}

on_failure(has_ts_meta) <- function(call, env) {
  paste0(deparse(call$x), " does not contain a ts_meta tag of class `",
         deparse(call$class), "`.")
}

xor_na <- function(x, y) all(xor(is.na(x), is.na(y)))

on_failure(xor_na) <- function(call, env) {
  paste0("Either ", deparse(call$x), " xor ", deparse(call$y),
         " are expected to be `NA`.")
}

