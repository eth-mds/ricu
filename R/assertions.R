
#' @importFrom assertthat on_failure<- validate_that
NULL

is_dt <- function(x) data.table::is.data.table(x)

on_failure(is_dt) <- function(call, env) {
  paste0(deparse(call$x), " is not a `data.table`")
}

has_cols <- function(x, cols) {
  is.character(cols) && length(cols) > 0L &&
    length(cols) == unique(length(cols)) &&
    all(vapply(cols, str_in_vec_once, logical(1L), colnames(x)))
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

has_time_col <- function(x, col) has_col(x, col) && is_time(x[[col]])

on_failure(has_time_col) <- function(call, env) {
  paste0(deparse(call$x), " does not contain column `",
         eval(call$col, env), "` of class `difftime`.")
}

is_time <- function(x, allow_neg = TRUE) {
  inherits(x, "difftime") && (allow_neg || all(x >= 0))
}

on_failure(is_time) <- function(call, env) {
  pos <- !eval(call$allow_neg, env)
  paste0(deparse(call$x), " is not a",
         if (pos) " strictly positive " else " ",
         "`difftime` object")
}

same_time_unit <- function(x, y)
 is_time(x) && is_time(y) && identical(units(x), units(y))

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

all_fun <- function(x, is_fun) all(vapply(x, is_fun, logical(1L)))

on_failure(all_fun) <- function(call, env) {
  paste0("some of ", deparse(call$x), " do not satisfy `", deparse(call$x),
         "`.")
}

