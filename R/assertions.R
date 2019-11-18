
#' @importFrom assertthat on_failure<-
NULL

is_dt <- function(x) data.table::is.data.table(x)

on_failure(is_dt) <- function(call, env) {
  paste0(deparse(call$x), " is not a `data.table`")
}

has_cols <- function(x, cols) {
  is.character(cols) && length(cols) > 0L &&
    all(vapply(cols, str_in_vec_once, logical(1L), colnames(x)))
}

on_failure(has_cols) <- function(call, env) {
  out_names <- paste0("`", paste0(eval(call$cols, env), collapse = "`, `"),
                      "`")
  paste0(deparse(call$x), " does not contain exactly one of the following ",
         "column names: ", out_names)
}

has_col <- function(x, col) {
  is.string(col) && str_in_vec_once(col, colnames(x))
}

on_failure(has_col) <- function(call, env) {
  paste0(deparse(call$x), " does not contain exactly one column `",
         eval(call$col, env), "`.")
}

has_time_col <- function(x, col) has_col(x, col) && is_difftime(x[[col]])

on_failure(has_time_col) <- function(call, env) {
  paste0(deparse(call$x), " does not contain column `",
         eval(call$col, env), "` of class `difftime`.")
}

is_difftime <- function(x, allow_neg = TRUE) {
  inherits(x, "difftime") && (allow_neg || all(x >= 0))
}

on_failure(is_difftime) <- function(call, env) {
  pos <- !eval(call$allow_neg, env)
  paste0(deparse(call$x), " is not a",
         if (pos) " strictly positive " else " ",
         "`difftime` object")
}

same_time_unit <- function(x, y)
 is_difftime(x) && is_difftime(y) && identical(units(x), units(y))

on_failure(same_time_unit) <- function(call, env) {
  paste0("`", deparse(call$x), "` and `", deparse(call$y),
         "` are not on the same time scale.")
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

same_time_spec <- function(x, y, time_col = "hadm_time", time_x = time_col,
                            time_y = time_col) {

  x <- attributes(x[[time_x]])
  y <- attributes(y[[time_y]])

  !is.null(x[["units"]]) && !is.null(y[["units"]]) &&
    identical(x[["units"]], y[["units"]]) &&
  !is.null(x[["step_size"]]) && !is.null(y[["step_size"]]) &&
    identical(x[["step_size"]], y[["step_size"]])
}

on_failure(same_time_spec) <- function(call, env) {
  paste0("column `", eval(call$time_x, env), "` of ", deparse(call$x),
         " does not have the same time specification as column `",
         eval(call$time_y, env), "` of ", deparse(call$y), ".")
}

same_by_cols <- function(x, y) setequal(by_cols(x), by_cols(y))

on_failure(same_time_unit) <- function(call, env) {
  paste0(deparse(call$x), " and ", deparse(call$y),
         " do not share the same `by` columns.")
}

check_ts_tbl <- function(x, key, ind, unit = NULL, step = NULL) {
  is_dt(x) && has_cols(x, key) && has_time_col(x, ind) &&
    (is.null(unit) || identical(unit, units(x[[ind]]))) &&
    (is.null(step) || identical(step, ts_step(x)))
}

on_failure(check_ts_tbl) <- function(call, env) {
  paste0(deparse(call$x), " does not fulfill `ts_tbl` requirements.")
}
