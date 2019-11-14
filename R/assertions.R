
#' @importFrom assertthat on_failure<-
NULL

is_dt <- function(x) data.table::is.data.table(x)

on_failure(is_dt) <- function(call, env) {
  paste0(deparse(call$x), " is not a `data.table`")
}

has_cols <- function(x, cols) all(cols %in% colnames(x))

on_failure(has_cols) <- function(call, env) {
  out_names <- paste0("`", paste0(eval(call$cols, env), collapse = "`, `"),
                      "`")
  paste0(deparse(call$x), " does not have all of these column name(s): ",
         out_names)
}

is_difftime <- function(x, allow_neg = TRUE, need_step = FALSE) {
  inherits(x, "difftime") &&
    if (allow_neg) TRUE else all(x >= 0) &&
    if (need_step) !is.null(attr(x, "step_size")) else TRUE
}

on_failure(is_difftime) <- function(call, env) {
  pos <- !eval(call$allow_neg, env)
  paste0(deparse(call$x), " is not a",
         if (pos) " strictly positive " else " ",
         "`difftime` object")
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
