
#' @importFrom assertthat on_failure<-
NULL

is_dt <- function(x) data.table::is.data.table(x)

on_failure(is_dt) <- function(call, env) {
  paste0(deparse(call$x), " is not a `data.table`")
}

has_cols <- function(x, cols) setequal(colnames(x), cols)

on_failure(has_cols) <- function(call, env) {
  out_names <- paste0("`", paste0(eval(call$cols, env), collapse = "`, `"),
                      "`")
  paste0(deparse(call$x), " does not have all of these column name(s): ",
         out_names)
}

is_difftime <- function(x, allow_neg = TRUE) {
  inherits(x, "difftime") && if (allow_neg) TRUE else x >= 0
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
