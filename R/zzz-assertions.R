
#' @importFrom assertthat assert_that on_failure<- validate_that
#' @importFrom assertthat is.string is.flag is.dir is.scalar is.count
#' @importFrom assertthat has_name has_attr are_equal is.number
NULL


on_failure(is_tbl_index) <- function(call, env) {
  paste0(deparse(call$x), " is not a `tbl_index` object")
}

on_failure(is_tbl_id) <- function(call, env) {
  paste0(deparse(call$x), " is not a `tbl_id` object")
}

on_failure(is_ts_meta) <- function(call, env) {
  paste0(deparse(call$x), " is not a `ts_meta` object")
}

on_failure(is_ts_tbl) <- function(call, env) {
  paste0(deparse(call$x), " is not a `ts_tbl` object")
}

on_failure(is_unique) <- function(call, env) {
  paste0(deparse(call$x), " contains duplicate elements")
}

is_dt <- function(x) data.table::is.data.table(x)

on_failure(is_dt) <- function(call, env) {
  paste0(deparse(call$x), " is not a `data.table` object")
}

has_cols <- function(x, cols) {
  is.character(cols) && length(cols) > 0L &&
    length(cols) == unique(length(cols)) &&
    all_fun(cols, str_in_vec_once, colnames(x))
}

on_failure(has_cols) <- function(call, env) {
  cols <- eval(call$cols, env)
  all <- colnames(eval(call$x, env))
  paste0(deparse(call$x), " does not contain the following columns: ",
         paste0("`", paste0(cols[!cols %in% all], collapse = "`, `"), "`"))
}

has_col <- function(x, col) {
  is.string(col) && str_in_vec_once(col, colnames(x))
}

on_failure(has_col) <- function(call, env) {
  paste0(deparse(call$x), " does not contain column `",
         eval(call$col, env), "`")
}

has_time_col <- function(x, col, ...) {
  has_col(x, col) && is_time_vec(x[[col]], ...)
}

on_failure(has_time_col) <- function(call, env) {
  paste0(deparse(call$x), " does not contain column `",
         eval(call$col, env), "` of class `difftime`")
}

has_time_cols <- function(x, cols, ...) {
  all_fun(cols, function(col, ...) has_time_col(x, col, ...), ...)
}

on_failure(has_time_cols) <- function(call, env) {
  paste0("Not all of ", paste0(eval(call$col, env), collapse = ", "),
         " are contained in ", deparse(call$x), " as `difftime` objects")
}

on_failure(is_time) <- function(call, env) {
  pos <- is.null(call$allow_neg) || !eval(call$allow_neg, env)
  paste0(deparse(call$x), " is not a",
         if (pos) " strictly positive " else " ",
         "`difftime` object of length 1")
}

on_failure(is_time_vec) <- function(call, env) {
  pos <- !eval(call$allow_neg, env)
  paste0(deparse(call$x), " is not a",
         if (pos) " strictly positive " else " ",
         "`difftime` object")
}

same_time_unit <- function(x, y)
  is_time_vec(x) && is_time_vec(y) && identical(units(x), units(y))

on_failure(same_time_unit) <- function(call, env) {
  paste0("`", deparse(call$x), "` and `", deparse(call$y),
         "` are not on the same time scale")
}

same_interval <- function(x, y)
 isTRUE(all.equal(interval(x), interval(y)))

on_failure(same_interval) <- function(call, env) {
  paste0("`", deparse(call$x), "` and `", deparse(call$y),
         "` are not on the same time scale")
}

same_id <- function(x, y) identical(id(x), id(y))

on_failure(same_id) <- function(call, env) {
  paste0(deparse(call$x), " and ", deparse(call$y),
         " do not share the same `id`columns")
}

same_meta_cols <- function(x, y) setequal(meta_cols(x), meta_cols(y))

on_failure(same_meta_cols) <- function(call, env) {
  paste0(deparse(call$x), " and ", deparse(call$y),
         " do not share the same `id` and `index` columns")
}

same_time_cols <- function(x, y) {
  identical(index(x), index(y)) &&
    identical(time_unit(x), time_unit(y)) &&
    all.equal(time_step(x), time_step(y))
}

on_failure(same_time_cols) <- function(call, env) {
  paste0(deparse(call$x), " and ", deparse(call$y),
         " do not share the same `time` columns and scale")
}

has_unit <- function(x, col, unit) {
  is_dt(x) && if (nrow(x) > 0L && !all(is.na(x[[col]]))) {
    identical(attr(x[[col]], "unit"), unit)
  } else TRUE
}

on_failure(has_unit) <- function(call, env) {
  paste0("column `", eval(call$col, env), "` of ", deparse(call$x),
         " does not have unit `", eval(call$unit, env), "`")
}

all_fun <- function(x, fun, ...) all(lgl_ply(x, fun, ...))

on_failure(all_fun) <- function(call, env) {
  paste0("some of ", deparse(call$x), " do not satisfy `",
         deparse(call$fun), "`")
}

all_null <- function(x) all_fun(x, is.null)

on_failure(all_null) <- function(call, env) {
  paste0("some of ", deparse(call$x), " are not NULL")
}

same_length <- function(x, y) identical(length(x), length(y))

on_failure(same_length) <- function(call, env) {
  paste0(deparse(call$x), " does not have the same length as ",
         deparse(call$y))
}

xor_na <- function(x, y) all(xor(is.na(x), is.na(y)))

on_failure(xor_na) <- function(call, env) {
  paste0("Either ", deparse(call$x), " xor ", deparse(call$y),
         " are expected to be `NA`")
}

is_disjoint <- function(x, y) length(intersect(x, y)) == 0L

on_failure(is_disjoint) <- function(call, env) {
  paste0("`", deparse(call$x), "` and `", deparse(call$y),
         "` have a nonempty intersection")
}

not_na <- function(x) length(x) == 1L && !is.na(x)

on_failure(not_na) <- function(call, env) {
  paste0("`", deparse(call$x), "` is not a length 1 non-NA value")
}

no_na <- function(x) !anyNA(x)

on_failure(no_na) <- function(call, env) {
  paste0("`", deparse(call$x), "` contains at least 1 NA value")
}

all_na <- function(x) all(is.na(x))

on_failure(all_na) <- function(call, env) {
  paste0("`", deparse(call$x), "` contains at least 1 non NA value")
}

not_null <- function(x) !is.null(x)

on_failure(not_null) <- function(call, env) {
  paste0("`", deparse(call$x), "` is NULL")
}

null_or_na <- function(x) is.null(x) || is.na(x)

on_failure(null_or_na) <- function(call, env) {
  paste0("`", deparse(call$x), "` is neither NULL nor NA")
}

same_ts <- function(x, y) {
  identical(id(x), id(y)) && identical(index(x), index(y)) &&
    all.equal(interval(x), interval(y))
}

on_failure(same_ts) <- function(call, env) {
  paste0("`", deparse(call$x), "` and `", deparse(call$y),
         "` differ in id, index and/or interval")
}

has_interval <- function(x, col, interval) {
  all(is.na(x[[col]]) |
    as.double(x[[col]]) %% as.double(interval, units = units(x[[col]])) == 0)
}

on_failure(has_interval) <- function(call, env) {
  paste0("column `", eval(call$col, env), "` of ", deparse(call$x),
         " does not conform to an interval of ",
         format(eval(call$interval, env)))
}

is_scalar <- function(x, allow_null = FALSE) {
  is.atomic(x) && length(x) == 1L && !(is.null(x) && allow_null)
}

on_failure(is_scalar) <- function(call, env) {
  paste0(deparse(call$x), "is not scalar (i.e. atomic",
    if (eval(call$col, env)) ", not NULL" else "", " and length 1)")
}

null_or <- function(x, what, ...) {
  is.null(x) || what(x, ...)
}

on_failure(null_or) <- function(call, env) {
  paste0(deparse(call$x), " is neither NULL, nor ", deparse(call$what))
}

all_is <- function(x, what, ...) all(lgl_ply(x, what, ...))

on_failure(null_or) <- function(call, env) {
  paste0("not all of ", deparse(call$x), " satisfy ", deparse(call$what))
}
