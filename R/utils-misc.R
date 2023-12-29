
agg_or_na <- function(agg_fun) {
  function(x) {
    if (all(is.na(x))) return(x[1L])
    res <- agg_fun(x, na.rm = TRUE)
    if (is.na(res)) x[1L] else res
  }
}

#' Utility functions
#'
#' Several utility functions exported for convenience.
#'
#' @details
#' The two functions `min_or_na()` and `max_or_na()` overcome a design choice
#' of [base::min()] (or [base::max()]`) that can yield undesirable results. If
#' called on a vector of all missing values with `na.rm = TRUE`, `Inf` (and
#' `-Inf` respectively) are returned. This is changed to returning a missing
#' value of the same type as `x`.
#'
#' The functions `is_val()` and `not_val()` (as well as analogously
#' `is_true()` and `is_false()`) return logical vectors of the same length as
#' the value passed as `x`, with non-base R semanticists of comparing against
#' `NA`: instead of returning `c(NA, TRUE)` for `c(NA, 5) == 5`, `is_val()`
#' will return `c(FALSE TRUE)`. Passing `NA` as `val` might lead to unintended
#' results but no warning is thrown.
#'
#' Finally, `first_elem()` and `last_elem()` has the same semantics as
#' [utils::head()] and [utils::tail()] with `n = 1L` and `replace_na()` will
#' replace all occurrences of `NA` in `x` with `val` and can be called on both
#' objects inheriting from `data.table` in which case internally
#' [data.table::setnafill()] is called or other objects.
#'
#' @param x Object to use
#' @param val Value to compare against or to use as replacement
#'
#' @return
#' * `min_or_na()`/`max_or_na()`: scalar-valued extrema of a vector
#' * `is_val()`/`not_val()`/`is_true()`/`is_false()`: Logical vector of the
#'    same length as the object passed as `x`
#' * `first_elem()`/`last_elem()`: single element of the object passed as `x`
#' * `replace_na()`: modified version of the object passed as `x`
#'
#' @examples
#' some_na <- c(NA, sample(1:10, 5), NA)
#' identical(min(some_na, na.rm = TRUE), min_or_na(some_na))
#'
#' all_na <- rep(NA, 5)
#' min(all_na, na.rm = TRUE)
#' min_or_na(all_na)
#'
#' is_val(some_na, 5)
#' some_na == 5
#'
#' is_val(some_na, NA)
#'
#' identical(first_elem(letters), head(letters, n = 1L))
#' identical(last_elem(letters), tail(letters, n = 1L))
#'
#' replace_na(some_na, 11)
#' replace_na(all_na, 11)
#' replace_na(1:5, 11)
#'
#' tbl <- ts_tbl(a = 1:10, b = hours(1:10), c = c(NA, 1:5, NA, 8:9, NA))
#' res <- replace_na(tbl, 0)
#' identical(tbl, res)
#'
#' @rdname utils
#' @export
min_or_na <- agg_or_na(min)

#' @rdname utils
#' @export
max_or_na <- agg_or_na(max)

reduce <- function(f, x, ...) Reduce(function(x, y) f(x, y, ...), x)

round_to <- function(x, to = 1) {
  if (all_equal(to, 1)) floor(x) else to * floor(x / to)
}

#' @rdname utils
#' @export
is_val <- function(x, val) !is.na(x) & x == val

#' @rdname utils
#' @export
not_val <- function(x, val) !is.na(x) & x != val

val_or_na <- function(x, val) is.na(x) | x == val

#' @rdname utils
#' @export
is_true <- function(x) !is.na(x) & x

#' @rdname utils
#' @export
is_false <- function(x) !(is.na(x) | x)

#' @rdname utils
#' @export
last_elem <- function(x) x[length(x)]

#' @rdname utils
#' @export
first_elem <- function(x) x[1L]

null_or_subs <- function(x, where = parent.frame(1L)) {
  if (missing(x)) NULL else do.call("substitute", list(substitute(x), where))
}

new_names <- function(old_names = character(0L), n = 1L,
                      chars = c(letters, LETTERS, 0L:9L), length = 15L) {

  if (inherits(old_names, "data.frame")) {
    old_names <- colnames(old_names)
  }

  assert_that(
    is.null(old_names) || is.character(old_names),
    is.count(n), is.count(length),
    is.character(chars), length(chars) >= 1L
  )

  repeat{
    res <- replicate(n, paste(sample(chars, length), collapse = ""))
    if (length(res) == length(unique(res)) && !any(res %in% old_names)) break
  }

  res
}

chr_ply <- function(x, fun, ..., length = 1L, use_names = FALSE) {
  vapply(x, fun, character(length), ..., USE.NAMES = use_names)
}

lgl_ply <- function(x, fun, ..., length = 1L, use_names = FALSE) {
  vapply(x, fun, logical(length), ..., USE.NAMES = use_names)
}

int_ply <- function(x, fun, ..., length = 1L, use_names = FALSE) {
  vapply(x, fun, integer(length), ..., USE.NAMES = use_names)
}

dbl_ply <- function(x, fun, ..., length = 1L, use_names = FALSE) {
  vapply(x, fun, double(length), ..., USE.NAMES = use_names)
}

col_ply <- function(x, cols, fun, ply_fun = lgl_ply, ...) {
  ply_fun(cols, function(y, ...) fun(x[[y]], ...), ...)
}

lst_xtr <- function(x, i) lapply(x, `[[`, i)

chr_xtr <- function(x, i, length = 1L) chr_ply(x, `[[`, i, length = length)

chr_xtr_null <- function(x, i, length = 1L) {
  chr_ply(x, xtr_null, i, rep(NA_character_, length), length = length)
}

lgl_xtr <- function(x, i, length = 1L) lgl_ply(x, `[[`, i, length = length)

lgl_xtr_null <- function(x, i, length = 1L) {
  lgl_ply(x, xtr_null, i, rep(NA, length), length = length)
}

int_xtr <- function(x, i, length = 1L) int_ply(x, `[[`, i, length = length)

int_xtr_null <- function(x, i, length = 1L) {
  int_ply(x, xtr_null, i, rep(NA_integer_, length), length = length)
}

dbl_xtr <- function(x, i, length = 1L) dbl_ply(x, `[[`, i, length = length)

dbl_xtr_null <- function(x, i, length = 1L) {
  dbl_ply(x, xtr_null, i, rep(NA_real_, length), length = length)
}

xtr_null <- function(x, i, null_val) {
  if (is.null(res <- x[[i]])) null_val else res
}

lst_inv <- function(x) {
  nms <- sort(unique(unlist(lapply(x, names))))
  lapply(setNames(nms, nms), function(y) lst_xtr(x, y))
}

map <- function(f, ...) Map(f, ..., USE.NAMES = FALSE)

do_call <- function(x, fun, ...) do.call(fun, c(x, list(...)))

coalesce <- function(...) {
  for (i in seq_len(...length())) {
    x <- ...elt(i)
    if (is.null(x)) next else return(x)
  }
}

rep_arg <- function(arg, names) {

  if (is.count(names)) {
    len <- names
  } else {
    assert_that(is.character(names), has_length(names))
    len <- length(names)
  }

  if (!is.list(arg) && length(arg) <= 1L) {
    arg <- rep(list(arg), len)
  } else if (length(arg) != len && len == 1L) {
    arg <- list(arg)
  } else if (length(arg) != len) {
    arg <- as.list(arg)
  }

  assert_that(all_equal(length(arg), len))

  if (is.count(names)) {
    return(arg)
  }

  if (is.null(names(arg))) {
    names(arg) <- names
  } else {
    arg <- arg[names]
  }

  assert_that(identical(names(arg), names))

  arg
}

unlst <- function(x, recursive = FALSE, use_names = FALSE) {
  unlist(x, recursive = recursive, use.names = use_names)
}

unlst_str <- function(x) chr_ply(x, identity)

rep_along <- function(x, times) rep(x, length(times))

cat_line <- function(...) {
  line <- trimws(paste0(...), "right")
  cat(paste0(line, "\n"), sep = "")
}

ms_as_mins <- function(x) min_as_mins(as.integer(x / 6e4))

min_as_mins <- function(x) as.difftime(x, units = "mins")

digest_lst <- function(x) as.character(openssl::md5(serialize(x, NULL)))

digest <- function(...) digest_lst(list(...))

sys_name <- function() Sys.info()[["sysname"]]

sys_env <- function(...) Sys.getenv(...)

set_units <- function(x, value) units::set_units(x, value, mode = "standard")

set_names <- function(object = nm, nm) {
  names(object) <- nm
  object
}
