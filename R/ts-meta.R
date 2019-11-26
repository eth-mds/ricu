
#' @export
new_ts_def <- function(x) {

  assert_that(is.list(x))

  null <- vapply(x, is.null, logical(1L))

  if(any(null)) x <- x[!null]

  assert_that(all_fun(x, is_ts_meta))

  classes <- lapply(x, class)

  assert_that(length(classes) == length(unique(classes)))

  structure(x, class = "ts_def", "ts_meta")
}

#' @export
is_ts_def <- function(x) inherits(x, "ts_def")

#' @export
as_ts_def <- function(x) UseMethod()

#' @export
as_ts_def.list <- function(x) new_ts_def(x)

#' @export
as_ts_def.ts_def <- function(x) x

#' @export
as_ts_def.ts_meta <- function(x) new_ts_def(list(x))

#' @export
c.ts_def <- function(x, ...) {

  lst <- list(...)
  meta <- vapply(lst, Negate(is_ts_def), logical(1L))
  if (any(meta)) lst[meta] <- lapply(lst[meta], list)

  new_ts_def(c(unclass(x), lst))
}

#' @export
`[[.ts_def` <- function(x, i, ...) {
  if (is.character(i)) x[[which(vapply(i, inherits, logical(1L), i))]]
  else NextMethod()
}

#' @export
is_ts_meta <- function(x) inherits(x, "ts_meta")

#' @export
c.ts_meta <- function(x, ...) {

  lst <- list(...)
  meta <- vapply(lst, Negate(is_ts_def), logical(1L))
  if (any(meta)) lst[meta] <- lapply(lst[meta], list)

  new_ts_def(c(list(x), lst))
}

#' @export
print.ts_def <- function(x, ...) {
  cat_line(format(x, ...))
}

#' @export
print.ts_meta <- function(x, ...) {
  cat_line(format(x, ...))
}

#' @export
new_ts_index <- function(col, interval) {

  assert_that(is.string(col), is_time(interval))

  structure(list(meta_cols = col, interval = interval),
            class = c("ts_index", "ts_meta"))
}

#' @export
new_ts_key <- function(cols) {

  assert_that(is.character(cols))

  structure(list(meta_cols = cols), class = c("ts_key", "ts_meta"))
}

#' @export
new_ts_uncertainty <- function(meta_cols, time_cols, direction = "forwards") {

  assert_that(is.character(meta_cols), is.character(time_cols),
              length(meta_cols) == length(time_cols))

  directions <- c("forwards", "backwards", "both")

  assert_that(length(direction) > 0L, all(direction %in% directions))

  if (length(direction) == 1L) direction <- rep(direction, length(meta_cols))

  assert_that(length(direction) == length(meta_cols))

  structure(list(meta_cols = meta_cols, time_cols = time_cols,
                 direction = direction),
            class = c("ts_uncertainty", "ts_meta"))
}

#' @export
is_ts_index <- function(x) inherits(x, "ts_index")

#' @export
is_ts_key <- function(x) inherits(x, "ts_key")

#' @export
is_ts_uncertainty <- function(x) inherits(x, "ts_uncertainty")

#' @export
col_names <- function(x) UseMethod()

#' @export
col_names.ts_def <- function(x) {

  res <- lapply(x, col_names)

  names(res) <- vapply(x, format_class, character(1L))

  res
}

#' @export
col_names.ts_meta <- function(x) x[["meta_cols"]]

#' @export
col_names.ts_uncertainty <- function(x) x[c("meta_cols", "time_cols")]

#' @export
is_required <- function(x, ...) UseMethod()

#' @export
is_required.ts_meta <- function(x, ...) FALSE

#' @export
is_required.ts_index <- function(x, ...) TRUE

#' @export
is_required.ts_key <- function(x, ...) TRUE

#' @export
validate_def.ts_index <- function(x, tbl, stop_req, warn_opt, ...) {

  col <- col_names(x)
  interv <- interval(x)
  unit <- units(tbl[[col]])

  validation_helper(x, stop_req, warn_opt,
    has_col(tbl, col), identical(col, last_elem(data.table::key(tbl))),
    is_time(interv, allow_neg = FALSE), length(interv) == 1L,
    all(as.double(tbl[[col]]) %% as.double(interv, units = unit) == 0)
  )
}

#' @export
validate_def.ts_key <- function(x, tbl, stop_req, warn_opt, ...) {

  cols <- col_names(x)

  validation_helper(x, stop_req, warn_opt,
    has_cols(tbl, cols),
    identical(cols, head(data.table::key(tbl), n = length(cols)))
  )
}

#' @export
validate_def.ts_uncertainty <- function(x, tbl, stop_req, warn_opt, ...) {

  cols <- col_names(x)
  aux <- x[["time_cols"]]

  directions <- c("forwards", "backwards", "both")

  validation_helper(x, stop_req, warn_opt,
    has_time_cols(x, c(cols, aux)), all(x[["direction"]] %in% directions),
    all(vapply(x, same_length, logical(1L), x[[1L]]))
  )
}

#' @export
validate_def.ts_meta <- function(x, ...) {
  stop("Please add a `validate_def` method for `", class(x)[1L], "` classes.")
}

validation_helper <- function(x, stop_req, warn_opt, ...) {

  msg_fun <- function(fun, msg) {
    fun("Error validating ", format(x), ":\n  ", msg)
  }

  res <- validate_that(..., env = parent.frame(2L))

  if (!isTRUE(res)) {
    if (is_required(x)) {
      if (stop_req) msg_fun(stop, res)
      res <- NA
    } else {
      if (warn_opt) msg_fun(warning, res)
      res <- FALSE
    }
  }

  res
}

#' @export
rm_cols.ts_index <- function(x, cols, ...) {

  if (col_names(x) %in% cols) {
    stop("Cannot remove the only column that defines an index.")
  }

  x
}

#' @export
rm_cols.ts_key <- function(x, cols, ...) {
  new_ts_key(setdiff(col_names(x), cols))
}

#' @export
rm_cols.ts_uncertainty <- function(x, cols, ...) {

  meta <- col_names(x)
  times <- x[["time_cols"]]

  hits <- (meta %in% cols) | (times %in% cols)

  if (all(hits)) return(NULL)

  new_ts_uncertainty(meta[!hits], times[!hits], x[["direction"]][!hits])
}

#' @export
rm_cols.ts_meta <- function(x, cols, ...) {
  stop("Please add a `rm_cols` method for `", class(x)[1L], "` classes.")
}

#' @export
rename_cols.ts_meta <- function(x, new, old, ...) {

  x[["meta_cols"]] <- replace_with(col_names(x), old, new)

  x
}

#' @export
rename_cols.ts_uncertainty <- function(x, new, old, ...) {

  x <- NextMethod()

  x[["time_cols"]] <- replace_with(x[["time_cols"]], old, new)

  x
}

#' @export
format.ts_def <- function(x, ...) {
  paste(vapply(x, format, character(1L)), collapse = ", ")
}

#' @export
format.ts_meta <- function(x, ...) format_ts_meta(x, col_names(x))

#' @export
format.ts_index <- function(x, ...) {
  format_ts_meta(x, col_names(x), format(interval(x)))
}

#' @export
format.ts_uncertainty <- function(x, ...) {
  dir <- replace_with(x[["direction"]], c("forwards", "backwards", "both"),
                      c("+", "-", "+/-"))
  format_ts_meta(x, col_names(x), paste0(dir, x[["direction"]]))
}

format_class <- function(x) sub("^ts_", "", class(x)[1L])

format_body <- function(...) paste(..., sep = ", ", collapse = "; ")

format_ts_meta <- function(x, ...) {
  paste0(format_class(x), if(is_required(x)) "*", "<", format_body(...), ">")
}

#' @export
index.ts_def <- function(x) index(x[["ts_index"]])

#' @export
index.ts_index <- col_names

#' @export
key.ts_def <- function(x) key(x[["ts_key"]])

#' @export
key.ts_key <- col_names

#' @export
interval.ts_def <- function(x) interval(x[["ts_index"]])

#' @export
interval.ts_index <- function(x) x[["interval"]]
