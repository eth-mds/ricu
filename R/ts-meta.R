
#' @export
new_ts_def <- function(x) {

  assert_that(is.list(x))

  null <- vapply(x, is.null, logical(1L))

  if(any(null)) x <- x[!null]

  assert_that(all_fun(x, is_ts_meta), all_fun(x, Negate(is_ts_def)))

  classes <- lapply(x, class)

  assert_that(length(classes) == length(unique(classes)))

  structure(x, class = c("ts_def", "ts_meta"))
}

#' @export
is_ts_def <- function(x) inherits(x, "ts_def")

#' @export
as_ts_def <- function(x) UseMethod("as_ts_def", x)

#' @export
as_ts_def.list <- function(x) new_ts_def(x)

#' @export
as_ts_def.ts_def <- function(x) x

#' @export
as_ts_def.ts_meta <- function(x) new_ts_def(list(x))

#' @export
c.ts_def <- function(x, ...) c_ts_def(x, list(...))

c_ts_def <- function(x, lst) {
  new_ts_def(c(unclass(x), unlist(lapply(lst, as_ts_def), recursive = FALSE)))
}

#' @export
`[.ts_def` <- function(x, i, ...) {
  new_ts_def(NextMethod())
}

#' @export
`[[.ts_def` <- function(x, i, ...) {
  if (is.character(i)) {
    hits <- vapply(x, inherits, logical(1L), i)
    if (sum(hits) == 1L) x[[which(hits)]]
    else NULL
  } else NextMethod()
}

#' @export
is_ts_meta <- function(x) inherits(x, "ts_meta")

#' @export
c.ts_meta <- function(x, ...) c_ts_def(as_ts_def(x), list(...))

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
new_ts_window <- function(meta_cols, time_cols, direction = "forwards") {

  assert_that(is.character(meta_cols), is.character(time_cols),
              length(meta_cols) > 0L, same_length(meta_cols, time_cols))

  assert_that(length(direction) > 0L, all(direction %in% ts_window_dirs))

  if (length(direction) == 1L) direction <- rep(direction, length(meta_cols))

  assert_that(length(direction) == length(meta_cols))

  structure(list(meta_cols = meta_cols, time_cols = time_cols,
                 direction = direction),
            class = c("ts_window", "ts_meta"))
}

ts_window_dirs <- c("forwards", "backwards", "both")

#' @export
new_ts_unit <- function(val_cols, unit_cols) {

  assert_that(is.character(val_cols), is.character(unit_cols),
              length(val_cols) > 0L, same_length(val_cols, unit_cols))

  structure(list(meta_cols = val_cols, unit_cols = unit_cols),
            class = c("ts_unit", "ts_meta"))
}

#' @export
is_ts_index <- function(x) inherits(x, "ts_index")

#' @export
is_ts_key <- function(x) inherits(x, "ts_key")

#' @export
is_ts_window <- function(x) inherits(x, "ts_window")

#' @export
col_names <- function(x) UseMethod("col_names", x)

#' @export
col_names.ts_def <- function(x) {

  res <- lapply(x, col_names)

  names(res) <- vapply(x, format_class, character(1L))

  res
}

#' @export
col_names.ts_meta <- function(x) x[["meta_cols"]]

#' @export
is_required <- function(x, ...) UseMethod("is_required", x)

#' @export
is_required.ts_meta <- function(x, ...) FALSE

#' @export
is_required.ts_index <- function(x, ...) TRUE

#' @export
is_required.ts_key <- function(x, ...) TRUE

#' @export
validate_def.ts_def <- function(x, tbl, stop_req = TRUE, warn_opt = TRUE,
                                ...) {

  vapply(x, validate_def, logical(1L), tbl, stop_req, warn_opt)
}

#' @export
validate_def.ts_index <- function(x, tbl, stop_req = TRUE, warn_opt = TRUE,
                                  ...) {

  col <- col_names(x)
  interv <- interval(x)
  unit <- units(tbl[[col]])

  validation_helper(x, stop_req, warn_opt,
    has_col(tbl, col),
    identical(col, last_elem(data.table::key(tbl))),
    is_time(interv, allow_neg = FALSE),
    length(interv) == 1L,
    all(as.double(tbl[[col]]) %% as.double(interv, units = unit) == 0)
  )
}

#' @export
validate_def.ts_key <- function(x, tbl, stop_req = TRUE, warn_opt = TRUE,
                                ...) {

  cols <- col_names(x)

  validation_helper(x, stop_req, warn_opt,
    has_cols(tbl, cols),
    identical(cols, head(data.table::key(tbl), n = length(cols)))
  )
}

#' @export
validate_def.ts_window <- function(x, tbl, stop_req = TRUE, warn_opt = TRUE,
                                   ...) {

  cols <- col_names(x)
  aux <- x[["time_cols"]]

  validation_helper(x, stop_req, warn_opt,
    has_time_cols(tbl, cols),
    has_time_cols(tbl, aux, allow_neg = FALSE)
  )
}

#' @export
validate_def.ts_unit <- function(x, tbl, stop_req = TRUE, warn_opt = TRUE,
                                 ...) {

  validation_helper(x, stop_req, warn_opt,
    has_cols(tbl, c(col_names(x), x[["unit_cols"]]))
  )
}

#' @export
validate_def.ts_meta <- function(x, ...) {
  stop("Please add a `validate_def` method for `", class(x)[1L], "` classes.")
}

validation_helper <- function(x, stop_req, warn_opt, ...,
                              env = parent.frame()) {

  msg_fun <- function(fun, msg) {
    fun("Error validating ", format(x), ":\n  ", msg)
  }

  res <- validate_that(..., env = env)

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
rm_cols.ts_window <- function(x, cols, ...) {

  meta <- col_names(x)
  times <- x[["time_cols"]]

  hits <- (meta %in% cols) | (times %in% cols)

  if (all(hits)) return(NULL)

  new_ts_window(meta[!hits], times[!hits], x[["direction"]][!hits])
}

#' @export
rm_cols.ts_unit <- function(x, cols, ...) {

  meta <- col_names(x)
  unit <- x[["unit_cols"]]

  hits <- (meta %in% cols) | (unit %in% cols)

  if (all(hits)) return(NULL)

  new_ts_unit(meta[!hits], unit[!hits])
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
rename_cols.ts_window <- function(x, new, old, ...) {

  x <- NextMethod()

  x[["time_cols"]] <- replace_with(x[["time_cols"]], old, new)

  x
}

#' @export
rename_cols.ts_unit <- function(x, new, old, ...) {

  x <- NextMethod()

  x[["unit_cols"]] <- replace_with(x[["unit_cols"]], old, new)

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
format.ts_window <- function(x, ...) {
  dir <- replace_with(x[["direction"]], ts_window_dirs,
                      c("+", "-", "+/-"))
  format_ts_meta(x, col_names(x), paste0(dir, x[["time_cols"]]))
}

#' @export
format.ts_unit <- function(x, ...) {
  format_ts_meta(x, col_names(x), x[["unit_cols"]])
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
