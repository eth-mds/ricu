
#' @export
ts_def <- function(...) new_ts_def(list(...))

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
as_ts_def.ts_meta <- function(x) ts_def(x)

#' @export
c.ts_def <- function(x, ...) {

  lst <- list(...)
  meta <- vapply(lst, Negate(is_ts_def), logical(1L))
  if (any(meta)) lst[meta] <- lapply(lst[meta], list)

  new_ts_def(c(unclass(x), lst))
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
`[[.ts_def` <- function(x, i, ...) {
  if (is.string(i)) x[[which(vapply(i, inherits, logical(1L), i))]]
  else NextMethod()
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
is_required <- function(x, ...) UseMethod()

#' @export
is_required.ts_meta <- function(x, ...) FALSE

#' @export
is_required.ts_index <- function(x, ...) TRUE

#' @export
is_required.ts_key <- function(x, ...) TRUE

#' @export
validate_meta.ts_index <- function(x, tbl, stop_req, warn_opt, ...) {

  col <- meta_cols(x)
  interv <- interval(x)
  unit <- units(tbl[[col]])

  validation_helper(x, stop_req, warn_opt,
    has_col(tbl, col), identical(col, last_elem(data.table::key(tbl))),
    is_time(interv, allow_neg = FALSE), length(interv) == 1L,
    all(as.double(tbl[[col]]) %% as.double(interv, units = unit) == 0)
  )
}

#' @export
validate_meta.ts_key <- function(x, tbl, stop_req, warn_opt, ...) {

  cols <- meta_cols(x)

  validation_helper(x, stop_req, warn_opt,
    has_cols(tbl, cols),
    identical(cols, head(data.table::key(tbl), n = length(cols)))
  )
}

#' @export
validate_meta.ts_uncertainty <- function(x, tbl, stop_req, warn_opt, ...) {
  browser()
}

#' @export
validate_meta.ts_meta <- function(x, ...) {
  stop("Please add a `validate_meta` method for `", class(x)[1L], "` classes.")
}

#' @export
rm_cols.ts_meta <- function(x, cols, ...) {

  new_cols <- setdiff(x[["meta_cols"]], cols)

  if (length(new_cols) == 0L) return(NULL)

  x[["meta_cols"]] <- new_cols

  x
}

#' @export
rm_cols.ts_uncertainty <- function(x, cols, ...) {

  hits <- (x[["meta_cols"]] %in% cols) | (x[["time_cols"]] %in% cols)

  if (all(hits)) return(NULL)

  clapply(x, `[`, !hits)
}



#' @export
meta_cols <- function(x, ...) UseMethod()

#' @export
meta_cols.ts_meta <- function(x, ...) x[["meta_cols"]]

#' @export
`meta_cols<-` <- function(x, ...) UseMethod()

#' @export
`meta_cols<-.ts_meta` <- function(x, value) x[["meta_cols"]] <- value

#' @export
all_cols <- function(x, ...) UseMethod()

#' @export
all_cols.ts_meta <- function(x, ...) list(meta_cols(x, ...))

#' @export
`all_cols<-` <- function(x, value) UseMethod()

#' @export
interval <- function(x, ...) UseMethod()

#' @export
time_cols <- function(x, tbl, ...) UseMethod()

#' @export
print.ts_meta <- function(x, ...) {
  cat_line(format(x, ...))
}

#' @export
format.ts_meta <- function(x, ...) format_ts_meta(x, meta_cols(x))


#' @export
interval.ts_index <- function(x, ...) x[["interval"]]


#' @export
format.ts_index <- function(x, ...) {
  format_ts_meta(x, meta_cols(x), format(interval(x), ...))
}


#' @export
format.ts_uncertainty <- function(x, ...) {
  format_ts_meta(x, meta_cols(x), format(interval(x), ...))
}

#' @export
time_cols.ts_uncertainty <- function(x, ...) x[["time_cols"]]

#' @export
all_cols.ts_uncertainty <- function(x, ...) list(meta_cols(x), time_cols(x))

format_class <- function(x) sub("^ts_", "", class(x)[1L])

format_body <- function(...) paste(..., sep = ", ", collapse = "; ")

format_ts_meta <- function(x, ...) {
  paste0(format_class(x), "<", format_body(...), ">")
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
