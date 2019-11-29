
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
    if (sum(hits) == 0L) NULL else x[[which(hits)]]
  } else NextMethod()
}

#' @export
is_ts_meta <- function(x) inherits(x, "ts_meta")

#' @export
c.ts_meta <- function(x, ...) c_ts_def(as_ts_def(x), list(...))

#' @export
print.ts_def <- function(x, ...) cat_line(format(x, ...))

#' @export
print.ts_meta <- function(x, ...) cat_line(format(x, ...))

#' @export
new_ts_index <- function(col, interval) {

  assert_that(is.string(col), !is.na(col), is_time(interval))

  structure(list(list(meta_col = col, aux_data = interval)),
            class = c("ts_index", "ts_meta"))
}

#' @export
new_ts_key <- function(cols) {

  assert_that(is.character(cols), length(cols) > 0L)

  structure(Map(list, meta_col = as.list(cols)),
            class = c("ts_key", "ts_meta"))
}

#' @export
new_ts_window <- function(time_cols, delta_cols = NA, delta_vals = NA) {

  assemble <- function(tim, dco, dva) {

    if (is.na(dco)) dco <- NA_character_
    if (is.na(dva)) dva <- hours(NA_integer_)

    assert_that(is.string(tim), is.string(dco), is_time(dva),
                !is.na(tim), xor_na(dco, dva))

    list(meta_col = tim, aux_col = dco, aux_data = dva)
  }

  structure(Map(assemble, as.list(time_cols), delta_cols, delta_vals),
            class = c("ts_window", "ts_meta"))
}

#' @export
new_ts_unit <- function(val_cols, unit_cols = NA, unit_vals = NA) {

  assemble <- function(vco, uco, uva) {

    if (is.na(uco)) uco <- NA_character_
    if (is.na(uva)) uva <- NA_character_

    assert_that(is.string(vco), is.string(uco), is_time(uva),
                !is.na(vco), xor_na(uco, uva))

    list(meta_col = vco, aux_col = uco, aux_data = uva)
  }

  structure(Map(assemble, as.list(val_cols), unit_cols),
            class = c("ts_unit", "ts_meta"))
}

#' @export
new_ts_date <- function(time_cols, ind_cols = NA) {

  assemble <- function(tco, ico) {

    if (is.na(ico)) ico <- NA_character_

    assert_that(is.string(tco), is.string(ico))

    list(meta_col = tco, aux_col = ico)
  }

  structure(Map(assemble, as.list(time_cols), ind_cols),
            class = c("ts_date", "ts_meta"))
}

#' @export
is_ts_index <- function(x) inherits(x, "ts_index")

#' @export
is_ts_key <- function(x) inherits(x, "ts_key")

#' @export
is_ts_window <- function(x) inherits(x, "ts_window")

#' @export
meta_names <- function(x) UseMethod("meta_names", x)

#' @export
meta_names.ts_def <- function(x) {

  res <- lapply(x, meta_names)

  names(res) <- vapply(x, format_class, character(1L))

  res
}

#' @export
meta_names.ts_meta <- function(x) vapply(x, `[[`, character(1L), "meta_col")

`meta_names<-` <- function(x, value) {
  Map(function(x, value) `[[<-`(x, "meta_col", value), x, value)
}

#' @export
is_required <- function(x, ...) UseMethod("is_required", x)

#' @export
is_required.ts_meta <- function(x, ...) FALSE

#' @export
is_required.ts_index <- function(x, ...) TRUE

#' @export
is_required.ts_key <- function(x, ...) TRUE

has_aux_names <- function(x) !is.null(x[[1L]][["aux_col"]])

#' @export
aux_names <- function(x) UseMethod("aux_names", x)

#' @export
aux_names.ts_def <- function(x) {

  res <- lapply(x, aux_names)

  names(res) <- vapply(x, format_class, character(1L))

  res
}

#' @export
aux_names.ts_meta <- function(x) {

  if (!has_aux_names(x)) return(rep(NA_character_, length(x)))

  res <- vapply(x, `[[`, character(1L), "aux_col")
  names(res) <- meta_names(x)

  res
}

`aux_names<-` <- function(x, value) {
  Map(function(x, value) `[[<-`(x, "aux_col", value), x, value)
}

has_aux_data <- function(x) !is.null(x[[1L]][["aux_data"]])

#' @export
aux_data <- function(x) UseMethod("aux_data", x)

#' @export
aux_data.ts_def <- function(x) {

  res <- lapply(x, aux_data)

  names(res) <- vapply(x, format_class, character(1L))

  res
}

#' @export
aux_data.ts_meta <- function(x) {

  if (!has_aux_data(x)) return(NULL)

  res <- lapply(x, `[[`, "aux_data")
  names(res) <- meta_names(x)

  res
}

#' @export
validate_def.ts_def <- function(x, tbl, stop_req = TRUE, warn_opt = TRUE,
                                ...) {

  vapply(x, validate_def, logical(1L), tbl, stop_req, warn_opt)
}

#' @export
validate_def.ts_index <- function(x, tbl, stop_req = TRUE, warn_opt = TRUE,
                                  ...) {

  col <- meta_names(x)
  interv <- interval(x)

  validation_helper(x, stop_req, warn_opt,
    has_col(tbl, col),
    identical(col, last_elem(data.table::key(tbl))),
    is_time(interv, allow_neg = FALSE),
    length(interv) == 1L,
    all(as.double(tbl[[col]]) %%
        as.double(interv, units = units(tbl[[col]])) == 0)
  )
}

#' @export
validate_def.ts_key <- function(x, tbl, stop_req = TRUE, warn_opt = TRUE,
                                ...) {

  cols <- meta_names(x)

  validation_helper(x, stop_req, warn_opt,
    has_cols(tbl, cols),
    identical(cols, head(data.table::key(tbl), n = length(cols)))
  )
}

#' @export
validate_def.ts_window <- function(x, tbl, stop_req = TRUE, warn_opt = TRUE,
                                   ...) {

  validation_helper(x, stop_req, warn_opt,
    has_time_cols(tbl, c(meta_names(x), aux_names(x)))
  )
}

#' @export
validate_def.ts_date <- function(x, tbl, stop_req = TRUE, warn_opt = TRUE,
                                 ...) {

  is_lgl <- function(col) is.logical(tbl[[col]])

  aux <- aux_names(x)
  aux <- aux[!is.na(aux)]

  validation_helper(x, stop_req, warn_opt,
    has_cols(tbl, c(meta_names(x), aux)),
    all_fun(aux, is_lgl)
  )
}

#' @export
validate_def.ts_meta <- function(x, tbl, stop_req = TRUE, warn_opt = TRUE,
                                 ...) {

  validation_helper(x, stop_req, warn_opt,
    has_cols(tbl, c(meta_names(x), aux_names(x)))
  )
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
      return(NA)
    } else {
      if (warn_opt) msg_fun(warning, res)
      return(FALSE)
    }
  }

  TRUE
}

#' @export
rm_cols.ts_index <- function(x, cols, ...) {

  if (meta_names(x) %in% cols) {
    stop("Cannot remove the only column that defines an index.")
  }

  x
}

#' @export
rm_cols.ts_key <- function(x, cols, ...) {
  new_ts_key(setdiff(meta_names(x), cols))
}

#' @export
rm_cols.ts_meta <- function(x, cols, ...) {

  hits <- meta_names(x) %in% cols

  if (has_aux_names(x)) {
    hits <- hits | (aux_names(x) %in% cols)
  }

  if (all(hits)) return(NULL)
  else structure(x[!hits], class = class(x))
}

#' @export
rename_cols.ts_meta <- function(x, new, old, ...) {

  meta_names(x) <- replace_with(meta_names(x), old, new)

  if (has_aux_names(x)) {
    aux_names(x) <- replace_with(aux_names(x), old, new)
  }

  x
}

#' @export
format.ts_def <- function(x, ...) {
  vap <- function(fun) vapply(x, fun, character(1L))
  req <- function(x) if(is_required(x)) "*" else ""
  paste0(vap(format_class), vap(req), vap(format), collapse = ", ")
}

#' @export
format.ts_meta <- function(x, ...) {
  args <- c(
    meta_names(x),
    if (has_aux_names(x)) aux_names(x),
    if (has_aux_data(x)) vapply(aux_data(x), format, character(1L))
  )
  do.call(format_ts_meta, as.list(args))
}

format_class <- function(x) sub("^ts_", "", class(x)[1L])

format_ts_meta <- function(...) {
  paste0("<", paste(..., sep = ", ", collapse = "; "), ">")
}

#' @export
index.ts_def <- function(x) index(x[["ts_index"]])

#' @export
index.ts_index <- meta_names

#' @export
key.ts_def <- function(x) key(x[["ts_key"]])

#' @export
key.ts_key <- meta_names

#' @export
interval.ts_def <- function(x) interval(x[["ts_index"]])

#' @export
interval.ts_index <- function(x) aux_data(x)[[1L]]

#' @export
any_date <- function(x, col) is_any_date_helper(x, col, 1L)

#' @export
is_date <- function(x, col) is_any_date_helper(x, col, nrow(x))

is_any_date_helper <- function(x, col, length) {

  assert_that(is_ts_tbl(x), is.string(col))

  date <- ts_def(x)[["ts_date"]]

  if (is.null(date)) return(rep(FALSE, length))

  hits <- col == meta_names(date)

  if (sum(hits) == 0L) {

    rep(FALSE, length)

  } else if (sum(hits) == 1L) {

    aux <- aux_names(date)[[hits]]

    if (is.na(aux)) {
      rep(TRUE, length)
    } else {
      if (length == 1L) any(x[[aux]]) else x[[aux]]
    }
  } else {
    stop("Only a single `ts_date` entry is allowed per column.")
  }
}

