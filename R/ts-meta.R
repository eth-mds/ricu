
#' @export
new_ts_index <- function(col, interval) {

  assert_that(is.string(col), !is.na(col), is_time(interval))

  structure(list(list(meta_col = col, aux_data = interval)),
            class = c("ts_index", "ts_meta"))
}

#' @export
new_ts_key <- function(cols) {

  assert_that(is.character(cols), length(cols) > 0L, !anyNA(cols),
              is_unique(cols))

  structure(Map(list, meta_col = as.list(cols)),
            class = c("ts_key", "ts_meta"))
}

#' @export
new_ts_unit <- function(val_cols, unit_cols = NA, unit_vals = NA) {

  assemble <- function(vco, uco, uva) {

    if (is.na(uco)) uco <- NA_character_
    if (is.na(uva)) uva <- NA_character_

    assert_that(is.string(vco), is.string(uco), is.string(uva),
                !is.na(vco), xor_na(uco, uva))

    list(meta_col = vco, aux_col = uco, aux_data = uva)
  }

  res <- structure(Map(assemble, as.list(val_cols), unit_cols, unit_vals),
                   class = c("ts_unit", "ts_meta"))

  met <- meta_names(res)
  aux <- aux_names(res)

  assert_that(is_unique(met), is_unique(aux), is_disjoint(met, aux))

  res
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
is_ts_meta <- function(x) inherits(x, "ts_meta")

#' @export
is_ts_index <- function(x) inherits(x, "ts_index")

#' @export
is_ts_key <- function(x) inherits(x, "ts_key")

#' @export
is_ts_window <- function(x) inherits(x, "ts_window")

#' @export
is_ts_unit <- function(x) inherits(x, "ts_unit")

#' @export
is_ts_date <- function(x) inherits(x, "ts_date")

#' @export
meta_names.ts_meta <- function(x) vapply(x, `[[`, character(1L), "meta_col")

`meta_names<-` <- function(x, value) {
  Map(function(x, value) `[[<-`(x, "meta_col", value), x, value)
}

#' @export
is_required.ts_meta <- function(x, ...) FALSE

#' @export
is_required.ts_index <- function(x, ...) TRUE

#' @export
is_required.ts_key <- function(x, ...) TRUE

#' @export
has_aux_names.ts_meta <- function(x) !is.null(x[[1L]][["aux_col"]])

#' @export
aux_names.ts_meta <- function(x, meta_col = NULL, allow_multiple = TRUE, ...) {

  if (!has_aux_names(x)) {
    res <- rep(NA_character_, length(x))
  } else {
    res <- vapply(x, `[[`, character(1L), "aux_col")
  }

  nms <- meta_names(x)

  if (is.null(meta_col)) {

    stats::setNames(res, nms)

  } else {

    assert_that(is.string(meta_col), is.flag(allow_multiple))

    hits <- meta_col == nms

    if (sum(hits) == 0L) {
      NULL
    } else if (sum(hits) == 1L) {
      res[[hits]]
    } else if (allow_multiple) {
      res[hits]
    } else {
      stop("If `!allow_multiple` a single entry is allowed per column.")
    }
  }

}

`aux_names<-` <- function(x, value) {
  Map(function(x, value) `[[<-`(x, "aux_col", value), x, value)
}

#' @export
has_aux_data.ts_meta <- function(x) !is.null(x[[1L]][["aux_data"]])

#' @export
aux_data.ts_meta <- function(x) {

  if (!has_aux_data(x)) return(NULL)

  res <- lapply(x, `[[`, "aux_data")
  names(res) <- meta_names(x)

  res
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
index.ts_index <- meta_names

#' @export
key.ts_key <- meta_names

#' @export
interval.ts_index <- function(x) aux_data(x)[[1L]]

#' @export
any_date <- function(x, col = index(x)) is_any_date_helper(x, col, 1L)

#' @export
is_date <- function(x, col = index(x)) is_any_date_helper(x, col, nrow(x))

is_any_date_helper <- function(x, col, length) {

  aux_col <- aux_names(x, "ts_date", col, FALSE)

  if (is.null(aux_col)) rep(FALSE, length)
  else if (is.na(aux_col)) rep(TRUE, length)
  else if (length == 1L) any(x[[aux_col]])
  else x[[aux_col]]
}

compact_unit <- function(x, col, handler = NULL, expected = NULL) {

  unit <- aux_names(x, "ts_unit", col, FALSE)

  assert_that(is.string(unit))

  unit <- ts_meta(x, "ts_unit")
  hits <- col == meta_names(unit)

  assert_that(!is.null(units), sum(hits) == 1L)

}

