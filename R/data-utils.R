
#' @export
data_ts <- function(source, table, row_expr, ...) {
  data_ts_quo(source, table, null_or_subs(row_expr), ...)
}

#' @export
data_ts_quo <- function(source, ...) {

  fun <- switch(
    as_src(source),
    mimic = mimic_ts_quo,
    eicu  = eicu_ts_quo,
    hirid = stop("TODO"),
    stop("Data source not recognized.")
  )

  fun(..., envir = source)
}

#' @export
data_tbl <- function(source, table, row_expr, ...) {
  data_tbl_quo(source, table, null_or_subs(row_expr), ...)
}

#' @export
data_tbl_quo <- function(source, ...) {

  fun <- switch(
    as_src(source),
    mimic = mimic_tbl_quo,
    eicu  = eicu_tbl_quo,
    hirid = stop("TODO"),
    stop("Data source not recognized.")
  )

  fun(..., envir = source)
}

prepare_patient_ids <- function(x, key) {

  res <- if (inherits(x, "data.frame")) {

    assert_that(has_name(x, key))

    if (inherits(x, "data.table")) x[, key, with = FALSE]
    else                           x[, key]

  } else {

    assert_that(is.atomic(x))
    setnames(setDT(list(x)), key)
  }

  unique(res)
}

#' @export
load_items <- function(source, table, item_col, items, names, id_col,
                       time_col, val_col, patient_ids = NULL,
                       resolvers = NULL, interval = hours(1L), ...) {

  extract_col <- function(col, x) x[, c(id_cols(dat), col), with = FALSE]

  rm_na <- function(x, col) x[!is.na(get(col)), ]

  map_names <- function(old) stats::setNames(names, items)[as.character(old)]

  resolve <- function(x, fun) {
    if (!is.null(fun) && !length(extra_cols)) fun(x)
    else if (!is.null(fun)) do.call(fun, c(list(x), extra_cols))
    else x
  }

  extra_cols <- list(...)

  if (is.null(item_col)) {
    query <- NULL
  } else {
    query <- substitute(col %in% id, list(col = as.name(item_col), id = items))
  }

  if (length(extra_cols) && is.null(resolvers)) {
    warning("`extra_cols` is only effective is `resolvers` is specified.")
  }

  message("loading from `", source, "::", table, "`\n  * ",
          paste0("`", unique(names), "`", collapse = "\n  * "))

  dat <- data_ts_quo(source, table = table, row_quo = query,
                     cols = c(item_col, val_col, unlist(extra_cols)),
                     id_cols = id_col, time_col = time_col,
                     interval = interval)

  if (!is.null(patient_ids)) {

    join <- prepare_patient_ids(patient_ids, key(dat))
    dat  <- merge(dat, join, by = key(dat), all = FALSE)
  }

  if (is.null(query)) {

    if (!is.null(resolvers)) dat <- resolvers(dat)
    if (length(extra_cols)) dat <- rm_cols(dat, unlist(extra_cols))

    dat <- rename_cols(dat, names, items)
    dat <- lapply(names, extract_col, dat)
    names(dat) <- names

  } else {

    if (is.null(resolvers)) {

      if (length(extra_cols)) {
        dat <- rm_cols(dat, unlist(extra_cols))
      }

      dat <- dat[, c(item_col) := map_names(get(item_col))]
      dat <- split(dat, by = item_col, keep.by = FALSE)
      dat <- Map(rename_cols, dat, names(dat), val_col)

    } else {

      if (is.function(resolvers)) {
        resolvers <- rep(list(resolvers), length(items))
      }

      if (is.null(names(resolvers))) {
        names(resolvers) <- items
      }

      assert_that(is.list(resolvers), has_name(resolvers, items))

      dat <- split(dat, by = item_col, keep.by = FALSE)
      dat <- Map(resolve, dat, resolvers[names(dat)])
      dat <- lapply(dat, rm_cols, unlist(extra_cols))
      dat <- Map(rename_cols, dat, map_names(names(dat)), val_col)
      dat <- combine_feats(dat)
    }
  }

  Map(rm_na, dat, vapply(dat, data_cols, character(1L)))
}
