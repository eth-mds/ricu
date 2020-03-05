
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
    hirid = hirid_ts_quo,
    stop("Data source not recognized.")
  )

  fun(..., source = source)
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
    hirid = hirid_tbl_quo,
    stop("Data source not recognized.")
  )

  fun(..., source = source)
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
load_items <- function(source, table, item_col, items = NULL, names = NULL,
                       id_col = default_id_col(source, table),
                       time_col = default_time_col(source, table),
                       val_col = default_val_col(source, table),
                       patient_ids = NULL, callback = NULL, regex = FALSE,
                       unit = NULL, interval = hours(1L), ...) {

  extra_cols <- list(...)

  if (length(extra_cols) && is.null(callback)) {
    warning("`extra_cols` is only effective is `callback` is specified.")
  }

  info <- paste0("`", sort(unique(
    if (length(names)) names else if (length(items)) items else item_col
  )), "`", collapse = "\n  * ")

  message("loading from `", source, "::", table, "`\n  * ", info)

  if (length(items) == 0L) {

    if (length(val_col) > 0L) {
      warning("argument `val_col` is ignored then `items` is NULL.")
    }

    load_wide(item_col, id_col, time_col, extra_cols, names,
              patient_ids, unit, callback, source = source, table = table,
              interval = interval)

  } else if (isTRUE(regex)) {

    load_grep(items, item_col, id_col, time_col, val_col, extra_cols, names,
              patient_ids, unit, callback, source = source, table = table,
              interval = interval)

  } else {

    load_long(items, item_col, id_col, time_col, val_col, extra_cols, names,
              patient_ids, unit, callback, source = source, table = table,
              interval = interval)
  }
}

do_callback <- function(x, fun, unit, val, ...) {

  if (is.null(fun) || (!is.function(fun) && is.na(fun))) {
    return(x)
  }

  do.call(fun, c(list(x), list(unit = unit, val_col = val), ...))
}

map_names <- function(old, map_val, map_key) {

  map <- rep(map_val, length(map_key))
  names(map) <- map_key

  map[as.character(old)]
}

prep_args <- function(arg, items, names) {

  assert_that(same_length(names, items))

  if (is.null(arg)) {

    arg <- rep(list(arg), length(names))
    names(arg) <- names

  } else if (length(arg) == 1L) {

    arg <- rep(list(arg), length(names))
    names(arg) <- names

  } else if (same_length(arg, items) && is.null(names(arg))) {

    arg <- lapply(split(arg, names), unique)

  } else {

    arg <- as.list(arg)
  }

  assert_that(is.list(arg), all(lengths(arg) <= 1L), has_name(arg, names),
              same_length(arg, unique(names)))

  arg
}

add_unit <- function(x, unit) {

  if (is.null(unit) || is.na(unit)) {
    return(x)
  }

  col <- data_cols(x)

  assert_that(is.string(unit), is.string(col))

  setattr(x[[col]], "units", unit)

  x
}

load_wide <- function(item_cols, id_col, time_col, extra_cols, names,
                      patient_ids, unit, callback, ...) {

  assert_that(length(item_cols) > 0L)

  if (is.null(names)) {
    names <- item_cols
  } else if (length(names) == 1L) {
    names <- rep(names, length(item_cols))
  }

  callback <- prep_args(callback, item_cols, names)
  unit     <- prep_args(unit, item_cols, names)

  to_rm <- unique(unlist(extra_cols))

  dat <- data_ts_quo(cols = c(item_cols, to_rm), id_cols = id_col,
                     time_col = time_col, ...)

  if (!is.null(patient_ids)) {
    join <- prepare_patient_ids(patient_ids, key(dat))
    dat  <- merge(dat, join, by = key(dat), all = FALSE)
  }

  if (all_null(callback)) {

    dat <- rm_cols(dat, to_rm)
    dat <- rename_cols(dat, names, item_cols)
    dat <- unmerge(dat, names, na_rm = FALSE)

  } else {

    dat <- unmerge(dat, item_cols, c(id_cols(dat), to_rm), FALSE)
    dat <- Map(do_callback,
      dat, callback[names(dat)], unit[names(dat)], names(dat),
      MoreArgs = c(list(id_col = id_col, time_col = time_col), extra_cols)
    )
    dat <- lapply(dat, rm_cols, to_rm)

    dat <- Map(rename_cols, dat, names, item_cols)
    dat <- combine_feats(dat)
  }

  dat <- lapply(dat, rm_na)

  if (!is.null(unit)) {
    dat <- Map(add_unit, dat, unit[names(dat)])
  }

  names(dat) <- NULL

  dat
}

load_long <- function(items, item_col, id_col, time_col, val_col, extra_cols,
                      names, patient_ids, unit, callback, ...) {

  assert_that(length(items) > 0L, is.string(item_col), is.string(val_col))

  if (is.null(names)) {

    names <- items
    has_names <- FALSE

  } else {

    if (length(names) == 1L) {
      names <- rep(names, length(items))
    }

    has_names <- TRUE
  }

  callback <- prep_args(callback, items, names)
  unit     <- prep_args(unit, items, names)

  uq_items <- unique(items)
  uq_extra <- unique(unlist(extra_cols))

  if (length(uq_items) == 1L) {

    lst <- list(col = as.name(item_col), id = uq_items, is_fun = is_val)
    query <- substitute(is_fun(col, id), lst)

  } else {

    lst <- list(col = as.name(item_col), id = uq_items)
    query <- substitute(col %in% id, lst)
  }

  dat <- data_ts_quo(row_quo = query, cols = c(item_col, val_col, uq_extra),
                     id_cols = id_col, time_col = time_col, ...)

  if (!is.null(patient_ids)) {
    join <- prepare_patient_ids(patient_ids, key(dat))
    dat  <- merge(dat, join, by = key(dat), all = FALSE)
  }

  if (nrow(dat) == 0L) {
    dat <- rm_cols(dat, c(uq_extra,
                          if (!identical(val_col, item_col)) item_col))
    return(list(dat))
  }

  split_col <- new_names(dat)

  if (has_names) {
    dat <- dat[, c(split_col) := map_names(get(item_col), names, items)]
  } else {
    dat <- dat[, c(split_col) := get(item_col)]
  }

  if (!identical(val_col, item_col)) {
    dat <- rm_cols(dat, item_col)
  }

  dat <- split(dat, by = split_col, keep.by = FALSE)

  if (!all_null(callback)) {

    dat <- Map(do_callback, dat, callback[names(dat)], unit[names(dat)],
      MoreArgs = c(list(val = val_col, id_col = id_col, time_col = time_col),
                   extra_cols)
    )
  }

  dat <- lapply(dat, rm_cols, extra_cols)
  dat <- Map(rename_cols, dat, names(dat), val_col)

  if (!is.null(callback)) {
    dat <- combine_feats(dat)
  }

  dat <- lapply(dat, rm_na)

  if (!is.null(unit)) {
    dat <- Map(add_unit, dat, unit[names(dat)])
  }

  names(dat) <- NULL

  dat
}

load_grep <- function(items, item_col, id_col, time_col, val_col, extra_cols,
                      names, patient_ids, unit, callback, ...) {

  prep_arg <- function(x) {

    if (is.null(x)) return(x)

    assert_that(length(x) == 1L)

    if (is.list(x)) x <- x[[1L]]

    x
  }

  if (is.null(names)) {
    names <- val_col
  }

  assert_that(is.string(names))

  callback <- prep_arg(callback)
  unit     <- prep_arg(unit)

  uq_extra <- unique(unlist(extra_cols))

  to_rm <- c(uq_extra, if (!identical(val_col, item_col)) item_col)

  lst <- list(col = as.name(item_col),
              id = paste(unique(items), collapse = "|"))
  query <- substitute(grepl(id, col, ignore.case = TRUE), lst)

  dat <- data_ts_quo(row_quo = query, cols = c(item_col, val_col, uq_extra),
                     id_cols = id_col, time_col = time_col, ...)

  if (!is.null(patient_ids)) {
    patient_ids <- prepare_patient_ids(patient_ids, key(dat))
    dat  <- merge(dat, join, by = key(dat), all = FALSE)
  }

  if (nrow(dat) == 0L) {
    return(list(rm_cols(dat, to_rm)))
  }

  if (is.function(callback) || is.string(callback)) {

    dat <- do.call(callback,
      c(list(dat), list(unit = unit, val_col = val_col, id_col = id_col,
                        time_col = time_col), extra_cols)
    )
  }

  dat <- rm_cols(dat, to_rm)
  dat <- rename_cols(dat, names, val_col)
  dat <- rm_na(dat)
  dat <- add_unit(dat, unit)

  list(dat)
}
