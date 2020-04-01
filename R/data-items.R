
#' @export
load_items <- function(source, table, item_col, items = NULL, names = NULL,
                       id_col = default_id_col(config = cfg),
                       time_col = default_time_col(table, config = cfg),
                       val_col = default_val_col(table, config = cfg),
                       unit_col = default_unit_col(table, config = cfg),
                       patient_ids = NULL, callback = NULL, regex = FALSE,
                       unit = NULL, interval = hours(1L),
                       cfg = get_col_config(source, "all"), ...) {

  extra_cols <- list(...)

  if (length(extra_cols) && is.null(callback)) {
    warning("`extra_cols` is only effective is `callback` is specified.")
  }

  info <- paste0("`", sort(unique(
    if (length(names)) names else if (length(items)) items else item_col
  )), "`", collapse = "\n  * ")

  message("loading from `", source, "::", table, "`\n  * ", info)

  if (length(items) == 0L) {

    load_wide(item_col, id_col, time_col, extra_cols, names,
              patient_ids, unit, callback, source = source, table = table,
              interval = interval, cfg = cfg)

  } else if (isTRUE(regex)) {

    load_grep(items, item_col, id_col, time_col, val_col, extra_cols, names,
              patient_ids, unit, callback, source = source, table = table,
              interval = interval, cfg = cfg)

  } else {

    load_long(items, item_col, id_col, time_col, val_col, extra_cols, names,
              patient_ids, unit, callback, source = source, table = table,
              interval = interval, cfg = cfg)
  }
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

prep_args <- function(arg, names, items = NULL) {

  is_ok <- function(x) {
    is.null(x) || (is.atomic(x) && length(x) == 1L) || is.function(x) ||
      is.language(x)
  }

  all_ok <- function(x) all(lgl_ply(x, is_ok))

  if (length(arg) <= 1L) {

    arg <- rep(list(arg), length(names))
    names(arg) <- names

  } else if (same_length(arg, items) && is.null(names(arg))) {

    assert_that(same_length(names, items))

    arg <- lapply(split(arg, names), unique)
    arg <- arg[names]

  } else {

    arg <- as.list(arg)

    if (is.null(names(arg))) {
      if (same_length(arg, names)) names(arg) <- names
    } else {
      arg <- arg[names]
    }

    assert_that(has_name(arg, names))
  }

  assert_that(is.list(arg), all_ok(arg))

  arg
}

add_unit <- function(x, unit) {

  if (is.null(unit) || is.na(unit)) {
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

  callback <- prep_args(callback, item_cols)
  unit_itm <- prep_args(unit, item_cols)
  unit_con <- prep_args(unit, names, item_cols)

  to_rm <- unique(unlist(extra_cols))

  if (is.null(time_col)) {
    dat <- data_id_quo(cols = c(item_cols, to_rm), id_col = id_col,
                       ...)
  } else {
    dat <- data_ts_quo(cols = c(item_cols, to_rm), id_col = id_col,
                       time_col = time_col, ...)
  }

  if (!is.null(patient_ids)) {
    join <- prepare_patient_ids(patient_ids, id(dat))
    dat  <- merge(dat, join, by = id(dat), all = FALSE)
  }

  if (all_null(callback)) {

    dat <- rm_cols(dat, to_rm)
    dat <- rename_cols(dat, names, item_cols)
    dat <- unmerge(dat, names, na_rm = FALSE)

  } else {

    dat <- unmerge(dat, item_cols, c(meta_cols(dat), to_rm), FALSE)
    dat <- Map(do_callback,
      dat, callback[names(dat)], unit_itm[names(dat)], names(dat),
      MoreArgs = c(list(id_col = id_col, time_col = time_col), extra_cols)
    )
    dat <- lapply(dat, rm_cols, to_rm)

    dat <- Map(rename_cols, dat, names, item_cols)
    dat <- combine_feats(dat)
  }

  dat <- lapply(dat, rm_na)

  if (!is.null(unit)) {
    dat <- Map(add_unit, dat, unit_con[names(dat)])
  }

  names(dat) <- NULL

  dat
}

load_long <- function(items, item_col, id_col, time_col, val_col, extra_cols,
                      names, patient_ids, unit, callback, ...) {

  assert_that(length(items) > 0L, is.string(item_col), is.string(val_col))

  if (is.null(names)) {
    names <- items
  } else if (length(names) == 1L) {
    names <- rep(names, length(items))
  }

  callback <- prep_args(callback, as.character(items))
  unit_itm <- prep_args(unit, as.character(items))
  names    <- prep_args(as.character(names), as.character(items))

  unit_con <- prep_args(unit, as.character(names), items)

  uq_items <- unique(items)
  uq_extra <- unique(unlist(extra_cols))

  if (length(uq_items) == 1L) {

    lst <- list(col = as.name(item_col), id = uq_items, is_fun = is_val)
    query <- substitute(is_fun(col, id), lst)

  } else {

    lst <- list(col = as.name(item_col), id = uq_items)
    query <- substitute(col %in% id, lst)
  }

  if (is.null(time_col)) {
    dat <- data_id_quo(row_quo = query, cols = c(item_col, val_col, uq_extra),
                       id_col = id_col, ...)
  } else {
    dat <- data_ts_quo(row_quo = query, cols = c(item_col, val_col, uq_extra),
                       id_col = id_col, time_col = time_col, ...)
  }

  if (!is.null(patient_ids)) {
    join <- prepare_patient_ids(patient_ids, id(dat))
    dat  <- merge(dat, join, by = id(dat), all = FALSE)
  }

  if (nrow(dat) == 0L) {
    dat <- rm_cols(dat, c(uq_extra,
                          if (!identical(val_col, item_col)) item_col))
    return(list(dat))
  }

  dat <- split(dat, by = item_col, keep.by = identical(val_col, item_col))
  dat <- dat[names(names)]

  ok  <- lgl_ply(dat, Negate(is.null))
  dat <- copy_dt_refs(dat[ok])

  if (!(all_null(callback) || all_na(callback))) {
    dat <- Map(do_callback, dat, callback[ok], unit_itm[ok],
      MoreArgs = c(list(val = val_col, id_col = id_col, time_col = time_col),
                   extra_cols)
    )
  }

  dat <- lapply(dat, rm_cols, extra_cols)
  dat <- Map(rename_cols, dat, names[ok], val_col)

  dat <- combine_feats(dat)
  dat <- lapply(dat, rm_na)

  if (!is.null(unit)) {
    dat <- Map(add_unit, dat, unit_con[chr_ply(dat, data_cols)])
  }

  names(dat) <- NULL

  dat
}

load_grep <- function(items, item_col, id_col, time_col, val_col, extra_cols,
                      names, patient_ids, unit, callback, ...) {

  prep_arg <- function(x) {

    if (is.null(x)) return(x)

    if (!is.function(x)) {
      x <- unique(x)
    }

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

  if (is.null(time_col)) {
    dat <- data_id_quo(row_quo = query, cols = c(item_col, val_col, uq_extra),
                       id_col = id_col, ...)
  } else {
    dat <- data_ts_quo(row_quo = query, cols = c(item_col, val_col, uq_extra),
                       id_col = id_col, time_col = time_col, ...)
  }

  if (!is.null(patient_ids)) {
    join <- prepare_patient_ids(patient_ids, id(dat))
    dat  <- merge(dat, join, by = id(dat), all = FALSE)
  }

  if (nrow(dat) == 0L) {
    return(list(rm_cols(dat, to_rm)))
  }

  if (is.function(callback) || is.string(callback)) {

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

copy_dt_refs <- function(x) {

  to_copy <- duplicated(chr_ply(x, data.table::address))

  if (any(to_copy)) {
    x[to_copy] <- lapply(x[to_copy], data.table::copy)
  }

  x
}
