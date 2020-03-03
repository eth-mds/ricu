
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
    hirid = hirid_tbl_quo,
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
load_items <- function(source, table, item_col, items = NULL, names = NULL,
                       id_col = default_id_col(source, table),
                       time_col = default_time_col(source, table),
                       val_col = default_val_col(source, table),
                       patient_ids = NULL, callback = NULL, regex = FALSE,
                       unit = NULL, interval = hours(1L), ...) {

  extra_cols <- list(...)

  assert_that(is.string(source), is.string(table), is.flag(regex),
              is.string(id_col), is.string(time_col))

  if (length(extra_cols) && is.null(callback)) {
    warning("`extra_cols` is only effective is `callback` is specified.")
  }

  if (is.null(names)) {
    if (isTRUE(regex)) {
      names <- items
    } else {
      names <- item_col
    }
  }

  message("loading from `", source, "::", table, "`\n  * ",
          paste0("`", sort(unique(names)), "`", collapse = "\n  * "))

  if (!is.null(patient_ids)) {
    patient_ids <- prepare_patient_ids(patient_ids, key(dat))
  }

  if (length(items) == 0L) {

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

do_callback <- function(x, fun, val, ...) {

  if (is.null(fun) || is.na(fun)) {
    return(x)
  }

  do.call(fun, c(list(x), list(val_col = val), ...))
}

map_names <- function(old, map_val, map_key) {

  map <- rep(map_val, lengths(map_key))
  names(map) <- map_key

  map[as.character(old)]
}

load_wide <- function(item_cols, id_col, time_col, extra_cols, names,
                      patient_ids, unit, callback, ...) {

  to_rm <- unique(unlist(extra_cols))

  dat <- data_ts_quo(cols = c(item_cols, to_rm), id_cols = id_col,
                     time_col = time_col, ...)

  if (!is.null(patient_ids)) {
    dat  <- merge(dat, patient_ids, by = key(dat), all = FALSE)
  }

  if (nrow(dat) == 0L) {
    return(list(rm_cols(dat, to_rm)))
  }

  if (is.null(callback)) {

    dat <- rm_cols(dat, to_rm)
    dat <- rename_cols(dat, names, item_cols)
    dat <- unmerge(dat, names, na_rm = FALSE)

  } else {

    if (is.function(callback)) {
      callback <- rep(list(callback), length(item_cols))
    }

    assert_that(length(callback) == length(item_cols))

    dat <- unmerge(dat, item_cols, c(id_cols(dat), to_rm), FALSE)
    dat <- Map(do_callback, dat, callback, item_cols,
      MoreArgs = c(list(id_col = id_col, time_col = time_col), extra_cols)
    )
    dat <- lapply(dat, rm_cols, to_rm)

    dat <- Map(rename_cols, dat, names, item_cols)
    dat <- combine_feats(dat)
  }

  lapply(dat, rm_na)
}

load_long <- function(items, item_col, id_col, time_col, val_col, extra_cols,
                      names, patient_ids, unit, callback, ...) {

  uq_items <- unique(items)
  uq_extra <- unique(unlist(extra_cols))

  to_rm <- c(uq_extra, if (!identical(val_col, item_col)) item_col)

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
    dat  <- merge(dat, join, by = key(dat), all = FALSE)
  }

  if (nrow(dat) == 0L) {
    return(list(rm_cols(dat, to_rm)))
  }

  tmp_col <- new_names(dat)

  if (is.null(callback)) {

    dat <- dat[, c(tmp_col) := map_names(get(item_col), names, items)]
    dat <- rm_cols(dat, to_rm)

    dat <- split(dat, by = tmp_col, keep.by = FALSE)
    dat <- Map(rename_cols, dat, names(dat), val_col)

  } else {

    if (is.function(callback)) {
      callback <- rep(list(callback), length(items))
    }

    assert_that(length(callback) == length(items))

    dat <- dat[, c(tmp_col) := map_names(get(item_col),
                                         seq_along(callback), items)]
    dat <- rm_cols(dat, item_col)

    dat <- split(dat, by = tmp_col, keep.by = FALSE)
    dat <- Map(do_callback, dat, callback[as.integer(names(dat))],
      MoreArgs = c(list(val = val_col, id_col = id_col, time_col = time_col),
                   extra_cols)
    )
    dat <- lapply(dat, rm_cols, uq_extra)

    dat <- Map(rename_cols, dat, names[as.integer(names(dat))], val_col)
    dat <- combine_feats(dat)
  }

  lapply(dat, rm_na)
}

load_grep <- function(items, item_col, id_col, time_col, val_col, extra_cols,
                      names, patient_ids, unit, callback, ...) {

  assert_that(is.string(names))

  uq_extra <- unique(unlist(extra_cols))

  to_rm <- c(uq_extra, if (!identical(val_col, item_col)) item_col)

  lst <- list(col = as.name(item_col),
              id = paste(unique(items), collapse = "|"))
  query <- substitute(grepl(id, col, ignore.case = TRUE), lst)

  dat <- data_ts_quo(row_quo = query, cols = c(item_col, val_col, uq_extra),
                     id_cols = id_col, time_col = time_col, ...)

  if (!is.null(patient_ids)) {
    dat  <- merge(dat, join, by = key(dat), all = FALSE)
  }

  if (nrow(dat) == 0L) {
    return(list(rm_cols(dat, to_rm)))
  }

  if (!is.null(callback) && !is.na(callback)) {

    assert_that(length(callback) == 1L)

    dat <- do.call(callback[[1L]],
      c(list(dat), list(val_col = val_col, id_col = id_col,
                        time_col = time_col), extra_cols)
    )
  }

  dat <- rm_cols(dat, to_rm)
  dat <- rename_cols(dat, names, val_col)
  dat <- rm_na(dat)

  list(dat)
}
