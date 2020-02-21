
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

  extra_cols <- list(...)

  extract_col <- function(col, x, extra = NULL) {
    x[, c(id_cols(x), col, extra), with = FALSE]
  }

  rm_na <- function(x, col) x[!is.na(get(col)), ]

  map_names <- function(old, map_val, map_key) {
    map <- rep(map_val, lengths(map_key))
    names(map) <- unlist(map_key)
    map[as.character(old)]
  }

  resolve <- function(x, fun, val) {
    if (!is.null(fun)) {
      do.call(fun, c(list(x), list(id_col = id_col, time_col = time_col,
                                   val_col = val), extra_cols))
    } else x
  }

  if (is.null(unlist(items))) {
    query <- NULL
  } else {
    # TODO: regex for string, `==` for length 1 numeric, etc.
    query <- substitute(col %in% id, list(col = as.name(item_col),
                                          id = unique(unlist(items))))
  }

  if (length(extra_cols) && is.null(resolvers)) {
    warning("`extra_cols` is only effective is `resolvers` is specified.")
  }

  message("loading from `", source, "::", table, "`\n  * ",
          paste0("`", sort(unique(names)), "`", collapse = "\n  * "))

  dat <- data_ts_quo(source, table = table, row_quo = query,
                     cols = c(item_col, val_col, unlist(extra_cols)),
                     id_cols = id_col, time_col = time_col,
                     interval = interval)

  if (!is.null(patient_ids)) {

    join <- prepare_patient_ids(patient_ids, key(dat))
    dat  <- merge(dat, join, by = key(dat), all = FALSE)
  }

  if (nrow(dat) == 0L) {

    to_rm <- c(unlist(extra_cols),
               if (!identical(val_col, item_col)) item_col)

    if (length(to_rm)) {
      dat <- rm_cols(dat, to_rm)
    }

    return(list(dat))
  }

  if (is.null(query)) {

    if (is.null(resolvers)) {

      if (length(extra_cols)) {
        dat <- rm_cols(dat, unlist(extra_cols))
      }

      dat <- rename_cols(dat, names, item_col)
      dat <- lapply(names, extract_col, dat)
      names(dat) <- names

    } else {

      if (is.function(resolvers)) {
        resolvers <- rep(list(resolvers), length(item_col))
      }

      assert_that(is.list(resolvers), length(resolvers) == length(item_col))

      dat <- lapply(item_col, extract_col, dat, unlist(extra_cols))
      dat <- Map(resolve, dat, resolvers, item_col)

      if (length(extra_cols)) {
        dat <- lapply(dat, rm_cols, unlist(extra_cols))
      }

      dat <- Map(rename_cols, dat, names, item_col)
      dat <- combine_feats(dat)
    }

  } else {

    tmp_col <- new_names(colnames(dat))

    if (is.null(resolvers)) {

      if (length(extra_cols)) {
        dat <- rm_cols(dat, unlist(extra_cols))
      }

      dat <- dat[, c(tmp_col) := map_names(get(item_col), names, items)]

      if (!identical(val_col, item_col)) {
        dat <- rm_cols(dat, item_col)
      }

      dat <- split(dat, by = tmp_col, keep.by = FALSE)
      dat <- Map(rename_cols, dat, names(dat), val_col)

    } else {

      if (is.function(resolvers)) {
        resolvers <- rep(list(resolvers), length(items))
      }

      assert_that(is.list(resolvers), length(resolvers) == length(items))

      dat <- dat[, c(tmp_col) := map_names(get(item_col),
                                            seq_along(resolvers), items)]

      if (!identical(val_col, item_col)) {
        dat <- rm_cols(dat, item_col)
      }

      dat <- split(dat, by = tmp_col, keep.by = FALSE)
      dat <- Map(resolve, dat, resolvers[as.integer(names(dat))], val_col)

      if (length(extra_cols)) {
        dat <- lapply(dat, rm_cols, unlist(extra_cols))
      }

      dat <- Map(rename_cols, dat, names[as.integer(names(dat))], val_col)
      dat <- combine_feats(dat)
    }
  }

  Map(rm_na, dat, vapply(dat, data_cols, character(1L)))
}
