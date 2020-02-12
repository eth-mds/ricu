
#' @export
data_ts <- function(source, table, row_expr, ...) {
  data_ts_quo(source, table, null_or_subs(row_expr), ...)
}

#' @export
data_ts_quo <- function(source, ...) {

  fun <- switch(
    sub("_demo$", "", source),
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
    sub("_demo$", "", source),
    mimic = mimic_tbl_quo,
    eicu  = eicu_tbl_quo,
    hirid = stop("TODO"),
    stop("Data source not recognized.")
  )

  fun(..., envir = source)
}

#' @export
load_items <- function(source, table, item_col, items, names, id_col,
                       time_col, val_col, patient_ids = NULL,
                       extra_cols = NULL, interval = hours(1L)) {

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

  extract_col <- function(col, x) x[, c(id_cols(dat), col), with = FALSE]

  rm_na <- function(x, col) x[!is.na(col), ]

  if (is.null(item_col)) {
    query <- NULL
  } else {
    query <- substitute(col %in% id, list(col = as.name(item_col), id = items))
  }

  message("fetching\n  * ",
          paste0("`", unique(names), "`", collapse = "\n  * "),
          "\nfrom `", source, "::", table, "`.")

  dat <- data_ts_quo(source, table = table, row_quo = query,
                     cols = c(item_col, val_col, extra_cols),
                     id_cols = id_col, time_col = time_col,
                     interval = interval)

  if (!is.null(patient_ids)) {

    join <- prepare_patient_ids(patient_ids, key(dat))
    dat  <- merge(dat, join, by = key(dat), all = FALSE)
  }

  if (is.null(query)) {

    dat <- rename_cols(dat, names, items)
    dat <- lapply(names, extract_col, dat)
    names(dat) <- names

  } else {

    map <- stats::setNames(names, items)
    dat <- dat[, c(item_col) := map[as.character(get(item_col))]]

    dat <- split(dat, by = item_col, keep.by = FALSE)
    dat <- Map(rename_cols, dat, names(dat), val_col)
  }

  Map(rm_na, dat, vapply(dat, data_cols, character(1L)))
}
