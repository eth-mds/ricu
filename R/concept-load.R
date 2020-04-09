
#' @export
load_dictionary <- function(source = NULL, concepts = NULL,
                            dictionary = read_dictionary(), ...) {

  dictionary <- as_dictionary(dictionary)

  if (is.null(concepts)) {
    conc <- dictionary[, source = source]
  } else {
    conc <- dictionary[concepts, source = source]
  }

  load_concepts(conc, ...)
}

#' @export
load_concepts <- function(concepts, aggregate = NA_character_,
                          merge_data = TRUE, ...) {

  do_load <- function(x, agg, progress_bar, ...) {
    progr_iter(x[["name"]], progress_bar)
    load_concept(concept = x, aggregate = agg, ...)
  }

  concepts <- as_concept(concepts)

  assert_that(is.flag(merge_data), is_concept(concepts))

  aggregate <- rep_arg(aggregate, names(concepts))

  pb <- progr_init(length(concepts))

  res <- Map(do_load, concepts, aggregate[names(concepts)],
             MoreArgs = c(list(progress_bar = pb), list(...)))

  if (!merge_data) {
    res
  } else if (length(res) > 1L) {
    reduce(merge, res, all = TRUE)
  } else {
    res[[1L]]
  }
}

#' @export
load_concept <- function(concept, aggregate = NA_character_, na_rm = TRUE,
                         ...) {

  concept <- as_concept(concept)

  if (length(concept) > 1L) {
    res <- load_concepts(concept, aggregate = aggregate, merge_data = FALSE,
                         na_rm = na_rm, ...)
    return(res)
  }

  assert_that(length(concept) == 1L, is.flag(na_rm))

  res <- load_items(as_item(concept), units = concept[["unit"]], ...)
  res <- rbind_lst(res)

  if (na_rm) {
    res <- rm_na(res)
  }

  if (isFALSE(aggregate)) {
    res
  } else {
    do_aggregate(res, aggregate)
  }
}

#' @export
load_items <- function(items, units = NULL, ...) {

  items <- as_item(items)

  units <- rep_arg(units, names(items))

  Map(load_item, item = items, unit = units[names(items)],
      MoreArgs = list(...))
}

#' @export
load_item <- function(item, unit = NULL, id_type = "hadm",
                      patient_ids = NULL, interval = hours(1L),
                      cfg = get_col_config(get_source(item), "all")) {

  item <- as_item(item)

  if (length(item) > 1L) {
    res <- load_items(item, unit, id_type = id_type, patient_ids = patient_ids,
                      interval = interval, cfg = cfg)
    return(res)
  }

  assert_that(length(item) == 1L,
              has_name(cfg, c("data_fun", "id_cols", "tables")))

  item <- item[[1L]]

  id_col <- get_col_config(NULL, "id_cols", cfg, type = id_type)
  ex_col <- get_col_config(NULL, config = cfg, table = item[["table"]])

  if (!is.null(patient_ids)) {

    if (inherits(patient_ids, "data.frame")) {
      assert_that(has_name(patient_ids, id_col))
      patient_ids <- patient_ids[[id_col]]
    }

    assert_that(is.atomic(patient_ids), length(patient_ids) > 0L)
    patient_ids <- setnames(setDT(list(unique(patient_ids))), id_col)
  }

  load_args <- c(
    item[setdiff(names(item), c("concept", "callback"))],
    list(id_col = id_col), ex_col, list(cfg = cfg, interval = interval)
  )

  res <- do.call(do_load, load_args[!duplicated(names(load_args))])

  if (!is.null(patient_ids)) {
    res  <- merge(res, patient_ids, by = id_col, all = FALSE)
  }

  cb_args <- c(
    list(x = res, unit = unit),
    item[setdiff(names(item), c("concept", "table", "regex"))],
    list(id_col = id_col), ex_col
  )

  res <- do.call(do_callback, cb_args[!duplicated(names(cb_args))])
  res <- rename_cols(res, item[["concept"]], data_cols(res))
  res <- add_unit(res, unit)

  res
}

do_load <- function(source, table, column, ids, regex, ..., id_col, time_col,
                    val_col, unit_col, interval, cfg) {

  extra_cols <- list(...)

  ids  <- unique(ids)
  cols <- c(column, unit_col, unique(unlist(extra_cols)))

  if (is.null(cols)) {
    cols <- character(0L)
  }

  if (length(ids) == 0L) {

    query <- NULL

  } else {

    cols <- c(val_col, cols)
    col  <- as.name(column)

    if (isTRUE(regex)) {
      query <- substitute(grepl(id, col, ignore.case = TRUE),
        list(col = col, id = paste(ids, collapse = "|"))
      )
    } else if (length(ids) == 1L) {
      query <- substitute(is_fun(col, id),
        list(col = col, id = ids, is_fun = is_val)
      )
    } else {
      query <- substitute(col %in% id, list(col = col, id = ids))
    }
  }

  if (is.null(time_col)) {
    data_id_quo(source, table, query, cols, id_col, interval, cfg)
  } else {
    data_ts_quo(source, table, query, cols, id_col, time_col, interval, cfg)
  }
}

do_callback <- function(x, unit, source, column, ids, callback, ..., id_col,
                        time_col, val_col, unit_col) {

  extra_cols <- list(...)

  if (length(extra_cols) && is.null(callback)) {
    warning("`extra_cols` is only effective is `callback` is specified.")
  }

  if (length(ids) == 0L) {
    val <- column
  } else {
    val <- val_col
  }

  if (is.string(callback)) {
    callback <- get0(callback, mode = "function")
  }

  if (nrow(x) > 0L && is.function(callback)) {

    args <- c(list(x),
              list(id_col = id_col, time_col = time_col, val_col = val,
                   item_col = column, unit_col = unit_col),
              extra_cols,
              list(source = source, unit = unit))

    x <- do.call(callback, args)
  }

  if (is_id_tbl(x)) {
    assert_that(identical(id(x), id_col))
  } else if (is_ts_tbl(x)) {
    assert_that(identical(meta_cols(x), c(id_col, time_col)))
  } else {
    stop("expecting an `id_tbl` or `ts_tbl` object")
  }

  rm_cols(x, setdiff(colnames(x), c(id_col, time_col, val)))
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

do_aggregate <- function(x, fun) {

  if (nrow(x) == 0L) {
    return(x)
  }

  if (is.function(fun)) {

    x[, lapply(.SD, fun), by = c(meta_cols(x)), .SDcols = data_cols(x)]

  } else if (!is.language(fun) && is.null(fun)) {

    assert_that(is_unique(x, by = meta_cols(x)))
    x

  } else if (is.string(fun)) {

    if (is.na(fun)) {
      if (is.numeric(x[[data_cols(x)]])) {
        fun <- "median"
      } else {
        fun <- "first"
      }
    }

    dt_gforce(x, fun)

  } else {

    x[, eval(fun), by = c(meta_cols(x))]
  }
}

rep_arg <- function(arg, names) {

  if (length(arg) <= 1L) {
    arg <- rep(list(arg), length(names))
  }

  if (is.null(names(arg))) {
    names(arg) <- names
  }

  assert_that(has_name(arg, names))

  arg
}

progr_init <- function(len = NULL) {
  if (is_pkg_available("progress")) {
    progress::progress_bar$new(
      format = "loading :what [:bar] :percent",
      total = len
    )
  } else {
    message("loading")
    NULL
  }
}

progr_iter <- function(name, pb = NULL) {
  if (is.null(pb)) {
    message("  * `", name, "`")
  } else {
    pb$tick(tokens = list(what = name))
  }
}
