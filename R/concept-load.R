
#' Load data from a dictionary
#'
#' Using a data `dictionary` (or a subset thereof), data is loaded and pre-
#' processed as specified by the selected `concepts`.
#'
#' @param source,concepts A character vector, used to subset the `dictionary`;
#' `NULL` means no subsetting
#' @param dictionary The dictionary to be used
#' @param ... Passed to [load_concepts()]
#'
#' @export
#'
load_dictionary <- function(source = NULL, concepts = NULL,
                            dictionary = read_dictionary(), ...) {

  dictionary <- as_dictionary(dictionary)

  if (is.null(concepts)) {
    conc <- dictionary[, source = source]
  } else if (is_concept(concepts)) {
    conc <- dictionary[names(concepts), source = source]
  } else {
    conc <- dictionary[concepts, source = source]
  }

  load_concepts(conc, ...)
}

#' Load concept data
#'
#' Data specified as `concept` objects is loaded, aggregated per combination
#' of id and time-step and merged into wide format.
#'
#' @param concepts Vector of `concept` objects to be loaded
#' @param aggregate Controls how data within concepts is aggregated
#' @param merge_data Logical flag, specifying whether to merge concepts into
#' wide format or return a list, each entry corresponding to a concept
#' @param ... Passed to `load_concept()` and on to [load_items()]
#'
#' @rdname load_concept
#'
#' @export
#'
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

#' @param concept Single `concept` object to be loaded
#' @param na_rm Logical flag, indicating whether to remove `NA` rows
#'
#' @rdname load_concept
#'
#' @export
#'
load_concept <- function(concept, aggregate = NA_character_, na_rm = TRUE,
                         ...) {

  concept <- as_concept(concept)

  if (length(concept) > 1L) {
    res <- load_concepts(concept, aggregate = aggregate, merge_data = FALSE,
                         na_rm = na_rm, ...)
    return(res)
  }

  assert_that(length(concept) == 1L, is.flag(na_rm))

  arg <- c(list(as_item(concept)), concept_meta(concept), list(...))
  res <- do.call(load_items, arg)
  res <- rbind_lst(res)

  if (na_rm) {
    res <- rm_na(res)
  }

  if (isFALSE(aggregate)) {
    res
  } else {
    make_unique(res, fun = aggregate)
  }
}

#' Load data items
#'
#' Load data specified as data `item` objects.
#'
#' @param items Vector of data `item` objects
#' @param units Character vector of units (one per data item)
#' @param min,max Rage of plausible values
#' @param ... Passed on to [load_item()]
#'
#' @rdname load_item
#'
#' @export
#'
load_items <- function(items, units = NULL, min = NULL, max = NULL, ...) {

  items <- as_item(items)
  names <- names(items)

  Map(load_item, item = items, unit = rep_arg(units, names),
      min = rep_arg(min, names), max = rep_arg(max, names),
      MoreArgs = list(...))
}

#' @param item Single `item` object
#' @param unit String describing the unit of measurement of the given item
#' @param id_type String specifying the patient id type to return
#' @param patient_ids Optional vector of patient ids to subset the fetched data
#' with
#'
#' @inheritParams data_ts_quo
#'
#' @rdname load_item
#'
#' @export
#'
load_item <- function(item, unit = NULL, min = NULL, max = NULL,
                      id_type = "icustay", patient_ids = NULL,
                      interval = hours(1L), cfg = get_src_config(item)) {

  item <- as_item(item)

  if (length(item) > 1L) {
    res <- load_items(item, unit, id_type = id_type, patient_ids = patient_ids,
                      interval = interval, cfg = cfg)
    return(res)
  }

  assert_that(length(item) == 1L, is_src_config(cfg))

  item <- item[[1L]]

  id_col <- get_id_cols(cfg, id_type = id_type)
  ex_col <- get_col_defaults(cfg, table = item[["table"]])

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
  res <- do_range(res, min, max)

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

  if (is.function(callback)) {

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

do_range <- function(x, min, max) {

  if (is.null(min) && is.null(max)) {
    return(x)
  }

  old_nrow <- nrow(x)

  if (is.null(min)) {

    min_ind <- TRUE

  } else {

    assert_that(is.number(min))

    min_ind <- x[[data_cols(x)]] >= min
  }

  if (is.null(max)) {

    max_ind <- TRUE

  } else {

    assert_that(is.number(max))

    max_ind <- x[[data_cols(x)]] <= max
  }

  x <- x[min_ind & max_ind, ]

  new_nrow <- nrow(x)

  if (new_nrow != old_nrow) {
    message("removed ", old_nrow - new_nrow,
            " rows based on range specification")
  }

  x
}

rep_arg <- function(arg, names) {

  if (length(arg) <= 1L) {
    arg <- rep(list(arg), length(names))
  }

  if (is.null(names(arg))) {
    names(arg) <- names
  } else {
    arg <- arg[names]
  }

  assert_that(identical(names(arg), names))

  arg
}

progr_init <- function(len = NULL) {
  if (is_pkg_available("progress") && length(len) > 1L) {
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
