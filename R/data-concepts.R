
#' @export
new_item <- function(concept, source, table, column = NULL, ids = NULL,
                     regex = FALSE, callback = NULL, ...) {

  assert_that(is.string(concept), is.string(source), is.string(table),
              is.null(column) || is.string(column),
              is.atomic(ids), is.flag(regex),
              is.null(callback) || is.string(callback))

  if (!is.null(callback)) {
    assert_that(exists(callback, mode = "function"))
  }

  item <- list(concept = concept, source = source, table = table,
               column = column, ids = ids, regex = regex, callback = callback)

  extra <- list(...)

  if (length(extra) > 0L) {

    assert_that(all(lgl_ply(extra, is.string)),
                !is.null(names(extra)), is_unique(names(extra)))

    item <- c(item, extra)
  }

  structure(list(item), class = "item")
}

#' @export
is_item <- function(x) inherits(x, "item")

#' @export
names.item <- function(x) chr_ply(x, .subset2, "concept")

#' @export
c.item <- function(...) {

  items <- list(...)
  items <- Filter(Negate(is.null), items)

  if (all(lgl_ply(items, is_item))) {
    structure(NextMethod(), class = "item")
  } else {
    NextMethod()
  }
}

#' @export
`[.item` <- function(x, i, source = NULL, ...) {

  recreate <- function(y) do.call(new_item, y)

  if (!missing(i)) {
    x <- .subset(x, i)
  }

  if (!is.null(source)) {
    assert_that(is.character(source), length(source) > 0L)
    x <- .subset(x, get_source(x) %in% source)
  }

  do.call(c, lapply(x, recreate))
}

#' @export
get_source.item <- function(x) chr_ply(x, .subset2, "source")

#' @export
as_item <- function(x) UseMethod("as_item", x)

#' @export
as_item.item <- function(x) x

#' @export
as_item.list <- function(x) try_new(x, new_item)

#' @export
new_concept <- function(name, items, unit = NULL) {

  assert_that(is.string(name), is_item(items),
              all(lgl_ply(names(items), identical, name)),
              is.null(unit) || is.string(unit))

  concept <- list(name = name, unit = unit, items = items)

  structure(list(concept), class = "concept")
}

#' @export
is_concept <- function(x) inherits(x, "concept")

#' @export
names.concept <- function(x) chr_ply(x, .subset2, "name")

#' @export
c.concept <- function(...) {

  concepts <- list(...)
  concepts <- Filter(Negate(is.null), concepts)

  if (all(lgl_ply(concepts, is_concept))) {
    res <- structure(NextMethod(), class = "concept")
    assert_that(is_unique(names(res)))
  } else {
    res <- NextMethod()
  }

  res
}

#' @export
concept <- function(name, ..., unit = NULL) {

  args <- list(...)
  lens <- lengths(args)

  assert_that(all(lens == max(lens) | lens == 1L))

  items <- do.call(c,
    do.call(map, c(new_item, list(concept = name), args))
  )

  new_concept(name, items, unit)
}

#' @export
`[.concept` <- function(x, i, source = NULL, ...) {

  do_one <- function(y, src) {

    if (!is.null(src)) {

      itms <- .subset2(y, "items")[source = source]

      if (is.null(itms)) {
        return(NULL)
      }

      y[["items"]] <- itms
    }

    do.call(new_concept, y)
  }

  if (missing(i)) {
    i <- TRUE
  } else if (is.character(i)) {
    assert_that(length(i) > 0L, all(i %in% names(x)), !anyNA(i))
    i <- match(i, names(x))
  }

  res <- lapply(.subset(x, i), do_one, source)

  do.call(c, Filter(Negate(is.null), res))
}

#' @export
get_source.concept <- function(x) {
  res <- lapply(x, `[[`, "items")
  res <- lapply(res, get_source)
  chr_ply(res, unique)
}

#' @export
as_item.concept <- function(x) do.call(c, lapply(x, `[[`, "items"))

#' @export
as_concept <- function(x, ...) UseMethod("as_concept", x)

#' @export
as_concept.concept <- function(x, ...) x

#' @export
as_concept.item <- function(x, ...) {

  nms <- unique(names(x))

  assert_that(length(nms) == 1L)

  new_concept(nms, x, ...)
}

#' @export
as_concept.list <- function(x, ...) try_new(x, new_concept)

#' @export
as_dictionary.concept <- function(x) new_dictionary(x)

#' @export
new_dictionary <- function(concepts) {

  assert_that(is_concept(concepts), is_unique(names(concepts)))

  structure(list(concepts), class = "dictionary")
}

#' @export
as_dictionary <- function(x) UseMethod("as_dictionary", x)

#' @export
as_dictionary.dictionary <- function(x) x

#' @export
as_dictionary.list <- function(x) do.call(new_dictionary, x)

#' @export
is_dictionary <- function(x) inherits(x, "dictionary")

#' @export
names.dictionary <- function(x) names(as_concept(x))

#' @export
length.dictionary <- function(x) length(as_concept(x))

#' @export
`[.dictionary` <- function(x, i, source = NULL, ...) {
  new_dictionary(as_concept(x)[i, source = source, ...])
}

#' @export
get_source.dictionary <- function(x) {
  res <- unique(get_source(as_concept(x)))
  assert_that(length(res) == 1L)
  res
}

#' @export
str.dictionary <- function(object, ...) str(as_concept(object), ...)

#' @export
as_item.dictionary <- function(x) as_item(as_concept(x))

#' @export
as_concept.dictionary <- function(x, ...) .subset2(x, 1L)

#' @export
read_dictionary <- function(name = "concept-dict", file = NULL, ...) {

  do_itm <- function(x, nme, conc) {
    do.call(new_item, c(list(concept = conc, source = nme), x))
  }

  do_itms <- function(itms, nme, conc) {
    do.call(c, lapply(itms, do_itm, nme, conc))
  }

  do_conc <- function(conc, name) {
    items <- Map(do_itms, conc[["sources"]], names(conc[["sources"]]), name,
                 USE.NAMES = FALSE)
    items <- Filter(Negate(is.null), items)
    args <- c(list(name = name), list(do.call(c, items)),
              conc[names(conc) != "sources"])
    do.call(new_concept, args)
  }

  if (!is.null(file)) {

    assert_that(missing(name), file.exists(file))

    dat <- read_json(file, ...)

  } else {

    dat <- get_config(name, ...)
  }

  concepts <- Map(do_conc, dat, names(dat), USE.NAMES = FALSE)
  concepts <- do.call(c, concepts)

  new_dictionary(concepts)
}

do_new <- function(x, fun, ...) do.call(fun, x, ...)

try_new <- function(x, fun) {
  tryCatch(do.call(fun, x),
           error = function(...) unname(do.call("c", lapply(x, do_new, fun))))
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

  concepts <- as_concept(concepts)

  assert_that(is.flag(merge_data), is_concept(concepts))

  aggregate <- rep_arg(aggregate, names(concepts))

  res <- Map(load_concept, concepts, aggregate[names(concepts)],
             MoreArgs = list(...))

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
                   unit_col = unit_col),
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

