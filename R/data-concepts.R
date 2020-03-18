
#' @export
new_item <- function(concept, source, table, column, ids = NULL,
                     regex = FALSE, callback = NULL, ...) {

  assert_that(is.string(concept), is.string(source), is.string(table),
              is.string(column), is.atomic(ids), is.flag(regex),
              is.null(callback) || is.string(callback))

  if (!is.null(callback)) {
    assert_that(exists(callback, mode = "function"))
  }

  item <- list(concept = concept, source = source, table = table,
               column = column, ids = ids, regex = regex, callback = callback)

  extra <- list(...)

  if (length(extra) > 0L) {

    assert_that(all(vapply(extra, is.string, logical(1L))),
                !is.null(names(extra)), is_unique(names(extra)))

    item <- c(item, extra)
  }

  structure(list(item), class = "item")
}

#' @export
is_item <- function(x) inherits(x, "item")

#' @export
names.item <- function(x) vapply(x, .subset2, character(1L), "concept")

#' @export
c.item <- function(...) {

  items <- list(...)
  items <- Filter(Negate(is.null), items)

  assert_that(all(vapply(items, is_item, logical(1L))))

  structure(NextMethod(), class = "item")
}

#' @export
`[.item` <- function(x, i, source = NULL, ...) {

  recreate <- function(y) do.call(new_item, y)

  if (!missing(i)) {
    x <- .subset(x, i)
  }

  if (!is.null(source)) {
    assert_that(is.string(source))
    srcs <- vapply(x, .subset2, character(1L), "source")
    x <- .subset(x, source == srcs)
  }

  do.call(c, lapply(x, recreate))
}

#' @export
as_item <- function(x) UseMethod("as_item", x)

#' @export
as_item.item <- function(x) x

#' @method as.data.table item
#' @export
as.data.table.item <- function(x, ...) {
  res <- lapply(x, function(y) do.call(data.table::data.table, y))
  rbindlist(res, fill = TRUE)
}

#' @export
new_concept <- function(name, items, unit = NULL) {

  assert_that(is.string(name), is_item(items),
              all(vapply(names(items), identical, logical(1L), name)),
              is.null(unit) || is.string(unit))

  concept <- list(name = name, unit = unit, items = items)

  structure(list(concept), class = "concept")
}

#' @export
is_concept <- function(x) inherits(x, "concept")

#' @export
names.concept <- function(x) vapply(x, .subset2, character(1L), "name")

#' @export
c.concept <- function(...) {

  concepts <- list(...)
  concepts <- Filter(Negate(is.null), concepts)

  assert_that(all(vapply(concepts, is_concept, logical(1L))))

  res <- structure(NextMethod(), class = "concept")

  assert_that(is_unique(names(res)))

  res
}

#' @export
concept <- function(name, ..., unit = NULL) {

  items <- do.call(c,
    do.call(map, c(new_item, list(concept = name),  list(...)))
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
as_item.concept <- function(x) do.call(c, lapply(x, `[[`, "items"))

#' @export
as_concept <- function(x) UseMethod("as_concept", x)

#' @export
as_concept.concept <- function(x) x

#' @export
as_dictionary.concept <- function(x) new_dictionary(x)

#' @method as.data.table concept
#' @export
as.data.table.concept <- function(x, ...) {

  do_dt <- function(y) {
    do.call(data.table, c(as.data.table(y[["items"]]),
                          y[!names(y) %in% c("items", "name")]))
  }

  rbindlist(lapply(x, do_dt), fill = TRUE)
}

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
str.dictionary <- function(object, ...) str(as_concept(object), ...)

#' @export
as_item.dictionary <- function(x) as_item(as_concept(x))

#' @export
as_concept.dictionary <- function(x) .subset2(x, 1L)

#' @method as.data.table dictionary
#' @export
as.data.table.dictionary <- function(x, ...) as.data.table(as_concept(x))

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

#' @export
get_concepts <- function(source, concepts, ...) {
  dict <- read_dictionary(...)
  dict[concepts, source = source]
}

#' @export
group_concepts <- function(concepts) {

  swap_items <- function(x, new) {
    x[["items"]] <- new
    do.call(new_concept, x)
  }

  split_swap <- function(x, conc) {
    x <- split(x, names(x))
    new_dictionary(do.call(c, Map(swap_items, as_concept(conc[names(x)]), x)))
  }

  if (is_concept(concepts)) {
    concepts <- as_dictionary(concepts)
  }

  assert_that(is_dictionary(concepts))

  items <- as_item(concepts)
  wide <- vapply(lapply(items, `[[`, "ids"), is.null, logical(1L))

  splt <- list(
    vapply(items, `[[`, character(1L), "table"),
    ifelse(wide, "", vapply(items, `[[`, character(1L), "column")),
    vapply(items, `[[`, logical(1L), "regex")
  )

  lapply(split(items, splt, drop = TRUE), split_swap, concepts)
}

combine_feats <- function(x) {

  do_rbind <- function(needle, haystack) rbind_lst(x[haystack == needle])

  feats <- vapply(x, data_cols, character(1L))
  dups <- unique(feats[duplicated(feats)])

  if (length(dups) == 0L) return(x)

  c(lapply(dups, do_rbind, feats), x[!feats %in% dups])
}

#' @export
load_concepts <- function(source, concepts = get_concepts(source),
                          patient_ids = NULL, col_cfg = get_col_config(source),
                          aggregate = NA_character_, interval = hours(1L),
                          merge_data = TRUE) {

  do_aggregate <- function(x, fun) {

    if (is.function(fun)) {

      x[, lapply(.SD, fun), by = c(meta_cols(x)), .SDcols = data_cols(x)]

    } else if (!is.language(fun) && is.null(fun)) {

      assert_that(is_unique(x, by = meta_cols(x)))
      x

    } else if (is.string(fun)) {

      if (is.na(fun)) {
        if (is.numeric(x[[data_cols(x)]])) fun <- "median"
        else                               fun <- "first"
      }

      dt_gforce(x, fun)

    } else {

      x[, eval(fun), by = c(meta_cols(x))]
    }
  }

  do_load <- function(concept, source, table, column, ids = NULL,
                      regex = FALSE, callback = NULL, unit = NULL, ...) {

    uq_na_rm <- function(x) {
      res <- unique(x)
      res <- res[!is.na(res)]
      if (length(res)) res else NULL
    }

    tbl <- unique(table)
    rgx <- unique(regex)

    if (isTRUE(rgx)) {
      concept <- unique(concept)
    }

    cfg_names <- c("id_col", "time_col", "val_col", "unit_col")

    args <- c(list(source = source, table = tbl, item_col = unique(column),
                   items = ids, names = concept),
              setNames(col_cfg[[tbl]][cfg_names], cfg_names),
              list(patient_ids = patient_ids, callback = callback, regex = rgx,
                   unit = unit, interval = interval),
              lapply(list(...), uq_na_rm))

    do.call(load_items, args)
  }

  prep_load <- function(x, src) {

    args <- c(as.data.table(x))

    assert_that(all(lgl_ply(args[["source"]], identical, src)))
    args[["source"]] <- src

    do.call(do_load, args)
  }

  assert_that(is.flag(merge_data), is_time(interval, allow_neg = FALSE))

  if (is.character(concepts)) {
    concepts <- get_concepts(source, concepts, "concept-dict")
  }

  res <- lapply(group_concepts(concepts), prep_load, source)
  res <- unlist(res, recursive = FALSE, use.names = FALSE)

  res   <- combine_feats(res)
  feats <- vapply(res, data_cols, character(1L))

  if (!merge_data && isFALSE(aggregate)) return(res)

  aggregate <- prep_args(aggregate, names(concepts))

  res <- map(do_aggregate, res, aggregate[feats])

  if (!merge_data) return(res)

  if (length(res) > 1L) {
    reduce(merge, res, all = TRUE)
  } else {
    res[[1L]]
  }
}
