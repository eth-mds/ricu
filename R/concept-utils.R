
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
names.item <- function(x) vapply(x, `[[`, character(1L), "concept")

#' @export
c.item <- function(...) {

  items <- list(...)
  items <- Filter(Negate(is.null), items)

  assert_that(all(vapply(items, is_item, logical(1L))))

  structure(NextMethod(), class = "item")
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
names.concept <- function(x) vapply(x, `[[`, character(1L), "name")

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
new_dictionary <- function(concepts) {

  assert_that(is_concept(concepts), is_unique(names(concepts)))

  structure(list(concepts), class = "dictionary")
}

#' @export
read_dictionary <- function(name = "concept-dict", file = NULL, ...) {

  do_itm <- function(x, nme, conc) {
    do.call(new_item, c(list(concept = conc, source = nme), x))
  }

  do_itms <- function(itms, nme, conc) {
    do.call(c, lapply(itms, do_itm, nme, conc))
  }

  do_conc <- function(conc, name) {
    items <- Map(do_itms, conc[["sources"]], names(conc[["sources"]]), name)
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

  concepts <- Map(do_conc, dat, names(dat))
  concepts <- do.call(c, concepts)

  new_dictionary(concepts)
}

#' @export
is_dictionary <- function(x) inherits(x, "dictionary")

#' @export
names.dictionary <- function(x) names(x[[1L]])

#' @export
length.dictionary <- function(x) length(x[[1L]])

#' @export
get_concepts <- function(source, concept_sel = NULL,
                         dictionary = get_config("concept-dict")) {

  check_each <- function(x) {
    is.list(x) && all(vapply(x, check_item, logical(1L)))
  }

  check_item <- function(x) {
    is.list(x) && has_name(x, c("id", "table", "column")) &&
      all(lengths(x[setdiff(names(x), "id")]) == 1L)
  }

  check_name <- function(table, x) x %in% names(table)

  assert_that(is.list(dictionary))

  if (!is.null(concept_sel)) {

    assert_that(is.character(concept_sel))
    hits <- check_name(dictionary, concept_sel)

    if (any(hits)) {
      assert_that(all(hits))
      dictionary <- dictionary[concept_sel]
    }
  }

  if (!is.null(source)) {

    src <- as_src(source)
    hits <- vapply(dictionary, check_name, logical(1L), src)

    if (any(hits)) {
      assert_that(all(hits))
      dictionary <- lapply(dictionary, `[[`, src)
    }
  }

  dictionary <- dictionary[vapply(dictionary, Negate(is.null), logical(1L))]

  assert_that(is.list(dictionary), !is.null(names(dictionary)),
              all(vapply(dictionary, check_each, logical(1L))))

  dictionary
}

group_concepts <- function(concepts) {

  id_as_lst <- function(x) {
    x[["id"]] <- list(x[["id"]])
    x
  }

  is_miss <- function(x) {
    if (length(x) > 1L) anyNA(x) else is.null(x) || is.na(x)
  }

  add_name <- function(x, name) x[, concept := name]

  as_funs <- function(x) {
    lapply(x, function(x) if (is.na(x)) NULL else get(x, mode = "function"))
  }

  all_na <- function(x) all(is.na(x))

  uq_one <- function(x) {
    res <- unique(x)
    assert_that(length(res) == 1L)
    res
  }

  uq_no_na <- function(x) uq_one(x[!is.na(x)])

  this_that <- function(x, check_fun, true_val, false_fun) {
    if (check_fun(x)) true_val else false_fun(x)
  }

  rm_null <- function(x) {
    res <- Filter(Negate(is.null), x)
    if (length(res) == 0L) NULL else res
  }

  cleanup <- function(x) {

    if (!has_name(x, "resolver")) {
      x[, resolver := NA]
    }

    names <- c("item_col", "items", "names", "resolvers")

    x <- c(setnames(x, c("column", "id", "concept", "resolver"), names))

    x[["table"]] <- uq_one(x[["table"]])
    x[["item_col"]] <- unique(x[["item_col"]])

    x[["items"]] <- this_that(x[["items"]], anyNA, list(NULL), unique)
    x[["resolvers"]] <- this_that(x[["resolvers"]], all_na, NULL, as_funs)

    rest <- setdiff(names(x), c(names, "table"))
    x[["extra_args"]] <- rm_null(
      lapply(x[rest], this_that, all_na, NULL, uq_no_na)
    )
    x[rest] <- NULL

    x
  }

  res <- get_concepts(NULL, NULL, concepts)
  res <- lapply(res, lapply, id_as_lst)

  res <- lapply(res, rbindlist, fill = TRUE)
  res <- Map(add_name, res, names(res))
  res <- rbindlist(res, fill = TRUE)

  if (has_name(res, "regex")) {

    newnam <- new_names(colnames(res), 3L)

    res <- res[, c(newnam) := list(
      table,
      fifelse(vapply(id, is_miss, logical(1L)), NA_character_, column),
      fifelse(is_true(regex), seq_len(nrow(res)), NA_integer_)
    )]

  } else {

    newnam <- new_names(colnames(res), 2L)

    res <- res[, c(newnam) := list(
      table,
      fifelse(vapply(id, is_miss, logical(1L)), NA_character_, column)
    )]
  }

  unname(lapply(split(res, by = newnam, keep.by = FALSE), cleanup))
}

#' @export
get_col_config <- function(source, table = NULL,
                           config = get_config("default-cols")) {

  if (!is.null(source)) {
    source <- as_src(source)
    assert_that(is.string(source), source %in% names(config))
    config <- config[[source]]
  }

  if (!is.null(table)) {
    assert_that(is.string(table), table %in% names(config))
    config <- config[[table]]
  }

  assert_that(is.list(config))

  config
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
                          aggregate = "median", interval = hours(1L),
                          merge_data = TRUE) {

  do_aggregate <- function(x, fun) {
    if (is.function(fun)) {
      x[, lapply(.SD, fun), by = c(id_cols(x)), .SDcols = data_cols(x)]
    } else if (!is.language(fun) && (is.null(fun) || is.na(fun))) {
      assert_that(is_unique(x, by = id_cols(x)))
      x
    } else if (is.string(fun)) {
      dt_gforce(x, fun)
    } else {
      x[, eval(fun), by = c(id_cols(x))]
    }
  }

  assert_that(is.flag(merge_data), is_time(interval, allow_neg = FALSE))

  if (is.character(concepts)) {
    concepts <- get_concepts(source, concept_sel = concepts,
                             dictionary = get_config("concept-dict"))
  }

  assert_that(is.list(concepts), !is.null(names(concepts)))

  if (length(aggregate) == 1L && !is.list(aggregate)) {
    aggregate <- rep(list(aggregate), length(concepts))
    names(aggregate) <- names(concepts)
  } else if (is.atomic(aggregate)) {
    aggregate <- as.list(aggregate)
  }

  assert_that(is.list(aggregate), has_name(aggregate, names(concepts)))

  grouped_concepts <- group_concepts(concepts)
  tables <- vapply(grouped_concepts, `[[`, character(1L), "table")
  extra <- lapply(grouped_concepts, `[[`, "extra_args")
  grouped_concepts <- lapply(grouped_concepts, `[<-`, "extra_args", NULL)

  args <- Map(c, grouped_concepts, extra, col_cfg[tables])
  extra_args <- list(source = source, patient_ids = patient_ids,
                     interval = interval)

  res <- lapply(args, function(x) do.call(load_items, c(x, extra_args)))
  res <- unlist(res, recursive = FALSE)

  res   <- combine_feats(res)
  feats <- vapply(res, data_cols, character(1L))

  names(res) <- feats

  if (!merge_data && isFALSE(aggregate)) return(res)

  res <- Map(do_aggregate, res, aggregate[feats])

  if (!merge_data) return(res)

  if (length(res) > 1L) {
    reduce(merge, res, all = TRUE)
  } else {
    res[[1L]]
  }
}
