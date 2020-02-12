
#' @export
get_concepts <- function(source, concept_sel = NULL,
                         dictionary = get_config("concept-dict")) {

  check_each <- function(x, entries = c("table", "column")) {
    is.list(x) && all(vapply(x, has_name, logical(1L), entries))
  }

  check_name <- function(table, x) x %in% names(table)

  assert_that(is.string(source), is.list(dictionary))

  src <- sub("_demo$", "", source)

  if (!is.null(concept_sel)) {

    assert_that(
      is.character(concept_sel),
      all(concept_sel %in% names(dictionary))
    )

    dictionary <- dictionary[concept_sel]
  }

  if (!is.null(source)) {

    assert_that(
      all(vapply(dictionary, check_name, logical(1L), src))
    )

    dictionary <- lapply(dictionary, `[[`, src)
  }

  dictionary <- dictionary[vapply(dictionary, Negate(is.null), logical(1L))]

  assert_that(all(vapply(dictionary, check_each, logical(1L))))

  dictionary
}

group_concepts <- function(concepts) {

  add_name <- function(x, name) x[, concept := name]

  as_fun <- function(x) if (is.na(x)) NULL else get(x, mode = "function")

  as_funs <- function(x) lapply(x, as_fun)

  all_na <- function(x) all(is.na(x))

  uq_no_na <- function(x) unique(x[!is.na(x)])

  this_that <- function(x, check_fun, true_val, false_fun) {
    if (check_fun(x)) true_val else false_fun(x)
  }

  cleanup <- function(x) {

    x <- c(setnames(x[, wide := NULL],
                    c("column", "id", "concept", "resolver"),
                    c("item_col", "items", "names", "resolvers")))

    x[["table"]] <- unique(x[["table"]])
    x[["item_col"]] <- unique(x[["item_col"]])

    x[["items"]] <- this_that(x[["items"]], anyNA, list(NULL), unique)
    x[["extra_cols"]] <- this_that(x[["extra_cols"]], all_na, NULL, uq_no_na)
    x[["resolvers"]] <- this_that(x[["resolvers"]], all_na, NULL, as_funs)

    x
  }

  res <- lapply(concepts, rbindlist, fill = TRUE)
  res <- Map(add_name, res, names(res))
  res <- rbindlist(res, fill = TRUE)
  res <- res[, wide := fifelse(is.na(id), "", column)]

  res <- split(res, by = c("table", "wide"))

  unname(lapply(res, cleanup))
}

#' @export
get_col_config <- function(source = NULL, table = NULL,
                           config = get_config("default-cols")) {

  if (!is.null(source)) {
    source <- sub("_demo$", "", source)
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

  feats <- vapply(x, data_cols, character(1L))
  dups <- feats[duplicated(feats)]

  if (length(dups) == 0L) return(x)

  c(lapply(dups, function(y) rbind_lst(x[feats == y])), x[!feats %in% dups])
}

#' @export
load_concepts <- function(source, concepts, patient_ids = NULL,
                          items = get_concepts(source, concepts),
                          col_cfg = get_col_config(source),
                          aggregate = "median", interval = hours(1L)) {

  do_aggregate <- function(x, fun) {
    if (is.function(x)) {
      x[, lapply(.SD, fun), by = id_cols(x), .SDcols = data_cols(x)]
    } else {
      dt_gforce(x, fun, by = id_cols(x), cols = data_cols(x), na.rm = TRUE)
    }
  }

  if (is.function(aggregate) || is.string(aggregate)) {
    aggregate <- rep(list(aggregate), length(items))
    names(aggregate) <- names(items)
  } else if (is.atomic(aggregate)) {
    aggregate <- as.list(aggregate)
  }

  assert_that(is.list(aggregate), has_name(aggregate, names(items)))

  grouped_concepts <- group_concepts(items)
  tables <- vapply(grouped_concepts, `[[`, character(1L), "table")

  args <- Map(c, grouped_concepts, col_cfg[tables])
  extra_args <- list(source = source, patient_ids = patient_ids,
                     interval = interval)

  res <- lapply(args, function(x) do.call(load_items, c(x, extra_args)))
  res <- unlist(res, recursive = FALSE)

  res   <- combine_feats(res)
  feats <- vapply(res, data_cols, character(1L))

  res <- Map(do_aggregate, res, aggregate[feats])

  if (length(res) > 1L) {
    reduce(merge, res, all = TRUE)
  } else {
    res[[1L]]
  }
}
