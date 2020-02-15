
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

  is_na <- function(x) if (length(x) > 1L) anyNA(x) else is.na(x)

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

    names <- c("item_col", "items", "names", "resolvers")

    x <- c(setnames(x[, wide := NULL],
                    c("column", "id", "concept", "resolver"), names))

    x[["table"]] <- uq_one(x[["table"]])
    x[["item_col"]] <- unique(x[["item_col"]])

    x[["items"]] <- this_that(x[["items"]], anyNA, list(NULL), unique)
    x[["resolvers"]] <- this_that(x[["resolvers"]], all_na, NULL, as_funs)

    rest <- setdiff(names(x), c(names, "table"))
    x[["extra_cols"]] <- rm_null(
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
  res <- res[, wide := fifelse(vapply(id, is_na, logical(1L)), "", column)]

  res <- split(res, by = c("table", "wide"))

  unname(lapply(res, cleanup))
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

  feats <- vapply(x, data_cols, character(1L))
  dups <- feats[duplicated(feats)]

  if (length(dups) == 0L) return(x)

  c(lapply(dups, function(y) rbind_lst(x[feats == y])), x[!feats %in% dups])
}

#' @export
load_concepts <- function(source, concepts = get_concepts(source),
                          patient_ids = NULL, col_cfg = get_col_config(source),
                          aggregate = "median", interval = hours(1L),
                          merge = TRUE) {

  do_aggregate <- function(x, fun) {
    if (is.function(fun)) {
      x[, lapply(.SD, fun), by = id_cols(x), .SDcols = data_cols(x)]
    } else if (is.null(fun) || is.na(fun)) {
      assert_that(is_unique(x, by = id_cols(x)))
      x
    } else {
      dt_gforce(x, fun, by = id_cols(x), cols = data_cols(x), na.rm = TRUE)
    }
  }

  assert_that(is.flag(merge), is_time(interval, allow_neg = FALSE))

  if (is.character(concepts)) {
    concepts <- get_concepts(source, concept_sel = concepts,
                             dictionary = get_config("concept-dict"))
  }

  assert_that(is.list(concepts), !is.null(names(concepts)))

  if (is.null(aggregate) || is.function(aggregate) || is.string(aggregate)) {
    aggregate <- rep(list(aggregate), length(items))
    names(aggregate) <- names(items)
  } else if (is.atomic(aggregate)) {
    aggregate <- as.list(aggregate)
  }

  assert_that(is.list(aggregate), has_name(aggregate, names(concepts)))

  grouped_concepts <- group_concepts(concepts)
  tables <- vapply(grouped_concepts, `[[`, character(1L), "table")
  extra <- lapply(grouped_concepts, `[[`, "extra_cols")
  grouped_concepts <- lapply(grouped_concepts, `[<-`, "extra_cols", NULL)

  args <- Map(c, grouped_concepts, extra, col_cfg[tables])
  extra_args <- list(source = source, patient_ids = patient_ids,
                     interval = interval)

  res <- lapply(args, function(x) do.call(load_items, c(x, extra_args)))
  res <- unlist(res, recursive = FALSE)

  res <- combine_feats(res)

  if (!merge && !aggregate) return(res)

  res <- Map(do_aggregate, res,
             aggregate[vapply(res, data_cols, character(1L))])

  if (!merge) return(res)

  if (length(res) > 1L) {
    reduce(merge, res, all = TRUE)
  } else {
    res[[1L]]
  }
}
