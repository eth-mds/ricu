
#' @export
get_concepts <- function(source = NULL, concept_sel = NULL,
                         dictionary = get_config("concept-dict")) {

  check_each <- function(x, entries = c("id", "table", "column")) {
    is.list(x) && all(vapply(x, has_name, logical(1L), entries))
  }

  check_name <- function(table, x) x %in% names(table)

  if (!is.null(concept_sel)) {
    assert_that(all(concept_sel %in% names(dictionary)))
    dictionary <- dictionary[concept_sel]
  }

  if (!is.null(source)) {

    assert_that(
      is.string(source),
      all(vapply(dictionary, check_name, logical(1L), source))
    )

    dictionary <- lapply(dictionary, `[[`, sub("_demo$", "", source))
  }

  assert_that(all(vapply(dictionary, check_each, logical(1L))))

  dictionary
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

prepare_concepts <- function(items) {

  inside_out <- function(x) {
    res <- lapply(names(x[[1L]]), function(i) lapply(x, `[[`, i))
    names(res) <- names(x[[1L]])
    res
  }

  ulst <- function(x) unlist(x, recursive = FALSE, use.names = FALSE)

  prep_item <- function(x, nme) {
    c(inside_out(x), list(name = rep(list(nme), length(x))))
  }

  prep_items <- function(x) {
    res <- Map(prep_item, x, names(x))
    lapply(inside_out(res), ulst)
  }

  structure_concept <- function(id, table, column, name) {

    tbl  <- unique(ulst(table))
    cols <- ulst(column)
    nme  <- ulst(name)

    assert_that(is.string(tbl), is.character(nme))

    null_id <- vapply(id, is.null, logical(1L))

    if (any(null_id)) {

      assert_that(all(null_id), is.character(cols),
                  length(nme) == length(cols))

      item_col <- NULL
      ids <- cols

    } else {

      item_col <- unique(cols)
      ids <- ulst(id)

      assert_that(is.string(item_col), length(nme) == length(ids))
    }

    list(table = tbl, item_col = item_col, items = ids, names = nme)
  }

  itms <- prep_items(items)
  splt <- fifelse(vapply(itms[["id"]], is.null, logical(1L)), "",
                  ulst(itms[["column"]]))
  splt <- interaction(ulst(itms[["table"]]), splt, drop = TRUE)

  do.call(Map, c(structure_concept, lapply(itms, split, splt)))
}

#' @export
load_concepts <- function(source, concepts, patient_ids = NULL,
                          items = get_concepts(source, concepts),
                          col_cfg = get_col_config(source),
                          load_fun = determine_loader(source),
                          agg_fun = dt_gmedian, interval = hours(1L)) {

  combine_feats <- function(x) {

    feats <- vapply(x, data_cols, character(1L))
    dups <- feats[duplicated(feats)]

    if (length(dups) == 0L) return(x)

    c(lapply(dups, function(dup) do.call(rbind, x[feats == dup])),
      x[!feats %in% dups])
  }

  unique_feats <- function(x, agg) {

  }

  if (is.function(agg_fun)) {
    agg_fun <- rep(list(agg_fun), length(items))
    names(agg_fun) <- names(items)
  }

  assert_that(is.list(agg_fun), has_name(agg_fun, names(items)),
              all(vapply(agg_fun, is.function, logical(1L))))

  grouped_concepts <- prepare_concepts(items)
  tables <- vapply(grouped_concepts, `[[`, character(1L), "table")

  args <- Map(c,
    list(load_items, source = source), grouped_concepts, col_cfg[tables],
    list(patient_ids = patient_ids, extra_cols = NULL, interval = interval)
  )

  res <- lapply(args, do.call)
  res <- unlist(res, recursive = FALSE)

  res   <- combine_feats(res)
  feats <- vapply(res, data_cols, character(1L))

  res <- Map(do.call, agg_fun[feats], res)

  if (length(res) > 1L) {
    reduce(merge, res, all = TRUE)
  } else {
    res[[1L]]
  }
}
