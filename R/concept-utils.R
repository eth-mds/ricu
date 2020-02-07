
#' @export
get_concepts <- function(envir = NULL, concept_sel = NULL,
                         dictionary = get_config("concept_dict")) {

  check_each <- function(x, entries = c("id", "table", "column")) {
    is.list(x) && all(vapply(x, has_name, logical(1L), entries))
  }

  check_name <- function(table, x) x %in% names(table)

  if (!is.null(concept_sel)) {
    assert_that(all(concept_sel %in% names(dictionary)))
    dictionary <- dictionary[concept_sel]
  }

  if (!is.null(envir)) {

    assert_that(
      is.string(envir),
      all(vapply(dictionary, check_name, logical(1L), envir))
    )

    dictionary <- lapply(dictionary, `[[`, sub("_demo$", "", envir))
  }

  assert_that(all(vapply(dictionary, check_each, logical(1L))))

  dictionary
}

#' @export
get_col_config <- function(envir = NULL, config = get_config("default_cols")) {

  if (!is.null(envir)) {
    assert_that(is.string(envir), envir %in% names(config))
    config <- config[[sub("_demo$", "", envir)]]
  }

  assert_that(is.list(config))

  config
}

prepare_queries <- function(items) {

  inside_out <- function(ind, lst) lapply(lst, `[[`, ind)

  ulst <- function(x) unlist(x, recursive = FALSE, use.names = FALSE)

  split_uq <- function(x, y) lapply(split(x, y), unique)

  build_query <- function(id, tbl, col, map) {

    map <- stats::setNames(ulst(inside_out("name", map)),
                                inside_out("id", map))

    assert_that(is.string(col), is.string(tbl))

    list(table = tbl, id_col = col, mapping = map,
      query = substitute(col %in% id, list(col = as.name(col), id = id))
    )
  }

  mapping <- Map(
    function(nme, lst) Map(list, name = nme, id = lapply(lst, `[[`, "id")),
    names(items), items
  )

  items <- lapply(c("id", "table", "column"), inside_out, ulst(items))
  items <- lapply(items, ulst)

  mapping <- split_uq(ulst(mapping), items[c(2L, 3L)])
  items <- lapply(items, split_uq, items[c(2L, 3L)])

  res <- do.call(Map, c(build_query, items, list(mapping)))
  lapply(seq_along(res[[1L]]), inside_out, res)
}

#' @export
load_data <- function(items = get_concepts(envir),
                      col_cfg = get_col_config(envir), envir = "mimic",
                      agg_fun = function(x) median(x, na.rm = TRUE), ...) {

  preproc_each <- function(x, nme, val) {

    x <- x[, agg_fun(get(val)), by = c(id_cols(x))]
    x <- data.table::setnames(x, c(id_cols(x), nme))

    x
  }

  load_each <- function(tbl, item_col, mapping, query, ...) {

    cfg <- col_cfg[[tbl]]

    assert_that(has_name(cfg, c("id_col", "time_col", "val_col")))

    dat <- load_fun(tbl, query, c(item_col, cfg[["val_col"]]),
                    id_cols = cfg[["id_col"]], time_col = cfg[["time_col"]],
                    ..., envir = envir)

    dat <- dat[, feature := mapping[as.character(get(item_col))]]
    dat <- split(dat, by = "feature")
    dat <- Map(preproc_each, dat, names(dat), cfg[["val_col"]])

    reduce(merge, dat, all = TRUE)
  }

  load_fun <- switch(sub("_demo$", "", envir),
                     mimic = mimic_ts_quo,
                     eicu = eicu_ts_quo)

  res <- do.call(Map, c(load_each, prepare_queries(items),
                 MoreArgs = list(...)))

  if (length(res) > 1L) {
    reduce(merge, res, all = TRUE)
  } else {
    res
  }
}
