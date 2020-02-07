
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

  build_query <- function(id, table, column, name) {

    tbl  <- unique(ulst(table))
    cols <- ulst(column)
    nme  <- ulst(name)

    assert_that(is.string(tbl), is.character(nme))

    null_id <- vapply(id, is.null, logical(1L))

    if (any(null_id)) {

      assert_that(all(null_id), is.character(cols),
                  length(name) == length(cols))

      map <- list(new = nme, old = cols)
      query <- NULL

    } else {

      cols <- unique(cols)
      ids <- ulst(id)

      assert_that(is.string(cols), length(nme) == length(ids))

      map <- list(new = nme, old = ids)
      query <- substitute(col %in% id, list(col = as.name(cols), id = ids))
    }

    list(table = tbl, id_col = cols, mapping = map, query = query)
  }

  itms <- prep_items(items)
  splt <- data.table::fifelse(vapply(itms[["id"]], is.null, logical(1L)), "",
                              ulst(itms[["column"]]))
  splt <- interaction(ulst(itms[["table"]]), splt, drop = TRUE)

  do.call(Map, c(build_query, lapply(itms, split, splt)))
}

#' @export
load_data <- function(items = get_concepts(envir),
                      col_cfg = get_col_config(envir), envir = "mimic",
                      patient_ids = NULL, agg_fun = dbl_med, ...) {

  load_each <- function(x, ...) {

    tbl <- x[["table"]]
    cfg <- col_cfg[[tbl]]
    qry <- x[["query"]]
    map <- x[["mapping"]]

    assert_that(has_name(cfg, c("id_col", "time_col", "val_col")))

    dat <- load_fun(tbl, qry, c(x[["id_col"]], cfg[["val_col"]]),
                    id_cols = cfg[["id_col"]], time_col = cfg[["time_col"]],
                    ..., envir = envir)

    if (!is.null(patient_ids)) {
      browser()
    }

    if (is.null(qry)) {

      dat <- rename_cols(dat, map[["new"]], map[["old"]])

      keep <- rowSums(
        is.na(dat[, map[["new"]], with = FALSE])) < length(map[["new"]]
      )

      make_unique(dat[keep, ], fun = agg_fun)

    } else {

      dat <- split(dat, by = x[["id_col"]], keep.by = FALSE)
      new_names <- map[["new"]][match(names(dat), map[["old"]])]

      dat <- Map(rename_cols, dat, new_names, cfg[["val_col"]])
      dat <- lapply(dat, make_unique, fun = agg_fun)

      reduce(merge, dat, all = TRUE)
    }
  }

  load_fun <- switch(sub("_demo$", "", envir),
                     mimic = mimic_ts_quo,
                     eicu = eicu_ts_quo)

  res <- lapply(prepare_queries(items), load_each, ...)

  if (length(res) > 1L) {
    reduce(merge, res, all = TRUE)
  } else {
    res[[1L]]
  }
}
