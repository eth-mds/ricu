
#' @export
get_concepts <- function(envir = NULL, concept_sel = NULL,
                         dictionary = get_config("concept-dict")) {

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
get_col_config <- function(envir = NULL, config = get_config("default-cols")) {

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
  splt <- fifelse(vapply(itms[["id"]], is.null, logical(1L)), "",
                  ulst(itms[["column"]]))
  splt <- interaction(ulst(itms[["table"]]), splt, drop = TRUE)

  do.call(Map, c(build_query, lapply(itms, split, splt)))
}

determine_loader <- function(envir) {
  fun <- switch(sub("_demo$", "", envir), mimic = mimic_ts_quo,
                                          eicu  = eicu_ts_quo)
  function(...) fun(..., envir = envir)
}

#' @export
load_data <- function(envir, concepts, patient_ids = NULL,
                      items = get_concepts(envir, concepts),
                      col_cfg = get_col_config(envir),
                      load_fun = determine_loader(envir), agg_fun = dt_gmedian,
                      interval = hours(1L)) {

  load_each <- function(x) {

    cfg <- col_cfg[[x[["table"]]]]
    map <- x[["mapping"]]

    assert_that(has_name(cfg, c("id_col", "time_col", "val_col")))

    message("fetching ",
            paste0("`", unique(map[["new"]]), "`", collapse = ", "),
            " from `", x[["table"]], "`.")

    dat <- load_fun(table = x[["table"]], row_quo = x[["query"]],
                    cols = c(x[["id_col"]], cfg[["val_col"]]),
                    id_cols = cfg[["id_col"]], time_col = cfg[["time_col"]],
                    interval = interval)

    if (!is.null(patient_ids)) {

      if (inherits(patient_ids, "data.frame")) {
        assert_that(has_name(patient_ids, key(dat)))
        join <- patient_ids[, key(dat), with = FALSE]
      } else {
        assert_that(is.atomic(patient_ids))
        join <- setnames(setDT(list(patient_ids)), key(dat))
      }

      dat <- merge(dat, unique(join), by = key(dat), all = FALSE)
    }

    if (is.null(x[["query"]])) {

      dat <- rename_cols(dat, map[["new"]], map[["old"]])
      dat <- dat[not_all_na(dat), ]

      agg_fun(dat)

    } else {

      map <- stats::setNames(map[["new"]], map[["old"]])
      dat <- dat[, c(x[["id_col"]]) := map[as.character(get(x[["id_col"]]))]]

      dat <- split(dat, by = x[["id_col"]], keep.by = FALSE)
      dat <- Map(rename_cols, dat, names(dat), cfg[["val_col"]])
      dat <- lapply(dat, agg_fun)

      reduce(merge, dat, all = TRUE)
    }
  }

  is_hit <- function(haystack, needle) needle %in% names(haystack)

  keep <- function(x) length(id_cols(x)) < ncol(x)

  move_feature <- function(tbl, feat) {
    ret <- tbl[, c(id_cols(tbl), feat), with = FALSE]
    set(tbl, j = feat, value = NULL)
    ret
  }

  regroup_features <- function(x, dups) {

    dups <- lapply(dups, function(dup) {
      hits <- vapply(x, is_hit, logical(1L), dup)
      new_tbl <- lapply(x[hits], move_feature, dup)
      if (sum(hits) > 1L) agg_fun(do.call(rbind, new_tbl))
      else new_tbl
    })

    c(dups, x[vapply(x, keep, logical(1L))])
  }

  res <- lapply(prepare_queries(items), load_each)

  if (length(res) > 1L) {

    feats <- unlist(lapply(res, data_cols))
    dups <- feats[duplicated(feats)]

    if (length(dups)) {
      res <- regroup_features(res, dups)
    }

    reduce(merge, res, all = TRUE)

  } else {

    res[[1L]]
  }
}
