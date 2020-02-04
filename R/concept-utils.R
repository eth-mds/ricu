
get_concepts <- function(cfg = get_config("concepts"), envir = NULL) {

  check_each <- function(x, entries = c("id", "table", "column")) {
    is.list(x) && all(vapply(x, has_name, logical(1L), entries))
  }

  if (!is.null(envir)) {
    assert_that(is.string(envir))
    cfg <- lapply(cfg, `[[`, sub("_demo$", "", envir))
  }

  assert_that(all(vapply(cfg, check_each, logical(1L))))

  cfg
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

load_data <- function(items = get_concepts(envir = envir), envir = "mimic",
                      val_col = "valuenum", agg_fun = mean, ...) {

  drop_cols <- function(x, cols) x[, c(cols) := NULL]

  preproc_each <- function(x, nme, rm) {

    x <- x[, agg_fun(get(val_col)), by = c(id_cols(x))]
    x <- data.table::setnames(x, c(id_cols(x), nme))

    x
  }

  load_each <- function(tbl, id_col, mapping, query, ...) {

    dat <- load_fun(tbl, query, c(id_col, val_col) ,..., envir = envir)
    dat <- dat[, feature := mapping[as.character(itemid)]]
    dat <- split(dat, by = "feature")
    dat <- Map(preproc_each, dat, names(dat),
               MoreArgs = list(rm = c("feature", id_col)))

    reduce(merge, dat, by = id_cols(dat[[1L]]), all = TRUE)
  }

  assert_that(is.string(val_col))

  load_fun <- switch(sub("_demo$", "", envir),
                     mimic = mimic_ts_quo,
                     eicu = eicu_ts_quo)

  do.call(Map, c(load_each, prepare_queries(items), MoreArgs = list(...)))
}
