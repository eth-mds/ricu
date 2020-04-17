
new_src_config <- function(name, id_cols, tables, attach_hook = NULL,
                           data_fun = NULL, url = NULL) {

  assert_that(is.list(id_cols), all_is(id_cols, is.string),
              all(lgl_ply(tables, has_name, c("name", "defaults"))))

  id_cols <- as_id_cols(id_cols)
  col_defaults <- as_col_defaults(tables)

  if (all(lgl_ply(tables, has_name, c("file", "cols")))) {
    table_specs <- as_table_spec(tables)
  } else {
    table_specs <- NULL
  }

  if (is.null(data_fun)) {
    data_fun <- default_data_fun
  } else {
    data_fun <- get(data_fun, mode = "function")
  }

  if (!is.null(attach_hook)) {
    attach_hook <- get(attach_hook, mode = "function")
  }

  assert_that(is.string(name), is_id_cols(id_cols),
              all_is(col_defaults, is_col_defaults),
              null_or(table_specs, all_is, is_table_spec),
              is.function(data_fun), null_or(url, is.string),
              null_or(attach_hook, is.function))

  structure(list(name = name, id_cols = id_cols, col_defaults = col_defaults,
                 data_fun = data_fun, attach_hook = attach_hook, url = url,
                 table_specs = table_specs),
            class = "src_config")
}

is_src_config <- function(x) inherits(x, "src_config")

#' @export
get_source.src_config <- function(x) x[["name"]]

new_id_cols <- function(cfg) {

  cfg <- as.list(cfg)

  assert_that(is.list(cfg), length(cfg) > 0L, has_name(cfg, "icustay"),
              all_is(cfg, is.string), is_unique(c(unlist(cfg), names(cfg))))

  structure(
    cfg[intersect(c("patient", "hadm", "icustay"), names(cfg))],
    class = "id_cols"
  )
}

is_id_cols <- function(x) inherits(x, "id_cols")

as_id_cols <- function(x) UseMethod("as_id_cols", x)

as_id_cols.id_cols <- function(x) x

as_id_cols.list <- function(x) new_id_cols(x)

new_col_defaults <- function(table, cfg) {

  opts <- c("id_col", "time_col", "val_col", "unit_col")

  cfg <- as.list(cfg)

  assert_that(is.list(cfg), all_is(cfg, is.string))

  if (length(cfg)) {
    assert_that(!is.null(names(cfg)), all(names(cfg) %in% opts))
  }

  cfg <- cfg[opts]
  names(cfg) <- opts

  structure(list(table = table, cols = cfg), class = "col_defaults")
}

is_col_defaults <- function(x) inherits(x, "col_defaults")

as_col_defaults <- function(x) UseMethod("as_col_defaults", x)

as_col_defaults.col_defaults <- function(x) x

as_col_defaults.list <- function(x) {
  lapply(x, do_call, new_col_defaults, c("name", "defaults"))
}

new_table_spec <- function(table, file, cols, nrow = NULL,
                           partitioning = NULL) {

  col_spec <- function(name, col, spec, ...) {
    do.call(call, c(list(spec), list(...)))
  }

  tbl_spec <- function(...) substitute(cols(...))

  assert_that(is.string(table), is.string(file), null_or(nrow, is.count),
              is.list(cols))

  spec <- lapply(cols, do_call, col_spec)
  names(spec) <- chr_ply(cols, `[[`, "col")

  spec <- do.call(tbl_spec, as.list(spec))
  spec <- eval(spec, envir = asNamespace("readr"))

  cols <- chr_ply(cols, `[[`, "name")

  if (is.null(partitioning)) {

    part <- NULL

  } else {

    nme <- names(partitioning)
    ids <- partitioning[[1L]]

    assert_that(is.string(nme), is.numeric(ids), length(ids) > 2L)

    part <- list(col = names(partitioning),
                 fun = function(x) findInterval(x, ids, all.inside = TRUE))
  }

  structure(list(table = table, file = file, cols = cols, spec = spec,
                 nrow = nrow, partitioning = part), class = "table_spec")
}

is_table_spec <- function(x) inherits(x, "table_spec")

as_table_spec <- function(x) UseMethod("as_table_spec", x)

as_table_spec.table_spec <- function(x) x

as_table_spec.list <- function(x) {
  lapply(x, do_call, new_table_spec, c("name", "file", "cols", "num_rows",
                                       "partitioning"))
}

read_src_config <- function(name = "data-sources", file = NULL, ...) {

  if (!is.null(file)) {

    assert_that(missing(name), file.exists(file))

    cfg <- read_json(file, ...)

  } else {

    cfg <- get_config(name, ...)
  }

  Map(new_src_config, chr_ply(cfg, `[[`, "name"), lapply(cfg, `[[`, "id_cols"),
      lapply(cfg, `[[`, "tables"), chr_ply(cfg, `[[`, "attach_hook"),
      chr_ply(cfg, `[[`, "data_fun"), chr_ply(cfg, `[[`, "url"))
}

get_src_config <- function(x, ...) UseMethod("get_src_config", x)

get_src_config.src_config <- function(x, ...) x

get_src_config.character <- function(x, ...) {

  assert_that(is.string(x))

  cfg <- read_src_config(...)

  assert_that(is.list(cfg), all_is(cfg, is_src_config))

  hit <- x == chr_ply(cfg, get_source)

  assert_that(sum(hit) == 1L)

  cfg[[which(hit)]]
}

get_src_config.default <- function(x, ...) {
  get_src_config(unique(get_source(x)), ...)
}

get_id_cols <- function(x, id_type = NULL, ...) {

  if (is_id_cols(x)) {
    cfg <- x
  } else {
    cfg <- get_src_config(x, ...)[["id_cols"]]
    assert_that(is_id_cols(cfg))
  }

  if (is.null(id_type)) {
    return(unlist(cfg))
  }

  assert_that(is.string(id_type), has_name(cfg, id_type))

  cfg[[id_type]]
}

get_col_defaults <- function(x, table = NULL, ...) {

  assert_that(null_or(table, is.string))

  if (is_col_defaults(x)) {

    res <- x[["cols"]]

    if (is.string(table)) {
      assert_that(identical(table, x[["table"]]))
    }

    return(res)
  }

  if (all_is(x, is_col_defaults)) {
    cfg <- x
  } else {
    cfg <- get_src_config(x, ...)[["col_defaults"]]
    assert_that(all_is(cfg, is_col_defaults))
  }

  res <- lapply(cfg, `[[`, "cols")

  null_id <- lgl_ply(lapply(res, `[[`, "id_col"), is.null)
  def_id  <- get_id_cols(x, "icustay", ...)

  res[null_id] <- Map(`[[<-`, res[null_id], "id_col", def_id)

  if (is.null(table)) {
    return(res)
  }

  hit <- table == chr_ply(cfg, `[[`, "table")

  assert_that(sum(hit) == 1L)

  res[[which(hit)]]
}

get_data_fun <- function(...) get_src_config(...)[["data_fun"]]

default_id_col <- function(table, ...) {
  get_col_defaults(..., table = table)[["id_col"]]
}

default_time_col <- function(table, ...) {
  get_col_defaults(..., table = table)[["time_col"]]
}

default_val_col <- function(table, ...) {
  get_col_defaults(..., table = table)[["val_col"]]
}

default_unit_col <- function(table, ...) {
  get_col_defaults(..., table = table)[["unit_col"]]
}
