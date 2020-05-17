
#' Data source configuration
#'
#' In order to use a data source with `ricu`, a configuration object has to be
#' created.
#'
#' @param name Name of the data source
#' @param id_cols List passed to [new_id_cols()]
#' @param tables List passed to [as_col_defaults()] and potentially to
#' [as_table_spec()]
#' @param url String valued url used for data downloading (see
#' [download_source()])
#'
#' @rdname src_config
#' @keywords internal
#'
new_src_config <- function(name, id_cols, tables, url = NULL) {

  assert_that(is.string(name), is.list(id_cols),
              all(lgl_ply(tables, has_name, c("name", "defaults"))))

  if (!is_id_cols(id_cols)) {
    id_cols <- new_id_cols(id_cols, name)
  } else {
    assert_that(identical(get_src_name(id_cols), name))
  }

  col_defaults <- as_col_defaults(tables)

  if (all(lgl_ply(tables, has_name, c("files", "cols")))) {
    table_specs <- as_table_spec(tables)
  } else {
    table_specs <- NULL
  }

  assert_that(is_id_cols(id_cols),
              all_is(col_defaults, is_col_defaults),
              null_or(table_specs, all_is, is_table_spec),
              null_or(url, is.string))

  structure(list(name = name, id_cols = id_cols, col_defaults = col_defaults,
                 url = url, table_specs = table_specs),
            class = c(paste0(name, "_cfg"), "src_config"))
}

#' @param x Object to test/coerce
#'
#' @rdname src_config
#' @keywords internal
#'
is_src_config <- function(x) inherits(x, "src_config")

#' @rdname src_config
#' @keywords internal
#'
get_url <- function(x) {
  assert_that(is_src_config(x))
  x[["url"]]
}

#' @keywords internal
#' @export
get_src_name.src_config <- function(x) x[["name"]]

#' ID columns
#'
#' A set of id columns corresponding to `patient`, `hadm` (hospital stay) and
#' `icustay` (ICU stay) can be represented by an `id_cols` object.
#'
#' @param cfg List containing string valued column names
#'
#' @rdname id_cols
#' @keywords internal
#'
new_id_cols <- function(cfg, src) {

  cfg <- as.list(cfg)

  assert_that(is_unique(names(cfg)))

  start <- lapply(cfg, `[[`, "start")
  end   <- lapply(cfg, `[[`, "end")
  table <- lapply(cfg, `[[`, "table")

  check <- ifelse(lgl_ply(start, not_null) | lgl_ply(end, not_null),
                  lgl_ply(table, not_null),  lgl_ply(table, is.null))

  assert_that(all(check))

  if (not_null(src)) {
    src <- paste0(src, "_ids")
  }

  structure(
    Map(list, name = names(cfg), id = chr_ply(cfg, `[[`, "id"),
        pos = int_ply(cfg, `[[`, "position"), start = start, end = end,
        table = table),
    class = c(src, "id_cols")
  )
}

#' @param x Object to test/coerce
#'
#' @rdname id_cols
#' @keywords internal
#'
is_id_cols <- function(x) inherits(x, "id_cols")

#' @export
get_src_name.id_cols <- function(x) {
  sub("_ids$", "", class(x)[1L])
}

#' @rdname id_cols
#' @keywords internal
#' @export
#'
get_id_cols <- function(x, ...) UseMethod("get_id_cols", x)

#' @rdname id_cols
#' @keywords internal
#' @export
#'
get_id_cols.id_cols <- function(x, id_name = NULL, ...) {

  assert_that(...length() == 0L, null_or(id_name, is.string))

  if (is.null(id_name)) {
    return(x)
  }

  opt <- chr_ply(x, `[[`, "id")
  ind <- id_name == opt

  assert_that(sum(ind) == 1L, msg = paste0("Cannot find exactly one of `",
    id_name, "` among options ", paste0("`", opt, "`", collapse = ", "),
    " for subsetting `id_cols`"))

  res <- x[which(ind)]
  class(res) <- class(x)

  res
}

#' @rdname id_cols
#' @keywords internal
#' @export
#'
get_id_cols.src_config <- function(x, ...) get_id_cols(x[["id_cols"]], ...)

#' @rdname id_cols
#' @keywords internal
#' @export
#'
get_id_cols.data_src <- function(x, ...) get_id_cols(attr(x, "id_cols"), ...)

#' @rdname id_cols
#' @keywords internal
#' @export
#'
get_id_cols.default <- function(x, id_name = NULL, ...) {
  get_id_cols(get_src_config(x, ...), id_name = id_name)
}

id_positions <- function(x, id = NULL, ...) {

  x <- get_id_cols(x, ...)

  res <- setNames(int_ply(x, `[[`, "pos"), chr_ply(x, `[[`, "id"))

  if (is.null(id)) {
    return(res)
  }

  assert_that(all(id %in% names(res)))

  res[id]
}

id_types <- function(x, ...) {

  x <- get_id_cols(x, ...)

  setNames(chr_ply(x, `[[`, "id"), chr_ply(x, `[[`, "name"))
}

#' Default columns
#'
#' A table can have some of its columns marked as default columns for the
#' following concepts:
#'
#' * `id_col`: column will be used for as id for `icu_tbl` objects
#' * `index_col`: column represents a timestamp variable and will be use as
#'   such for `ts_tbl` objects
#' * `val_col`: column contains the measured variable of interest
#' * `unit_col`: column specifies the unit of measurement in the corresponding
#'   `val_col`
#'
#' @param table Table name (string)
#' @param cfg Named list containing column names
#'
#' @rdname col_defaults
#' @keywords internal
#'
new_col_defaults <- function(table, cfg) {

  opts <- c("id_col", "index_col", "val_col", "unit_col")

  cfg <- as.list(cfg)

  assert_that(is.list(cfg), all_is(cfg, is.string))

  if (length(cfg)) {
    assert_that(!is.null(names(cfg)), all(names(cfg) %in% opts))
  }

  cfg <- cfg[opts]
  names(cfg) <- opts

  structure(list(table = table, cols = cfg), class = "col_defaults")
}

#' @param x Object to test/coerce
#'
#' @rdname col_defaults
#' @keywords internal
#'
is_col_defaults <- function(x) inherits(x, "col_defaults")

#' @rdname col_defaults
#' @keywords internal
#' @export
#'
as_col_defaults <- function(x) UseMethod("as_col_defaults", x)

#' @rdname col_defaults
#' @keywords internal
#' @export
#'
as_col_defaults.col_defaults <- function(x) x

#' @rdname col_defaults
#' @keywords internal
#' @export
#'
as_col_defaults.list <- function(x) {
  lapply(x, do_call, new_col_defaults, c("name", "defaults"))
}

#' @rdname col_defaults
#' @keywords internal
#' @export
#'
get_col_defaults <- function(x, ...) UseMethod("get_col_defaults", x)

#' @rdname col_defaults
#' @keywords internal
#' @export
#'
get_col_defaults.list <- function(x, table = NULL, ...) {

  assert_that(...length() == 0L, all_fun(x, is_col_defaults))

  if (is.null(table)) {
    return(x)
  }

  is.string(table)

  opt <- chr_ply(x, `[[`, "table")
  ind <- table == opt

  assert_that(sum(ind) == 1L, msg = paste0("Cannot find exactly one of `",
    table, "` among options ", paste0("`", opt, "`", collapse = ", "),
    " for subsetting `col_defaults`")
  )

  x[[which(ind)]]
}

#' @rdname col_defaults
#' @keywords internal
#' @export
#'
get_col_defaults.src_config <- function(x, ...) {
  get_col_defaults(x[["col_defaults"]], ...)
}

#' Table specification
#'
#' Used when importing a data source, a table specification defines column
#' data types, source file names, a mapping of source to imported column
#' names and optionally a number of expected rows and a partitioning scheme.
#'
#' @param table String valued table name
#' @param files A character vector of file names
#' @param cols List containing a list per column each holding string valued
#' entries `name` (column name as used by `ricu`), `col` (column name as used
#' in the raw data) and `spec` (name of [readr::cols()] column specification).
#' Further entries will be passed as argument to the respective `readr` column
#' specification
#' @param nrow A count indicating the expected number of rows
#' @param partitioning A table partitioning is defined by a column name and a
#' vector of numeric values that are passed as `vec` argument to
#' `base::findInterval()`
#'
#' @rdname table_spec
#' @keywords internal
#'
new_table_spec <- function(table, files, cols, nrow = NULL,
                           partitioning = NULL) {

  col_spec <- function(name, col, spec, ...) {
    do.call(call, c(list(spec), list(...)))
  }

  tbl_spec <- function(...) substitute(cols(...))

  assert_that(is.string(table), is.character(files), length(files) > 0L,
              null_or(nrow, is.count), is.list(cols),
              null_or(partitioning, is.list),
              null_or(partitioning, has_name, c("col", "breaks")))

  spec <- lapply(cols, do_call, col_spec)
  names(spec) <- chr_ply(cols, `[[`, "col")

  spec <- do.call(tbl_spec, as.list(spec))
  spec <- eval(spec, envir = asNamespace("readr"))

  cols <- chr_ply(cols, `[[`, "name")

  structure(list(table = table, files = files, cols = cols, spec = spec,
                 nrow = nrow, partitioning = partitioning),
            class = "table_spec")
}

#' @param x Object to test/coerce
#'
#' @rdname table_spec
#' @keywords internal
#'
is_table_spec <- function(x) inherits(x, "table_spec")

#' @rdname table_spec
#' @keywords internal
#' @export
#'
as_table_spec <- function(x) UseMethod("as_table_spec", x)

#' @rdname table_spec
#' @keywords internal
#' @export
#'
as_table_spec.table_spec <- function(x) x

#' @rdname table_spec
#' @keywords internal
#' @export
#'
as_table_spec.list <- function(x) {
  lapply(x, do_call, new_table_spec, c("name", "files", "cols", "num_rows",
                                       "partitioning"))
}

#' @rdname table_spec
#' @keywords internal
#'
get_table_specs <- function(x) {
  assert_that(is_src_config(x))
  x[["table_specs"]]
}

#' @rdname table_spec
#' @keywords internal
#' @export
#'
table_names <- function(x) UseMethod("table_names", x)

#' @rdname table_spec
#' @keywords internal
#' @export
#'
table_names.src_config <- function(x) table_names(get_table_specs(x))

#' @rdname table_spec
#' @keywords internal
#' @export
#'
table_names.list <- function(x) chr_ply(x, table_names)

#' @rdname table_spec
#' @keywords internal
#' @export
#'
table_names.table_spec <- function(x) x[["table"]]

#' @rdname table_spec
#' @keywords internal
#' @export
#'
file_names <- function(x) UseMethod("file_names", x)

#' @rdname table_spec
#' @keywords internal
#' @export
#'
file_names.src_config <- function(x) file_names(get_table_specs(x))

#' @rdname table_spec
#' @keywords internal
#' @export
#'
file_names.list <- function(x) lapply(x, file_names)

#' @rdname table_spec
#' @keywords internal
#' @export
#'
file_names.table_spec <- function(x) x[["files"]]

#' @rdname table_spec
#' @keywords internal
#' @export
#'
n_partitions <- function(x) UseMethod("n_partitions", x)

#' @rdname table_spec
#' @keywords internal
#' @export
#'
n_partitions.src_config <- function(x) n_partitions(get_table_specs(x))

#' @rdname table_spec
#' @keywords internal
#' @export
#'
n_partitions.list <- function(x) int_ply(x, n_partitions)

#' @rdname table_spec
#' @keywords internal
#' @export
#'
n_partitions.table_spec <- function(x) {
  abs(length(x[["partitioning"]][["breaks"]]) - 1L)
}

#' @rdname table_spec
#' @keywords internal
#'
fst_names <- function(x) {

  to_filenames <- function(tbl_name, n_part) {

    if (n_part > 1L) {
      tbl_name <- file.path(tbl_name, seq_len(n_part))
    }

    paste0(tbl_name, ".fst")
  }

  if (is_table_spec(x)) {
    to_filenames(table_names(x), n_partitions(x))
  } else {
    Map(to_filenames, table_names(x), n_partitions(x))
  }
}

#' @rdname table_spec
#' @keywords internal
#'
get_col_spec <- function(x) {

  assert_that(is_table_spec(x))

  x[["spec"]]
}

#' @rdname table_spec
#' @keywords internal
#'
check_n_rows <- function(x, comparison) {

  assert_that(is_table_spec(x), is.count(comparison))

  nrow <- x[["nrow"]]

  if (!is.null(nrow)) {
    assert_that(are_equal(nrow, comparison))
  }

  comparison
}

#' @rdname table_spec
#' @keywords internal
#'
get_col_names <- function(x) {

  assert_that(is_table_spec(x))

  res <- x[["cols"]]
  names(res) <- names(get_col_spec(x)[["cols"]])

  res
}

#' @rdname table_spec
#' @keywords internal
#'
get_part_fun <- function(x, orig_names = FALSE) {

  assert_that(is_table_spec(x), is.flag(orig_names))

  part <- x[["partitioning"]]

  col <- part[["col"]]
  breaks <- force(part[["breaks"]])

  if (orig_names) {
    nms <- get_col_names(x)
    col <- names(nms[nms == col])
  }

  assert_that(is.string(col))

  function(x) {
    findInterval(x[[col]], breaks, all.inside = TRUE)
  }
}

#' Load configuration for a data source
#'
#' For a data source to become available to ricu, a JSON base configuration
#' file is required which is parsed into a `src_config` object (see
#' [new_src_config()]).
#'
#' @param x Object used to determine the data source
#' @param ... Generic consistency/passed on to further `get_src_config()` calls
#'
#' @export
#'
get_src_config <- function(x, ...) UseMethod("get_src_config", x)

#' @rdname get_src_config
#'
#' @export
#'
get_src_config.src_config <- function(x, ...) x

#' @param name String valued name of a config file which will be looked up in
#' the default config directors
#' @param file Full file name to load
#'
#' @rdname get_src_config
#'
#' @export
get_src_config.character <- function(x, name = "data-sources",
                                     file = NULL, ...) {

  x <- unique(x)

  assert_that(is.string(x))

  if (is.null(file)) {

    cfg <- get_config(name)

  } else {

    assert_that(missing(name), file.exists(file))

    cfg <- read_json(file)
  }

  hit <- which(x == chr_ply(cfg, `[[`, "name"))

  assert_that(is.count(hit))

  cfg <- cfg[[hit]]

  do.call(new_src_config, cfg[c("name", "id_cols", "tables", "url")])
}

#' @rdname get_src_config
#'
#' @export
#'
get_src_config.default <- function(x, ...) get_src_config(get_src_name(x), ...)

get_data_fun <- function(...) get_src_config(...)[["data_fun"]]

#' Get default columns
#'
#' For a table, query the default columns as specified by an `id_cols`
#' ([new_id_cols()]) or `col_defaults` ([new_col_defaults()]) object.
#'
#' @param x Object used for dispatch
#' @param type The column type, e.g. `id`, `index`, `unit`, `value`
#'
#' @rdname default_col
#' @export
#'
default_col <- function(x, type) UseMethod("default_col", x)

#' @export
default_col.data_src <- function(x, type = "id") {

  assert_that(is.string(type))

  def <- attr(x, "defaults")[["cols"]]
  res <- def[[paste0(type, "_col")]]

  if (identical(type, "id") && is.null(res)) {
    opt <- get_id_cols(x)
    res <- opt[[which.max(int_ply(opt, `[[`, "pos"))]][["id"]]
  }

  assert_that(is.string(res))

  res
}

#' @rdname get_src_config
#' @export
get_aux_env <- function(x) UseMethod("get_aux_env", x)

#' @rdname get_src_config
#' @export
get_aux_env.data_src <- function(x) attr(x, "aux_env")

