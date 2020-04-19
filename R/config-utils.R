
#' Data source configuration
#'
#' In order to use a data source with `ricu`, a configuration object has to be
#' created.
#'
#' @param name Name of the data source
#' @param id_cols List passed to [as_id_cols()]
#' @param tables List passed to [as_col_defaults()] and potentially to
#' [as_table_spec()]
#' @param attach_hook,data_fun Function names (passed as strings), specifying
#' a function to be run upon data set attaching (see [attach_source()]) and a
#' function resposible for data loading (see [data_tbl()])
#' @param url String valued url used for data downloading (see
#' [download_source()])
#'
#' @rdname src_config
#' @keywords internal
#'
new_src_config <- function(name, id_cols, tables, attach_hook = NULL,
                           data_fun = NULL, url = NULL) {

  assert_that(is.list(id_cols), all_is(id_cols, is.string),
              all(lgl_ply(tables, has_name, c("name", "defaults"))))

  id_cols <- as_id_cols(id_cols)
  col_defaults <- as_col_defaults(tables)

  if (all(lgl_ply(tables, has_name, c("files", "cols")))) {
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
#'
#' @export
#'
get_source.src_config <- function(x) x[["name"]]

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
new_id_cols <- function(cfg) {

  cfg <- as.list(cfg)

  assert_that(is.list(cfg), length(cfg) > 0L, has_name(cfg, "icustay"),
              all_is(cfg, is.string), is_unique(c(unlist(cfg), names(cfg))))

  structure(
    cfg[intersect(c("patient", "hadm", "icustay"), names(cfg))],
    class = "id_cols"
  )
}

#' @param x Object to test/coerce
#'
#' @rdname id_cols
#' @keywords internal
#'
is_id_cols <- function(x) inherits(x, "id_cols")

#' @rdname id_cols
#' @keywords internal
#' @export
#'
as_id_cols <- function(x) UseMethod("as_id_cols", x)

#' @rdname id_cols
#' @keywords internal
#' @export
#'
as_id_cols.id_cols <- function(x) x

#' @rdname id_cols
#' @keywords internal
#' @export
#'
as_id_cols.list <- function(x) new_id_cols(x)

#' @rdname id_cols
#' @keywords internal
#' @export
#'
as_id_cols.src_config <- function(x) x[["id_cols"]]

#' Default columns
#'
#' A table can have some of its columns marked as default columns for the
#' following concepts:
#'
#' * `id_col`: column will be used for as id for `icu_tbl` objects
#' * `time_col`: column represents a timestamp variable and will be use as such
#'   for `ts_tbl` objects
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
as_col_defaults.src_config <- function(x) x[["col_defaults"]]

#' Table specification
#'
#' Used when importing a data source, a table specification defines column
#' data types, source file names, a mapping of source to imported column
#' names and optionally a number of expected rows and a partitioning scheme.
#'
#' @param table String valued table name
#' @param fies A character vector of file names
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

  do.call(new_src_config, cfg[c("name", "id_cols", "tables", "attach_hook",
                                "data_fun", "url")])
}

#' @rdname get_src_config
#'
#' @export
#'
get_src_config.default <- function(x, ...) get_src_config(get_source(x), ...)

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

#' Get default columns
#'
#' For a table, query the default columns as specified by an `id_cols`
#' ([new_id_cols()]) or `col_defaults` ([new_col_defaults()]) object.
#'
#' @param table String valued table name
#' @param ... Passed to further methods
#'
#' @rdname get_col_defaults
#' @export
#'
default_id_col <- function(table, ...) {
  get_col_defaults(..., table = table)[["id_col"]]
}

#' @rdname get_col_defaults
#' @export
#'
default_time_col <- function(table, ...) {
  get_col_defaults(..., table = table)[["time_col"]]
}

#' @rdname get_col_defaults
#' @export
#'
default_val_col <- function(table, ...) {
  get_col_defaults(..., table = table)[["val_col"]]
}

#' @rdname get_col_defaults
#' @export
#'
default_unit_col <- function(table, ...) {
  get_col_defaults(..., table = table)[["unit_col"]]
}

new_tbl_src <- function(files, id_cols, defaults, load_fun) {

  assert_that(is_id_cols(id_cols), is_col_defaults(defaults),
              is.function(load_fun))

  res <- prt::new_prt(files)
  class(res) <- c("tbl_src", class(res))

  attr(res, "id_cols")  <- id_cols
  attr(res, "defaults") <- defaults
  attr(res, "load_fun") <- load_fun

  res
}

is_tbl_src <- function(x) inherits(x, "tbl_src")
