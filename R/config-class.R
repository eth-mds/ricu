
#' Data source configuration
#'
#' In order to use a data source with `ricu`, a configuration object has to be
#' created.
#'
#' @param name Name of the data source
#' @param id_cfg List passed to [new_id_cfg()]
#' @param tables List passed to [as_col_cfg()] and potentially to
#' [as_tbl_cfg()]
#' @param url String valued url used for data downloading (see
#' [download_src()])
#'
#' @rdname src_cfg
#' @keywords internal
#'
new_src_cfg <- function(name, id_cfg, col_cfg, tbl_cfg = NULL, url = NULL,
                        class_prefix = name) {

  assert_that(
    is.string(name), null_or(url, is.string), is_id_cfg(id_cfg),
    all_fun(col_cfg, is_col_cfg), null_or(tbl_cfg, all_fun, is_tbl_cfg)
  )

  names(col_cfg) <- chr_ply(col_cfg, tbl_name)
  names(tbl_cfg) <- chr_ply(tbl_cfg, tbl_name)

  structure(
    list(name = name, url = url, prefix = class_prefix, id_cfg = id_cfg,
         col_cfg = col_cfg, tbl_cfg = tbl_cfg),
    class = paste0(c(class_prefix, "src"), "_cfg")
  )
}

#' @param x Object to test/coerce
#'
#' @rdname src_cfg
#' @keywords internal
#'
is_src_cfg <- function(x) inherits(x, "src_cfg")

#' ID columns
#'
#' A set of id columns corresponding to `patient`, `hadm` (hospital stay) and
#' `icustay` (ICU stay) can be represented by an `id_cfg` object.
#'
#' @param cfg List containing string valued column names
#'
#' @rdname id_cfg
#' @keywords internal
#'
new_id_cfg <- function(name, src, id, pos, start = NA_character_,
                       end = NA_character_, table = NA_character_,
                       class_prefix = src) {

  assert_that(
    is.character(name), is_unique(name), is.character(id),
    is.integer(pos), is.character(start), is.character(end),
    is.character(table), is.string(src), is.character(class_prefix)
  )

  check <- ifelse(is.na(start) & is.na(end), is.na(table), !is.na(table))

  assert_that(all(check), msg = paste0("Invalid ID config for IDs ",
    concat(quote_bt(id[!check])), ": table name needed due to start/end args")
  )

  if (has_length(class_prefix)) {
    class_prefix <- paste0(class_prefix, "_ids")
  }

  new_rcrd(
    vec_recycle_common(name = name, id = id, pos = pos, start = start,
                       end = end, table = table),
    src = src, class = c(class_prefix, "id_cfg")
  )
}

#' @param x Object to test/coerce
#'
#' @rdname id_cfg
#' @keywords internal
#'
is_id_cfg <- function(x) inherits(x, "id_cfg")

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
#' @param defaults Named list containing column names
#' @param times Character vector enumerating columns that contain time
#' information
#'
#' @rdname col_cfg
#' @keywords internal
#'
new_col_cfg <- function(table, src, id_var = NULL, index_var = NULL,
                        val_var = NULL, unit_var = NULL, time_vars = NULL,
                        class_prefix = src) {

  assert_that(
    is.string(table), is.string(src), null_or_str(id_var),
    null_or_str(index_var), null_or_str(val_var), null_or_str(unit_var),
    null_or(time_vars, is.character), is.character(class_prefix)
  )

  if (not_null(index_var)) {
    assert_that(
      index_var %in% time_vars, msg = paste0("expecting the index variable ",
      "to be among the time variables")
    )
  }

  if (has_length(class_prefix)) {
    class_prefix <- paste0(class_prefix, "_cols")
  }

  structure(
    list(table = table, src = src, id_var = id_var, index_var = index_var,
         val_var = val_var, unit_var = unit_var, time_vars = time_vars),
    class = c(class_prefix, "col_cfg")
  )
}

#' @param x Object to test/coerce
#'
#' @rdname col_cfg
#' @keywords internal
#'
is_col_cfg <- function(x) inherits(x, "col_cfg")

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
#' @rdname tbl_cfg
#' @keywords internal
#'
new_tbl_cfg <- function(table, src, files, cols, num_rows = NULL,
                        partitioning = NULL, class_prefix = src) {

  col_spc <- function(name, col, spec, ...) {
    do.call(call, c(list(spec), list(...)))
  }

  tbl_spc <- function(...) substitute(cols(...))

  if (is.character(files)) {
    files <- list(files)
  }

  assert_that(
    is.string(table), is.string(src), all_fun(files, is.character),
    all_fun(cols, is.list), all_fun(cols, has_name, c("name", "col", "spec")),
    null_or(num_rows, is.count), null_or(partitioning, is.list),
    null_or(partitioning, has_name, c("col", "breaks"))
  )

  if (is.null(num_rows)) {
    num_rows <- NA_integer_
  }

  if (is.null(names(files))) {
    names(files) <- chr_xtr(files, 1L)
  }

  if (has_length(class_prefix)) {
    class_prefix <- paste0(class_prefix, "_cols")
  }

  spec <- lapply(cols, do_call, col_spc)
  names(spec) <- chr_xtr(cols, "col")

  spec <- eval(do.call(tbl_spc, spec), envir = asNamespace("readr"))
  cols <- chr_xtr(cols, "name")

  structure(
    list(table = table, src = src, files = files, cols = cols, spec = spec,
         nrow = num_rows, partitioning = partitioning),
    class = c(class_prefix, "tbl_cfg")
  )
}

#' @param x Object to test/coerce
#'
#' @rdname tbl_cfg
#' @keywords internal
#'
is_tbl_cfg <- function(x) inherits(x, "tbl_cfg")

#' Load configuration for a data source
#'
#' For a data source to become available to ricu, a JSON base configuration
#' file is required which is parsed into a `src_cfg` object (see
#' [new_src_cfg()]).
#'
#' @param src (Optional) name(s) of data sources used for subsetting
#' @param name String valued name of a config file which will be looked up in
#' the default config directors
#' @param file Full file name to load
#'
#' @export
#'
read_src_cfg <- function(src = NULL, name = "data-sources", file = NULL) {

  mk_id_cfg <- function(name, id_cfg, class_prefix = name, ...) {
    do.call(new_id_cfg,
      list(
        names(id_cfg), name, chr_xtr(id_cfg, "id"),
        int_xtr(id_cfg, "position"), chr_xtr_null(id_cfg, "start"),
        chr_xtr_null(id_cfg, "end"), chr_xtr_null(id_cfg, "table"),
        class_prefix = class_prefix
      )
    )
  }

  mk_col_cfg <- function(name, tables, class_prefix = name, ...) {
    args <- map(c, chr_xtr(tables, "name"), name, lst_xtr(tables, "defaults"),
                class_prefix = list(list(class_prefix)))
    lapply(args, do_call, new_col_cfg)
  }

  mk_tbl_cfg <- function(name, tables, class_prefix = name, ...) {
    args <- map(c, chr_xtr(tables, "name"), name,
      mul_xtr(tables, c("files", "cols", "num_rows", "partitioning")),
      class_prefix = list(list(class_prefix))
    )
    lapply(args, do_call, new_tbl_cfg)
  }

  mk_cfg <- function(x) {
    new_src_cfg(x[["name"]], do.call(mk_id_cfg, x), do.call(mk_col_cfg, x),
                do.call(mk_tbl_cfg, x), x[["url"]], x[["class_prefix"]])
  }

  if (is.null(file)) {

    cfg <- get_config(name)

  } else {

    assert_that(missing(name), file.exists(file))

    cfg <- read_json(file)
  }

  cfg_nme <- chr_xtr(cfg, "name")

  if (is.null(src)) {
    src <- cfg_nme
  }

  assert_that(is.character(src), all(src %in% cfg_nme))

  lapply(cfg[cfg_nme %in% src], mk_cfg)
}
