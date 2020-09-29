
#' Internal utilities for working with data source configurations
#'
#' Data source configuration objects store information on data sources used
#' throughout `ricu`. This includes URLs for data set downloading, Column
#' specifications used for data set importing, default values per table for
#' important columns such as index columns when loading data and how different
#' patient identifiers used throughout a dataset relate to another. Per
#' dataset, a `src_cfg` object is created from a JSON file (see
#' [load_src_cfg()]), consisting of several helper-classes compartmentalizing
#' the pieces of information outlined above. Alongside constructors for the
#' various classes, several utilities, such as inheritance checks, coercion
#' functions, as well as functions to extract pieces of information from these
#' objects are provided.
#'
#' @details
#' The following classes are used to represent data source configuration
#' objects:
#'
#' * `src_cfg`: wraps objects `id_cfg`, `col_cfg` and optionally `tbl_cfg`
#' * `id_cfg`: contains information in ID systems and is created from `id_cfg`
#'   entries in config files
#' * `col_cfg`: contains column default settings represented by `defaults`
#'   entries in table configuration blocks
#' * `tbl_cfg`: used when importing data and therefore encompasses information
#'   in `files`, `num_rows` and `cols` entries of table configuration blocks
#'
#' Represented by a `col_cfg`, a table can have some of its columns marked as
#' default columns for the following concepts and further column meanings can
#' be specified via `...`:
#'
#' * `id_col`: column will be used for as id for `icu_tbl` objects
#' * `index_col`: column represents a timestamp variable and will be use as
#'   such for `ts_tbl` objects
#' * `val_col`: column contains the measured variable of interest
#' * `unit_col`: column specifies the unit of measurement in the corresponding
#'   `val_col`
#'
#' Alongside constructors (`new_*()`), inheritance checking functions
#' (`is_*()`), as well as coercion functions (`as_*(`), relevant utility
#' functions include:
#'
#' * `src_url()`: retrieve the URL of a data source
#' * `id_var_opts()`: column name(s) corresponding to ID systems
#' * `src_name()`: name of the data source
#' * `tbl_name()`: name of a table
#'
#' Coercion between objects under some circumstances can yield list-of object
#' return types. For example when coercing `src_cfg` to `tbl_cfg`, this will
#' result in a list of `tbl_cfg` objects, as multiple tables typically
#' correspond to a data source.
#'
#' @param name Name of the data source
#' @param id_cfg An `id_cfg` object for the given data source
#' @param col_cfg A list of `col_cfg` objects representing column defaults for
#' all tables of the
#' @param tbl_cfg A list of `tbl_cfg` containing information on how tables are
#' organized (may be `NULL`)
#' @param ... Further objects to add (such as an URL specification)
#' @param class_prefix A character vector of class prefixes that are added to
#' the instantiated classes
#'
#' @rdname src_cfg
#' @keywords internal
#'
new_src_cfg <- function(name, id_cfg, col_cfg, tbl_cfg = NULL, ...,
                        class_prefix = name) {

  if (all_fun(tbl_cfg, is.null)) {
    tbl_cfg <- NULL
  }

  assert_that(
    is.string(name), is_id_cfg(id_cfg), all_fun(col_cfg, is_col_cfg),
    null_or(tbl_cfg, all_fun, is_tbl_cfg), is.character(class_prefix)
  )

  if (has_length(class_prefix)) {
    subclass <- paste0(class_prefix, "_cfg")
  }

  names(col_cfg) <- chr_ply(col_cfg, tbl_name)

  if (not_null(tbl_cfg)) {

    names(tbl_cfg) <- chr_ply(tbl_cfg, tbl_name)

    assert_that(setequal(names(col_cfg), names(tbl_cfg)))
  }

  structure(
    list(name = name, ..., prefix = class_prefix, id_cfg = id_cfg,
         col_cfg = col_cfg, tbl_cfg = tbl_cfg),
    class = c(subclass, "src_cfg")
  )
}

#' @param src Data source name
#' @param id,start,end Name(s) of ID column(s), as well as respective start
#' and end timestamps
#' @param pos Integer valued position, ordering IDs by their cardinality
#' @param table Table name
#'
#' @rdname src_cfg
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

#' @param id_var,index_var,val_var,unit_var,time_vars Names of columns with
#' respective meanings
#'
#' @rdname src_cfg
#' @keywords internal
#'
new_col_cfg <- function(table, src, id_var = NULL, index_var = NULL,
                        val_var = NULL, unit_var = NULL, time_vars = NULL, ...,
                        class_prefix = src) {

  extra <- list(...)

  assert_that(
    is.string(table), is.string(src), null_or(id_var, is.string),
    null_or(index_var, is.string), null_or(val_var, is.string),
    null_or(unit_var, is.string), null_or(time_vars, is.character),
    is.character(class_prefix), all_fun(extra, is.character)
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

  res <- c(
    list(table = table, src = src, id_var = id_var, index_var = index_var,
         val_var = val_var, unit_var = unit_var, time_vars = time_vars),
    extra
  )

  structure(res, class = c(class_prefix, "col_cfg"))
}

#' @param cols List containing a list per column each holding string valued
#' entries `name` (column name as used by `ricu`), `col` (column name as used
#' in the raw data) and `spec` (name of [readr::cols()] column specification).
#' Further entries will be passed as argument to the respective `readr` column
#' specification
#' @param num_rows A count indicating the expected number of rows
#' @param partitioning A table partitioning is defined by a column name and a
#' vector of numeric values that are passed as `vec` argument to
#' `base::findInterval()`
#'
#' @rdname src_cfg
#' @keywords internal
#'
new_tbl_cfg <- function(table, src, files = NULL, cols = NULL, num_rows = NULL,
                        partitioning = NULL, ..., class_prefix = src) {

  col_spc <- function(name, spec, ...) {
    do.call(call, c(list(spec), list(...)))
  }

  tbl_spc <- function(...) substitute(cols(...))

  assert_that(
    is.string(table), is.string(src), null_or(files, is.character),
    null_or(cols, all_fun, is.list),
    null_or(cols, all_fun, has_name, c("name", "spec")),
    null_or(num_rows, is.count), null_or(partitioning, is.list),
    null_or(partitioning, has_name, c("col", "breaks"))
  )

  if (is.null(num_rows)) {
    num_rows <- NA_integer_
  }

  if (has_length(class_prefix)) {
    class_prefix <- paste0(class_prefix, "_cols")
  }

  if (not_null(cols)) {
    spec <- lapply(cols, do_call, col_spc)
    names(spec) <- chr_xtr(cols, "name")

    spec <- eval(do.call(tbl_spc, spec), envir = asNamespace("readr"))
    cols <- names(cols)
  } else {
    spec <- NULL
  }

  structure(
    list(table = table, src = src, files = files, cols = cols, spec = spec,
         nrow = num_rows, partitioning = partitioning, ...),
    class = c(class_prefix, "tbl_cfg")
  )
}

#' Load configuration for a data source
#'
#' For a data source to be accessible by `ricu`, a configuration object
#' inheriting from the S3 class `src_cfg` is required. Such objects can be
#' generated from JSON based configuration files, using `load_src_cfg()`.
#' Information encoded by this configuration object includes available ID
#' systems (mainly for use in [change_id()], default column names per table
#' for columns with special meaning (such as index column, value columns, unit
#' columns, etc.), as well as a specification used for initial setup of the
#' dataset which includes file names and column names alongside their data
#' types.
#'
#' @details
#' Configuration files are looked for as files `name` with added suffix
#' `.json` starting with the directory (or directories) supplied as `cfg_dirs`
#' argument, followed by the directory specified by the environment variable
#' `RICU_CONFIG_PATH`, and finally in `extdata/config` of the package install
#' directory. If files with matching names are found in multiple places they
#' are concatenated such that in cases of name clashes. the earlier hits take
#' precedent over the later ones. The following JSON code blocks show excerpts
#' of the config file available at
#'
#' ```
#' system.file("extdata", "config", "data-sources.json", package = "ricu")
#' ```
#'
#' A data source configuration entry in a config file starts with a name,
#' followed by optional entries `class_prefix` and further (variable)
#' key-value pairs, such as an URL. For more information on `class_prefix`,
#' please refer to the end of this section. Further entries include `id_cfg`
#' and `tables` which are explained in more detail below. As outline, this
#' gives for the data source `mimic_demo`, the following JSON object:
#'
#' ```
#' {
#'   "name": "mimic_demo",
#'   "class_prefix": ["mimic_demo", "mimic"],
#'   "url": "https://physionet.org/files/mimiciii-demo/1.4",
#'   "id_cfg": {
#'     ...
#'   },
#'   "tables": {
#'     ...
#'   }
#' }
#' ```
#'
#' The `id_cfg` entry is used to specify the available ID systems for a data
#' source and how they relate to each other. An ID system within the context
#' of `ricu` is a patient identifier of which typically several are present in
#' a data set. In MIMIC-III, for example, three ID systems are available:
#' patient IDs (`subject_id`), hospital admission IDs (`hadm_id`) and ICU stay
#' IDs (`icustay_id`). Furthermore there is a one-to-many relationship between
#' `subject_id` and `hadm_id`, as well as between `hadm_id` and `icustay_id`.
#' Required for defining an ID system are a name, a `position` entry which
#' orders the ID systems by their cardinality, a `table` entry, alongside
#' column specifications `id`, `start` and `end`, which define how the IDs
#' themselves, combined with start and end times can be loaded from a table.
#' This gives the following specification for the ICU stay ID system in
#' MIMIC-III:
#'
#' ```
#' {
#'   "icustay": {
#'     "id": "icustay_id",
#'     "position": 3,
#'     "start": "intime",
#'     "end": "outtime",
#'     "table": "icustays"
#'   }
#' }
#' ```
#'
#' Tables are defined by a name and entries `files`, `defaults`, and `cols`,
#' as well as optional entries `num_rows` and `partitioning`. As `files` entry,
#' a character vector of file names is expected. For all of MIMIC-III a single
#' `.csv` file corresponds to a table, but for example for HiRID, some tables
#' are distributed in partitions. The `defaults` entry consists of key-value
#' pairs, identifying columns in a table with special meaning, such as the
#' default index column or the set of all columns that represent timestamps.
#' This gives as an example for a table entry for the `chartevents` table in
#' MIMIC-III a JSON object like:
#'
#' ```
#' {
#'   "chartevents": {
#'     "files": "CHARTEVENTS.csv.gz",
#'     "defaults": {
#'       "index_var": "charttime",
#'       "val_var": "valuenum",
#'       "unit_var": "valueuom",
#'       "time_vars": ["charttime", "storetime"]
#'     },
#'     "num_rows": 330712483,
#'     "cols": {
#'       ...
#'     },
#'     "partitioning": {
#'       "col": "itemid",
#'       "breaks": [127, 210, 425, 549, 643, 741, 1483, 3458, 3695, 8440,
#'                  8553, 220274, 223921, 224085, 224859, 227629]
#'     }
#'   }
#' }
#' ```
#'
#' The optional `num_rows` entry is used when importing data (see
#' [import_src()]) as a sanity check, which is not performed if this entry is
#' missing from the data source configuration. The remaining table entry,
#' `partitioning`, is optional in the sense that if it is missing, the table
#' is not partitioned and if it is present, the table will be partitioned
#' accordingly when being imported (see [import_src()]). In order to specify a
#' partitioning, two entries are required, `col` and `breaks`, where the former
#' denotes a column and the latter a numeric vector which is used to construct
#' intervals according to which `col` is binned. As such, currently `col` is
#' required to be of numeric type. A `partitioning` entry as in the example
#' above will assign rows corresponding to `idemid` 1 through 126 to partition
#' 1, 127 through 209 to partition 2 and so on up to partition 17.
#'
#' Column specifications consist of a `name` and a `spec` entry alongside a
#' name which determines the column name that will be used by `ricu`. The
#' `spec` entry is expected to be the name of a column specification function
#' of the `readr` package (see [readr::cols()]) and all further entries in a
#' `cols` object are used as arguments to the `readr` column specification.
#' For the `admissions` table of MIMIC-III the columns `hadm_id` and
#' `admittime` are represented by:
#'
#' ```
#' {
#'   ...,
#'   "hadm_id": {
#'     "name": "HADM_ID",
#'     "spec": "col_integer"
#'   },
#'   "admittime": {
#'     "name": "ADMITTIME",
#'     "spec": "col_datetime",
#'     "format": "%Y-%m-%d %H:%M:%S"
#'   },
#'   ...
#' }
#' ```
#'
#' Internally, a `src_cfg` object consist of further S3 classes, which are
#' instantiated when loading a JSON source configuration file. Functions for
#' creating and manipulating `src_cfg` and related objects are marked
#' `internal` but a brief overview is given here nevertheless:
#'
#' * `src_cfg`: wraps objects `id_cfg`, `col_cfg` and optionally `tbl_cfg`
#' * `id_cfg`: contains information in ID systems and is created from `id_cfg`
#'   entries in config files
#' * `col_cfg`: contains column default settings represented by `defaults`
#'   entries in table configuration blocks
#' * `tbl_cfg`: used when importing data and therefore encompasses information
#'   in `files`, `num_rows` and `cols` entries of table configuration blocks
#'
#' A `src_cfg` can be instantiated without corresponding `tbl_cfg` but
#' consequently cannot be used for data import (see [import_src()]). In that
#' sense, table config entries `files` and `cols` are optional as well with
#' the restriction that the data source has to be already available in `.fst`
#' format
#'
#' An example for such a slimmed down config file is available at
#'
#' ```
#' system.file("extdata", "config", "demo-sources.json", package = "ricu")
#' ```
#'
#' The `class_prefix` entry in a data source configuration is used create sub-
#' classes to `src_cfg`, `id_cfg`, `col_cfg` and `tbl_cfg` classes and passed
#' on to constructors of `src_env` ([new_src_env()]) and `src_tbl`
#' [new_src_tbl()] objects. As an example, for the above `class_prefix` value
#' of `c("mimic_demo", "mimic")`, the corresponding `src_cfg` will be assigned
#' classes `c("mimic_demo_cfg", "mimic_cfg", "src_cfg")` and consequently the
#' `src_tbl` objects will inherit from `"mimic_demo_tbl"`, `"mimic_tbl"` and
#' ` "src_tbl"`. This can be used to adapt the behavior of involved S3 generic
#' function to specifics of the different data sources. An example for this is
#' how [load_difftime()] uses theses sub-classes to smoothen out different
#' time-stamp representations. Furthermore, such a design was chosen with
#' extensibility in mind. Currently, [download_src()] is designed around data
#' sources hosted on PhysioNet, but in order to include a dataset external to
#' PhysioNet, the `download_src()` generic can simply be extended for the new
#' class.
#'
#' @param src (Optional) name(s) of data sources used for subsetting
#' @param name String valued name of a config file which will be looked up in
#' the default config directors
#' @param cfg_dirs Additional directory/ies to look for configuration files
#'
#' @examples
#' cfg <- load_src_cfg("mimic_demo")
#' str(cfg, max.level = 1L)
#' cfg <- cfg[["mimic_demo"]]
#' str(cfg, max.level = 1L)
#'
#' cols <- as_col_cfg(cfg)
#' index_var(cols[["chartevents"]])
#' time_vars(cols[["chartevents"]])
#'
#' as_id_cfg(cfg)
#'
#' @export
#'
load_src_cfg <- function(src = NULL, name = "data-sources", cfg_dirs = NULL) {

  res <- read_src_cfg(src, name, cfg_dirs)

  if (is.null(src)) {
    src <- names(res)
  }

  lapply(res, parse_src_cfg)
}

read_src_cfg <- function(src = NULL, name = "data-sources", cfg_dirs = NULL) {

  combine_srcs <- function(x, y) {

    names(y) <- chr_xtr(y, "name")

    if (is.null(x)) {
      return(y)
    } else if (is.null(y)) {
      return(x)
    }

    c(x, y[setdiff(names(x), names(y))])
  }

  res <- get_config(name, unique(c(cfg_dirs, config_paths())), combine_srcs)

  if (not_null(src)) {

    assert_that(are_in(src, names(res)))
    res[src]

  } else {

    assert_that(has_length(res))
    res
  }
}

parse_src_cfg <- function(x) {

  mk_id_cfg <- function(name, id_cfg, class_prefix = name, ...) {

    id <- chr_xtr(id_cfg, "id")
    po <- int_xtr(id_cfg, "position")
    st <- chr_xtr_null(id_cfg, "start")
    en <- chr_xtr_null(id_cfg, "end")
    tb <- chr_xtr_null(id_cfg, "table")

    args <- list(
      names(id_cfg), name, id, po, st, en, tb, class_prefix = class_prefix
    )

    do.call(new_id_cfg, args)
  }

  mk_col_cfg <- function(name, tables, class_prefix = name, ...) {

    na <- names(tables)
    df <- lst_xtr(tables, "defaults")
    pf <- list(list(class_prefix))

    args <- map(c, na, name, df, class_prefix = pf)

    lapply(args, do_call, new_col_cfg)
  }

  mk_tbl_cfg <- function(name, tables, class_prefix = name, ...) {

    na <- names(tables)
    rs <- Map(`[`, tables, lapply(lapply(tables, names), setdiff, "defaults"))
    pf <- list(list(class_prefix))

    args <- map(c, na, name, rs, class_prefix = pf)

    lapply(args, do_call, new_tbl_cfg)
  }

  assert_that(is.list(x), has_name(x, c("name", "id_cfg", "tables")))

  x[["id_cfg"]]  <- do.call(mk_id_cfg, x)
  x[["col_cfg"]] <- do.call(mk_col_cfg, x)
  x[["tbl_cfg"]] <- do.call(mk_tbl_cfg, x)
  x[["tables"]] <- NULL

  do.call(new_src_cfg, x)
}
