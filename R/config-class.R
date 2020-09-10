
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
                        class_prefix = NULL) {

  if (is.null(class_prefix)) {
    class_prefix <- name
  }

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
is_src_cfg <- is_type("src_cfg")

#' A set of id columns corresponding to `patient`, `hadm` (hospital stay) and
#' `icustay` (ICU stay) can be represented by an `id_cfg` object.
#'
#' @param cfg List containing string valued column names
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

#' @param x Object to test/coerce
#'
#' @rdname src_cfg
#' @keywords internal
#'
is_id_cfg <- is_type("id_cfg")

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
#' @rdname src_cfg
#' @keywords internal
#'
new_col_cfg <- function(table, src, id_var = NULL, index_var = NULL,
                        val_var = NULL, unit_var = NULL, time_vars = NULL,
                        class_prefix = src) {

  assert_that(
    is.string(table), is.string(src), null_or(id_var, is.string),
    null_or(index_var, is.string), null_or(val_var, is.string),
    null_or(unit_var, is.string), null_or(time_vars, is.character),
    is.character(class_prefix)
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
#' @rdname src_cfg
#' @keywords internal
#'
is_col_cfg <- is_type("col_cfg")

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
#' @rdname src_cfg
#' @keywords internal
#'
new_tbl_cfg <- function(table, src, files = NULL, cols = NULL, num_rows = NULL,
                        partitioning = NULL, class_prefix = src) {

  col_spc <- function(name, spec, ...) {
    do.call(call, c(list(spec), list(...)))
  }

  tbl_spc <- function(...) substitute(cols(...))

  if (is.character(files)) {
    files <- list(files)
  }

  assert_that(
    is.string(table), is.string(src), null_or(files, all_fun, is.character),
    null_or(cols, all_fun, is.list),
    null_or(cols, all_fun, has_name, c("name", "spec")),
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
         nrow = num_rows, partitioning = partitioning),
    class = c(class_prefix, "tbl_cfg")
  )
}

#' @param x Object to test/coerce
#'
#' @rdname src_cfg
#' @keywords internal
#'
is_tbl_cfg <- is_type("tbl_cfg")

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
#' `.json` starting with the directory (or directories) supplied as `dir`
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
#' followed by optional entries `class_prefix` and `url`. While the URL is
#' only used initially for data download (see [download_src()]),
#' `class_prefix` is used for instantiations of [`src_env`][new_src_env()] and
#' [`src_tbl`][new_src_tbl()] objects to specify sub-classes (see
#' [attach_src()]). Further entries include `id_cfg` and `tables` which
#' are explained in more detail below. As outline, this gives for the data
#' source `mimic_demo`, the following JSON object:
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
#' `cols` object are used as arugments to the `readr` column specification.
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
#' @param src (Optional) name(s) of data sources used for subsetting
#' @param name String valued name of a config file which will be looked up in
#' the default config directors
#' @param dir Additional directory/ies to look for configuration files
#'
#' @export
#'
load_src_cfg <- function(src = NULL, name = "data-sources", dir = NULL) {

  res <- read_src_cfg(src, name, dir)

  if (is.null(src)) {
    src <- names(res)
  }

  lapply(res, parse_src_cfg)
}

read_src_cfg <- function(src = NULL, name = "data-sources", dir = NULL) {

  file <- paste0(name, ".json")
  res  <- NULL
  dirs <- unique(c(dir, user_config_path(), default_config_path()))

  for (dir in dirs) {

    path <- file.path(dir, file)

    if (file.exists(path)) {

      cfg <- read_json(path)
      nme <- chr_xtr(cfg, "name")

      names(cfg) <- nme

      res <- c(res, cfg[nme %in% setdiff(nme, names(res))])

      if (not_null(src) && all(src %in% nme)) {
        break
      }
    }
  }

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
    rs <- mul_xtr(tables, c("files", "cols", "num_rows", "partitioning"))
    pf <- list(list(class_prefix))

    args <- map(c, na, name, rs, class_prefix = pf)

    lapply(args, do_call, new_tbl_cfg)
  }

  assert_that(is.list(x), has_name(x, c("name", "id_cfg", "tables")))

  id_cfg  <- do.call(mk_id_cfg, x)
  col_cfg <- do.call(mk_col_cfg, x)
  tbl_cfg <- do.call(mk_tbl_cfg, x)

  new_src_cfg(
    x[["name"]], id_cfg, col_cfg, tbl_cfg, x[["url"]], x[["class_prefix"]]
  )
}
