
#' Data import utilities
#'
#' In order to speed up data access operations, `sepsr` does not directly use
#' the PhysioNet provided CSV files, but converts all data to [fst::fst()]
#' format, which allows for random row and column access. Large tables are
#' split into chunks in order to keep memory requirements under control.
#'
#' A nested list supplied as `config` argument controls how the CSV data is
#' parsed and ingested. A the top level, elements `name` (string), `base_url`
#' (string), `version` (string) and `tables` (list) are expected. For every
#' file to be read in, an entry in `tables` with the name of the corresponding
#' file is assumed, requiring a list named `col_spec` and optionally a list
#' named `partitioning`.
#'
#' Every `col_spec` node again is expected to be a named list with names
#' corresponding to column names in the CSV and list valued entries. Finally,
#' a list node defining a single column is a named list with a required,
#' string-valued `type` entry which is interpreted as `readr` column
#' specification function (see [readr::cols()]). Further list entries will be
#' passed as arguments to the column specification function. A column defined
#' as
#'
#' ```
#' list(type = "col_datetime", format = "%Y-%m-%d %H:%M:%S")
#' ```
#'
#' will therefore result in a `readr` column specification like
#'
#' ```
#' col_datetime(format = "%Y-%m-%d %H:%M:%S")
#' ```
#'
#' Entries controlling the partitioning of tables, specified as `partitioning`
#' list nodes are expected to be named lists of length 1, where the name
#' corresponds to the column used for partitioning and partitions are defined
#' as integer vector which is passed to [base::findInterval()]. This implies
#' that partitions can only be created on integer valued columns.
#'
#' Putting all this together, the expected structure of `config` is as
#'
#' ```
#' list(
#'   name = "some_name",
#'   base_url = "some_url",
#'   version = "major.minor",
#'   tables = list(
#'     file_name_table_1 = list(
#'       col_spec = list(
#'         colname_1 = list(type = "col_*"),
#'         colname_2 = list(type = "col_*"),
#'         ...
#'       )
#'     ),
#'     file_name_table_2 = list(
#'       col_spec = list(...),
#'       partitioning = list(colname_i = c(0L, ))
#'     ),
#'     ...
#'   )
#' )
#' ```
#'
#' Configurations for the MIMIC-III and eICU databases are available in
#' `extdata/config` as JSON files and can be read in using [get_config()].
#'
#' @param demo Logical flag for switching between default arguments for demo
#' or full data sources.
#' @param dir The directory where the data was downloaded to (see
#' [download_mimic()]).
#' @param config Import configuration specified as nested list.
#' @param cleanup Logical flag, if `TRUE` the original CSV data is deleted
#' after successful import.
#'
#' @rdname data_import
#'
#' @examples
#' \dontrun{
#'
#' dir <- tempdir()
#' list.files(dir)
#'
#' download_mimic(demo = TRUE, dest = dir)
#' list.files(dir)
#'
#' import_mimic(demo = TRUE, dir = dir)
#' list.files(dir)
#'
#' unlink(dir, recursive = TRUE)
#'
#' }
#'
#' @export
#'
import_mimic <- function(demo = FALSE, dir = mimic_data_dir(demo),
                         config = mimic_config(demo), cleanup = TRUE) {

  import_datasource(dir, config, cleanup)
}

#' @rdname data_import
#'
#' @export
#'
import_eicu <- function(demo = FALSE, dir = eicu_data_dir(demo),
                        config = eicu_config(demo), cleanup = TRUE) {

  import_datasource(dir, config, cleanup)
}

#' @rdname data_import
#'
#' @export
#'
import_datasource <- function(dir, config, cleanup = TRUE) {

  process_table <- function(x, name) {

    if (has_name(x, "partitioning")) {
      partition_table(dir, name, x, cleanup = cleanup)
    } else {
      csv_to_fst(dir, name, x, cleanup = cleanup)
    }

    message("Successfully imported ", tolower(name))
  }

  assert_that(is.dir(dir), is.list(config), is.flag(cleanup))

  message("Importing ", config[["name"]], " v", config[["version"]])

  tables <- names(config[["tables"]])
  files <- file.path(dir, tables)

  avail <- file.exists(files)

  if (any(!avail)) {
    imported <- table_exists_as_fst(config, dir)
    missing <- !avail & !imported
    if (any(missing)) {
      message("Skipping unavailable files:\n  ",
              paste(basename(files)[missing], collapse = "\n  "))
    }
  }

  assert_that(sum(avail) > 0L)

  Map(process_table, config[["tables"]][avail], tables[avail])

  invisible(NULL)
}

create_col_spec <- function(x) {

  ind <- vapply(x, function(y) which(names(y) == "type"), integer(1L))

  do.call(
    readr::cols,
    Map(do.call,
      lapply(Map(`[[`, x, ind), get, asNamespace("readr")), Map(`[`, x, -ind)
    )
  )
}

read_csv_colnames <- function(x) {
  con <- gzfile(x, "r")
  on.exit(close(con))
  readr::tokenize(paste0(readLines(con, n = 1L), "\n"))[[1L]]
}

merge_fst_chunks <- function(tmp_dir, targ_dir) {

  files <- list.files(tmp_dir, full.names = TRUE)

  sort_ind <- order(
    as.integer(sub("^chunk_", "", sub("\\.fst$", "", basename(files))))
  )

  dat <- lapply(files[sort_ind], fst::read_fst, as.data.table = TRUE)
  dat <- data.table::rbindlist(dat)

  new_file <- file.path(targ_dir,
    paste0(sub("part_", "", basename(tmp_dir)), ".fst")
  )

  fst::write_fst(dat, new_file, compress = 100L)

  invisible(NULL)
}

partition_table <- function(dir, table, cfg, chunk_length = 10 ^ 7,
                            cleanup = TRUE) {

  chunk_fun <- function(x, pos, ...) {

    chunk_no <- ((pos - 1L) / chunk_length) + 1L

    x <- split(x, part_fun(x))

    tmp_nme <- file.path(tempdir, paste0("part_", names(x)),
                         paste0("chunk_", chunk_no, ".fst"))

    dirs <- unique(dirname(tmp_nme))
    todo <- !dir.exists(dirs)
    if (any(todo)) lapply(dirs[todo], dir.create, recursive = TRUE)

    Map(fst::write_fst, x, tmp_nme)

    invisible(NULL)
  }

  message("Parititioning ", tolower(table))

  assert_that(has_name(cfg, c("col_spec", "partitioning")))

  file <- file.path(dir, table)

  tempdir <- tempfile()
  dir.create(tempdir, recursive = TRUE)
  on.exit(unlink(tempdir, recursive = TRUE))

  col_names <- read_csv_colnames(file)

  col_spec <- cfg[["col_spec"]]
  assert_that(has_name(col_spec, col_names))
  col_spec <- create_col_spec(col_spec[col_names])

  part <- cfg[["partitioning"]]
  assert_that(length(part) == 1L, names(part) %in% col_names)
  part_fun <- function(x) findInterval(x[[names(part)]], part[[1L]])

  readr::read_csv_chunked(file, chunk_fun, chunk_length, col_names, col_spec,
                          skip = 1L)

  targ_dir <- file.path(dir, tolower(sub("\\.csv(\\.gz)?", "", table)))
  if (!dir.exists(targ_dir)) dir.create(targ_dir)

  lapply(list.files(tempdir, full.names = TRUE), merge_fst_chunks, targ_dir)

  if (cleanup) unlink(file)

  invisible(NULL)
}

csv_to_fst <- function(dir, table, cfg, cleanup = TRUE) {

  file <- file.path(dir, table)

  assert_that(has_name(cfg, "col_spec"))

  col_names <- read_csv_colnames(file)

  col_spec <- cfg[["col_spec"]]
  assert_that(has_name(col_spec, col_names))
  col_spec <- create_col_spec(col_spec[col_names])

  dat <- readr::read_csv(file, col_names = col_names, col_types = col_spec,
                         skip = 1L)
  readr::stop_for_problems(dat)

  colnames(dat) <- tolower(colnames(dat))
  new_file <- file.path(dir,
    paste0(sub("\\.csv(\\.gz)?$", "", tolower(table)), ".fst")
  )

  fst::write_fst(dat, new_file, compress = 100L)

  if (cleanup) unlink(file)

  invisible(NULL)
}
