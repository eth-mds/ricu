
#' Data import utilities
#'
#' In order to speed up data access operations, `ricu` does not directly use
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
#' @param config Import configuration specified as nested list.
#' @param dir The directory where the data was downloaded to (see
#' [download_source()]).
#' @param cleanup Logical flag, if `TRUE` the original CSV data is deleted
#' after successful import.
#'
#' @examples
#' \dontrun{
#'
#' dir <- tempdir()
#' list.files(dir)
#'
#' download_source("mimic_demo", dir)
#' list.files(dir)
#'
#' import_source("mimic_demo", dir)
#' list.files(dir)
#'
#' unlink(dir, recursive = TRUE)
#'
#' }
#'
#' @export
#'
import_source <- function(config, dir = NULL, cleanup = TRUE) {

  config <- get_src_config(config)

  if (is.null(dir)) {
    dir <- source_data_dir(config)
  }

  assert_that(is.dir(dir), is.flag(cleanup))

  message("importing `", get_source(config), "`")

  files <- file.path(dir, file_names(config))
  avail <- lgl_ply(files, all_is, file.exists)

  if (any(!avail)) {

    imported <- Map(file.path, dir, fst_names(config))
    imported <- lgl_ply(imported, all_is, file.exists)

    missing <- !avail & !imported

    if (any(missing)) {
      message("skipping unavailable files for tables:\n  ",
              paste("`", table_names(config)[missing], "`", collapse = "\n  "))
    }
  }

  assert_that(sum(avail) > 0L)

  for (tbl in get_table_specs(config)) {

    if (n_partitions(tbl) > 1L) {
      partition_table(tbl, dir, cleanup = cleanup)
    } else {
      csv_to_fst(tbl, dir, cleanup = cleanup)
    }
  }

  invisible(NULL)
}

merge_fst_chunks <- function(src_dir, targ_dir, cols) {

  files <- list.files(src_dir, full.names = TRUE)

  sort_ind <- order(
    as.integer(sub("^chunk_", "", sub("\\.fst$", "", basename(files))))
  )

  dat <- lapply(files[sort_ind], fst::read_fst, as.data.table = TRUE)
  dat <- data.table::rbindlist(dat)
  dat <- data.table::setnames(dat, names(cols), cols)

  new_file <- file.path(ensure_dirs(targ_dir),
    paste0(sub("part_", "", basename(src_dir)), ".fst")
  )

  fst::write_fst(dat, new_file, compress = 100L)

  invisible(NULL)
}

split_write <- function(x, part_fun, dir, chunk_no) {

  x <- split(x, part_fun(x))

  tmp_nme <- file.path(dir, paste0("part_", seq_along(x)),
                       paste0("chunk_", chunk_no, ".fst"))

  ensure_dirs(dirname(tmp_nme))

  Map(fst::write_fst, x, tmp_nme)

  invisible(NULL)
}

partition_table <- function(cfg, dir, cleanup = TRUE, chunk_length = 10 ^ 7) {

  assert_that(is_table_spec(cfg), n_partitions(cfg) > 1L)

  message("splitting `", table_names(cfg), "` into ", n_partitions(cfg),
          " partitions")

  tempdir <- ensure_dirs(tempfile())
  on.exit(unlink(tempdir, recursive = TRUE))

  spec <- get_col_spec(cfg)
  pfun <- get_part_fun(cfg, orig_names = TRUE)
  file <- file.path(dir, file_names(cfg))

  if (length(file) == 1L) {

    callback <- function(x, pos, ...) {
      split_write(x, pfun, tempdir, ((pos - 1L) / chunk_length) + 1L)
    }

    readr::read_csv_chunked(file, callback, chunk_length, col_types = spec)

  } else {

    for (i in seq_along(file)) {

      dat <- readr::read_csv(file[i], col_types = spec)
      readr::stop_for_problems(dat)

      split_write(dat, pfun, tempdir, i)
    }
  }

  col_names <- get_col_names(cfg)
  targ_dir  <- file.path(dir, table_names(cfg))

  for (src_dir in list.files(tempdir, full.names = TRUE)) {
    merge_fst_chunks(src_dir, targ_dir, col_names)
  }

  fst_tables <- lapply(file.path(dir, fst_names(cfg)), fst::fst)
  total_rows <- sum(dbl_ply(fst_tables, nrow))

  message("successfully imported ", check_n_rows(cfg, total_rows),
          " rows for table `", table_names(cfg), "`")

  if (cleanup) {
    unlink(file)
  }

  invisible(NULL)
}

csv_to_fst <- function(cfg, dir, cleanup = TRUE) {

  assert_that(is_table_spec(cfg), n_partitions(cfg) == 1L)

  file <- file.path(dir, file_names(cfg))
  dat  <- readr::read_csv(file, col_types = get_col_spec(cfg))

  readr::stop_for_problems(dat)

  cols <- get_col_names(cfg)
  dat  <- data.table::setnames(dat, names(cols), cols)
  res  <- fst_names(cfg)

  fst::write_fst(dat, res, compress = 100L)

  message("successfully imported ", check_n_rows(cfg, nrow(fst::fst(res))),
          " rows for table `", table_names(cfg), "`")

  if (cleanup) {
    unlink(file)
  }

  invisible(NULL)
}
