
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
#' @param x Object specifying the source configuration
#' @param dir The directory where the data was downloaded to (see
#' [download_src()]).
#' @param ... Passed to downstream methods/generic consistency
#'
#' @examples
#' \dontrun{
#'
#' dir <- tempdir()
#' list.files(dir)
#'
#' download_src("mimic_demo", dir)
#' list.files(dir)
#'
#' import_src("mimic_demo", dir)
#' list.files(dir)
#'
#' unlink(dir, recursive = TRUE)
#'
#' }
#'
#' @rdname import
#' @export
#'
import_src <- function(x, ...) UseMethod("import_src", x)

#' @param dir The directory where the data was downloaded to (see
#' [download_src()]).
#' @param force Logical flag indicating whether to overwrite already imported
#' tables
#'
#' @rdname import
#' @export
import_src.src_cfg <- function(x, dir = src_data_dir(x), force = FALSE, ...) {

  all_avail <- function(y) all(file.exists(file.path(dir, y)))

  assert_that(is.dir(dir), is.flag(force))

  tbl <- as_tbl_cfg(x)

  todo <- lgl_ply(lapply(tbl, dst_files), all_avail)
  done <- lgl_ply(lapply(tbl, fst_names), all_avail)
  miss <- !done & !todo
  skip <- done & todo

  if (any(miss)) {
    warning("Tables ", concat(quote_bt(names(tbl)[miss])), " are required ",
            "but unavailable")
  }

  if (!force && any(skip)) {
    message("Tables ", concat(quote_bt(names(tbl)[skip])), " have already ",
            "been imported and will be skipped")
    todo <- todo & !skip
  }

  if (!any(todo)) {
    message("All required tables have already been imported")
    return(invisible(NULL))
  }

  tbl <- tbl[todo]
  pba <- progr_init(sum(int_ply(tbl, n_rows)),
    paste0("Importing ", length(tbl), " tables for ", quote_bt(src_name(x)))
  )

  for(table in tbl) {
    import_tbl(table, dir = dir, progress = pba, ...)
  }

  if (!(is.null(pba) || pba$finished)) {
    pba$update(1)
    pba$terminate()
  }

  invisible(NULL)
}

#' @inheritParams read_src_cfg
#' @rdname import
#' @export
import_src.character <- function(x, name = "data-sources", file = NULL,
                                 ...) {

  for (cfg in read_src_cfg(x, name, file)) {
    import_src(cfg, ...)
  }

  invisible(NULL)
}

#' @param progress Either `NULL` or a progress bar as created by
#' [progress::progress_bar()]
#' @param cleanup Logical flag, if `TRUE` the original CSV data is deleted
#' after successful import.
#'
#' @rdname import
#' @export
import_tbl <- function(x, ...) UseMethod("import_tbl", x)

#' @rdname import
#' @export
import_tbl.tbl_cfg <- function(x, dir = src_data_dir(x), progress = NULL,
                                cleanup = TRUE, ...) {

  warn_dots(...)

  assert_that(is.dir(dir), is.flag(cleanup))

  if (n_partitions(x) > 1L) {
    partition_table(x, dir, progress, cleanup)
  } else {
    csv_to_fst(x, dir, progress, cleanup)
  }
}

merge_fst_chunks <- function(src_dir, targ_dir, cols, prog, nme) {

  files <- list.files(src_dir, full.names = TRUE)

  sort_ind <- order(
    as.integer(sub("^chunk_", "", sub("\\.fst$", "", basename(files))))
  )

  dat <- lapply(files[sort_ind], fst::read_fst, as.data.table = TRUE)
  dat <- rbindlist(dat)
  dat <- setnames(dat, names(cols), cols)

  part_no  <- sub("part_", "", basename(src_dir))
  new_file <- file.path(ensure_dirs(targ_dir), paste0(part_no, ".fst"))

  fst::write_fst(dat, new_file, compress = 100L)

  progr_iter(paste(nme, "part", part_no), prog, floor(nrow(dat) / 2))

  invisible(NULL)
}

split_write <- function(x, part_fun, dir, chunk_no, prog, nme) {

  n_row <- nrow(x)

  x <- split(x, part_fun(x))

  tmp_nme <- file.path(dir, paste0("part_", names(x)),
                       paste0("chunk_", chunk_no, ".fst"))

  ensure_dirs(unique(dirname(tmp_nme)))
  assert_that(!any(file.exists(tmp_nme)))

  Map(fst::write_fst, x, tmp_nme)

  progr_iter(paste(nme, "chunk", chunk_no), prog, floor(n_row / 2))

  invisible(NULL)
}

partition_table <- function(x, dir, progress = NULL, cleanup = TRUE,
                            chunk_length = 10 ^ 7) {

  assert_that(n_partitions(x) > 1L)

  tempdir <- ensure_dirs(tempfile())
  on.exit(unlink(tempdir, recursive = TRUE))

  spec <- col_spec(x)
  pfun <- partition_fun(x, orig_names = TRUE)
  file <- file.path(dir, dst_files(x))
  name <- tbl_name(x)

  if (length(file) == 1L) {

    callback <- function(x, pos, ...) {
      split_write(x, pfun, tempdir, ((pos - 1L) / chunk_length) + 1L,
                  progress, name)
    }

    readr::read_csv_chunked(file, callback, chunk_length, col_types = spec,
                            progress = FALSE)

  } else {

    for (i in seq_along(file)) {

      dat <- readr::read_csv(file[i], col_types = spec, progress = FALSE)
      readr::stop_for_problems(dat)

      split_write(dat, pfun, tempdir, i, progress, name)
    }
  }

  cols <- col_names(x)
  targ <- file.path(dir, name)

  for (src_dir in list.files(tempdir, full.names = TRUE)) {
    merge_fst_chunks(src_dir, targ, cols, progress, name)
  }

  fst_tables <- lapply(file.path(dir, fst_names(x)), fst::fst)
  total_rows <- sum(dbl_ply(fst_tables, nrow))

  if (check_n_row(x, total_rows) && cleanup) {
    unlink(file)
  }

  invisible(NULL)
}

csv_to_fst <- function(x, dir, progress = NULL, cleanup = TRUE) {

  src <- file.path(dir, dst_files(x))
  dst <- file.path(dir, fst_names(x))

  assert_that(length(src) == 1L, length(dst) == 1L)

  dat  <- readr::read_csv(src, col_types = col_spec(x), progress = FALSE)
  readr::stop_for_problems(dat)

  cols <- col_names(x)
  dat  <- setnames(dat, names(cols), cols)

  fst::write_fst(dat, dst, compress = 100L)

  n_row <- nrow(fst::fst(dst))

  if (check_n_row(x, n_row) && cleanup) {
    unlink(src)
  }

  progr_iter(tbl_name(x), pb = progress, len = n_row)

  invisible(NULL)
}
