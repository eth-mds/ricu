
#' Data import utilities
#'
#' Making a dataset available to `ricu` consists of 3 steps: downloading
#' ([download_src()]), importing ([import_src()]) and attaching
#' ([attach_src()]). While downloading and importing are one-time procedures,
#' attaching of the dataset is repeated every time the package is loaded.
#' Briefly, downloading loads the raw dataset from the internet (most likely
#' in `.csv` format), importing consists of some preprocessing to make the
#' data available more efficiently and attaching sets up the data for use by
#' the package.
#'
#' @details
#' In order to speed up data access operations, `ricu` does not directly use
#' the PhysioNet provided CSV files, but converts all data to [fst::fst()]
#' format, which allows for random row and column access. Large tables are
#' split into chunks in order to keep memory requirements reasonably low.
#'
#' The one-time step per dataset of data import is fairly resource intensive:
#' depending on CPU and available storage system, it will take on the order of
#' an hour to run to completion and depending on the dataset, somewhere
#' between 50 GB and 75 GB of temporary disk space are required as tables are
#' uncompressed, in case of partitioned data, rows are reordered and the data
#' again is saved to a storage efficient format.
#'
#' The S3 generic function `import_src()` performs import of an entire data
#' source, internally calling the S3 generic function `import_tbl()` in order
#' to perform import of individual tables. Method dispatch is intended to
#' occur on objects inheriting from `src_cfg` and `tbl_cfg` respectively. Such
#' objects can be generated from JSON based configuration files which contain
#' information such as table names,  column types or row numbers, in order to
#' provide safety in parsing of `.csv` files. For more information on data
#' source configuration, refer to [load_src_cfg()].
#'
#' Current import capabilities include re-saving a `.csv` file to `.fst` at
#' once (used for smaller sized tables), reading a large `.csv` file using the
#' [readr::read_csv_chunked()] API, while partitioning chunks and reassembling
#' sub-partitions (used for splitting a large file into partitions), as well
#' as re-partitioning an already partitioned table according to a new
#' partitioning scheme. Care has been taken to keep the maximal memory
#' requirements for this reasonably low, such that data import is feasible on
#' laptop class hardware.
#'
#' @param x Object specifying the source configuration
#' @param data_dir The directory where the data was downloaded to (see
#' [download_src()]).
#' @param ... Passed to downstream methods (finally to
#' [readr::read_csv]/[readr::read_csv_chunked])/generic consistency
#'
#' @return Called for side effects and returns `NULL` invisibly.
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

#' @param force Logical flag indicating whether to overwrite already imported
#' tables
#'
#' @rdname import
#' @export
import_src.src_cfg <- function(x, data_dir = src_data_dir(x), force = FALSE,
                               ...) {

  assert_that(is.dir(data_dir), is.flag(force))

  tbl <- as_tbl_cfg(x)

  todo <- src_file_exist(tbl, data_dir, "raw")
  done <- src_file_exist(tbl, data_dir, "fst")
  skip <- done & todo

  if (!force && any(skip)) {
    msg_ricu("Table{?s} {quote_bt(names(tbl)[skip])} ha{?s/ve} already been
             imported and will be skipped")
    todo <- todo & !skip
  }

  if (!any(todo)) {
    msg_ricu("All required tables have already been imported")
    return(invisible(NULL))
  }

  tbl <- tbl[todo]
  pba <- progress_init(sum(int_ply(tbl, nrow)),
    msg = "Importing {length(tbl)} table{?s} for {quote_bt(src_name(x))}"
  )

  for(table in tbl) {
    import_tbl(table, data_dir = data_dir, progress = pba, ...)
  }

  if (!(is.null(pba) || pba$finished)) {
    pba$update(1)
  }

  msg_ricu("Successfully imported {length(tbl)} table{?s}")

  invisible(NULL)
}

#' @rdname import
#' @export
import_src.aumc_cfg <- function(x, data_dir = src_data_dir(x), force = FALSE,
                                ...) {

  NextMethod(locale = readr::locale(encoding = "latin1"))
}

#' @export
import_src.character <- function(x, data_dir = src_data_dir(x), force = FALSE,
                                 ...) {

  Map(import_src, load_src_cfg(x, ...), data_dir,
      MoreArgs = list(force = force))

  invisible(NULL)
}

#' @export
import_src.default <- function(x, ...) stop_generic(x, .Generic)

#' @param progress Either `NULL` or a progress bar as created by
#' [progress::progress_bar()]
#'
#' @rdname import
#' @export
import_tbl <- function(x, ...) UseMethod("import_tbl", x)

#' @rdname import
#' @export
import_tbl.tbl_cfg <- function(x, data_dir = src_data_dir(x), progress = NULL,
                               ...) {

  assert_that(is.dir(data_dir))

  if (n_partitions(x) > 1L) {
    partition_table(x, data_dir, progress, ...)
  } else {
    csv_to_fst(x, data_dir, progress, ...)
  }
}

#' @export
import_tbl.default <- function(x, ...) stop_generic(x, .Generic)

merge_fst_chunks <- function(src_dir, targ_dir, cols, sort_col, prog, nme) {

  files <- list.files(src_dir, full.names = TRUE)

  sort_ind <- order(
    as.integer(sub("^chunk_", "", sub("\\.fst$", "", basename(files))))
  )

  dat <- lapply(files[sort_ind], fst::read_fst, as.data.table = TRUE)
  dat <- rbindlist(dat)
  dat <- data.table::setorderv(dat, sort_col)
  dat <- setnames(dat, names(cols), cols)

  part_no  <- sub("part_", "", basename(src_dir))
  new_file <- file.path(ensure_dirs(targ_dir), paste0(part_no, ".fst"))

  fst::write_fst(dat, new_file, compress = 100L)

  progress_tick(paste(nme, "part", part_no), prog, floor(nrow(dat) / 2))

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

  progress_tick(paste(nme, "chunk", chunk_no), prog, floor(n_row / 2))

  invisible(NULL)
}

partition_table <- function(x, dir, progress = NULL, chunk_length = 10 ^ 7,
                            ...) {

  assert_that(n_partitions(x) > 1L)

  tempdir <- ensure_dirs(tempfile())
  on.exit(unlink(tempdir, recursive = TRUE))

  spec <- col_spec(x)
  pfun <- partition_fun(x, orig_names = TRUE)
  pcol <- partition_col(x, orig_names = TRUE)
  file <- file.path(dir, raw_file_names(x))
  name <- tbl_name(x)

  if (length(file) == 1L) {

    callback <- function(x, pos, ...) {
      readr::stop_for_problems(x)
      split_write(x, pfun, tempdir, ((pos - 1L) / chunk_length) + 1L,
                  progress, name)
    }

    readr::read_csv_chunked(file, callback, chunk_length, col_types = spec,
                            progress = FALSE, ...)

  } else {

    for (i in seq_along(file)) {

      dat <- readr::read_csv(file[i], col_types = spec, progress = FALSE, ...)
      readr::stop_for_problems(dat)

      split_write(dat, pfun, tempdir, i, progress, name)
    }
  }

  cols <- col_names(x)
  targ <- file.path(dir, name)

  for (src_dir in list.files(tempdir, full.names = TRUE)) {
    merge_fst_chunks(src_dir, targ, cols, pcol, progress, name)
  }

  fst_tables <- lapply(file.path(dir, fst_file_names(x)), fst::fst)

  check_n_row(x, sum(dbl_ply(fst_tables, nrow)))

  invisible(NULL)
}

csv_to_fst <- function(x, dir, progress = NULL, ...) {

  src <- file.path(dir, raw_file_names(x))
  dst <- file.path(dir, fst_file_names(x))

  assert_that(length(src) == 1L, length(dst) == 1L)

  dat  <- readr::read_csv(src, col_types = col_spec(x), progress = FALSE, ...)
  readr::stop_for_problems(dat)

  cols <- col_names(x)
  dat  <- setnames(dat, names(cols), cols)

  fst::write_fst(dat, dst, compress = 100L)

  fst_table <- fst::fst(dst)

  progress_tick(tbl_name(x), progress, check_n_row(x, nrow(fst_table)))

  invisible(NULL)
}
