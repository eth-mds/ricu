
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

  merge_fun <- function(tmp_dir, targ_dir) {

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

  message("parititioning ", tolower(table))

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

  lapply(list.files(tempdir, full.names = TRUE), merge_fun, targ_dir)

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

import_datasource <- function(dir, cfg, cleanup) {

  process_table <- function(x, name) {

    if (has_name(x, "partitioning")) {
      partition_table(dir, name, x, cleanup = cleanup)
    } else {
      csv_to_fst(dir, name, x, cleanup = cleanup)
    }

    message("successfully imported ", tolower(name))
  }

  tables <- names(cfg)
  files <- file.path(dir, tables)

  avail <- file.exists(files)

  if (any(!avail)) {
    message("skipping unavailable files:\n  ",
            paste(basename(files)[!avail], collapse = "\n  "))
  }

  assert_that(sum(avail) > 0L)

  Map(process_table, cfg[avail], tables[avail])

  invisible(NULL)
}

#' @export
import_mimic <- function(demo = FALSE,
  dir = if (demo) data_dir("mimic-demo") else data_dir("mimic-data"),
  cfg = if (demo) get_config("mimic-demo") else get_config("mimic-setup"),
  cleanup = TRUE) {

  message("importing mimic-iii data")

  import_datasource(dir, cfg, cleanup)
}

#' @export
import_eicu <- function(demo = FALSE,
  dir = if (demo) data_dir("eicu-demo") else data_dir("eicu-data"),
  cfg = if (demo) get_config("eicu-demo") else get_config("eicu-setup"),
  cleanup = TRUE) {

  message("importing eicu data")

  import_datasource(dir, cfg, cleanup)
}
