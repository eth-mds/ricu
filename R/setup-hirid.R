
merge_tables <- function(folder, cfg, n_part = 5) {

  resave_tables <- function(old, new, ...) {

    dat <- lapply(old, readr::read_csv, ..., skip = 1L)
    dat <- data.table::rbindlist(lapply(dat, data.table::setDT))

    fst::write_fst(dat, new, compress = 100L)
    #unlink(old)

    invisible(NULL)
  }

  assert_that(is.count(n_part), has_name(cfg, "col_spec"))

  files <- list.files(folder, pattern = "part-[0-9]+\\.csv$",
                      full.names = TRUE)
  files <- files[
    order(as.integer(sub("\\.csv$", "", sub("^part-", "", basename(files)))))
  ]

  col_names <- read_csv_colnames(files[1L])

  col_spec <- cfg[["col_spec"]]
  assert_that(has_name(col_spec, col_names))
  col_spec <- create_col_spec(col_spec[col_names])

  n_part <- min(n_part, length(files))

  files <- split(files, split_indices(length(files), n_part))

  Map(resave_tables, files,
      file.path(folder, paste0(seq_along(files), ".fst")),
      MoreArgs = list(col_names = col_names, col_types = col_spec))

}
