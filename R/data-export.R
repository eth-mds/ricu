
#' @export
write_psv <- function(x, dir, na_rows = NULL) {

  if (isTRUE(na_rows)) {
    x <- fill_gaps(x)
  } else if (isFALSE(na_rows)) {
    x <- rm_na(x, cols = data_cols(x), mode = "all")
  } else {
    assert_that(is.null(na_rows))
  }

  dat <- split(x, by = id(x), keep.by = FALSE)
  files <- file.path(dir, paste0(names(dat), sep = ".psv"))

  Map(readr::write_delim, dat, files, delim = "|", na = "NaN")

  invisible(NULL)
}

#' @export
read_psv <- function(dir, col_spec = NULL, id_col = "stay_id") {

  add_id <- function(x, val) {
    x[[id_col]] <- val
    x
  }

  files <- list.files(dir, full.names = TRUE)

  dat <- lapply(files, readr::read_delim, col_types = col_spec, delim = "|",
                na = "NaN")
  ids <- as.integer(sub("\\.psv", "", sub("^p", "", basename(files))))

  dat <- Map(add_id, dat, ids)

  rbindlist(lapply(dat, setDT))
}
