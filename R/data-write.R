
#' @export
write_psv <- function(x, dir, na_rows = NULL) {

  if (isTRUE(na_rows)) {
    x <- fill_gaps(x)
  } else if (isFALSE(na_rows)) {
    x <- rm_na(x, cols = data_cols(x), mode = "all")
  } else {
    assert_that(is.null(na_rows))
  }

  dat <- split(x, by = id(x))
  files <- file.path(dir, paste0(names(dat), sep = ".psv"))

  Map(readr::write_delim, dat, files, delim = "|", na = "NaN")

  invisible(NULL)
}
