
#' @export
write_psv <- function(x, dir, na_rm = TRUE) {

  assert_that(is.flag(na_rm))

  if (na_rm) {
    x <- rm_na(x, cols = data_cols(x), fun = `&`)
  }

  dat <- split(x, by = key(x))
  files <- file.path(dir, paste0(names(dat), sep = ".psv"))

  Map(readr::write_delim, dat, files, delim = "|", na = "NaN")

  invisible(NULL)
}
