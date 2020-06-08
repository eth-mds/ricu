
#' Read and write utilities
#'
#' Support for reading from and writing to `.psv` files as used for the
#' Physionet Sepsis Challenge.
#'
#' @param x Object to write to files
#' @param dir Directory to write the (many) files to or read from
#' @param na_rows If `TRUE` missing time steps are filled with `NaN` values,
#' if `FALSE`, rows where all data columns entries are missing are removed and
#' if `NULL`, data is written as-is
#'
#' @references
#'
#' Reyna, M., Josef, C., Jeter, R., Shashikumar, S., Moody, B., Westover, M.
#' B., Sharma, A., Nemati, S., & Clifford, G. (2019). Early Prediction of
#' Sepsis from Clinical Data -- the PhysioNet Computing in Cardiology
#' Challenge 2019 (version 1.0.0). PhysioNet.
#' https://doi.org/10.13026/v64v-d857.
#'
#' @rdname tbl_meta
#' @export
#'
write_psv <- function(x, dir, na_rows = NULL) {

  if (isTRUE(na_rows)) {
    x <- fill_gaps(x)
  } else if (isFALSE(na_rows)) {
    x <- rm_na(x, cols = data_vars(x), mode = "all")
  } else {
    assert_that(is.null(na_rows))
  }

  dat <- split(x, by = id_vars(x), keep.by = FALSE)
  files <- file.path(dir, paste0(names(dat), sep = ".psv"))

  Map(readr::write_delim, dat, files, delim = "|", na = "NaN")

  invisible(NULL)
}

#' @param col_spec A column specification as created by [readr::cols()]
#' @param id_var Name of the id column (IDs are generated from file names)
#'
#' @rdname tbl_meta
#' @export
#'
read_psv <- function(dir, col_spec = NULL, id_var = "stay_id") {

  add_id <- function(x, val) {
    x[[id_var]] <- val
    x
  }

  files <- list.files(dir, full.names = TRUE)

  dat <- lapply(files, readr::read_delim, col_types = col_spec, delim = "|",
                na = "NaN")
  ids <- as.integer(sub("\\.psv", "", sub("^p", "", basename(files))))

  dat <- Map(add_id, dat, ids)

  rbindlist(lapply(dat, setDT))
}
