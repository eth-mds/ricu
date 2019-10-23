
new_file_fst <- function(file, envir) {

  assert_that(
    file.exists(file), length(file) == 1L, is.environment(envir)
  )

  name <- sub("\\.fst$", "", tolower(basename(file)))

  res <- structure(
    list(name = name, file = file, proxy = fst::fst(file),
         cache = NULL),
    class = "file_fst"
  )

  assign(name, res, envir)

  invisible(NULL)
}

is_file_fst <- function(x) inherits(x, "file_fst")

file_fst_name <- function(x) .subset2(x, "name")
file_fst_proxy <- function(x) .subset2(x, "proxy")
file_fst_file <- function(x) .subset2(x, "file")

#' @export
dim.file_fst <- function(x) as.integer(dim(file_fst_proxy(x)))

#' @export
dimnames.file_fst <- function(x) dimnames(file_fst_proxy(x))

#' @export
str.file_fst <- function(x, ...) {
  str(
    list(name = file_fst_name(x), file = file_fst_file(x),
         proxy = unclass(file_fst_proxy(x))),
    ...
  )
}
