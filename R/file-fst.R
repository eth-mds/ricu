
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

#' @export
head.file_fst <- function(x, n = 6L, ...) {

  assert_that(length(n) == 1L)

  if (n < 0L) n <- max(nrow(x) + n, 0L)
  else n <- min(n, nrow(x))

  if (n == 0L) to <- 1L
  else if (is.character(n)) to <- NULL
  else to <- floor(n)

  res <- fst::read_fst(file_fst_file(x), from = 1L, to = to,
                       as.data.table = TRUE)
  data.table::setattr(res, "row.names", NULL)

  if (n == 0L) res[-1L, ]
  else res
}

#' @export
tail.file_fst <- function(x, n = 6L, ...) {

  assert_that(length(n) == 1L)

  if (n < 0L) n <- max(nrow(x) + n, 0L)
  else n <- min(n, nrow(x))

  if (n == 0L) to <- 1L
  else if (is.character(n)) to <- NULL
  else to <- nrow(x)

  if (n == 0L || is.character(n)) from <- 1L
  else from <- nrow(x) - floor(n) + 1L

  res <- fst::read_fst(file_fst_file(x), from = from, to = to,
                       as.data.table = TRUE)
  data.table::setattr(res, "row.names", NULL)

  if (n == 0L) res[-1L, ]
  else res
}

#' @export
`[[.file_fst` <- function(x, ...) {
  `[[`(x = file_fst_proxy(x), ...)
}

#' @export
`$.file_fst` <- function(x, ...) {
  `$`(x = file_fst_proxy(x), ...)
}

#' @export
`[.file_fst` <- function(x, ...) {
  res <- `[`(x = file_fst_proxy(x), ...)
  if (is.data.frame(res)) data.table::setDT(res)
  res
}
