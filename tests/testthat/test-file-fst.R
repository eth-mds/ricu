
mtcars_data_frame <- function() {
  env <- new.env()
  name <- utils::data("mtcars", package = "datasets", envir = env)
  env[[name]]
}

mtcars_file_fst <- function() {
  env <- new.env()
  sepsr:::new_file_fst(file.path(tmp, "mtcars.fst"), env)
  get("mtcars", envir = env)
}

tmp <- tempfile()

setup({
  dir.create(tmp)
  fst::write_fst(mtcars_data_frame(), file.path(tmp, "mtcars.fst"))
})
teardown(unlink(tmp, recursive = TRUE))

test_that("file_fst constructor", {

  file <- system.file("extdata", "patients.fst", package = "mimic.demo")
  env <- new.env()

  expect_null(new_file_fst(file, env))

  name <- sub("\\.fst$", "", tolower(basename(file)))
  dat <- get(name, envir = env)

  expect_is(dat, "file_fst")
  expect_true(is_file_fst(dat))
  expect_identical(file_fst_name(dat), name)
  expect_identical(file_fst_file(dat), file)
  expect_is(file_fst_proxy(dat), "fst_table")

  fst <- fst::fst(file)

  expect_equal(nrow(dat), nrow(fst))
  expect_equal(ncol(dat), ncol(fst))
  expect_equal(dim(dat), dim(fst))

  expect_identical(colnames(dat), colnames(fst))
  expect_identical(rownames(dat), rownames(fst))
  expect_identical(dimnames(dat), dimnames(fst))
})

test_that("file_fst head/tail", {

  dat <- mtcars_file_fst()
  ref <- mtcars_data_frame()

  expect_equal_df(head(dat), head(ref))
  expect_equal_df(head(dat, n = 10L), head(ref, n = 10L))
  expect_equal_df(head(dat, n = -1L), head(ref, n = -1L))
  expect_equal_df(head(dat, n = 0), head(ref, n = 0))
  expect_equal_df(head(dat, n = Inf), head(ref, n = Inf))
  expect_equal_df(head(dat, n = -Inf), head(ref, n = -Inf))
  expect_equal_df(head(dat, n = "foo"), head(ref, n = "foo"))

  expect_error(head(dat, n = NA))
  expect_error(head(ref, n = NA))
  expect_error(head(dat, n = c(1L, 2L)))
  expect_error(head(ref, n = c(1L, 2L)))

  expect_equal_df(tail(dat), tail(ref))
  expect_equal_df(tail(dat, n = 10L), tail(ref, n = 10L))
  expect_equal_df(tail(dat, n = -1L), tail(ref, n = -1L))
  expect_equal_df(tail(dat, n = 0), tail(ref, n = 0))
  expect_equal_df(tail(dat, n = Inf), tail(ref, n = Inf))
  expect_equal_df(tail(dat, n = -Inf), tail(ref, n = -Inf))
  expect_equal_df(tail(dat, n = "foo"), tail(ref, n = "foo"))

  expect_error(tail(dat, n = NA))
  expect_error(tail(ref, n = NA))
  expect_error(tail(dat, n = c(1L, 2L)))
  expect_error(tail(ref, n = c(1L, 2L)))
})
