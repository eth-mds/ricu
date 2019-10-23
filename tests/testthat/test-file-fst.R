
test_that("fst files", {

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
