
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

mtcars_fst_table <- function() {
  fst::fst(file.path(tmp, "mtcars.fst"))
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

test_that("file_fst `[[` subsetting", {

  dat <- mtcars_file_fst()
  ref <- mtcars_fst_table()

  expect_identical(dat[[1L]], ref[[1L]])
  expect_identical(dat[[1L, 2L]], ref[[1L, 2L]])
  expect_identical(dat[[c(1L, 2L)]], ref[[c(1L, 2L)]])
  expect_identical(dat[[c(1L, 2L), 3L]], ref[[c(1L, 2L), 3L]])

  expect_error(dat[[1L, NA]])
  expect_error(dat[[]])
  expect_error(dat[[c(1L, NA)]])
  expect_error(dat[[NA, 1L]])

  expect_error(ref[[1L, NA]])
  expect_error(ref[[]])
  expect_error(ref[[c(1L, NA)]])
  expect_error(ref[[NA, 1L]])

  expect_error(dat[[TRUE]])
  expect_error(dat[[NA]])
  expect_error(dat[[c(TRUE, FALSE)]])
  expect_error(dat[[c(NA, NA)]])
  expect_error(dat[[NULL]])

  expect_error(ref[[TRUE]])
  expect_error(ref[[NA]])
  expect_error(ref[[c(TRUE, FALSE)]])
  expect_error(ref[[c(NA, NA)]])
  expect_error(ref[[NULL]])

  expect_identical(dat[["disp"]], ref[["disp"]])
  expect_identical(dat[["dis"]], ref[["dis"]])

  expect_error(dat[[1L, "disp"]])
  expect_error(dat[[c("dis", "drat")]])
  expect_error(dat[["dis", "drat"]])
  expect_error(dat[[NA, "disp"]])

  expect_error(ref[[1L, "disp"]])
  expect_error(ref[[c("dis", "drat")]])
  expect_error(ref[["dis", "drat"]])
  expect_error(ref[[NA, "disp"]])
})

test_that("file_fst `$` subsetting", {

  dat <- mtcars_file_fst()
  ref <- mtcars_fst_table()

  expect_identical(dat$disp, ref$disp)
  expect_identical(dat$dis, ref$dis)
})

test_that("file_fst `[` subsetting", {

  dat <- mtcars_file_fst()
  ref <- mtcars_fst_table()

  expect_equal_df(dat[2], ref[2])
  expect_equal_df(dat[2.6], ref[2.6])
  expect_equal_df(dat[TRUE], ref[TRUE])
  expect_equal_df(dat[c(1.1, 2)], ref[c(1.1, 2)])
  expect_equal_df(dat[c(TRUE, FALSE)], ref[c(TRUE, FALSE)])
  expect_equal_df(dat[c(TRUE, FALSE, TRUE)], ref[c(TRUE, FALSE, TRUE)])
  expect_equal_df(dat[i = 2], ref[i = 2])
  expect_equal_df(dat[j = 2], ref[j = 2])
  expect_equal_df(dat[drop = FALSE], ref[drop = FALSE])
  expect_equal_df(dat[,], ref[,])
  expect_equal_df(dat[, 2:3], ref[, 2:3])
  expect_equal_df(dat[2, drop = FALSE], ref[2, drop = FALSE])
  expect_equal_df(dat[,,], ref[,,])
  expect_equal_df(dat[,, drop = FALSE], ref[,, drop = FALSE])
  expect_equal_df(dat[j = 2, drop = FALSE], ref[j = 2, drop = FALSE])
})
