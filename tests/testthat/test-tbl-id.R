
test_that("id_tbl constructors", {

  tbl <- id_tbl(a = 1:10, b = rnorm(10), id = "a")

  expect_is(tbl, "id_tbl")
  expect_true(is_id_tbl(tbl))
  expect_identical(id(tbl), "a")
  expect_identical(data_cols(tbl), "b")
  expect_identical(meta_cols(tbl), "a")
  expect_error(index(tbl), "no applicable method")
  expect_error(interval(tbl), "no applicable method")
  expect_error(time_step(tbl), "no applicable method")
  expect_error(time_unit(tbl), "no applicable method")
  expect_error(time_col(tbl), "no applicable method")

  dat <- data.frame(a = 1:10, b = hours(1:10), c = rnorm(10))
  tbl <- as_id_tbl(dat, "a")

  expect_is(tbl, "id_tbl")
  expect_true(is_id_tbl(tbl))
  expect_identical(id(tbl), "a")
  expect_identical(data_cols(tbl), c("b", "c"))
  expect_identical(meta_cols(tbl), "a")

  meta <- tbl_meta(tbl)

  expect_identical(id(meta), "a")
  expect_error(index(meta), "no applicable method")
  expect_error(interval(meta), "no applicable method")
  expect_error(time_step(meta), "no applicable method")
  expect_error(time_unit(meta), "no applicable method")
  expect_error(time_col(meta), "no applicable method")

  expect_identical(nrow(id_tbl(a = c(1:5, NA, 7:10), b = rnorm(10),
                               id = "a")), 9L)

  expect_error(as_id_tbl(as.matrix(dat), "a"), "should be a")
  expect_error(as_id_tbl(dat, "d"), "does not contain column `d`")
  expect_error(id_tbl(a = 1:10, a = rnorm(10), id = "a"),
               "contains duplicate elements")
})

test_that("rename_cols for id_tbl", {

  tbl <- id_tbl(a = 1:10, b = rnorm(10), c = rnorm(10), id = "a")

  expect_identical(id(rename_cols(cpy(tbl), "d", "c")), "a")
  expect_identical(data_cols(rename_cols(cpy(tbl), "d", "c")), c("b", "d"))

  expect_identical(id(rename_cols(cpy(tbl), c("d", "e"), c("b", "c"))), "a")
  expect_identical(data_cols(rename_cols(cpy(tbl), c("d", "e"), c("b", "c"))),
                   c("d", "e"))

  expect_error(rename_cols(cpy(tbl), "e", "d"),
               "does not contain the following columns: `d`")
  expect_error(rename_cols(cpy(tbl), c("e", "e"), c("b", "c")),
               "contains duplicate elements")

  expect_identical(rename_cols(cpy(tbl), "e", "d", skip_absent = TRUE), tbl)
})

test_that("rm_cols for id_tbl", {

  tbl <- id_tbl(a = 1:10, b = rnorm(10), c = rnorm(10), id = "a")

  expect_identical(colnames(rm_cols(cpy(tbl), "c")), c("a", "b"))
  expect_is(rm_cols(cpy(tbl), "c"), "id_tbl")
  expect_identical(colnames(rm_cols(cpy(tbl), "a")), c("b", "c"))
  expect_is(rm_cols(cpy(tbl), "a"), "data.table")
  expect_identical(colnames(rm_cols(cpy(tbl), c("a", "b"))), "c")
  expect_is(rm_cols(cpy(tbl), c("a", "b")), "data.table")

  expect_identical(rm_cols(cpy(tbl), "d"), tbl)
  expect_identical(rm_cols(cpy(tbl), c("a", "d")), rm_cols(cpy(tbl), "a"))
  expect_identical(rm_cols(cpy(tbl), c("a", "a")), rm_cols(cpy(tbl), "a"))
})

test_that("set_* for id_tbl", {

  tbl <- id_tbl(a = 1:10, b = 1:10, c = rnorm(10), d = rnorm(10),
                e = rnorm(10), id = "a")

  expect_identical(id(tbl), "a")
  expect_setequal(meta_cols(tbl), "a")
  expect_setequal(data_cols(tbl), c("b", "c", "d", "e"))

  expect_identical(id(set_id(cpy(tbl), "b")), "b")
  expect_identical(id(set_id(cpy(tbl), "a")), "a")

  expect_error(set_id(cpy(tbl), c("a", "b")), "is not a string")
  expect_error(set_id(cpy(tbl), "f"), "does not contain column `f`")
})
