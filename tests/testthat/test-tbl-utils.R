
test_that("rename_cols for id_tbl", {

  tbl <- id_tbl(a = 1:10, b = rnorm(10), c = rnorm(10))

  expect_identical(id_vars(rename_cols(tbl, "d", "c")), "a")
  expect_identical(id_vars(tbl), "a")

  expect_identical(data_vars(rename_cols(tbl, "d", "c")), c("b", "d"))
  expect_identical(data_vars(tbl), c("b", "c"))

  expect_identical(id_vars(rename_cols(tbl, c("d", "e"), c("b", "c"))), "a")
  expect_identical(id_vars(tbl), "a")

  expect_identical(data_vars(rename_cols(tbl, c("d", "e"), c("b", "c"))),
                   c("d", "e"))
  expect_identical(data_vars(tbl), c("b", "c"))

  expect_error(rename_cols(tbl, "e", "d"),
               "does not contain the following columns: `d`")
  expect_identical(tbl, rename_cols(tbl, "e", "d", skip_absent = TRUE))

  expect_error(rename_cols(tbl, c("e", "e"), c("b", "c")),
               "contains duplicate elements")
})

test_that("rm_cols for id_tbl", {

  tbl <- id_tbl(a = 1:10, b = rnorm(10), c = rnorm(10))

  expect_identical(colnames(rm_cols(tbl, "c")), c("a", "b"))
  expect_identical(colnames(tbl), c("a", "b", "c"))

  expect_is(rm_cols(tbl, "c"), "id_tbl")
  expect_identical(colnames(rm_cols(tbl, "a")), c("b", "c"))
  expect_is(rm_cols(tbl, "a"), "data.table")
  expect_identical(colnames(rm_cols(tbl, c("a", "b"))), "c")
  expect_is(rm_cols(tbl, c("a", "b")), "data.table")

  expect_error(rm_cols(tbl, "d"),
               "x does not contain the following columns: `d`")
  expect_identical(rm_cols(tbl, "d", skip_absent = TRUE), tbl)

  expect_identical(rm_cols(tbl, c("a", "d"), skip_absent = TRUE),
                   rm_cols(tbl, "a"))
  expect_identical(rm_cols(tbl, c("a", "a")), rm_cols(tbl, "a"))

  expect_identical(rm_cols(tbl, "c"), rm_cols(tbl, "c", by_ref = TRUE))
  expect_identical(colnames(tbl), c("a", "b"))
})

test_that("rename_cols for ts_tbl", {

  tbl <- ts_tbl(a = 1:10, b = hours(1:10), c = rnorm(10))

  expect_identical(id_vars(rename_cols(tbl, "d", "c")), "a")
  expect_identical(index_var(rename_cols(tbl, "d", "c")), "b")
  expect_identical(data_vars(rename_cols(tbl, "d", "c")), "d")

  expect_identical(id_vars(rename_cols(tbl, c("d", "e"), c("b", "c"))),
                   "a")
  expect_identical(index_var(rename_cols(tbl, c("d", "e"), c("b", "c"))),
                   "d")
  expect_identical(data_vars(rename_cols(tbl, c("d", "e"), c("b", "c"))),
                   "e")

  expect_error(rename_cols(tbl, "e", "d"),
               "does not contain the following columns: `d`")
  expect_identical(tbl, rename_cols(tbl, "e", "d", skip_absent = TRUE))

  expect_error(rename_cols(tbl, c("e", "e"), c("b", "c")),
               "contains duplicate elements")
})

test_that("rm_cols for ts_tbl", {

  tbl <- ts_tbl(a = 1:10, b = hours(1:10), c = rnorm(10))

  expect_identical(colnames(rm_cols(tbl, "c")), c("a", "b"))
  expect_is(rm_cols(tbl, "c"), "ts_tbl")
  expect_identical(colnames(rm_cols(tbl, "a")), c("b", "c"))
  expect_is(rm_cols(tbl, "a"), "data.table")
  expect_identical(colnames(rm_cols(tbl, c("a", "b"))), "c")
  expect_is(rm_cols(tbl, "a"), "data.table")

  expect_error(rm_cols(tbl, "d"),
               "x does not contain the following columns: `d`")
  expect_identical(rm_cols(tbl, "d", skip_absent = TRUE), tbl)

  expect_identical(rm_cols(tbl, c("a", "d"), skip_absent = TRUE),
                   rm_cols(tbl, "a"))
  expect_identical(rm_cols(tbl, c("a", "a")), rm_cols(tbl, "a"))

  expect_identical(rm_cols(tbl, "c"), rm_cols(tbl, "c", by_ref = TRUE))
  expect_identical(colnames(tbl), c("a", "b"))
})

test_that("icu_tbl rbinding", {

  dat1 <- rnorm(10)
  dat2 <- rnorm(10)
  dat3 <- rnorm(10)

  x <- ts_tbl(a = 1:10, b = hours(1:10), c = dat1)
  y <- ts_tbl(a = 1:10, b = hours(1:10), c = dat2)
  z <- ts_tbl(a = 1:10, b = hours(1:10), c = dat3)

  res <- rbind_lst(list(x, y, z))

  expect_is(res, "ts_tbl")
  expect_identical(id_vars(res), "a")
  expect_identical(index_var(res), "b")
  expect_setequal(data_vars(res), "c")
  expect_identical(nrow(res), 30L)

  y2 <- data.table::data.table(a = 1:10, b = hours(1:10), c = dat2)

  expect_identical(res, rbind_lst(list(x, y2, z)))

  y2 <- ts_tbl(d = 1:10, e = hours(1:10), c = dat2)
  y3 <- data.table::copy(y2)

  expect_identical(res, rbind_lst(list(x, y2, z)))
  expect_identical(y2, y3)

  y2 <- id_tbl(a = 1:10, c = dat2)
  res2 <- rbind_lst(list(x, y2, z), fill = TRUE)

  expect_is(res2, "id_tbl")
  expect_identical(id_vars(res2), "a")
  expect_setequal(data_vars(res2), c("b", "c"))
  expect_identical(nrow(res2), 30L)
})
