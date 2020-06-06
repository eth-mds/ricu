
test_that("icu_tbl merging", {

  dat1 <- rnorm(10)
  dat2 <- rnorm(10)

  x <- ts_tbl(a = 1:10, b = hours(1:10), c = dat1)
  y <- ts_tbl(a = 1:10, b = hours(1:10), d = dat2)

  res <- merge(x, y)

  expect_is(res, "ts_tbl")
  expect_identical(id_vars(res), "a")
  expect_identical(index_var(res), "b")
  expect_setequal(data_vars(res), c("c", "d"))

  x <- ts_tbl(a = 1:10, b = hours(1:10), c = dat1)
  y <- ts_tbl(d = 1:10, e = hours(1:10), f = dat2)

  res <- merge(x, y)

  expect_is(res, "ts_tbl")
  expect_identical(id_vars(res), "a")
  expect_identical(index_var(res), "b")
  expect_setequal(data_vars(res), c("c", "f"))

  x <- id_tbl(a = 1:10, c = dat1)
  y <- ts_tbl(a = 1:10, b = hours(1:10), d = dat2)

  res <- merge(x, y)

  expect_is(res, "ts_tbl")
  expect_identical(id_vars(res), "a")
  expect_identical(index_var(res), "b")
  expect_setequal(data_vars(res), c("c", "d"))

  x <- ts_tbl(a = 1:10, b = hours(1:10), d = dat2)
  y <- id_tbl(a = 1:10, c = dat1)

  res2 <- merge(x, y)

  expect_identical(id_vars(res), id_vars(res2))
  expect_identical(index_var(res), index_var(res2))
  expect_setequal(data_vars(res), data_vars(res2))

  expect_equal(res, res2, ignore.col.order = TRUE)

  x <- id_tbl(a = 1:10, b = dat1)
  y <- ts_tbl(c = 1:10, d = hours(1:10), e = dat2)

  res <- merge(x, y)

  expect_is(res, "ts_tbl")
  expect_identical(id_vars(res), "a")
  expect_identical(index_var(res), "d")
  expect_setequal(data_vars(res), c("b", "e"))

  x <- ts_tbl(a = 1:10, d = hours(1:10), e = dat2)
  y <- id_tbl(c = 1:10, b = dat1)

  res2 <- merge(x, y)

  expect_identical(id_vars(res), id_vars(res2))
  expect_identical(index_var(res), index_var(res2))
  expect_setequal(data_vars(res), data_vars(res2))

  expect_equal(res, res2, ignore.col.order = TRUE)

  x <- id_tbl(a = 1:10, b = dat1)
  y <- id_tbl(a = 1:10, c = dat2)

  res <- merge(x, y)

  expect_is(res, "id_tbl")
  expect_identical(id_vars(res), "a")
  expect_setequal(data_vars(res), c("b", "c"))

  x <- id_tbl(a = 1:10, b = dat1)
  y <- id_tbl(c = 1:10, d = dat2)

  res <- merge(x, y)

  expect_is(res, "id_tbl")
  expect_identical(id_vars(res), "a")
  expect_setequal(data_vars(res), c("b", "d"))

  x <- id_tbl(a = 1:10, b = dat1)
  y <- data.table::data.table(a = 1:10, c = dat2)

  res <- merge(x, y)

  expect_is(res, "id_tbl")
  expect_identical(id_vars(res), "a")
  expect_setequal(data_vars(res), c("b", "c"))

  x <- id_tbl(a = 1:10, b = dat1)
  y <- data.table::data.table(c = 1:10, d = dat2)

  expect_error(merge(x, y))
})
