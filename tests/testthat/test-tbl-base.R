
test_that("icu_tbl merging", {

  dat1 <- rnorm(10)
  dat2 <- rnorm(10)

  x <- ts_tbl(a = 1:10, b = hours(1:10), c = dat1, id = "a")
  y <- ts_tbl(a = 1:10, b = hours(1:10), d = dat2, id = "a")

  res <- merge(x, y)

  expect_is(res, "ts_tbl")
  expect_identical(id(res), "a")
  expect_identical(index(res), "b")
  expect_setequal(data_cols(res), c("c", "d"))

  x <- ts_tbl(a = 1:10, b = hours(1:10), c = dat1, id = "a")
  y <- ts_tbl(d = 1:10, e = hours(1:10), f = dat2, id = "d")

  res <- merge(x, y)

  expect_is(res, "ts_tbl")
  expect_identical(id(res), "a")
  expect_identical(index(res), "b")
  expect_setequal(data_cols(res), c("c", "f"))

  x <- id_tbl(a = 1:10, c = dat1, id = "a")
  y <- ts_tbl(a = 1:10, b = hours(1:10), d = dat2, id = "a")

  res <- merge(x, y)

  expect_is(res, "ts_tbl")
  expect_identical(id(res), "a")
  expect_identical(index(res), "b")
  expect_setequal(data_cols(res), c("c", "d"))

  x <- ts_tbl(a = 1:10, b = hours(1:10), d = dat2, id = "a")
  y <- id_tbl(a = 1:10, c = dat1, id = "a")

  res2 <- merge(x, y)

  expect_identical(id(res), id(res2))
  expect_identical(index(res), index(res2))
  expect_setequal(data_cols(res), data_cols(res2))

  expect_equal(res, res2, ignore.col.order = TRUE)

  x <- id_tbl(a = 1:10, b = dat1, id = "a")
  y <- ts_tbl(c = 1:10, d = hours(1:10), e = dat2, id = "c")

  res <- merge(x, y)

  expect_is(res, "ts_tbl")
  expect_identical(id(res), "a")
  expect_identical(index(res), "d")
  expect_setequal(data_cols(res), c("b", "e"))

  x <- ts_tbl(a = 1:10, d = hours(1:10), e = dat2, id = "a")
  y <- id_tbl(c = 1:10, b = dat1, id = "c")

  res2 <- merge(x, y)

  expect_identical(id(res), id(res2))
  expect_identical(index(res), index(res2))
  expect_setequal(data_cols(res), data_cols(res2))

  expect_equal(res, res2, ignore.col.order = TRUE)

  x <- id_tbl(a = 1:10, b = dat1, id = "a")
  y <- id_tbl(a = 1:10, c = dat2, id = "a")

  res <- merge(x, y)

  expect_is(res, "id_tbl")
  expect_identical(id(res), "a")
  expect_setequal(data_cols(res), c("b", "c"))

  x <- id_tbl(a = 1:10, b = dat1, id = "a")
  y <- id_tbl(c = 1:10, d = dat2, id = "c")

  res <- merge(x, y)

  expect_is(res, "id_tbl")
  expect_identical(id(res), "a")
  expect_setequal(data_cols(res), c("b", "d"))

  x <- id_tbl(a = 1:10, b = dat1, id = "a")
  y <- data.table::data.table(a = 1:10, c = dat2)

  res <- merge(x, y)

  expect_is(res, "id_tbl")
  expect_identical(id(res), "a")
  expect_setequal(data_cols(res), c("b", "c"))

  x <- id_tbl(a = 1:10, b = dat1, id = "a")
  y <- data.table::data.table(c = 1:10, d = dat2)

  expect_error(merge(x, y))
})
