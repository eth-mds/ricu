
test_that("icu_tbl rbinding", {

  dat1 <- rnorm(10)
  dat2 <- rnorm(10)
  dat3 <- rnorm(10)

  x <- ts_tbl(a = 1:10, b = hours(1:10), c = dat1, id = "a")
  y <- ts_tbl(a = 1:10, b = hours(1:10), c = dat2, id = "a")
  z <- ts_tbl(a = 1:10, b = hours(1:10), c = dat3, id = "a")

  res <- rbind_lst(list(x, y, z))

  expect_is(res, "ts_tbl")
  expect_identical(id(res), "a")
  expect_identical(index(res), "b")
  expect_setequal(data_cols(res), "c")
  expect_identical(nrow(res), 30L)

  y2 <- data.table::data.table(a = 1:10, b = hours(1:10), c = dat2)

  expect_identical(res, rbind_lst(list(x, y2, z)))

  y2 <- ts_tbl(d = 1:10, e = hours(1:10), c = dat2, id = "d")
  y3 <- data.table::copy(y2)

  expect_identical(res, rbind_lst(list(x, y2, z)))
  expect_identical(y2, y3)

  y2 <- id_tbl(a = 1:10, c = dat2, id = "a")
  res2 <- rbind_lst(list(x, y2, z), fill = TRUE)

  expect_is(res2, "id_tbl")
  expect_identical(id(res2), "a")
  expect_setequal(data_cols(res2), c("b", "c"))
  expect_identical(nrow(res2), 30L)
})
