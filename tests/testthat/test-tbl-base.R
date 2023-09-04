skip_on_cran()

test_that("icu_tbl subsetting", {

  col <- runif(10)
  dat <- ts_tbl(a = 1:10, b = hours(1:10), c = col)

  expect_gte(dat$c[5L], 0)

  dat[5L, "c"] <- -1

  expect_identical(dat$c[5L], -1)

  dat$c <- `[<-`(col, 5L, -2)

  expect_identical(dat$c[5L], -2)
})

test_that("icu_tbl metadata", {

  col <- runif(10)
  dat <- ts_tbl(a = 1:10, b = hours(1:10), c = col)

  expect_null(rownames(dat))
  expect_null(row.names(dat))
  expect_identical(dimnames(dat), list(NULL, c("a", "b", "c")))

  expect_warning(rownames(dat) <- letters[1:10], class = "arg_ignored")
  expect_warning(row.names(dat) <- letters[1:10], class = "arg_ignored")

  expect_null(rownames(dat))
  expect_null(row.names(dat))

  colnames(dat) <- c("a", "b", "d")
  expect_identical(colnames(dat), c("a", "b", "d"))

  expect_warning(dimnames(dat) <- list(letters[1:10], c("a", "b", "e")),
                 class = "arg_ignored")
  expect_identical(dimnames(dat), list(NULL, c("a", "b", "e")))

  dimnames(dat) <- list(NULL, c("a", "b", "f"))
  expect_identical(dimnames(dat), list(NULL, c("a", "b", "f")))

  expect_identical(
    tbl_sum(dat),
    c(`A \`ts_tbl\`` = "10 x 3",
      `Id var` = "\`a\`",
      `Index var` = "\`b\` (1 hours)"
    )
  )

  expect_identical(
    tbl_sum(as_id_tbl(dat)),
    c(`An \`id_tbl\`` = "10 x 3",
      `Id var` = "\`a\`"
    )
  )
})

test_that("icu_tbl merging", {

  dat1 <- rnorm(10)
  dat2 <- rnorm(10)

  x <- ts_tbl(a = 1:10, b = hours(1:10), c = dat1)
  y <- ts_tbl(a = 1:10, b = hours(1:10), d = dat2)

  res <- merge(x, y)

  expect_s3_class(res, "ts_tbl")
  expect_identical(id_vars(res), "a")
  expect_identical(index_var(res), "b")
  expect_setequal(data_vars(res), c("c", "d"))

  x <- ts_tbl(a = 1:10, b = hours(1:10), c = dat1)
  y <- ts_tbl(d = 1:10, e = hours(1:10), f = dat2)

  res <- merge(x, y)

  expect_s3_class(res, "ts_tbl")
  expect_identical(id_vars(res), "a")
  expect_identical(index_var(res), "b")
  expect_setequal(data_vars(res), c("c", "f"))

  x <- id_tbl(a = 1:10, c = dat1)
  y <- ts_tbl(a = 1:10, b = hours(1:10), d = dat2)

  res <- merge(x, y)

  expect_s3_class(res, "ts_tbl")
  expect_identical(id_vars(res), "a")
  expect_identical(index_var(res), "b")
  expect_setequal(data_vars(res), c("c", "d"))

  x <- ts_tbl(a = 1:10, b = hours(1:10), d = dat2)
  y <- id_tbl(a = 1:10, c = dat1)

  res2 <- merge(x, y)

  expect_identical(id_vars(res), id_vars(res2))
  expect_identical(index_var(res), index_var(res2))
  expect_setequal(data_vars(res), data_vars(res2))

  expect_true(all.equal(res, res2, ignore.col.order = TRUE))

  x <- id_tbl(a = 1:10, b = dat1)
  y <- ts_tbl(c = 1:10, d = hours(1:10), e = dat2)

  res <- merge(x, y)

  expect_s3_class(res, "ts_tbl")
  expect_identical(id_vars(res), "a")
  expect_identical(index_var(res), "d")
  expect_setequal(data_vars(res), c("b", "e"))

  x <- ts_tbl(a = 1:10, d = hours(1:10), e = dat2)
  y <- id_tbl(c = 1:10, b = dat1)

  res2 <- merge(x, y)

  expect_identical(id_vars(res), id_vars(res2))
  expect_identical(index_var(res), index_var(res2))
  expect_setequal(data_vars(res), data_vars(res2))

  expect_true(all.equal(res, res2, ignore.col.order = TRUE))

  x <- id_tbl(a = 1:10, b = dat1)
  y <- id_tbl(a = 1:10, c = dat2)

  res <- merge(x, y)

  expect_s3_class(res, "id_tbl")
  expect_identical(id_vars(res), "a")
  expect_setequal(data_vars(res), c("b", "c"))

  x <- id_tbl(a = 1:10, b = dat1)
  y <- id_tbl(c = 1:10, d = dat2)

  res <- merge(x, y)

  expect_s3_class(res, "id_tbl")
  expect_identical(id_vars(res), "a")
  expect_setequal(data_vars(res), c("b", "d"))

  x <- id_tbl(a = 1:10, b = dat1)
  y <- data.table::data.table(a = 1:10, c = dat2)

  res <- merge(x, y)

  expect_s3_class(res, "id_tbl")
  expect_identical(id_vars(res), "a")
  expect_setequal(data_vars(res), c("b", "c"))

  x <- id_tbl(a = 1:10, b = dat1)
  y <- data.table::data.table(c = 1:10, d = dat2)

  expect_error(merge(x, y))

  x <- win_tbl(x = 1:10, y = hours(1:10), z = mins(1:10 * 30), val = dat1)
  y <- id_tbl(a = 1:10, b = dat2)

  res <- merge(x, y)

  expect_s3_class(res, "win_tbl")
  expect_identical(id_var(res), "x")
  expect_identical(index_var(res), "y")
  expect_identical(dur_var(res), "z")
  expect_setequal(data_vars(res), c("val", "b"))

  res <- merge(y, x)

  expect_s3_class(res, "win_tbl")
  expect_identical(id_var(res), "a")
  expect_identical(index_var(res), "y")
  expect_identical(dur_var(res), "z")
  expect_setequal(data_vars(res), c("val", "b"))

  y <- ts_tbl(a = 1:10, b = hours(1:10), d = dat2)

  expect_error(merge(x, y), class = "merge_win_tbl")
  expect_error(merge(y, x), class = "merge_win_tbl")

  y <- win_tbl(a = 1:10, b = hours(1:10), c = mins(1:10 * 30), d = dat2)

  res <- merge(x, y)

  expect_s3_class(res, "win_tbl")
  expect_identical(id_var(res), "x")
  expect_identical(index_var(res), "y")
  expect_identical(dur_var(res), "z")
  expect_setequal(data_vars(res), c("val", "d"))

  res <- merge(y, x)

  expect_s3_class(res, "win_tbl")
  expect_identical(id_var(res), "a")
  expect_identical(index_var(res), "b")
  expect_identical(dur_var(res), "c")
  expect_setequal(data_vars(res), c("val", "d"))
})

test_that("icu_tbl merge list", {

  idt <- rnorm(2)
  ts1 <- ts_tbl(a = rep(1:2, each = 5), b = hours(rep(1:5, 2)), c = rnorm(10))
  ts2 <- ts_tbl(a = rep(1:2, each = 5), b = hours(rep(1:5, 2)), d = rnorm(10))

  lst <- list(ts1, id_tbl(a = 1:2, e = idt), ts2)

  res <- merge_lst(lst)

  expect_s3_class(res, c("ts_tbl", "id_tbl"))
  expect_identical(data_vars(res), c("c", "e", "d"))
  expect_identical(res$e, rep(idt, each = 5))

  fin <- unmerge(res)

  expect_identical(fin[[1L]], ts1)
  expect_identical(fin[[3L]], ts2)
})
