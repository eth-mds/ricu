
test_that("id_tbl constructors", {

  tbl <- id_tbl(a = 1:10, b = rnorm(10))

  expect_s3_class(tbl, "id_tbl")
  expect_true(is_id_tbl(tbl))
  expect_identical(id_vars(tbl), "a")
  expect_identical(data_vars(tbl), "b")
  expect_identical(meta_vars(tbl), "a")
  expect_error(index_var(tbl), class = "generic_no_fun")
  expect_error(index_col(tbl), class = "generic_no_fun")
  expect_error(interval(tbl), class = "generic_no_fun")
  expect_error(time_step(tbl), class = "generic_no_fun")
  expect_error(time_unit(tbl), class = "generic_no_fun")

  dat <- data.frame(a = 1:10, b = hours(1:10), c = rnorm(10))
  tbl <- as_id_tbl(dat)

  expect_s3_class(dat, "data.frame")
  expect_s3_class(tbl, "id_tbl")
  expect_true(is_id_tbl(tbl))
  expect_identical(id_vars(tbl), "a")
  expect_identical(data_vars(tbl), c("b", "c"))
  expect_identical(meta_vars(tbl), "a")

  ptyp <- as_ptype(tbl)

  expect_identical(id_vars(ptyp), "a")
  expect_error(index_var(ptyp), class = "generic_no_fun")
  expect_error(index_col(ptyp), class = "generic_no_fun")
  expect_error(interval(ptyp), class = "generic_no_fun")
  expect_error(time_step(ptyp), class = "generic_no_fun")
  expect_error(time_unit(ptyp), class = "generic_no_fun")

  expect_identical(nrow(id_tbl(a = c(1:5, NA, 7:10), b = rnorm(10))), 9L)

  expect_s3_class(as_id_tbl(as.matrix(dat)), "id_tbl")
  expect_error(as_id_tbl(as.matrix(dat), by_ref = TRUE), "should be a")

  expect_error(as_id_tbl(dat, "d"), class = "has_cols_assert")
  expect_error(id_tbl(a = 1:10, a = rnorm(10), id_vars = "a"),
               class = "is_unique_assert")

  dat <- data.table::data.table(a = 1:10, b = hours(1:10), c = rnorm(10))
  tbl <- as_id_tbl(dat, by_ref = TRUE)

  expect_s3_class(tbl, "id_tbl")
  expect_s3_class(dat, "id_tbl")
})

test_that("ts_tbl constructors", {

  tbl <- ts_tbl(a = 1:10, b = hours(1:10))

  expect_s3_class(tbl, "ts_tbl")
  expect_true(is_ts_tbl(tbl))
  expect_identical(id_vars(tbl), "a")
  expect_identical(index_var(tbl), "b")
  expect_equal(interval(tbl), hours(1L))

  dat <- data.frame(a = 1:10, b = hours(1:10), c = rnorm(10))
  tbl <- as_ts_tbl(dat, "a")

  expect_identical(as_ts_tbl(c(dat), "a"), tbl)

  expect_s3_class(dat, "data.frame")
  expect_s3_class(tbl, "id_tbl")
  expect_true(is_ts_tbl(tbl))
  expect_identical(id_vars(tbl), "a")
  expect_identical(index_var(tbl), "b")
  expect_identical(index_col(tbl), hours(1:10))
  expect_identical(data_vars(tbl), "c")
  expect_identical(meta_vars(tbl), c("a", "b"))
  expect_equal(interval(tbl), hours(1L))
  expect_equal(time_step(tbl), 1L)
  expect_identical(time_unit(tbl), "hours")

  ptyp <- as_ptype(tbl)

  expect_identical(id_vars(ptyp), "a")
  expect_identical(index_var(ptyp), "b")
  expect_equal(interval(ptyp), hours(1L))
  expect_equal(time_step(ptyp), 1L)
  expect_identical(time_unit(ptyp), "hours")

  expect_identical(nrow(ts_tbl(a = 1:10, b = hours(c(1:5, NA, 7:10)))), 9L)
  expect_identical(nrow(ts_tbl(a = c(1:5, NA, 7:10), b = hours(c(1:10)))), 9L)

  expect_error(ts_tbl(a = 1:10, b = c(hours(1:9), mins(45))),
               class = "obeys_interval_assert")
  expect_error(ts_tbl(a = 1:10, b = c(hours(1:9), mins(45)),
                      interval = hours(1L)),
               class = "obeys_interval_assert")

  expect_identical(time_unit(ts_tbl(a = 1:10, b = mins(1:10),
                                    interval = mins(1L))), "mins")
  expect_error(ts_tbl(a = 1:10, b = hours(1:10),
                      interval = hours(2L)),
               class = "obeys_interval_assert")

  expect_error(as_ts_tbl(as.matrix(dat)))
  expect_error(as_ts_tbl(as.matrix(dat, by_ref = TRUE)))

  expect_error(as_ts_tbl(dat, index_var = "c"),
               class = "has_time_cols_assert")
  expect_error(as_ts_tbl(dat, index_var = "d"),
               class = "has_cols_assert")
  expect_error(as_ts_tbl(dat, id_vars = "b", index_var = "b"),
               class = "is_disjoint_assert")
  expect_error(ts_tbl(a = 1:10, a = hours(1:10)),
               class = "has_time_cols_assert")
})

test_that("win_tbl constructors", {

  tbl <- win_tbl(a = 1:10, b = hours(1:10), c = mins(1:10))

  expect_s3_class(tbl, "win_tbl")
  expect_true(is_win_tbl(tbl))
  expect_identical(id_vars(tbl), "a")
  expect_identical(index_var(tbl), "b")
  expect_equal(interval(tbl), hours(1L))
  expect_identical(dur_var(tbl), "c")

  dat <- data.frame(a = 1:10, b = hours(1:10), c = mins(1:10), d = rnorm(10))
  tbl <- as_win_tbl(dat)

  expect_identical(as_win_tbl(c(dat)), tbl)

  expect_s3_class(dat, c("ts_tbl", "id_tbl", "data.frame"))
  expect_true(is_win_tbl(tbl))
  expect_identical(id_vars(tbl), "a")
  expect_identical(index_var(tbl), "b")
  expect_identical(index_col(tbl), hours(1:10))
  expect_identical(dur_var(tbl), "c")
  expect_identical(dur_col(tbl), mins(1:10))
  expect_identical(data_vars(tbl), "d")
  expect_identical(meta_vars(tbl), c("a", "b", "c"))
  expect_equal(interval(tbl), hours(1L))
  expect_equal(time_step(tbl), 1L)
  expect_identical(time_unit(tbl), "hours")

  ptyp <- as_ptype(tbl)

  expect_identical(id_vars(ptyp), "a")
  expect_identical(index_var(ptyp), "b")
  expect_equal(interval(ptyp), hours(1L))
  expect_equal(time_step(ptyp), 1L)
  expect_identical(time_unit(ptyp), "hours")
  expect_identical(dur_var(ptyp), "c")

  expect_identical(nrow(win_tbl(a = c(1:9, NA), b = hours(c(1:5, NA, 7:10)),
                                c = mins(c(NA, 2:10)))), 7L)

  expect_error(win_tbl(a = 1:10, b = hours(1:10), c = 1:10, dur_var = "c"),
               class = "is_difftime_assert")
  expect_error(win_tbl(a = 1:10, b = hours(1:10), c = mins(1:10),
                       index_var = "b", dur_var = "b"),
               class = "is_disjoint_assert")

  expect_error(as_win_tbl(as.matrix(dat)))
  expect_error(as_win_tbl(as.matrix(dat, by_ref = TRUE)))
})

test_that("icu_tbl coercion", {

  dat <- runif(10)

  ts <- ts_tbl(a = 1:10, b = hours(1:10), c = dat)
  id <- id_tbl(a = 1:10, c = dat)

  expect_identical(as_ts_tbl(ts), ts)
  expect_identical(as_ts_tbl(as_id_tbl(ts)), ts)

  expect_identical(as_id_tbl(id), id)

  expect_identical(as_ts_tbl(as.data.table(ts), "a"), ts)
  expect_identical(as.data.table(ts), as.data.table(as_id_tbl(ts)),
                   ignore_attr = TRUE)

  expect_identical(as_id_tbl(as.data.table(id), "a"), id)
  expect_identical(as.data.table(id), as.data.table(as_id_tbl(id)),
                   ignore_attr = TRUE)

  expect_identical(as_ts_tbl(as.data.frame(ts), "a"), ts)
  expect_identical(as.data.frame(ts), as.data.frame(as_id_tbl(ts)),
                   ignore_attr = TRUE)

  expect_identical(as_id_tbl(as.data.frame(id), "a"), id)
  expect_identical(as.data.frame(id), as.data.frame(as_id_tbl(id)),
                   ignore_attr = TRUE)
})

