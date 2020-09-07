
test_that("id_tbl constructors", {

  tbl <- id_tbl(a = 1:10, b = rnorm(10))

  expect_is(tbl, "id_tbl")
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

  expect_is(dat, "data.frame")
  expect_is(tbl, "id_tbl")
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

  expect_is(as_id_tbl(as.matrix(dat)), "id_tbl")
  expect_error(as_id_tbl(as.matrix(dat), by_ref = TRUE), "should be a")

  expect_error(as_id_tbl(dat, "d"), class = "has_cols_assert")
  expect_error(id_tbl(a = 1:10, a = rnorm(10), id_vars = "a"),
               class = "is_unique_assert")

  dat <- data.table::data.table(a = 1:10, b = hours(1:10), c = rnorm(10))
  tbl <- as_id_tbl(dat, by_ref = TRUE)

  expect_is(tbl, "id_tbl")
  expect_is(dat, "id_tbl")
})

test_that("ts_tbl constructors", {

  tbl <- ts_tbl(a = 1:10, b = hours(1:10))

  expect_is(tbl, "ts_tbl")
  expect_true(is_ts_tbl(tbl))
  expect_identical(id_vars(tbl), "a")
  expect_identical(index_var(tbl), "b")
  expect_equal(interval(tbl), hours(1L))

  dat <- data.frame(a = 1:10, b = hours(1:10), c = rnorm(10))
  tbl <- as_ts_tbl(dat, "a")

  expect_identical(as_ts_tbl(c(dat), "a"), tbl)

  expect_is(dat, "data.frame")
  expect_is(tbl, "id_tbl")
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
