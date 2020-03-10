
test_that("ts_tbl constructors", {

  tbl <- ts_tbl(a = 1:10, b = hours(1:10), key = "a")

  expect_is(tbl, "ts_tbl")
  expect_true(is_ts_tbl(tbl))
  expect_identical(key(tbl), "a")
  expect_identical(index(tbl), "b")
  expect_equal(interval(tbl), hours(1L))

  dat <- data.frame(a = 1:10, b = hours(1:10), c = rnorm(10))

  tbl <- as_ts_tbl(dat, "a")

  expect_is(tbl, "ts_tbl")
  expect_true(is_ts_tbl(tbl))
  expect_identical(key(tbl), "a")
  expect_identical(index(tbl), "b")
  expect_identical(data_cols(tbl), "c")
  expect_identical(meta_cols(tbl), c("a", "b"))
  expect_equal(interval(tbl), hours(1L))
  expect_equal(time_step(tbl), 1L)
  expect_identical(time_unit(tbl), "hours")
  expect_identical(time_col(tbl), hours(1:10))

  meta <- ts_meta(tbl)

  expect_identical(key(meta), "a")
  expect_identical(index(meta), "b")
  expect_equal(interval(meta), hours(1L))
  expect_equal(time_step(meta), 1L)
  expect_identical(time_unit(meta), "hours")

  expect_identical(nrow(ts_tbl(a = 1:10, b = hours(c(1:5, NA, 7:10)),
                               key = "a")), 9L)
  expect_identical(nrow(ts_tbl(a = c(1:5, NA, 7:10), b = hours(c(1:10)),
                               key = "a")), 9L)

  expect_error(ts_tbl(a = 1:10, b = mins(1:10), key = "a"),
               "does not conform to an interval of 1 hours")
  expect_identical(time_unit(ts_tbl(a = 1:10, b = mins(1:10), key = "a",
                                    interval = mins(1L))), "mins")
  expect_error(ts_tbl(a = 1:10, b = hours(1:10), key = "a",
                      interval = hours(2L)),
               "does not conform to an interval of 2 hours")
  expect_error(as_ts_tbl(c(dat), "a"),
               "does not inherit from class data.frame")
  expect_error(as_ts_tbl(dat, "a", "c"),
               "does not contain column `c` of class `difftime`")
  expect_error(as_ts_tbl(dat, "a", "d"), "does not contain column `d`")
  expect_error(as_ts_tbl(dat, "b", "b"), "not not equal to")
  expect_error(ts_tbl(a = 1:10, a = hours(1:10), key = "a"),
               "contains duplicate elements")
})

test_that("rename_cols for ts_tbl", {

  tbl <- ts_tbl(a = 1:10, b = hours(1:10), c = rnorm(10), key = "a")

  expect_identical(key(rename_cols(cpy(tbl), "d", "c")), "a")
  expect_identical(index(rename_cols(cpy(tbl), "d", "c")), "b")
  expect_identical(data_cols(rename_cols(cpy(tbl), "d", "c")), "d")

  expect_identical(key(rename_cols(cpy(tbl), c("d", "e"), c("b", "c"))), "a")
  expect_identical(index(rename_cols(cpy(tbl), c("d", "e"), c("b", "c"))), "d")
  expect_identical(data_cols(rename_cols(cpy(tbl), c("d", "e"), c("b", "c"))),
                   "e")

  expect_error(rename_cols(cpy(tbl), "e", "d"),
               "does not contain the following columns: `d`")
  expect_error(rename_cols(cpy(tbl), c("e", "e"), c("b", "c")),
               "contains duplicate elements")

  expect_identical(rename_cols(cpy(tbl), "e", "d", skip_absent = TRUE), tbl)
})

test_that("rm_cols for ts_tbl", {

  tbl <- ts_tbl(a = 1:10, b = hours(1:10), c = rnorm(10), key = "a")

  expect_identical(colnames(rm_cols(cpy(tbl), "c")), c("a", "b"))
  expect_is(rm_cols(cpy(tbl), "c"), "ts_tbl")
  expect_identical(colnames(rm_cols(cpy(tbl), "a")), c("b", "c"))
  expect_is(rm_cols(cpy(tbl), "a"), "data.table")
  expect_identical(colnames(rm_cols(cpy(tbl), c("a", "b"))), "c")
  expect_is(rm_cols(cpy(tbl), "a"), "data.table")

  expect_identical(rm_cols(cpy(tbl), "d"), tbl)
  expect_identical(rm_cols(cpy(tbl), c("a", "d")), rm_cols(cpy(tbl), "a"))
  expect_identical(rm_cols(cpy(tbl), c("a", "a")), rm_cols(cpy(tbl), "a"))
})

test_that("set_* for ts_tbl", {

  tbl <- ts_tbl(a = 1:10, b = 1:10, c = hours(1:10), d = hours(1:10),
                e = rnorm(10), key = "a", index = "c")

  expect_identical(key(tbl), "a")
  expect_identical(index(tbl), "c")
  expect_equal(interval(tbl), hours(1L))
  expect_equal(time_step(tbl), 1L)
  expect_identical(time_unit(tbl), "hours")
  expect_identical(colnames(tbl), c("a", "c", "b", "d", "e"))
  expect_setequal(time_col(tbl), hours(1:10))

  expect_identical(key(set_key(cpy(tbl), "b")), "b")
  expect_identical(key(set_key(cpy(tbl), "a")), "a")

  expect_error(set_key(cpy(tbl), "c"), "not not equal to")
  expect_error(set_key(cpy(tbl), c("a", "b")), "is not a string")
  expect_error(set_key(cpy(tbl), "f"), "does not contain column `f`")

  expect_identical(index(set_index(cpy(tbl), "d")), "d")
  expect_identical(index(set_index(cpy(tbl), "c")), "c")

  expect_error(set_index(cpy(tbl), "a"), "not not equal to")
  expect_error(set_index(cpy(tbl), "f"), "does not contain column `f`")

  expect_equal(interval(set_interval(cpy(tbl), hours(2L))), hours(2L))
  expect_equal(interval(set_interval(cpy(tbl), hours(1L))), hours(1L))
  expect_equal(time_step(set_interval(cpy(tbl), hours(2L))), 2L)
  expect_identical(time_unit(set_interval(cpy(tbl), hours(2L))), "hours")
  expect_setequal(time_col(set_interval(cpy(tbl), hours(2L))),
                  hours(seq.int(0, 10, 2)))

  expect_equal(interval(set_interval(cpy(tbl), mins(90L))), mins(90L))
  expect_equal(interval(set_interval(cpy(tbl), mins(60L))), mins(60L))
  expect_equal(time_step(set_interval(cpy(tbl), mins(90L))), 90L)
  expect_identical(time_unit(set_interval(cpy(tbl), mins(90L))), "mins")
  expect_setequal(time_col(set_interval(cpy(tbl), mins(90L))),
                  hours(seq.int(0, 540, 90)))
  expect_equal(set_interval(cpy(tbl), mins(60L)),
               set_time_unit(cpy(tbl), "mins"))

  expect_warning(set_interval(cpy(tbl), mins(30L)),
                 "Higher time resolution does not add missing time steps")
  expect_error(set_interval(cpy(tbl), "a"),
               "not a strictly positive `difftime` object of length 1")
  expect_error(set_interval(cpy(tbl), "a"),
    "is not a strictly positive `difftime` object of length 1"
  )
  expect_error(set_interval(cpy(tbl), "f"),
    "is not a strictly positive `difftime` object of length 1"
  )
})
