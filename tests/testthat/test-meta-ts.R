
test_that("ts_meta", {

  tbl_id <- new_tbl_id("foo")
  tbl_ind <- new_tbl_index("bar", hours(1L))

  ts_met <- new_ts_meta(tbl_id, tbl_ind)

  expect_is(ts_met, "ts_meta")
  expect_true(is_ts_meta(ts_met))
  expect_false(is_ts_meta(tbl_id))
  expect_false(is_ts_meta(tbl_ind))

  expect_error(new_ts_meta(tbl_id, tbl_id), "is not a `tbl_index` object")
  expect_error(new_ts_meta(tbl_ind, tbl_ind), "is not a `tbl_id` object")
  expect_error(new_ts_meta(tbl_id, new_tbl_index("foo", hours(1L))),
               "not not equal to")

  expect_identical(tbl_meta(ts_met), ts_met)
  expect_identical(tbl_index(ts_met), tbl_ind)
  expect_identical(tbl_id(ts_met), tbl_id)

  renamed <- rename_cols(ts_met, "xyz", "bar")

  expect_identical(id(renamed), "foo")
  expect_identical(index(renamed), "xyz")
  expect_identical(interval(renamed), interval(ts_met))
  expect_identical(rename_cols(renamed, "bar", "xyz"), ts_met)

  renamed <- rename_cols(ts_met, c("abc", "xyz"), c("foo", "bar"))

  expect_identical(id(renamed), "abc")
  expect_identical(index(renamed), "xyz")
  expect_identical(interval(renamed), interval(ts_met))

  expect_error(rename_cols(ts_met, "bar", "foo"), "not not equal to")
  expect_error(rename_cols(ts_met, c("abc", "abc"), c("foo", "bar")),
               "contains duplicate elements")

  sk <- set_id(ts_met, "abc")
  expect_true(is_ts_meta(sk))

  expect_identical(id(sk), id(renamed))
  expect_identical(index(sk), index(ts_met))
  expect_identical(interval(sk), interval(ts_met))
  expect_identical(time_unit(sk), time_unit(ts_met))

  sid <- set_index(ts_met, "xyz")
  expect_true(is_ts_meta(sid))

  expect_identical(id(sid), id(ts_met))
  expect_identical(index(sid), index(renamed))
  expect_identical(interval(sid), interval(ts_met))
  expect_identical(time_unit(sid), time_unit(ts_met))

  expect_identical(set_id(ts_met, "foo"), ts_met)
  expect_identical(set_index(ts_met, "bar"), ts_met)
  expect_error(set_id(ts_met, "bar"), "not not equal to")
  expect_error(set_index(ts_met, "foo"), "not not equal to")

  siv <- set_interval(ts_met, mins(5L))
  expect_true(is_ts_meta(siv))

  expect_identical(id(siv), id(ts_met))
  expect_identical(index(siv), index(ts_met))
  expect_identical(interval(siv), mins(5L))
  expect_identical(time_unit(siv), "mins")
})
