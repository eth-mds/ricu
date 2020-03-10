
test_that("tbl_id", {

  tbl_id <- new_tbl_id("foo")

  expect_is(tbl_id, "tbl_id")
  expect_true(is_tbl_id(tbl_id))
  expect_false(is_tbl_id("tbl_id"))
  expect_identical(tbl_id(tbl_id), tbl_id)

  expect_error(new_tbl_id(character(0L)))
  expect_error(new_tbl_id(1L))
  expect_error(new_tbl_id(NA_character_))
  expect_error(new_tbl_id(NULL))
  expect_error(new_tbl_id(c("foo", "bar")))

  expect_identical(id(tbl_id), "foo")
  expect_error(id("tbl_id"))
  expect_error(id(1L))
  expect_error(index(tbl_id))
  expect_error(interval(tbl_id))

  expect_identical(set_id(tbl_id, "bar"), new_tbl_id("bar"))
  expect_error(set_id(tbl_id, c("foo", "xyz")))

  expect_identical(rename_cols(tbl_id, "bar", "foo"), new_tbl_id("bar"))
  expect_identical(rename_cols(tbl_id, "foo", "bar"), tbl_id)
  expect_error(rename_cols(tbl_id, c("xyz", "foo"), "bar"))
})

test_that("tbl_index", {

  tbl_ind <- new_tbl_index("foo", hours(1L))

  expect_is(tbl_ind, "tbl_index")
  expect_true(is_tbl_index(tbl_ind))
  expect_false(is_tbl_index("tbl_ind"))
  expect_identical(tbl_index(tbl_ind), tbl_ind)

  expect_error(new_tbl_index(c("foo", "bar"), hours(1L)))
  expect_error(new_tbl_index(character(0L), hours(1L)))
  expect_error(new_tbl_index(NA_character_, hours(1L)))
  expect_error(new_tbl_index(1L, hours(1L)))
  expect_error(new_tbl_index(NULL, hours(1L)))

  expect_error(new_tbl_index("foo", 1L))
  expect_error(new_tbl_index("foo", NA))
  expect_error(new_tbl_index("foo", as.difftime(NA, units = "hours")))
  expect_error(new_tbl_index("foo", NULL))

  expect_identical(index(tbl_ind), "foo")
  expect_identical(interval(tbl_ind), hours(1L))
  expect_error(id(tbl_ind))

  expect_error(index("tbl_ind"))
  expect_error(interval("tbl_ind"))

  renamed <- rename_cols(tbl_ind, "xyz", "foo")

  expect_is(renamed, "tbl_index")
  expect_true(is_tbl_index(renamed))

  expect_identical(index(renamed), "xyz")
  expect_identical(interval(renamed), interval(tbl_ind))

  fixed <- rename_cols(tbl_ind, "xyz", "bar")

  expect_is(fixed, "tbl_index")
  expect_true(is_tbl_index(fixed))

  expect_identical(index(fixed), index(tbl_ind))
  expect_identical(interval(fixed), interval(tbl_ind))
  expect_identical(fixed, tbl_ind)

  expect_identical(set_index(tbl_ind, "xyz"), renamed)
  expect_equal(interval(set_time_unit(tbl_ind, "mins")), mins(60L))
  expect_equal(interval(set_interval(tbl_ind, mins(5L))), mins(5L))
  expect_error(set_id(tbl_ind, "foo"))
})

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

  expect_identical(ts_meta(ts_met), ts_met)
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

