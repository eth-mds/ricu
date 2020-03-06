
test_that("ts_key", {

  ts_key <- new_ts_key("foo")

  expect_is(ts_key, "ts_key")
  expect_true(is_ts_key(ts_key))
  expect_false(is_ts_key("ts_key"))
  expect_identical(ts_key(ts_key), ts_key)

  expect_error(new_ts_key(character(0L)))
  expect_error(new_ts_key(1L))
  expect_error(new_ts_key(NA_character_))
  expect_error(new_ts_key(NULL))
  expect_error(new_ts_key(c("foo", "bar")))

  expect_identical(key(ts_key), "foo")
  expect_error(key("ts_key"))
  expect_error(key(1L))
  expect_error(index(ts_key))
  expect_error(interval(ts_key))

  expect_identical(set_key(ts_key, "bar"), new_ts_key("bar"))
  expect_error(set_key(ts_key, c("foo", "xyz")))

  expect_identical(rename_cols(ts_key, "bar", "foo"), new_ts_key("bar"))
  expect_identical(rename_cols(ts_key, "foo", "bar"), ts_key)
  expect_error(rename_cols(ts_key, c("xyz", "foo"), "bar"))
})

test_that("ts_index", {

  ts_ind <- new_ts_index("foo", hours(1L))

  expect_is(ts_ind, "ts_index")
  expect_true(is_ts_index(ts_ind))
  expect_false(is_ts_index("ts_ind"))
  expect_identical(ts_index(ts_ind), ts_ind)

  expect_error(new_ts_index(c("foo", "bar"), hours(1L)))
  expect_error(new_ts_index(character(0L), hours(1L)))
  expect_error(new_ts_index(NA_character_, hours(1L)))
  expect_error(new_ts_index(1L, hours(1L)))
  expect_error(new_ts_index(NULL, hours(1L)))

  expect_error(new_ts_index("foo", 1L))
  expect_error(new_ts_index("foo", NA))
  expect_error(new_ts_index("foo", as.difftime(NA, units = "hours")))
  expect_error(new_ts_index("foo", NULL))

  expect_identical(index(ts_ind), "foo")
  expect_identical(interval(ts_ind), hours(1L))
  expect_error(key(ts_ind))

  expect_error(index("ts_ind"))
  expect_error(interval("ts_ind"))

  renamed <- rename_cols(ts_ind, "xyz", "foo")

  expect_is(renamed, "ts_index")
  expect_true(is_ts_index(renamed))

  expect_identical(index(renamed), "xyz")
  expect_identical(interval(renamed), interval(ts_ind))

  fixed <- rename_cols(ts_ind, "xyz", "bar")

  expect_is(fixed, "ts_index")
  expect_true(is_ts_index(fixed))

  expect_identical(index(fixed), index(ts_ind))
  expect_identical(interval(fixed), interval(ts_ind))
  expect_identical(fixed, ts_ind)

  expect_identical(set_index(ts_ind, "xyz"), renamed)
  expect_equal(interval(set_time_unit(ts_ind, "mins")), mins(60L))
  expect_equal(interval(set_interval(ts_ind, mins(5L))), mins(5L))
  expect_error(set_key(ts_ind, "foo"))
})

test_that("ts_meta", {

  ts_key <- new_ts_key("foo")
  ts_ind <- new_ts_index("bar", hours(1L))

  ts_met <- new_ts_meta(ts_key, ts_ind)

  expect_is(ts_met, "ts_meta")
  expect_true(is_ts_meta(ts_met))
  expect_false(is_ts_meta(ts_key))
  expect_false(is_ts_meta(ts_ind))

  expect_error(new_ts_meta(ts_key, ts_key), "is not a `ts_index` object")
  expect_error(new_ts_meta(ts_ind, ts_ind), "is not a `ts_key` object")
  expect_error(new_ts_meta(ts_key, new_ts_index("foo", hours(1L))),
               "not not equal to")

  expect_identical(ts_meta(ts_met), ts_met)
  expect_identical(ts_index(ts_met), ts_ind)
  expect_identical(ts_key(ts_met), ts_key)

  renamed <- rename_cols(ts_met, "xyz", "bar")

  expect_identical(key(renamed), "foo")
  expect_identical(index(renamed), "xyz")
  expect_identical(interval(renamed), interval(ts_met))
  expect_identical(rename_cols(renamed, "bar", "xyz"), ts_met)

  renamed <- rename_cols(ts_met, c("abc", "xyz"), c("foo", "bar"))

  expect_identical(key(renamed), "abc")
  expect_identical(index(renamed), "xyz")
  expect_identical(interval(renamed), interval(ts_met))

  expect_error(rename_cols(ts_met, "bar", "foo"), "not not equal to")
  expect_error(rename_cols(ts_met, c("abc", "abc"), c("foo", "bar")),
               "contains duplicate elements")

  sk <- set_key(ts_met, "abc")
  expect_true(is_ts_meta(sk))

  expect_identical(key(sk), key(renamed))
  expect_identical(index(sk), index(ts_met))
  expect_identical(interval(sk), interval(ts_met))
  expect_identical(time_unit(sk), time_unit(ts_met))

  sid <- set_index(ts_met, "xyz")
  expect_true(is_ts_meta(sid))

  expect_identical(key(sid), key(ts_met))
  expect_identical(index(sid), index(renamed))
  expect_identical(interval(sid), interval(ts_met))
  expect_identical(time_unit(sid), time_unit(ts_met))

  expect_identical(set_key(ts_met, "foo"), ts_met)
  expect_identical(set_index(ts_met, "bar"), ts_met)
  expect_error(set_key(ts_met, "bar"), "not not equal to")
  expect_error(set_index(ts_met, "foo"), "not not equal to")

  siv <- set_interval(ts_met, mins(5L))
  expect_true(is_ts_meta(siv))

  expect_identical(key(siv), key(ts_met))
  expect_identical(index(siv), index(ts_met))
  expect_identical(interval(siv), mins(5L))
  expect_identical(time_unit(siv), "mins")
})

