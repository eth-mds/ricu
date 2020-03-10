
test_that("id_meta", {

  tbl_id <- new_tbl_id("foo")
  id_met <- new_id_meta(tbl_id)

  expect_is(id_met, "id_meta")
  expect_true(is_id_meta(id_met))
  expect_false(is_id_meta(tbl_id))

  expect_error(new_id_meta(NULL), "is not a `tbl_id` object")
  expect_error(new_id_meta("tbl_id"), "is not a `tbl_id` object")

  expect_identical(id_meta(id_met), id_met)
  expect_identical(tbl_id(id_met), tbl_id)

  expect_identical(id(id_met), "foo")
  expect_null(index(id_met))
  expect_null(interval(id_met))
  expect_null(time_unit(id_met))

  renamed <- rename_cols(id_met, "xyz", "foo")

  expect_identical(id(renamed), "xyz")
  expect_identical(rename_cols(renamed, "foo", "xyz"), id_met)

  expect_identical(rename_cols(id_met, "xyz", "bar"), id_met)

  expect_error(rename_cols(id_met, c("abc", "abc"), c("foo", "bar")),
               "contains duplicate elements")

  sk <- set_id(id_met, "abc")

  expect_true(is_id_meta(sk))
  expect_identical(id(sk), id(rename_cols(id_met, "abc", "foo")))

  expect_error(set_index(id_met, "xyz"), "no applicable method for")
  expect_error(set_interval(id_met, mins(5L)), "no applicable method for")
})
