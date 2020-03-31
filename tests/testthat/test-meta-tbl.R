
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
  expect_null(id_opts(tbl_id))

  expect_identical(set_id(tbl_id, "bar"), new_tbl_id("bar"))
  expect_error(set_id(tbl_id, c("foo", "xyz")))

  expect_identical(rename_cols(tbl_id, "bar", "foo"), new_tbl_id("bar"))
  expect_identical(rename_cols(tbl_id, "foo", "bar"), tbl_id)
  expect_error(rename_cols(tbl_id, c("xyz", "foo"), "bar"))

  opts <- c("bar", "foo", "baz")
  tbl_id <- new_tbl_id("foo", opts)

  expect_identical(id(tbl_id), "foo")
  expect_identical(id_opts(tbl_id), opts)

  expect_identical(set_id(tbl_id, "bar"), new_tbl_id("bar", opts))
  expect_warning(tbl_id <- set_id(tbl_id, "xyz"),
                "id `xyz` is incompatible with id options `bar`, `foo`, `baz`")
  expect_null(id_opts(tbl_id))

  tbl_id <- new_tbl_id("foo", opts)
  tbl_id <- set_id_opts(tbl_id, rev(opts))

  expect_identical(id(tbl_id), "foo")
  expect_identical(id_opts(tbl_id), rev(opts))

  expect_error(set_id_opts(tbl_id, rep(opts, 2)))

  expect_warning(tbl_id <- new_tbl_id("foo", opts[-2L]),
                 "id `foo` is incompatible with id options `bar`, `baz`")
  expect_null(id_opts(tbl_id))

  opts <- setNames(opts, LETTERS[1:3])
  tbl_id <- new_tbl_id("foo", opts)

  expect_identical(id(tbl_id), "foo")
  expect_identical(id_opts(tbl_id), opts)

  tbl_id <- rename_cols(tbl_id, letters[1:3], c("bar", "foo", "baz"))

  expect_identical(id(tbl_id), "b")
  expect_identical(id_opts(tbl_id), setNames(letters[1:3], LETTERS[1:3]))

  tbl_id <- new_tbl_id("foo", opts)
  tbl_id <- rename_cols(tbl_id, letters[1:3], c("bar", "foo", "xyz"))

  expect_identical(id(tbl_id), "b")
  expect_identical(id_opts(tbl_id), setNames(c("a", "b", "baz"), LETTERS[1:3]))

  tbl_id <- new_tbl_id("foo", opts)
  tbl_id <- rename_cols(tbl_id, "xyz", "bar")

  expect_identical(id(tbl_id), "foo")
  expect_identical(id_opts(tbl_id), setNames(c("xyz", "foo", "baz"),
                                             LETTERS[1:3]))

  expect_error(rename_cols(new_tbl_id("foo", opts), "baz", "bar"))

  tbl_id <- new_tbl_id("foo", opts)

  expect_identical(rename_cols(tbl_id, "a", "xyz"), tbl_id)
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
