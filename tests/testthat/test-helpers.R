test_that("expect_all_identical", {

  expect_success(expect_all_identical("foo", "foo"))
  expect_success(expect_all_identical(c("foo", "foo"), "foo"))
  expect_failure(expect_all_identical("foo", "bar"))

  expect_success(expect_all_identical(c("foo", "bar"), c("foo", "bar")))
  expect_failure(expect_all_identical(c("foo", "bar"), c("foo", "foo")))
  expect_success(expect_all_identical("foo", c("foo", "foo")))
  expect_failure(expect_all_identical("foo", c("foo", "bar")))

  expect_success(expect_all_identical(c("foo", "foo")))
  expect_failure(expect_all_identical(c("foo", "bar")))
})

test_that("expect_all_equal", {

  expect_success(expect_all_equal("foo", "foo"))
  expect_success(expect_all_equal(c("foo", "foo"), "foo"))
  expect_failure(expect_all_equal("foo", "bar"))

  expect_success(expect_all_equal(c("foo", "bar"), c("foo", "bar")))
  expect_failure(expect_all_equal(c("foo", "bar"), c("foo", "foo")))
  expect_success(expect_all_equal("foo", c("foo", "foo")))
  expect_failure(expect_all_equal("foo", c("foo", "bar")))

  expect_success(expect_all_equal(c("foo", "foo")))
  expect_failure(expect_all_equal(c("foo", "bar")))
})

test_that("expect_all_equal", {

  expect_success(expect_all(TRUE))
  expect_failure(expect_all(FALSE))
  expect_success(expect_all(c(TRUE, TRUE)))
  expect_failure(expect_all(c(TRUE, FALSE)))
})

test_that("expect_fsetequal", {

  dt1 <- data.table::data.table(a = letters[1:10], b = 1:10)
  dt2 <- data.table::data.table(a = LETTERS[1:10], b = 1:10)

  expect_success(expect_fsetequal(dt1, dt1))
  expect_success(expect_fsetequal(dt1, dt1[sample(1:10), ]))
  expect_failure(expect_fsetequal(dt1, dt2))
  expect_failure(expect_fsetequal(dt1, dt1[-1L, ]))
})
