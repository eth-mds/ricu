
test_that("assertions", {

  expect_true(is_dt(data.table::data.table(a = 1, b = 2)))
  expect_false(is_dt(NULL))
  expect_false(is_dt(list(a = 1, b = 2)))
  expect_false(is_dt(data.frame(a = 1, b = 2)))

})
