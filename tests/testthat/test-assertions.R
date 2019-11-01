
test_that("is_difftime", {

  expect_true(is_difftime(as.difftime(2L, units = "hours")))
  expect_true(is_difftime(as.difftime(-2L, units = "hours")))

  expect_true(is_difftime(as.difftime(0L, units = "hours"), TRUE))
  expect_true(is_difftime(as.difftime(0L, units = "hours"), FALSE))

  expect_true(is_difftime(as.difftime(2L, units = "hours"), FALSE))
  expect_false(is_difftime(as.difftime(-2L, units = "hours"), FALSE))

  expect_false(is_difftime(2L))
  expect_false(is_difftime(NULL))
  expect_false(is_difftime(NA))
})
