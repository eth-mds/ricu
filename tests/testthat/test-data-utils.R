
test_that("read data items", {

  dat <- load_items("eicu_demo", "vitalperiodic", c("temperature", "sao2"))

  expect_is(dat, "list")
  expect_length(dat, 2L)

  for (x in dat) {
    expect_is(x, "ts_tbl")
  }
})
