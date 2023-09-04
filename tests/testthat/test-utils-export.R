skip_on_cran()

test_that("psv export", {

  tmp <- withr::local_tempdir()

  dat <- ts_tbl(
    a = rep(1:3, each = 10), b = hours(rep(1:10, 3)), c = rnorm(30)
  )

  expect_null(write_psv(dat, tmp))

  res <- read_psv(tmp, col_spec = readr::cols(b = "d", c = "d"),
                  id_var = "a", index_var = "b")

  expect_fsetequal(dat, res)
})
