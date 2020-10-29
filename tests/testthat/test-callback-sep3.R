
test_that("susp_inf", {

  abx <- ts_tbl(hadm_id = rep(100012L, 13L), startdate = hours(c(48, 52, 57,
    61, 72, 80, 84, 92, 93, 104, 115, 192, 216)), abx = rep(TRUE, 13L),
    interval = hours(1L)
  )

  samp <- ts_tbl(hadm_id = rep(100012L, 3L), chartdate = hours(c(1, 48, 70)),
    samp = rep(TRUE, 3L), interval = hours(1L)
  )

  expected <- ts_tbl(hadm_id = rep(100012L, 6L), chartdate = hours(c(1, 48, 52,
    57, 61, 70)), susp_inf = rep(TRUE, 6L), interval = hours(1L)
  )

  expect_identical(susp_inf(abx, samp), expected)
})
