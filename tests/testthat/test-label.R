
test_that("sofa mimic", {

  sofa <- sofa_data("mimic_demo")

  expect_is(sofa, "ts_tbl")
  expect_true(is_ts_tbl(sofa))
  expect_identical(key(sofa), "hadm_id")
  expect_identical(index(sofa), "charttime")
  expect_setequal(data_cols(sofa),
    c("norepi", "dopa", "dobu", "map", "epi", "bili", "crea", "coag", "pafi",
      "gcs", "urine"))
  expect_gt(nrow(sofa), 0L)
  expect_equal(interval(sofa), hours(1L))
})
