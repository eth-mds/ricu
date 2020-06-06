
sofa  <- sofa_data("mimic_demo")
sowin <- sofa_window(sofa)
score <- sofa_compute(sowin)

test_that("sofa", {

  expect_is(sofa, "ts_tbl")
  expect_true(is_ts_tbl(sofa))
  expect_identical(id_vars(sofa), "icustay_id")
  expect_identical(index_var(sofa), "charttime")
  expect_setequal(data_vars(sofa),
    c("norepi", "dopa", "dobu", "map", "epi", "bili", "crea", "coag", "pafi",
      "gcs", "urine"))
  expect_gt(nrow(sofa), 0L)
  expect_equal(interval(sofa), hours(1L))

  expect_is(sowin, "ts_tbl")
  expect_true(is_ts_tbl(sowin))
  expect_identical(id_vars(sowin), "icustay_id")
  expect_identical(index_var(sowin), "charttime")
  expect_setequal(data_vars(sowin),
    c("norepi", "dopa", "dobu", "map", "epi", "bili", "crea", "coag", "pafi",
      "gcs", "urine"))
  expect_gte(nrow(sowin), nrow(sofa))
  expect_equal(interval(sowin), interval(sofa))

  expect_is(score, "ts_tbl")
  expect_true(is_ts_tbl(score))
  expect_identical(id_vars(score), "icustay_id")
  expect_identical(index_var(score), "charttime")
  expect_setequal(data_vars(score),
    c("sofa_resp", "sofa_coag", "sofa_liver", "sofa_cardio", "sofa_cns",
      "sofa_renal", "sofa_score"))
  expect_identical(nrow(score), nrow(sowin))
  expect_equal(interval(score), interval(sofa))
})

si    <- si_data("mimic_demo")
siwin <- si_windows(si)

test_that("suspicion of infection", {

  expect_is(si, "ts_tbl")
  expect_true(is_ts_tbl(si))
  expect_identical(id_vars(si), "icustay_id")
  expect_identical(index_var(si), "startdate")
  expect_setequal(data_vars(si), c("abx", "samp"))
  expect_gt(nrow(si), 0L)
  expect_equal(interval(si), hours(1L))

  expect_is(siwin, "ts_tbl")
  expect_true(is_ts_tbl(siwin))
  expect_identical(id_vars(siwin), "icustay_id")
  expect_identical(index_var(siwin), "si_time")
  expect_setequal(data_vars(siwin), c("si_lwr", "si_upr"))
  expect_lte(nrow(siwin), nrow(si))
  expect_equal(interval(siwin), interval(si))
})

sep3 <- sepsis_3(score, siwin)

test_that("sepsis 3", {

  expect_is(sep3, "ts_tbl")
  expect_true(is_ts_tbl(sep3))
  expect_identical(id_vars(sep3), "icustay_id")
  expect_identical(index_var(sep3), "sep3_time")
  expect_setequal(data_vars(sep3), "si_time")
  expect_gt(nrow(sep3), 0L)
  expect_equal(interval(sep3), hours(1L))
})
