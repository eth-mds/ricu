
mimic <- load_concepts("sofa_score", "mimic_demo")
eicu  <- load_concepts("sofa_score", "eicu_demo")

test_that("sofa", {

  expect_is(mimic, "ts_tbl")
  expect_true(is_ts_tbl(mimic))
  expect_identical(id_vars(mimic), "icustay_id")
  expect_identical(index_var(mimic), "charttime")
  expect_equal(interval(mimic), hours(1L))
  expect_setequal(data_vars(mimic),
    c("sofa_resp", "sofa_coag", "sofa_liver", "sofa_cardio", "sofa_cns",
      "sofa_renal", "sofa_score"))

  expect_is(eicu, "ts_tbl")
  expect_true(is_ts_tbl(eicu))
  expect_identical(id_vars(eicu), "patientunitstayid")
  expect_identical(index_var(eicu), "respchartoffset")
  expect_equal(interval(eicu), hours(1L))
  expect_setequal(data_vars(eicu),
    c("sofa_resp", "sofa_coag", "sofa_liver", "sofa_cardio", "sofa_cns",
      "sofa_renal", "sofa_score"))
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

sep3 <- sepsis_3(mimic, siwin)

test_that("sepsis 3", {

  expect_is(sep3, "ts_tbl")
  expect_true(is_ts_tbl(sep3))
  expect_identical(id_vars(sep3), "icustay_id")
  expect_identical(index_var(sep3), "sep3_time")
  expect_setequal(data_vars(sep3), "si_time")
  expect_gt(nrow(sep3), 0L)
  expect_equal(interval(sep3), hours(1L))
})
