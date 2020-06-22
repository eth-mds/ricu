
so_mi <- load_concepts("sofa_score", "mimic_demo")
so_ei <- load_concepts("sofa_score", "eicu_demo")

test_that("sofa", {

  expect_is(so_mi, "ts_tbl")
  expect_true(is_ts_tbl(so_mi))
  expect_identical(id_vars(so_mi), "icustay_id")
  expect_identical(index_var(so_mi), "charttime")
  expect_equal(interval(so_mi), hours(1L))
  expect_setequal(data_vars(so_mi),
    c("sofa_resp", "sofa_coag", "sofa_liver", "sofa_cardio", "sofa_cns",
      "sofa_renal", "sofa_score"))

  expect_is(so_ei, "ts_tbl")
  expect_true(is_ts_tbl(so_ei))
  expect_identical(id_vars(so_ei), "patientunitstayid")
  expect_identical(index_var(so_ei), "respchartoffset")
  expect_equal(interval(so_ei), hours(1L))
  expect_setequal(data_vars(so_ei),
    c("sofa_resp", "sofa_coag", "sofa_liver", "sofa_cardio", "sofa_cns",
      "sofa_renal", "sofa_score"))
})

si_mi <- load_concepts("susp_inf", "mimic_demo")
si_ei <- load_concepts("susp_inf", "eicu_demo", abx_min_count = 2L,
                       positive_cultures = TRUE, si_mode = "or")

test_that("suspicion of infection", {

  expect_is(si_mi, "ts_tbl")
  expect_true(is_ts_tbl(si_mi))
  expect_identical(id_vars(si_mi), "icustay_id")
  expect_identical(index_var(si_mi), "susp_inf")
  expect_setequal(data_vars(si_mi), c("si_lwr", "si_upr"))
  expect_equal(interval(si_mi), hours(1L))

  expect_is(si_ei, "ts_tbl")
  expect_true(is_ts_tbl(si_ei))
  expect_identical(id_vars(si_ei), "patientunitstayid")
  expect_identical(index_var(si_ei), "susp_inf")
  expect_setequal(data_vars(si_ei), c("si_lwr", "si_upr"))
  expect_equal(interval(si_ei), hours(1L))
})

sep3 <- sepsis_3(so_mi, si_mi)

test_that("sepsis 3", {

  expect_is(sep3, "ts_tbl")
  expect_true(is_ts_tbl(sep3))
  expect_identical(id_vars(sep3), "icustay_id")
  expect_identical(index_var(sep3), "sepsis_3")
  expect_setequal(data_vars(sep3), "susp_inf")
  expect_gt(nrow(sep3), 0L)
  expect_equal(interval(sep3), hours(1L))
})
