
skip_on_cran()

so_mi <- load_concepts("sofa", "mimic_demo")
so_ei <- load_concepts("sofa", "eicu_demo")

test_that("sofa", {

  expect_is(so_mi, "ts_tbl")
  expect_true(is_ts_tbl(so_mi))
  expect_identical(id_vars(so_mi), "icustay_id")
  expect_identical(index_var(so_mi), "charttime")
  expect_equal(interval(so_mi), hours(1L))
  expect_identical(data_vars(so_mi), "sofa")

  expect_is(so_ei, "ts_tbl")
  expect_true(is_ts_tbl(so_ei))
  expect_identical(id_vars(so_ei), "patientunitstayid")
  expect_identical(index_var(so_ei), "labresultoffset")
  expect_equal(interval(so_ei), hours(1L))
  expect_identical(data_vars(so_ei), "sofa")
})
