
skip_if_srcs_missing(c("mimic_demo", "eicu_demo"))

so_mi <- load_concepts("sofa", "mimic_demo", verbose = FALSE)
so_ei <- load_concepts("sofa", "eicu_demo", verbose = FALSE)

test_that("sofa", {

  expect_s3_class(so_mi, "ts_tbl")
  expect_true(is_ts_tbl(so_mi))
  expect_identical(id_vars(so_mi), "icustay_id")
  expect_identical(index_var(so_mi), "charttime")
  expect_equal(interval(so_mi), hours(1L))
  expect_identical(data_vars(so_mi), "sofa")

  expect_s3_class(so_ei, "ts_tbl")
  expect_true(is_ts_tbl(so_ei))
  expect_identical(id_vars(so_ei), "patientunitstayid")
  expect_identical(index_var(so_ei), "labresultoffset")
  expect_equal(interval(so_ei), hours(1L))
  expect_identical(data_vars(so_ei), "sofa")
})

si_mi <- load_concepts("susp_inf", "mimic_demo", verbose = FALSE)
si_ei <- load_concepts("susp_inf", "eicu_demo", verbose = FALSE,
                       abx_min_count = 2L, positive_cultures = TRUE,
                       si_mode = "or")

test_that("suspicion of infection", {

  expect_s3_class(si_mi, "ts_tbl")
  expect_true(is_ts_tbl(si_mi))
  expect_identical(id_vars(si_mi), "icustay_id")
  expect_identical(index_var(si_mi), "chartdate")
  expect_identical(data_vars(si_mi), "susp_inf")
  expect_equal(interval(si_mi), hours(1L))

  expect_s3_class(si_ei, "ts_tbl")
  expect_true(is_ts_tbl(si_ei))
  expect_identical(id_vars(si_ei), "patientunitstayid")
  expect_identical(index_var(si_ei), "infusionoffset")
  expect_identical(data_vars(si_ei), "susp_inf")
  expect_equal(interval(si_ei), hours(1L))
})

sep3 <- sep3(sofa = so_mi, susp_inf = si_mi)

test_that("sepsis 3", {

  expect_s3_class(sep3, "ts_tbl")
  expect_true(is_ts_tbl(sep3))
  expect_identical(id_vars(sep3), "icustay_id")
  expect_identical(index_var(sep3), "charttime")
  expect_identical(data_vars(sep3), "sep3")
  expect_type(data_col(sep3), "logical")
  expect_gt(nrow(sep3), 0L)
  expect_equal(interval(sep3), hours(1L))
})

qs_mi <- load_concepts("qsofa", "mimic_demo", verbose = FALSE)
qs_ei <- load_concepts("qsofa", "eicu_demo", verbose = FALSE)

test_that("qsofa", {

  expect_s3_class(qs_mi, "ts_tbl")
  expect_true(is_ts_tbl(qs_mi))
  expect_identical(id_vars(qs_mi), "icustay_id")
  expect_identical(index_var(qs_mi), "charttime")
  expect_equal(interval(qs_mi), hours(1L))
  expect_identical(data_vars(qs_mi), "qsofa")

  expect_s3_class(qs_ei, "ts_tbl")
  expect_true(is_ts_tbl(qs_ei))
  expect_identical(id_vars(qs_ei), "patientunitstayid")
  expect_identical(index_var(qs_ei), "nursingchartoffset")
  expect_equal(interval(qs_ei), hours(1L))
  expect_identical(data_vars(qs_ei), "qsofa")
})

sr_mi <- load_concepts("sirs", "mimic_demo", verbose = FALSE)
sr_ei <- load_concepts("sirs", "eicu_demo", verbose = FALSE)

test_that("sirs", {

  expect_s3_class(sr_mi, "ts_tbl")
  expect_true(is_ts_tbl(sr_mi))
  expect_identical(id_vars(sr_mi), "icustay_id")
  expect_identical(index_var(sr_mi), "charttime")
  expect_equal(interval(sr_mi), hours(1L))
  expect_identical(data_vars(sr_mi), "sirs")

  expect_s3_class(sr_ei, "ts_tbl")
  expect_true(is_ts_tbl(sr_ei))
  expect_identical(id_vars(sr_ei), "patientunitstayid")
  expect_identical(index_var(sr_ei), "observationoffset")
  expect_equal(interval(sr_ei), hours(1L))
  expect_identical(data_vars(sr_ei), "sirs")
})

ns_mi <- load_concepts("news", "mimic_demo", verbose = FALSE)
ns_ei <- load_concepts("news", "eicu_demo", verbose = FALSE)

test_that("news", {

  expect_s3_class(ns_mi, "ts_tbl")
  expect_true(is_ts_tbl(ns_mi))
  expect_identical(id_vars(ns_mi), "icustay_id")
  expect_identical(index_var(ns_mi), "charttime")
  expect_equal(interval(ns_mi), hours(1L))
  expect_identical(data_vars(ns_mi), "news")

  expect_s3_class(ns_ei, "ts_tbl")
  expect_true(is_ts_tbl(ns_ei))
  expect_identical(id_vars(ns_ei), "patientunitstayid")
  expect_identical(index_var(ns_ei), "observationoffset")
  expect_equal(interval(ns_ei), hours(1L))
  expect_identical(data_vars(ns_ei), "news")
})

ms_mi <- load_concepts("mews", "mimic_demo", verbose = FALSE)
ms_ei <- load_concepts("mews", "eicu_demo", verbose = FALSE)

test_that("mews", {

  expect_s3_class(ms_mi, "ts_tbl")
  expect_true(is_ts_tbl(ms_mi))
  expect_identical(id_vars(ms_mi), "icustay_id")
  expect_identical(index_var(ms_mi), "charttime")
  expect_equal(interval(ms_mi), hours(1L))
  expect_identical(data_vars(ms_mi), "mews")

  expect_s3_class(ms_ei, "ts_tbl")
  expect_true(is_ts_tbl(ms_ei))
  expect_identical(id_vars(ms_ei), "patientunitstayid")
  expect_identical(index_var(ms_ei), "observationoffset")
  expect_equal(interval(ms_ei), hours(1L))
  expect_identical(data_vars(ms_ei), "mews")
})
