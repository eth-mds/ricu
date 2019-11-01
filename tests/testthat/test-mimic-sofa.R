
pa <- sepsr:::mimic_sofa_pa(data_env = "mimic_demo")

test_that("fetch sofa pa", {
  expect_is(pa, "data.table")
  expect_named(pa, c("hadm_id", "rel_time", "pao2"))
  expect_identical(anyDuplicated(pa[, c("hadm_id", "rel_time")]), 0L)
})

fi <- sepsr:::mimic_sofa_fi(data_env = "mimic_demo")

test_that("fetch sofa fi", {
  expect_is(fi, "data.table")
  expect_named(fi, c("hadm_id", "rel_time", "fio2"))
  expect_identical(anyDuplicated(fi[, c("hadm_id", "rel_time")]), 0L)
})

pafi <- sepsr:::mimic_sofa_pafi(pa, fi)

test_that("fetch sofa pafi", {
  expect_is(pafi, "data.table")
  expect_named(pafi, c("hadm_id", "rel_time", "pafi"))
  expect_identical(anyDuplicated(pafi[, c("hadm_id", "rel_time")]), 0L)
})

test_that("fetch sofa vars", {

  res <- sepsr:::mimic_sofa_vars(pafi = pafi, data_env = "mimic_demo")

  expect_is(res, "data.table")
  expect_named(res, c("hadm_id", "rel_time", "pafi", "vent", "coag", "bili",
                      "map", "norepi", "dopa", "dobu", "epi", "gcs",
                      "crea", "urine_24"))
  expect_identical(anyDuplicated(res[, c("hadm_id", "rel_time")]), 0L)
})
