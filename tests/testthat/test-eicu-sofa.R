
pa <- sepsr:::eicu_sofa_pa(data_env = "eicu_demo")

test_that("fetch sofa pa", {
  expect_is(pa, "data.table")
  expect_named(pa, c("hadm_id", "rel_time", "pao2"))
  expect_identical(anyDuplicated(pa[, c("hadm_id", "rel_time")]), 0L)
})
