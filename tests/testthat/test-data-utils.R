
skip_on_cran()

test_that("id up/downgrades", {

  tbl  <- as_src_tbl("labevents", "mimic_demo")
  alb <- load_ts(tbl, is_val(itemid, 50862L), "value",
                 "icustay_id", "charttime")

  expect_is(alb, "ts_tbl")

  tbl  <- as_src_tbl("lab", "eicu_demo")
  alb <- load_ts(tbl, is_val(labname, "albumin"), "labresult",
                 "patienthealthsystemstayid", "labresultoffset")

  expect_is(alb, "ts_tbl")

  tbl  <- as_src_tbl("patients", "mimic_demo")
  sex <- load_id(tbl, cols = "gender", id_var = "hadm_id")

  expect_is(sex, "id_tbl")
})
