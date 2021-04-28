
skip_if_srcs_missing(c("mimic_demo", "eicu_demo"))

test_that("id up/downgrades", {

  tbl  <- as_src_tbl("labevents", "mimic_demo")
  alb <- load_ts(tbl, is_val(itemid, 50862L), "value",
                 "icustay_id", "charttime")

  expect_s3_class(alb, "ts_tbl")

  tbl  <- as_src_tbl("lab", "eicu_demo")
  alb <- load_ts(tbl, is_val(labname, "albumin"), "labresult",
                 "patienthealthsystemstayid", "labresultoffset")

  expect_s3_class(alb, "ts_tbl")

  tbl  <- as_src_tbl("patients", "mimic_demo")
  sex <- load_id(tbl, cols = "gender", id_var = "hadm_id")

  expect_s3_class(sex, "id_tbl")
})
