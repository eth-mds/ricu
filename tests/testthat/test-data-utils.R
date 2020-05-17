
test_that("id up/downgrades", {

  tbl  <- get("labevents", envir = get_src_env("mimic_demo"))
  alb <- load_ts(tbl, is_val(itemid, 50862L), "value",
                 "icustay_id", "charttime")

  expect_is(alb, "ts_tbl")

  tbl  <- get("lab", envir = get_src_env("eicu_demo"))
  alb <- load_ts(tbl, is_val(labname, "albumin"), "labresult",
                 "patienthealthsystemstayid", "labresultoffset")

  expect_is(alb, "ts_tbl")

  tbl  <- get("patients", envir = get_src_env("mimic_demo"))
  sex <- load_id(tbl, cols = "gender", id_col = "hadm_id")

  expect_is(sex, "id_tbl")
})
