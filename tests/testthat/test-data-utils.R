
test_that("id up/downgrades", {

  alb <- data_ts("mimic_demo", "labevents", is_val(itemid, 50862L), "value",
                 "icustay_id", "charttime")

  expect_is(alb, "ts_tbl")

  alb <- data_ts("eicu_demo", "lab", is_val(labname, "albumin"), "labresult",
                 "patienthealthsystemstayid", "labresultoffset")

  expect_is(alb, "ts_tbl")

  sex <- data_id("mimic_demo", "patients", NULL, "gender", "hadm_id")

  expect_is(sex, "id_tbl")
})
