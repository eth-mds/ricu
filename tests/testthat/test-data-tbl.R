
test_that("mimic ts_tbl", {

  albu <- data_ts("mimic_demo", "labevents", is_val(itemid, 50862L), "value",
                  time_col = "charttime")

  expect_is(albu, "ts_tbl")
  expect_true(is_ts_tbl(albu))
  expect_identical(id(albu), "hadm_id")
  expect_identical(index(albu), "charttime")
  expect_equal(interval(albu), hours(1L))
  expect_identical(data_cols(albu), "value")
})

test_that("mimic id_tbl", {

  sex <- data_id("mimic_demo", "patients", cols = "gender")

  expect_is(sex, "id_tbl")
  expect_true(is_id_tbl(sex))
  expect_identical(id(sex), "hadm_id")
  expect_identical(data_cols(sex), "gender")
})

test_that("eicu ts_tbl", {

  albu <- data_ts("eicu_demo", "lab", is_val(labname, "albumin"), "labresult",
                  time_col = "labresultoffset")

  expect_is(albu, "ts_tbl")
  expect_true(is_ts_tbl(albu))
  expect_identical(id(albu), "patienthealthsystemstayid")
  expect_identical(index(albu), "labresultoffset")
  expect_equal(interval(albu), hours(1L))
  expect_identical(data_cols(albu), "labresult")

})

test_that("eicu id_tbl", {

  age <- data_id("eicu_demo", "patient", cols = "age")

  expect_is(age, "id_tbl")
  expect_true(is_id_tbl(age))
  expect_identical(id(age), "patienthealthsystemstayid")
  expect_identical(data_cols(age), "age")

  adx <- data_id("eicu_demo", "admissiondx", cols = "admitdxname")

  expect_is(adx, "id_tbl")
  expect_true(is_id_tbl(adx))
  expect_identical(id(adx), "patienthealthsystemstayid")
  expect_identical(data_cols(adx), "admitdxname")
})
