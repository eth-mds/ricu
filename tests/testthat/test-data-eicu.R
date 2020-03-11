
test_that("eicu ts_tbl", {

  albu <- eicu_ts("lab", is_val(labname, "albumin"), "labresult",
                  time_col = "labresultoffset", source = "eicu_demo")

  expect_is(albu, "ts_tbl")
  expect_true(is_ts_tbl(albu))
  expect_identical(id(albu), "patienthealthsystemstayid")
  expect_identical(index(albu), "labresultoffset")
  expect_equal(interval(albu), hours(1L))
  expect_identical(data_cols(albu), "labresult")

})

test_that("eicu id_tbl", {

  age <- eicu_id("patient", cols = "age", source = "eicu_demo")

  expect_is(age, "id_tbl")
  expect_true(is_id_tbl(age))
  expect_identical(id(age), "patienthealthsystemstayid")
  expect_identical(data_cols(age), "age")

  adx <- eicu_id("admissiondx", cols = "admitdxname", source = "eicu_demo")

  expect_is(adx, "id_tbl")
  expect_true(is_id_tbl(adx))
  expect_identical(id(adx), "patienthealthsystemstayid")
  expect_identical(data_cols(adx), "admitdxname")
})
