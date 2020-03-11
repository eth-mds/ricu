
test_that("mimic ts_tbl", {

  albu <- mimic_ts("labevents", is_val(itemid, 50862L), "value",
                  time_col = "charttime", source = "mimic_demo")

  expect_is(albu, "ts_tbl")
  expect_true(is_ts_tbl(albu))
  expect_identical(id(albu), "hadm_id")
  expect_identical(index(albu), "charttime")
  expect_equal(interval(albu), hours(1L))
  expect_identical(data_cols(albu), "value")
})

test_that("mimic id_tbl", {

  sex <- mimic_id("patients", cols = "gender", source = "mimic_demo")

  expect_is(sex, "id_tbl")
  expect_true(is_id_tbl(sex))
  expect_identical(id(sex), "hadm_id")
  expect_identical(data_cols(sex), "gender")
})
