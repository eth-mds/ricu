
skip_if_srcs_missing("mimic_demo")

test_that("load concepts", {

  gluc <- concept("gluc",
    item("mimic_demo", "labevents", "itemid", list(c(50809L, 50931L)))
  )

  dat1 <- load_concepts(gluc)

  expect_is(dat1, "ts_tbl")
  expect_true(is_ts_tbl(dat1))
  expect_identical(colnames(dat1), c("icustay_id", "charttime", "gluc"))
  expect_equal(interval(dat1), hours(1L))

  albu <- concept("albu", item("mimic_demo", "labevents", "itemid", 50862L))

  dat2 <- load_concepts(c(albu, gluc))

  expect_setequal(data_vars(dat2), c("gluc", "albu"))

  dat3 <- load_concepts(gluc, merge_data = FALSE)

  expect_is(dat3, "list")
  expect_length(dat3, 1L)

  dat3 <- dat3[[1L]]

  expect_true(is_ts_tbl(dat3))
  expect_identical(colnames(dat3), c("icustay_id", "charttime", "gluc"))
  expect_equal(interval(dat3), hours(1L))

  dat4 <- load_concepts(gluc, aggregate = FALSE, merge_data = FALSE)

  expect_is(dat4, "list")
  expect_length(dat4, 1L)

  dat4 <- dat4[[1L]]

  expect_true(is_ts_tbl(dat4))
  expect_identical(colnames(dat4), c("icustay_id", "charttime", "gluc"))
  expect_gt(nrow(dat4), nrow(dat3))

  expect_identical(dat3,
    dat4[, list(gluc = median(gluc)), by = c(meta_vars(dat4))]
  )

  dat5 <- load_concepts(gluc, aggregate = identity, merge_data = FALSE)

  expect_is(dat5, "list")
  expect_length(dat5, 1L)
  expect_identical(dat4, dat5[[1L]])

  expect_error(
    load_concepts(gluc, aggregate = "identity")
  )

  static <- load_concepts(c("sex", "age"), "mimic_demo")

  expect_is(static, "id_tbl")
  expect_true(is_id_tbl(static))
  expect_setequal(colnames(static), c("icustay_id", "sex", "age"))
  expect_type(static[["age"]], "double")
  expect_type(static[["sex"]], "character")

  expect_error(
    concept("gluc",
      item("mimic_demo", "labevents", "itemid", c(50809L, 50931L))
    )
  )

  gluc2 <- concept("gluc",
    list(item("mimic_demo", "labevents", "itemid", c(50809L, 50931L)))
  )

  dat6 <- load_concepts(gluc2)

  expect_identical(dat1, dat6)

  gcs_raw <- concept("gcs_raw", load_dictionary(concepts = "gcs"),
                     set_sed_max = FALSE, class = "rec_cncpt")

  dat7 <- load_concepts(gcs_raw, "mimic_demo")

  expect_is(dat7, "ts_tbl")
  expect_true(is_ts_tbl(dat7))
  expect_identical(colnames(dat7), c("icustay_id", "charttime", "gcs_raw"))
  expect_equal(interval(dat7), hours(1L))
})
