
test_that("item constructor", {

  itms1 <- new_item("a", "src", "tbl", "col")

  expect_is(itms1, "item")
  expect_true(is_item(itms1))

  expect_length(itms1, 1L)
  expect_named(itms1, "a")

  itms2 <- c(itms1, new_item("b", "src", "tbl", "col"))

  expect_is(itms2, "item")
  expect_true(is_item(itms2))

  expect_length(itms2, 2L)
  expect_named(itms2, c("a", "b"))

  itms3 <- c(itms2, new_item("c", "src", "tbl", "col"))

  expect_is(itms3, "item")
  expect_true(is_item(itms3))

  expect_length(itms3, 3L)
  expect_named(itms3, c("a", "b", "c"))
})

test_that("concept/dictionary constructor", {

  conc1 <- new_concept("a", new_item("a", "src", "tbl", "col"))

  expect_is(conc1, "concept")
  expect_true(is_concept(conc1))

  expect_length(conc1, 1L)
  expect_named(conc1, "a")

  conc2 <- new_concept("b", c(new_item("b", "src", "tbl", "col"),
                              new_item("b", "src", "tbl", "col")))

  expect_is(conc2, "concept")
  expect_true(is_concept(conc2))

  expect_length(conc2, 1L)
  expect_named(conc2, "b")

  conc3 <- c(conc1, conc2)

  expect_is(conc3, "concept")
  expect_true(is_concept(conc3))

  expect_length(conc3, 2L)
  expect_named(conc3, c("a", "b"))

  dict <- new_dictionary(conc3)

  expect_is(dict, "dictionary")
  expect_true(is_dictionary(dict))

  expect_length(dict, 2L)
  expect_named(dict, c("a", "b"))
})


test_that("load concepts", {

  gluc <- concept("gluc", "mimic_demo", "labevents", "itemid",
                  c(50809L, 50931L))

  expect_message(dat1 <- load_concepts("mimic_demo", gluc))

  expect_is(dat1, "ts_tbl")
  expect_true(is_ts_tbl(dat1))
  expect_identical(colnames(dat1), c("hadm_id", "charttime", "gluc"))
  expect_equal(interval(dat1), hours(1L))

  albu <- concept("albu", "mimic_demo", "labevents", "itemid", 50862L)

  expect_message(dat2 <- load_concepts("mimic_demo", c(albu, gluc)))

  expect_setequal(data_cols(dat2), c("gluc", "albu"))

  expect_message(dat3 <- load_concepts("mimic_demo", gluc, merge_data = FALSE))

  expect_is(dat3, "list")
  expect_length(dat3, 1L)

  dat3 <- dat3[[1L]]

  expect_true(is_ts_tbl(dat3))
  expect_identical(colnames(dat3), c("hadm_id", "charttime", "gluc"))
  expect_equal(interval(dat3), hours(1L))

  expect_message(
    dat4 <- load_concepts("mimic_demo", gluc, aggregate = FALSE,
                          merge_data = FALSE)
  )

  expect_is(dat4, "list")
  expect_length(dat4, 1L)

  dat4 <- dat4[[1L]]

  expect_true(is_ts_tbl(dat4))
  expect_identical(colnames(dat4), c("hadm_id", "charttime", "gluc"))
  expect_gt(nrow(dat4), nrow(dat3))

  expect_identical(dat3,
    dat4[, list(gluc = median(gluc)), by = c(meta_cols(dat4))]
  )

  expect_message(
    dat5 <- load_concepts("mimic_demo", gluc, aggregate = identity,
                          merge_data = FALSE)
  )

  expect_is(dat5, "list")
  expect_length(dat5, 1L)
  expect_identical(dat4, dat5[[1L]])

  expect_error(
    load_concepts("mimic_demo", gluc, aggregate = "identity")
  )

  static <- load_concepts("mimic_demo", "sex")

  expect_is(static, "id_tbl")
  expect_true(is_id_tbl(static))
  expect_identical(colnames(static), c("hadm_id", "charttime", "sex"))
})
