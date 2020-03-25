
test_that("mimic data_tbl", {

  cols <- c("hadm_id", "charttime", "value")
  alb1 <- data_tbl("mimic_demo", "labevents", is_val(itemid, 50862L), cols)

  expect_is(alb1, "data.table")
  expect_named(alb1, cols)
  expect_is(alb1[["charttime"]], "difftime")
  expect_identical(units(alb1[["charttime"]]), "hours")

  alb2 <- data_tbl("mimic_demo", "labevents", is_val(itemid, 50862L), cols,
                   mins(60L))

  expect_identical(units(alb2[["charttime"]]), "mins")

  units(alb2[["charttime"]]) <- "hours"

  expect_fsetequal(alb1, alb2)

  alb2 <- data_tbl("mimic_demo", "labevents", is_val(itemid, 50862L),
                   cols[-2L])

  expect_fsetequal(alb1[, c("hadm_id", "value"), with = FALSE], alb2)

  expect_warning(
    alb2 <- data_tbl("mimic_demo", "labevents", is_val(itemid, 50862L),
                     cols[-1L]),
    "an ID column is required"
  )

  expect_is(alb2, "data.table")
  expect_named(alb2, cols[-1L])
  expect_is(alb2[["charttime"]], "POSIXt")

  expect_error(
    data_tbl("mimic_demo", "labevents", is_val(itemid, 50862L),
             c("icustay_id", "charttime", "value")),
    class = "vctrs_error_subscript_oob"
  )

  expect_fsetequal(data_tbl("mimic_demo", "d_labitems"),
                   get_table("d_labitems", "mimic_demo")[])
})

test_that("eicu data_tbl", {

  cols <- c("patientunitstayid", "labresultoffset", "labresult")
  alb1 <- data_tbl("eicu_demo", "lab", is_val(labname, "albumin"), cols)

  expect_is(alb1, "data.table")
  expect_named(alb1, cols)
  expect_is(alb1[["labresultoffset"]], "difftime")
  expect_identical(units(alb1[["labresultoffset"]]), "hours")

  alb2 <- data_tbl("eicu_demo", "lab", is_val(labname, "albumin"), cols,
                   mins(60L))

  expect_identical(units(alb2[["labresultoffset"]]), "mins")

  units(alb2[["labresultoffset"]]) <- "hours"

  expect_fsetequal(alb1, alb2)

  alb2 <- data_tbl("eicu_demo", "lab", is_val(labname, "albumin"),
                   cols[-2L])

  expect_fsetequal(alb1[, c("patientunitstayid", "labresult"), with = FALSE],
                   alb2)

  expect_warning(
    alb2 <- data_tbl("eicu_demo", "lab", is_val(labname, "albumin"),
                     cols[-1L]),
    "an ID column is required"
  )

  expect_is(alb2, "data.table")
  expect_named(alb2, cols[-1L])
  expect_is(alb2[["labresultoffset"]], "integer")

  expect_error(
    data_tbl("eicu_demo", "lab", is_val(labname, "albumin"),
             c("patienthealthsystemstayid", "labresultoffset", "labresult")),
    class = "vctrs_error_subscript_oob"
  )

  expect_fsetequal(data_tbl("eicu_demo", "hospital"),
                   get_table("hospital", "eicu_demo")[])
})
