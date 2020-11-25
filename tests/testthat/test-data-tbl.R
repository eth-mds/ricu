
skip_if_srcs_missing(c("mimic_demo", "eicu_demo"))

test_that("load_src()", {

  expect_fsetequal(
    load_src("d_labitems", "mimic_demo"),
    as_src_tbl("d_labitems", "mimic_demo")[]
  )

  expect_fsetequal(
    load_src("hospital", "eicu_demo"),
    as_src_tbl("hospital", "eicu_demo")[]
  )
})

test_that("mimic load_difftime()", {

  cols <- c("hadm_id", "charttime", "value")

  alb1 <- load_difftime("labevents", "mimic_demo", is_val(itemid, 50862L),
                        cols)

  expect_is(alb1, "id_tbl")
  expect_named(alb1, cols)
  expect_is(alb1[["charttime"]], "difftime")
  expect_identical(units(alb1[["charttime"]]), "mins")

  alb2 <- load_difftime("labevents", "mimic_demo", is_val(itemid, 50862L),
                        cols[-2L])
  expect_fsetequal(alb1[, cols[-2L], with = FALSE], alb2)

  alb2 <- load_difftime("labevents", "mimic_demo", is_val(itemid, 50862L),
                        cols[-1L])
  expect_fsetequal(alb1, alb2)

  expect_is(alb2, "id_tbl")
  expect_named(alb2, cols)
  expect_identical(units(alb1[["charttime"]]), "mins")

  expect_error(
    load_difftime("labevents", "mimic_demo", is_val(itemid, 50862L),
                  c("icustay_id", "charttime", "value")),
    class = "vctrs_error_subscript_oob"
  )
})

test_that("eicu load_difftime()", {

  cols <- c("patientunitstayid", "labresultoffset", "labresult")

  alb1 <- load_difftime("lab", "eicu_demo", is_val(labname, "albumin"), cols)

  expect_is(alb1, "id_tbl")
  expect_named(alb1, cols)
  expect_is(alb1[["labresultoffset"]], "difftime")
  expect_identical(units(alb1[["labresultoffset"]]), "mins")

  alb2 <- load_difftime("lab", "eicu_demo", is_val(labname, "albumin"),
                        cols[-2L])
  expect_fsetequal(alb1[, c("patientunitstayid", "labresult"), with = FALSE],
                   alb2)

  alb2 <- load_difftime("lab", "eicu_demo", is_val(labname, "albumin"),
                        cols[-1L])
  expect_fsetequal(alb1, alb2)

  expect_is(alb2, "id_tbl")
  expect_named(alb2, cols)
  expect_identical(units(alb1[["labresultoffset"]]), "mins")

  expect_error(
    load_difftime("lab", "eicu_demo", is_val(labname, "albumin"),
      c("patienthealthsystemstayid", "labresultoffset", "labresult")
    ),
    class = "vctrs_error_subscript_oob"
  )
})

test_that("mimic load_id()", {

  cols <- c("charttime", "value")

  alb1 <- load_id("labevents", "mimic_demo", is_val(itemid, 50862L), cols)

  expect_is(alb1, "id_tbl")
  expect_identical(units(alb1[["charttime"]]), "hours")

  alb2 <- load_id("labevents", "mimic_demo", is_val(itemid, 50862L), cols,
                  interval = mins(60L))

  expect_identical(units(alb2[["charttime"]]), "mins")

  units(alb2[["charttime"]]) <- "hours"

  expect_fsetequal(alb1, alb2)
})

test_that("eicu load_id()", {

  cols <- c("labresultoffset", "labresult")

  alb1 <- load_id("lab", "eicu_demo", is_val(labname, "albumin"), cols)

  expect_is(alb1, "id_tbl")
  expect_identical(units(alb1[["labresultoffset"]]), "hours")

  alb2 <- load_id("lab", "eicu_demo", is_val(labname, "albumin"), cols,
                  interval = mins(60L))

  expect_identical(units(alb2[["labresultoffset"]]), "mins")

  units(alb2[["labresultoffset"]]) <- "hours"

  expect_fsetequal(alb1, alb2)
})
