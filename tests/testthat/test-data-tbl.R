
test_that("load_src()", {

  tbl  <- get("d_labitems", envir = get_src_env("mimic_demo"))
  expect_fsetequal(load_src(tbl), tbl[])

  tbl  <- get("hospital", envir = get_src_env("eicu_demo"))
  expect_fsetequal(load_src(tbl), tbl[])
})

test_that("mimic load_difftime()", {

  tbl  <- get("labevents", envir = get_src_env("mimic_demo"))
  cols <- c("hadm_id", "charttime", "value")

  alb1 <- load_difftime(tbl, is_val(itemid, 50862L), cols)

  expect_is(alb1, "id_tbl")
  expect_named(alb1, cols)
  expect_is(alb1[["charttime"]], "difftime")
  expect_identical(units(alb1[["charttime"]]), "mins")

  alb2 <- load_difftime(tbl, is_val(itemid, 50862L), cols[-2L])
  expect_fsetequal(alb1[, cols[-2L], with = FALSE], alb2)

  alb2 <- load_difftime(tbl, is_val(itemid, 50862L), cols[-1L])
  expect_fsetequal(alb1, alb2)

  expect_is(alb2, "id_tbl")
  expect_named(alb2, cols)
  expect_identical(units(alb1[["charttime"]]), "mins")

  expect_error(
    load_difftime(tbl, is_val(itemid, 50862L),
                  c("icustay_id", "charttime", "value")),
    class = "vctrs_error_subscript_oob"
  )
})

test_that("eicu load_difftime()", {

  tbl  <- get("lab", envir = get_src_env("eicu_demo"))
  cols <- c("patientunitstayid", "labresultoffset", "labresult")

  alb1 <- load_difftime(tbl, is_val(labname, "albumin"), cols)

  expect_is(alb1, "id_tbl")
  expect_named(alb1, cols)
  expect_is(alb1[["labresultoffset"]], "difftime")
  expect_identical(units(alb1[["labresultoffset"]]), "mins")

  alb2 <- load_difftime(tbl, is_val(labname, "albumin"), cols[-2L])
  expect_fsetequal(alb1[, c("patientunitstayid", "labresult"), with = FALSE],
                   alb2)

  alb2 <- load_difftime(tbl, is_val(labname, "albumin"), cols[-1L])
  expect_fsetequal(alb1, alb2)

  expect_is(alb2, "id_tbl")
  expect_named(alb2, cols)
  expect_identical(units(alb1[["labresultoffset"]]), "mins")

  expect_error(
    load_difftime(tbl, is_val(labname, "albumin"),
      c("patienthealthsystemstayid", "labresultoffset", "labresult")
    ),
    class = "vctrs_error_subscript_oob"
  )
})

test_that("mimic load_id()", {

  tbl  <- get("labevents", envir = get_src_env("mimic_demo"))
  cols <- c("charttime", "value")

  alb1 <- load_id(tbl, is_val(itemid, 50862L), cols)

  expect_is(alb1, "id_tbl")
  expect_identical(units(alb1[["charttime"]]), "hours")

  alb2 <- load_id(tbl, is_val(itemid, 50862L), cols, interval = mins(60L))

  expect_identical(units(alb2[["charttime"]]), "mins")

  units(alb2[["charttime"]]) <- "hours"

  expect_fsetequal(alb1, alb2)
})

test_that("eicu load_id()", {

  tbl  <- get("lab", envir = get_src_env("eicu_demo"))
  cols <- c("labresultoffset", "labresult")

  alb1 <- load_id(tbl, is_val(labname, "albumin"), cols)

  expect_is(alb1, "id_tbl")
  expect_identical(units(alb1[["labresultoffset"]]), "hours")

  alb2 <- load_id(tbl, is_val(labname, "albumin"), cols, interval = mins(60L))

  expect_identical(units(alb2[["labresultoffset"]]), "mins")

  units(alb2[["labresultoffset"]]) <- "hours"

  expect_fsetequal(alb1, alb2)
})
