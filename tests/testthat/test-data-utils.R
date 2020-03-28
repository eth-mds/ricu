
test_that("mimic data_ts", {

  alb <- data_ts("mimic_demo", "labevents", is_val(itemid, 50862L), "value",
                 "icustay_id", "charttime")

  expect_is(alb, "ts_tbl")

  alb <- data_ts("eicu_demo", "lab", is_val(labname, "albumin"), "labresult",
                 "patienthealthsystemstayid", "labresultoffset")

  expect_is(alb, "ts_tbl")
})

test_that("read data items (wide)", {

  expect_message(dat <- load_items("eicu_demo", "vitalperiodic", "sao2"))

  expect_is(dat, "list")
  expect_length(dat, 1L)
  expect_all(lapply(dat, is_ts_tbl))
  expect_all_identical(lapply(dat, id), "patienthealthsystemstayid")
  expect_all_identical(lapply(dat, index), "observationoffset")
  expect_all_identical(lapply(dat, data_cols), "sao2")
  expect_all_equal(lapply(dat, interval), hours(1L))

  expect_message(
    dat2 <- load_items("eicu_demo", "vitalperiodic", "sao2",
                       callback = ident_cb)
  )

  expect_identical(dat, dat2)

  expect_message(
    dat2 <- load_items("eicu_demo", "vitalperiodic", "sao2",
                       callback = "ident_cb")
  )

  expect_identical(dat, dat2)

  expect_message(
    dat <- load_items("eicu_demo", "vitalperiodic", "sao2",
                      interval = mins(30L))
  )

  expect_all_equal(lapply(dat, interval), mins(30L))

  ids <- c(129391L, 131022L, 131736L, 132209L)

  expect_message(
    dat <- load_items("eicu_demo", "vitalperiodic", "sao2", patient_ids = ids)
  )

  expect_setequal(unique(dat[[1L]][["patienthealthsystemstayid"]]), ids)

  expect_message(
    dat <- load_items("eicu_demo", "vitalperiodic", c("temperature", "sao2"))
  )

  expect_is(dat, "list")
  expect_length(dat, 2L)
  expect_all(lapply(dat, is_ts_tbl))
  expect_all_identical(lapply(dat, id), "patienthealthsystemstayid")
  expect_all_identical(lapply(dat, index), "observationoffset")
  expect_setequal(chr_ply(dat, data_cols), c("temperature", "sao2"))
  expect_all_equal(lapply(dat, interval), hours(1L))

  expect_message(
    dat <- load_items("eicu_demo", "vitalperiodic", c("temperature", "sao2"),
                      names = c("temp", "sat"))
  )

  expect_setequal(chr_ply(dat, data_cols), c("temp", "sat"))
})

test_that("read data items (long)", {

  expect_message(
    dat <- load_items("mimic_demo", "labevents", "itemid", 50862L)
  )

  expect_is(dat, "list")
  expect_length(dat, 1L)
  expect_all(lapply(dat, is_ts_tbl))
  expect_all_identical(lapply(dat, id), "hadm_id")
  expect_all_identical(lapply(dat, index), "charttime")
  expect_all_identical(lapply(dat, data_cols), "50862")
  expect_all_equal(lapply(dat, interval), hours(1L))

  expect_message(
    dat2 <- load_items("mimic_demo", "labevents", "itemid", 50862L,
                       callback = ident_cb)
  )

  expect_identical(dat, dat2)

  expect_message(
    dat2 <- load_items("mimic_demo", "labevents", "itemid", 50862L,
                       callback = "ident_cb")
  )

  expect_identical(dat, dat2)

  expect_message(
    dat <- load_items("mimic_demo", "labevents", "itemid", 50862L,
                      interval = mins(30L))
  )

  expect_all_equal(lapply(dat, interval), mins(30L))

  ids <- c(129391L, 131022L, 131736L, 132209L)

  expect_message(
    dat <- load_items("mimic_demo", "labevents", "itemid", 50862L,
                       patient_ids = ids)
  )

  expect_identical(nrow(dat[[1L]]), 0L)

  ids <- c(100375L, 101361L, 102203L, 103379L, 103770L)

  expect_message(
    dat <- load_items("mimic_demo", "labevents", "itemid", 50862L,
                       patient_ids = ids)
  )

  expect_setequal(unique(dat[[1L]][["hadm_id"]]), ids)

  expect_message(
    dat <- load_items("mimic_demo", "labevents", "itemid", c(50809L, 50931L))
  )

  expect_is(dat, "list")
  expect_length(dat, 2L)
  expect_all(lapply(dat, is_ts_tbl))
  expect_all_identical(lapply(dat, id), "hadm_id")
  expect_all_identical(lapply(dat, index), "charttime")
  expect_setequal(chr_ply(dat, data_cols), c("50931", "50809"))
  expect_all_equal(lapply(dat, interval), hours(1L))

  expect_message(
    dat2 <- load_items("mimic_demo", "labevents", "itemid", c(50809L, 50931L),
                       c("glucA", "glucB"))
  )

  expect_length(dat2, 2L)
  expect_setequal(chr_ply(dat2, data_cols), c("glucA", "glucB"))
  expect_all_identical(dat,
    Map(rename_cols, dat2, c("50931", "50809"), lapply(dat2, data_cols))
  )

  expect_message(
    dat2 <- load_items("mimic_demo", "labevents", "itemid", c(50809L, 50931L),
                       "gluc")
  )

  expect_length(dat2, 1L)
  expect_fsetequal(dat2[[1L]],
    do.call(rbind, Map(rename_cols, cpy(dat), "gluc", lapply(dat, data_cols)))
  )

  expect_message(
    dat3 <- load_items("mimic_demo", "labevents", "itemid",
                       c(50809L, 50862L, 50931L), c("gluc", "albu", "gluc"))
  )

  expect_length(dat3, 2L)
  expect_setequal(chr_ply(dat3, data_cols), c("albu", "gluc"))
  expect_fsetequal(dat3[[1L]], dat2[[1L]])

  expect_message(
    dat3 <- load_items("mimic_demo", "labevents", "itemid",
                       c(50809L, 50862L, 50931L), c("gluc", "albu", "gluc"),
                       unit = c("mg/dL", "g/dL", "mg/dL"))
  )

  expect_setequal(chr_ply(dat3, data_unit), c("mg/dL", "g/dL"))

  expect_error(
    load_items("mimic_demo", "labevents", "itemid", c(50809L, 50862L, 50931L),
               c("gluc", "albu", "gluc"), unit = c("mg/dL", "g/dL"))
  )
})

test_that("read data items (grep)", {

  expect_message(
    dat <- load_items("eicu_demo", "infusiondrug", "drugname", "dopamine")
  )

  expect_identical(nrow(dat[[1L]]), 0L)

  expect_message(
    dat <- load_items("eicu_demo", "infusiondrug", "drugname", "dopamine",
                      regex = TRUE)
  )

  expect_is(dat, "list")
  expect_length(dat, 1L)
  expect_all(lapply(dat, is_ts_tbl))
  expect_all_identical(lapply(dat, id), "patienthealthsystemstayid")
  expect_all_identical(lapply(dat, index), "infusionoffset")
  expect_all_identical(lapply(dat, data_cols), "drugrate")
  expect_all_equal(lapply(dat, interval), hours(1L))

  expect_message(
    dat2 <- load_items("eicu_demo", "infusiondrug", "drugname",
                       c("dopamine", "dobutamine"), regex = TRUE)
  )

  expect_length(dat2, 1L)
  expect_gte(nrow(dat2[[1L]]), nrow(dat[[1L]]))

  expect_message(
    dat3 <- load_items("eicu_demo", "infusiondrug", "drugname", "dopamine",
                       "vaso", regex = TRUE)
  )

  expect_length(dat3, 1L)
  expect_identical(colnames(dat3[[1L]]),
    c("patienthealthsystemstayid", "infusionoffset", "vaso"))

  expect_error(
    load_items("eicu_demo", "infusiondrug", "drugname",
               c("dopamine", "dobutamine"), c("dopa", "dobu"), regex = TRUE)
  )

  expect_message(
    dat4 <- load_items("eicu_demo", "infusiondrug", "drugname",
                       c("dopamine", "dobutamine"), "vaso", regex = TRUE)
  )

  expect_length(dat4, 1L)
  expect_identical(colnames(dat4[[1L]]),
    c("patienthealthsystemstayid", "infusionoffset", "vaso"))

  expect_message(
    dat5 <- load_items("eicu_demo", "infusiondrug", "drugname", "dopamine",
                      regex = TRUE, unit = "mcg/kg/min")
  )

  expect_length(dat5, 1L)
  expect_identical(colnames(dat5[[1L]]),
    c("patienthealthsystemstayid", "infusionoffset", "drugrate"))
  expect_identical(data_unit(dat5[[1L]]), c(drugrate = "mcg/kg/min"))

  expect_message(
    dat6 <- load_items("eicu_demo", "infusiondrug", "drugname", "dopamine",
                       regex = TRUE, callback = ident_cb)
  )

  expect_identical(dat6, dat)
})
