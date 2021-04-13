
skip_if_srcs_missing("mimic_demo")

test_that("mimic itm callbacks", {

  cnc <- c("ins", "phn_rate", "los_hosp")
  dat <- load_concepts(cnc, "mimic_demo", verbose = FALSE)

  expect_s3_class(dat, c("ts_tbl", "id_tbl"))
  expect_setequal(data_vars(dat), cnc)
  expect_gt(nrow(dat), 0L)
})

test_that("mimic cncpt callbacks", {

  dat <- load_concepts("safi", "mimic_demo", verbose = FALSE,
                       mode = "extreme_vals")

  expect_s3_class(dat, c("ts_tbl", "id_tbl"))
  expect_identical(data_vars(dat), "safi")
  expect_gt(nrow(dat), 0L)

  cnc <- c("vaso_ind", "bmi", "norepi_equiv")
  dat <- load_concepts(cnc, "mimic_demo", verbose = FALSE)

  expect_s3_class(dat, c("ts_tbl", "id_tbl"))
  expect_setequal(data_vars(dat), cnc)
  expect_gt(nrow(dat), 0L)
})

skip_if_srcs_missing("eicu_demo")

test_that("eicu itm callbacks", {

  cnc <- c("adm", "age", "adh_rate")
  dat <- load_concepts(cnc, "eicu_demo", verbose = FALSE)

  expect_s3_class(dat, c("ts_tbl", "id_tbl"))
  expect_setequal(data_vars(dat), cnc)
  expect_gt(nrow(dat), 0L)
})

test_that("susp_inf", {

  abx <- ts_tbl(hadm_id = rep(100012L, 13L), startdate = hours(c(48, 52, 57,
    61, 72, 80, 84, 92, 93, 104, 115, 192, 216)), abx = rep(TRUE, 13L),
    interval = hours(1L)
  )

  samp <- ts_tbl(hadm_id = rep(100012L, 3L), chartdate = hours(c(1, 48, 70)),
    samp = rep(TRUE, 3L), interval = hours(1L)
  )

  expected <- ts_tbl(hadm_id = rep(100012L, 6L), chartdate = hours(c(1, 48, 52,
    57, 61, 70)), susp_inf = rep(TRUE, 6L), interval = hours(1L)
  )

  expect_identical(susp_inf(abx, samp), expected)
})
