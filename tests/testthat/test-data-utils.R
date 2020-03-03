
test_that("read data items (wide)", {

  dat <- load_items("eicu_demo", "vitalperiodic", "sao2")

  expect_is(dat, "list")
  expect_length(dat, 1L)
  expect_is(dat[[1L]], "ts_tbl")
  expect_identical(key(dat[[1L]]), "patienthealthsystemstayid")
  expect_identical(index(dat[[1L]]), "observationoffset")
  expect_identical(data_cols(dat[[1L]]), "sao2")
  expect_equal(interval(dat[[1L]]), hours(1L))

  dat <- load_items("eicu_demo", "vitalperiodic", c("temperature", "sao2"))

  expect_is(dat, "list")
  expect_length(dat, 2L)
  expect_identical(chr_ply(dat, key), rep("patienthealthsystemstayid", 2L))
  expect_identical(chr_ply(dat, index), rep("observationoffset", 2L))
  expect_identical(chr_ply(dat, data_cols), c("temperature", "sao2"))

  dat <- load_items("eicu_demo", "vitalperiodic", c("temperature", "sao2"),
                    names = c("temp", "sat"))

  expect_is(dat, "list")
  expect_length(dat, 2L)
  expect_identical(chr_ply(dat, key), rep("patienthealthsystemstayid", 2L))
  expect_identical(chr_ply(dat, index), rep("observationoffset", 2L))
  expect_identical(chr_ply(dat, data_cols), c("temp", "sat"))
})
