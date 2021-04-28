
test_that("data dir", {

  dir <- withr::with_envvar(
    c(RICU_DATA_PATH = NA),
    data_dir(create = FALSE)
  )

  expect_identical(basename(dir), "ricu")

  tmp <- withr::local_tempdir()

  expect_true(dir.exists(tmp))
  expect_false(dir.exists(file.path(tmp, "foo")))

  dir <- withr::with_envvar(
    c(RICU_DATA_PATH = tmp),
    data_dir("foo")
  )

  expect_true(dir.exists(file.path(tmp, "foo")))
})

test_that("set config", {

  cfg <- list(a = 1L, b = 1L:3L, c = list(a = 1L, b = 1L:3L))
  tmp <- withr::local_tempdir()

  expect_false(file.exists(file.path(tmp, "foo.json")))
  expect_null(set_config(cfg, "foo", tmp))
  expect_true(file.exists(file.path(tmp, "foo.json")))

  res <- get_config("foo", tmp)

  expect_identical(cfg, res)
})
