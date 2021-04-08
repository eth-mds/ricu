
test_that("auto attach env var", {

  srcs <- c("mimic_demo", "eicu_demo")

  withr::local_envvar(RICU_SRC_LOAD = paste(srcs, collapse = ","))

  expect_setequal(auto_attach_srcs(), srcs)
})

test_that("auto attach env var", {

  srcs <- attached_srcs()
  test <- c("mimic_test", "eicu_test")

  expect_null(
    attach_src(test, data_dir = src_data_dir(sub("_test", "_demo", test)),
               cfg_dirs = system.file("testdata", package = "ricu"))
  )

  expect_setequal(setdiff(attached_srcs(), srcs), test)

  mi_tst <- as_src_env("mimic_test")

  expect_s3_class(mi_tst, "mimic_test_env")
  expect_s3_class(mi_tst, "mimic_demo_env")
  expect_s3_class(mi_tst, "mimic_env")

  expect_setequal(names(mi_tst), names(mimic_demo))
  expect_fsetequal(data.table::as.data.table(mi_tst$admissions),
                   data.table::as.data.table(mimic_demo$admissions))

  expect_null(detach_src(test))
  expect_setequal(attached_srcs(), srcs)

  some_env <- new.env()

  expect_length(ls(envir = some_env), 0)

  expect_null(
    attach_src(test, data_dir = src_data_dir(sub("_test", "_demo", test)),
               cfg_dirs = system.file("testdata", package = "ricu"),
               assign_env = some_env)
  )

  expect_setequal(setdiff(attached_srcs(), srcs), test)
  expect_setequal(ls(envir = some_env), test)

  expect_identical(as_src_env("mimic_test"), some_env$mimic_test)

  expect_null(detach_src(test))
  expect_setequal(attached_srcs(), srcs)
  expect_length(ls(envir = some_env), 0)
})
