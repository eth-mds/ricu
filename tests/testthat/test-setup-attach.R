
test_that("auto attach env var", {

  srcs <- c("mimic_demo", "eicu_demo")

  withr::local_envvar(RICU_SRC_LOAD = paste(srcs, collapse = ","))

  expect_setequal(auto_attach_srcs(), srcs)
})

skip_if_srcs_missing(c("mimic_demo", "eicu_demo"))

test_that("attach srcs", {

  srcs <- attached_srcs()
  test <- c("mimic_test", "eicu_test")

  expect_null(
    attach_src(test, data_dir = src_data_dir(sub("_test", "_demo", test)),
               cfg_dirs = system.file("testdata", package = "ricu"))
  )

  expect_setequal(setdiff(attached_srcs(), srcs), test)

  mi_tst <- as_src_env("mimic_test")

  expect_s3_class(mi_tst, c("mimic_test_env", "mimic_demo_env", "mimic_env"))

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

  tmp <- withr::local_tempdir()
  dir <- file.path(tmp, test)

  expect_null(
    attach_src(test, data_dir = dir,
               cfg_dirs = system.file("testdata", package = "ricu"))
  )

  expect_error(
    expect_message(
      mockthat::with_mock(
        is_interactive = TRUE,
        read_line = "n",
        as_src_tbl("admissions", "mimic_test")
      ),
      class = "miss_tbl_msg"
    ),
    class = "miss_tbl_err"
  )

  mockthat::with_mock(
    download_src = NULL,
    import_src = function(x, data_dir, ...) {
      files <- list.files(src_data_dir(sub("_test", "_demo", src_name(x))),
                          full.names = TRUE)
      file.copy(files, data_dir, recursive = TRUE)
    },
    setup_src_data(test, dir,
      cfg_dirs = system.file("testdata", package = "ricu")
    )
  )

  mi_adm <- as_src_tbl("admissions", "mimic_test")

  expect_s3_class(mi_adm, c("mimic_test_tbl", "mimic_demo_tbl", "mimic_tbl",
                            "src_tbl"))

  stop_fun <- mockthat::mock(stop("some error"))

  expect_warning(
    expect_null(
      expect_invisible(
        mockthat::with_mock(
          read_src_cfg = stop_fun,
          attach_src("mimic_demo")
        )
      )
    ),
    class = "src_cfg_read_error"
  )

  expect_warning(
    expect_null(
      expect_invisible(
        mockthat::with_mock(
          parse_src_cfg = stop_fun,
          attach_src("mimic_demo")
        )
      )
    ),
    class = "src_cfg_parse_error"
  )

  expect_warning(
    expect_null(
      expect_invisible(
        mockthat::with_mock(
          setup_src_env = stop_fun,
          attach_src("mimic_demo")
        )
      )
    ),
    class = "src_attach_error"
  )

  expect_warning(
    mimic_demo[[names(mimic_demo)[1L]]] <- "foo",
    class = "assign_src_tbl"
  )
})
