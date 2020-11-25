
tmp_cars <- tempfile()

setup({

  dir.create(tmp_cars)

  write.csv(mtcars, file.path(tmp_cars, "mtcars.csv"), row.names = FALSE)

  Map(
    write.csv,
    split(mtcars, mtcars$vs),
    file.path(tmp_cars, paste0("cars_", unique(mtcars$vs), ".csv")),
    row.names = FALSE
  )
})

teardown(unlink(tmp_cars, recursive = TRUE))

spec <- list(
  trans = list(name = "am", spec = "col_integer"),
  carbu = list(name = "carb", spec = "col_integer"),
  cylin = list(name = "cyl", spec = "col_integer"),
  displ = list(name = "disp", spec = "col_double"),
  rear = list(name = "drat", spec = "col_double"),
  gears = list(name = "gear", spec = "col_integer"),
  power = list(name = "hp", spec = "col_double"),
  mpg = list(name = "mpg", spec = "col_double"),
  quar = list(name = "qsec", spec = "col_double"),
  engin = list(name = "vs", spec = "col_integer"),
  weight = list(name = "wt", spec = "col_double")
)

test_that("import csv", {

  cfg <- new_tbl_cfg("cars", "foo", "mtcars.csv", spec, 32L)

  expect_null(csv_to_fst(cfg, tmp_cars))
  expect_true("foo.fst" %in% list.files(tmp_cars))
  expect_equal(mtcars, fst::read_fst(file.path(tmp_cars, "foo.fst")),
               check.attributes = FALSE)
})

test_that("import partitioned", {

  part <- list(col = "carbu", breaks = 4)
  file <- paste0(seq_len(2L), ".fst")

  tbf <- "foo"
  foo <- file.path(tmp_cars, tbf)
  cfg <- new_tbl_cfg("cars", tbf, "mtcars.csv", spec, 32L, part)

  expect_null(partition_table(cfg, tmp_cars, chunk_length = 5))
  expect_true(dir.exists(foo))
  expect_setequal(list.files(foo), file)

  tbb <- "bar"
  bar <- file.path(tmp_cars, tbb)
  cfg <- new_tbl_cfg("cars", tbb, c("cars_0.csv", "cars_1.csv"), spec, 32L,
                     part)

  expect_null(partition_table(cfg, tmp_cars, chunk_length = 5))
  expect_true(dir.exists(bar))
  expect_setequal(list.files(bar), file)

  for (x in file) {
    expect_fsetequal(
      fst::read_fst(file.path(foo, x), as.data.table = TRUE),
      fst::read_fst(file.path(bar, x), as.data.table = TRUE)
    )
  }
})

tmp_srcs <- tempfile()

copy_test_file <- function(x, dir) {

  res <- readRDS(
    system.file("testdata", paste0(x, ".rds"), package = "ricu")
  )

  writeBin(res[["content"]], file.path(dir, x))
}

setup({
  dir.create(tmp_srcs)
  lapply(c("patients.csv", "services.csv"), copy_test_file, tmp_srcs)
})

teardown(unlink(tmp_srcs, recursive = TRUE))

test_that("import src", {

  tbls <- list.files(tmp_srcs, "\\.csv")
  fstf <- file.path(tmp_srcs, sub("\\.csv", ".fst", tbls))

  expect_invisible(res <- import_src("mimic_demo", tmp_srcs))
  expect_null(res)

  expect_true(all(file.exists(fstf)))

  expect_warning(import_src("mimic_demo", tmp_srcs), class = "no_import")
  expect_invisible(res <- import_src("mimic_demo", tmp_srcs, force = TRUE))
  expect_null(res)

  unlink(fstf)

  expect_invisible(
    res <- import_src("mimic_demo", tmp_srcs,
                      tables = sub("\\.csv", "", tbls[1L]))
  )
  expect_null(res)

  expect_true(file.exists(fstf[1L]))
  expect_false(file.exists(fstf[2L]))

  expect_error(import_src("mimic_demo", tmp_srcs, tables = "foo"),
                class = "are_in_assert")

  mock_aumc <- function(...) {
    with_mock(
      src_file_exist = function(x, dir, type) {
        if (identical(type, "raw")) rep(TRUE, length(x))
        else rep(FALSE, length(x))
      },
      import_tbl = function(x, data_dir, progress, ...) {
        assert_that(is_tbl_cfg(x), is.string(data_dir),
                    inherits(list(...)[["locale"]], "locale"))
        invisible(NULL)
      },
      ...
    )
  }

  expect_null(mock_aumc(import_src("aumc", tmp_srcs)))
  expect_null(mock_aumc(import_src("aumc", tmp_srcs, force = TRUE)))
})
