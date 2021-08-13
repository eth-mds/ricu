
skip_on_cran()

tmp_cars <- withr::local_tempdir()

write.csv(mtcars, file.path(tmp_cars, "mtcars.csv"), row.names = FALSE)

invisible(
  Map(
    write.csv,
    split(mtcars, mtcars$vs),
    lapply(file.path(tmp_cars, paste0("cars_", unique(mtcars$vs), ".csv.gz")),
           gzfile),
    row.names = FALSE
  )
)

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

  expect_null(csv_to_fst(cfg, tmp_cars, progress = FALSE))
  expect_true("foo.fst" %in% list.files(tmp_cars))
  expect_equal(mtcars, fst::read_fst(file.path(tmp_cars, "foo.fst")),
               ignore_attr = TRUE)

  cfg <- new_tbl_cfg("cars", "foo", "mtcars.csv", num_rows = 32L,
    cols = c(spec[-length(spec)],
             list(weight = list(name = "wt", spec = "col_logical")))
  )

  expect_warning(csv_to_fst(cfg, tmp_cars, progress = FALSE),
                 class = "csv_parsing_error")
})

test_that("import partitioned", {

  part <- list(col = "carbu", breaks = 4)
  file <- paste0(seq_len(2L), ".fst")

  tbf <- "foo"
  foo <- file.path(tmp_cars, tbf)
  cfg <- new_tbl_cfg("cars", tbf, "mtcars.csv", spec, 32L, part)

  expect_null(
    partition_table(cfg, tmp_cars, chunk_length = 5, progress = FALSE)
  )

  expect_true(dir.exists(foo))
  expect_setequal(list.files(foo), file)

  tbb <- "bar"
  bar <- file.path(tmp_cars, tbb)
  cfg <- new_tbl_cfg("cars", tbb, c("cars_0.csv.gz", "cars_1.csv.gz"), spec,
                     32L, part)

  expect_null(
    partition_table(cfg, tmp_cars, chunk_length = 5, progress = FALSE)
  )

  expect_true(dir.exists(bar))
  expect_setequal(list.files(bar), file)

  for (x in file) {
    expect_fsetequal(
      fst::read_fst(file.path(foo, x), as.data.table = TRUE),
      fst::read_fst(file.path(bar, x), as.data.table = TRUE)
    )
  }
})

tmp_srcs <- withr::local_tempdir()

for (x in c("PATIENTS.csv", "SERVICES.csv")) {

  res <- readRDS(
    system.file("testdata", paste0(x, ".rds"), package = "ricu")
  )

  writeBin(res[["content"]], file.path(tmp_srcs, x))
}

test_that("import src", {

  tbls <- list.files(tmp_srcs, "\\.csv")
  fstf <- file.path(tmp_srcs, sub("\\.csv", ".fst", tolower(tbls)))

  expect_invisible(
    res <- import_src("mimic_demo", tmp_srcs, verbose = FALSE)
  )
  expect_null(res)

  expect_true(all(file.exists(fstf)))

  expect_warning(
    import_src("mimic_demo", tmp_srcs, verbose = FALSE),
    class = "no_import"
  )

  expect_invisible(
    res <- import_src("mimic_demo", tmp_srcs, force = TRUE, verbose = FALSE)
  )
  expect_null(res)

  unlink(fstf)

  expect_invisible(
    res <- import_src("mimic_demo", tmp_srcs,
                      tables = sub("\\.csv", "", tolower(tbls[1L])),
                      verbose = FALSE)
  )
  expect_null(res)

  expect_true(file.exists(fstf[1L]))
  expect_false(file.exists(fstf[2L]))

  expect_error(
    import_src("mimic_demo", tmp_srcs, tables = "foo", verbose = FALSE),
    class = "are_in_assert"
  )

  mocks <- mockthat::local_mock(
    src_file_exist = quote({
      if (identical(type, "raw")) rep(TRUE, length(x))
      else rep(FALSE, length(x))
    }),
    import_tbl = quote(invisible(NULL))
  )

  expect_null(import_src("aumc", tmp_srcs, verbose = FALSE))
  expect_null(import_src("aumc", tmp_srcs, force = TRUE, verbose = FALSE))

  expect_s3_class(
    mockthat::mock_arg(mocks[["import_tbl"]], "locale"),
    "locale"
  )
})
