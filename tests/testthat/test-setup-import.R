
tmp <- tempfile()

setup({

  dir.create(tmp)

  write.csv(mtcars, file.path(tmp, "mtcars.csv"), row.names = FALSE)

  Map(
    write.csv,
    split(mtcars, mtcars$vs),
    file.path(tmp, paste0("cars_", unique(mtcars$vs), ".csv")),
    row.names = FALSE
  )
})

teardown(unlink(tmp, recursive = TRUE))

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

  expect_null(csv_to_fst(cfg, tmp))
  expect_true("foo.fst" %in% list.files(tmp))
  expect_equal(mtcars, fst::read_fst(file.path(tmp, "foo.fst")),
               check.attributes = FALSE)
})

test_that("import partitioned", {

  part <- list(col = "carbu", breaks = 4)
  file <- paste0(seq_len(2L), ".fst")

  tbf <- "foo"
  foo <- file.path(tmp, tbf)
  cfg <- new_tbl_cfg("cars", tbf, "mtcars.csv", spec, 32L, part)

  expect_null(partition_table(cfg, tmp, chunk_length = 5))
  expect_true(dir.exists(foo))
  expect_setequal(list.files(foo), file)

  tbb <- "bar"
  bar <- file.path(tmp, tbb)
  cfg <- new_tbl_cfg("cars", tbb, c("cars_0.csv", "cars_1.csv"), spec, 32L,
                     part)

  expect_null(partition_table(cfg, tmp, chunk_length = 5))
  expect_true(dir.exists(bar))
  expect_setequal(list.files(bar), file)

  for (x in file) {
    expect_fsetequal(
      fst::read_fst(file.path(foo, x), as.data.table = TRUE),
      fst::read_fst(file.path(bar, x), as.data.table = TRUE)
    )
  }
})
