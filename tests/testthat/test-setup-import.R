
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
})

test_that("import partitioned", {

  part <- list(col = "carbu", breaks = 4)

  cfg <- new_tbl_cfg("cars", "foo", "mtcars.csv", spec, 32L, part)

  expect_null(partition_table(cfg, tmp, chunk_length = 5))

  cfg <- new_tbl_cfg("cars", "foo", c("cars_0.csv", "cars_1.csv"), spec, 32L,
                     part)

  expect_null(partition_table(cfg, tmp, chunk_length = 5))
})
