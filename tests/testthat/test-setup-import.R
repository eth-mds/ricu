
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
  list(name = "trans", col = "am", spec = "col_integer"),
  list(name = "carbu", col = "carb", spec = "col_integer"),
  list(name = "cylin", col = "cyl", spec = "col_integer"),
  list(name = "displ", col = "disp", spec = "col_double"),
  list(name = "rear", col = "drat", spec = "col_double"),
  list(name = "gears", col = "gear", spec = "col_integer"),
  list(name = "power", col = "hp", spec = "col_double"),
  list(name = "mpg", col = "mpg", spec = "col_double"),
  list(name = "quar", col = "qsec", spec = "col_double"),
  list(name = "engin", col = "vs", spec = "col_integer"),
  list(name = "weight", col = "wt", spec = "col_double")
)

test_that("import csv", {

  cfg <- new_tbl_spec("cars", "mtcars.csv", list(spec), 32L, list(NULL), "foo")

  expect_null(csv_to_fst(cfg, tmp, cleanup = FALSE))
})

test_that("import partitioned", {

  part <- list(col = "carbu", breaks = c(0, 4, 8))

  cfg <- new_tbl_spec("cars", "mtcars.csv", list(spec), 32L, list(part), "foo")

  expect_null(partition_table(cfg, tmp, cleanup = FALSE, chunk_length = 5))

  cfg <- new_tbl_spec("cars", list(c("cars_0.csv", "cars_1.csv")),
                      list(spec), 32L, list(part), "foo")

  expect_null(partition_table(cfg, tmp, cleanup = FALSE, chunk_length = 5))
})
