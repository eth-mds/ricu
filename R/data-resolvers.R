
check_mmhg <- function(x, unit_col, ...) {

  assert_that(
    all(is.na(x[[unit_col]]) |
      grepl("mm hg", x[[unit_col]], ignore.case = TRUE))
  )

	x
}

as_flag <- function(x, val_col, ...) {
  x[, c(val_col) := 1]
  x
}

as_numeric <- function(x, val_col, ...) {
  x[, c("tmp_col") := as.numeric(val_col)]
  on.exit(x[, c("tmp_col") := NULL])
  x[, c(val_col) := NULL]
  rename_cols(x, val_col, "tmp_col")
}
