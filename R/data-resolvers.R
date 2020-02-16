
check_mmhg <- function(x, unit_col, ...) {

  assert_that(
    all(is.na(x[[unit_col]]) |
      grepl("mm hg", x[[unit_col]], ignore.case = TRUE))
  )

	x
}

as_flag <- function(x, val_col, ...) {
  x[, c(val_col) := 1]
}
