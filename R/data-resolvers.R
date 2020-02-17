
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

percent_as_numeric <- function(x, val_col, ...) {
  set(x, j = val_col, value = as.numeric(sub("%", "", x[[val_col]])))
  x
}
