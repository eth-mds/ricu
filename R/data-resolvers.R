
check_mmhg <- function(x, unit_col, ...) {
	assert_that(identical(unique(x[[unit_col]]), "mm Hg"))
	x
}
