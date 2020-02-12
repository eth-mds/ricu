
check_mmhg <- function(x) {
	assert_that(identical(unique(x[["valueuom"]]), "mm Hg"))
	x
}
