
.onLoad <- function(libname, pkgname) {

  attach_mimic(demo = TRUE)
  attach_mimic(demo = FALSE)
  attach_eicu(demo = TRUE)
  attach_eicu(demo = FALSE)
}
