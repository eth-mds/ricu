
.onLoad <- function(libname, pkgname) {

  attach_mimic(demo = FALSE)
  attach_eicu(demo = FALSE)

  if (is_pkg_available("mimic.demo")) {
    attach_mimic(
      demo = TRUE,
      dir = system.file("extdata", package = "mimic.demo"),
      cfg = get_config("mimic-demo",
        system.file("extdata", "config", package = "sepsr")
      )
    )
  } else {
    attach_mimic(demo = TRUE)
  }

  if (is_pkg_available("eicu.demo")) {
    attach_eicu(
      demo = TRUE,
      dir = system.file("extdata", package = "eicu.demo"),
      cfg = get_config("eicu-demo",
        system.file("extdata", "config", package = "sepsr")
      )
    )
  } else {
    attach_eicu(demo = TRUE)
  }
}
