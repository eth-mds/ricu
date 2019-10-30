
.onLoad <- function(libname, pkgname) {

  if (is_pkg_available("mimic.demo")) {
    attach_datasource(
      dir = system.file("extdata", package = "mimic.demo"),
      cfg = get_config("mimic-demo",
        system.file("extdata", "config", package = "sepsr")
      ),
      env_name = "mimic_demo", link_global = FALSE
    )
  }

  if (is_pkg_available("eicu.demo")) {
    attach_datasource(
      dir = system.file("extdata", package = "eicu.demo"),
      cfg = get_config("eicu-demo",
        system.file("extdata", "config", package = "sepsr")
      ),
      env_name = "eicu_demo", link_global = FALSE
    )
  }
}
