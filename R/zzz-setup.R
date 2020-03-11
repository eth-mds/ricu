
.onLoad <- function(libname, pkgname) {

  fix_base_fun <- function(fun, fix) {

    fun_name <- as.character(substitute(fun))

    fun_name <- fun_name[3L]
    fun_body <- body(fun)

    if (class(fun_body)[1L] != "{") {
      fun_body <- as.call(c(as.name("{"), fun_body))
    }

    if (!length(grep("icu_tbl", fun_body[[2L]], fixed = TRUE))) {

      fun_body <- fun_body[c(1L, NA, 2L:length(fun_body))]
      fun_body[[2L]] <- parse(text = fix)[[1L]]

      body(fun) <- fun_body

      (unlockBinding)(fun_name, baseenv())
      assign(fun_name, fun, envir = asNamespace("base"), inherits=FALSE)
      lockBinding(fun_name, baseenv())
    }

    invisible(NULL)
  }

  attach_mimic(demo = TRUE)
  attach_mimic(demo = FALSE)
  attach_eicu(demo = TRUE)
  attach_eicu(demo = FALSE)
  attach_hirid()

  if (base::getRversion() < "4.0.0") {

    if (missing(pkgname)) prefix <- ""
    else                  prefix <- "ricu::"

    cbind_fix <- paste0(
      "if (!identical(class(..1), 'data.frame')) for (x in list(...)) { ",
      "if (inherits(x, 'icu_tbl')) return(", prefix, ".cbind.icu_tbl(...)) }"
    )

    rbind_fix <- paste0("for (x in list(...)) { ",
      "if (inherits(x, 'icu_tbl')) return(", prefix, ".rbind.icu_tbl(...)) }")

    fix_base_fun(base::cbind.data.frame, cbind_fix)
    fix_base_fun(base::rbind.data.frame, rbind_fix)
  }
}

.datatable.aware = TRUE
