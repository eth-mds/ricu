
.onLoad <- function(libname, pkgname) {

  fix_base_fun <- function(fun, fix) {

    fun_name <- as.character(substitute(fun))

    fun_name <- fun_name[3L]
    fun_body <- body(fun)

    if (class(fun_body)[1L] != "{") {
      fun_body <- as.call(c(as.name("{"), fun_body))
    }

    if (!length(grep("id_tbl", fun_body[[2L]], fixed = TRUE))) {

      fun_body <- fun_body[c(1L, NA, 2L:length(fun_body))]
      fun_body[[2L]] <- parse(text = fix)[[1L]]

      body(fun) <- fun_body

      (unlockBinding)(fun_name, baseenv())
      assign(fun_name, fun, envir = asNamespace("base"), inherits=FALSE)
      lockBinding(fun_name, baseenv())
    }

    invisible(NULL)
  }

  backports::import(pkgname)

  srcs <- auto_load_src_names()
  attach_src(srcs, assign_env = pkg_env())
  namespaceExport(pkg_env(), srcs)

  if (base::getRversion() < "4.0.0") {

    if (missing(pkgname)) prefix <- ""
    else                  prefix <- paste0(methods::getPackageName(), "::")

    cbind_fix <- paste0(
      "if (!identical(class(..1), 'data.frame')) for (x in list(...)) { ",
      "if (inherits(x, 'id_tbl')) return(", prefix, ".cbind.id_tbl(...)) }"
    )

    rbind_fix <- paste0("for (x in list(...)) { ",
      "if (inherits(x, 'id_tbl')) return(", prefix, ".rbind.id_tbl(...)) }")

    fix_base_fun(base::cbind.data.frame, cbind_fix)
    fix_base_fun(base::rbind.data.frame, rbind_fix)
  }
}

.onAttach <- function(libname, pkgname) {

  out <- character(0L)
  con <- textConnection("out", "w", local = TRUE)
  on.exit(close(con))

  pkg <- methods::getPackageName()
  ver <- utils::packageVersion(pkg)

  srcs  <- auto_load_src_names()
  avail <- ls(envir = data_env())
  bull  <- ifelse(srcs %in% avail, "tick", "cross")
  color <- ifelse(srcs %in% avail, "green", "red")

  cli::cat_rule(paste(pkg, ver), file = con)

  cli::cat_line(
    "The following data sources are configured to be attached:", file = con
  )

  Map(cli::cat_bullet, srcs, bullet = bull, bullet_col = color,
      MoreArgs = list(file = con))

  cli::cat_rule(file = con)

  packageStartupMessage(paste(out, collapse = "\n"))
}

.datatable.aware = TRUE
