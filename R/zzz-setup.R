
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

  src_stats <- function(src) {

    src_env <- tryCatch(
      get0(src, envir = data_env(), inherits = FALSE),
      message = function(msg) attr(msg, "tbl_ok"),
      warning = function(warn) NULL,
      error = function(err) NULL
    )

    if (is.null(src_env)) {
      c(NA_integer_, NA_integer_)
    } else if (is_src_env(src_env)) {
      rep(length(src_env), 2L)
    } else {
      c(sum(src_env), length(src_env))
    }
  }

  out <- character(0L)
  con <- textConnection("out", "w", local = TRUE)
  on.exit(close(con))

  pkg <- methods::getPackageName()
  ver <- utils::packageVersion(pkg)

  srcs  <- auto_load_src_names()
  stats <- int_ply(srcs, src_stats, length = 2L)
  avail <- ls(envir = data_env())
  avail <- is_true(srcs %in% avail & stats[1L, ] == stats[2L, ])
  bull  <- ifelse(avail, "tick", "cross")
  color <- ifelse(avail, "green", "red")
  srcs  <- paste0(srcs, ": ", stats[1L, ], " of ", stats[2L, ],
                  " tables available")

  cli::cat_line(file = con)
  cli::cat_rule(paste(pkg, ver), file = con)

  cli::cat_line(
    "\nThe following data sources are configured to be attached:\n",
    "(the environment variable `RICU_SRC_LOAD` controls this)\n",
    file = con
  )

  Map(cli::cat_bullet, srcs, bullet = bull, bullet_col = color,
      MoreArgs = list(file = con))

  cli::cat_line(file = con)
  cli::cat_rule(file = con)
  cli::cat_line(file = con)

  packageStartupMessage(paste(out, collapse = "\n"))
}

.datatable.aware = TRUE
