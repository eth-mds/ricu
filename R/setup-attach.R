
#' Data attach utilities
#'
#' Attaching a data source sets up an environment containing [prt::new_prt()]
#' files using [base::delayedAssign()].
#'
#' @param x Data source to attach
#' @param ... Forwarded to further calls to `attach_source()`
#'
#' @export
#'
attach_source <- function(x, ...) UseMethod("attach_source", x)

#' @param file File name string pointing to a non-default source configuration
#' file
#'
#' @rdname attach_source
#'
#' @export
#'
attach_source.character <- function(x, file = NULL, ...) {

  for (src in x) {
    attach_source(get_src_config(src, file = file), ...)
  }

  invisible(NULL)
}

#' @rdname attach_source
#'
#' @export
#'
attach_source.list <- function(x, ...) {

  assert_that(all_is(x, is_src_config))

  for (src in x) {
    attach_source(src, ...)
  }

  invisible(NULL)
}

#' @param dir Directory used to look for [fst::fst()] files; `NULL` calls
#' [data_dir()] using the source name as `subdir` argument
#' @param assign_env Environment in which the data source will become available
#'
#' @rdname attach_source
#'
#' @export
#'
attach_source.src_config <- function(x, dir = NULL, assign_env = .GlobalEnv,
                                     ...) {


  assert_that(...length() == 0L, is.environment(assign_env))

  src <- get_source(x)

  if (is.null(dir)) {
    dir <- source_data_dir(src)
  }

  delayedAssign(src, attach_data_env(x, dir), assign.env = get_env("data"))
  delayedAssign(src, attach_aux_env(x),       assign.env = get_env("aux"))

  makeActiveBinding(src, get_from_data_env(src), assign_env)

  invisible(NULL)
}

#' @name mimic
#' @rdname attach_source
#' @export mimic
#'
NULL

#' @name mimic_demo
#' @rdname attach_source
#' @export mimic_demo
#'
NULL

#' @name eicu
#' @rdname attach_source
#' @export eicu
#'
NULL

#' @name eicu_demo
#' @rdname attach_source
#' @export eicu_demo
#'
NULL

#' @name hirid
#' @rdname attach_source
#' @export hirid
#'
NULL

attach_data_env <- function(x, dir) {

  assert_that(is_src_config(x), is.string(dir))

  files <- Map(file.path, dir, fst_names(x))
  names(files) <- table_names(x)
  missing <- lgl_ply(files, all_is, Negate(file.exists))

  if (any(missing)) {

    todo <- table_names(x)[missing]

    msg <- paste("The following tables are missing from", dir,
                 paste(todo, collapse = "\n  "), sep = "\n  ")

    if (interactive()) {

      message(msg)
      resp <- read_line("Download now (Y/n)? ")

      if (!identical(resp, "Y")) {
        stop("Cannot continue without the missing data.")
      }

    } else {

      stop(msg)
    }

    download_source(x, dir, table_sel = todo)
    import_source(x, dir, cleanup = TRUE)
  }

  dat_tbls <- Map(new_tbl_src, files, list(as_id_cols(x)), as_col_defaults(x),
                  list(get_data_fun(x)))

  list2env(dat_tbls, envir = NULL, parent = get_env("data"))
}

attach_aux_env <- function(x) {

  assert_that(is_src_config(x))

  hook <- x[["attach_hook"]]

  if (is.function(hook)) {

    aux_tbls <- hook(x)

    assert_that(is.list(aux_tbls), !is.null(names(aux_tbls)))

    list2env(aux_tbls, envir = NULL, parent = get_env("aux"))

  } else {

    new.env(parent = get_env("aux"))
  }
}

get_from_data_env <- function(source) {

  source <- force(source)

  function(value) {

    assert_that(missing(value),
      msg = paste0("Cannot update read-only data source `", source, "`")
    )

    tryCatch(
      get_src(source, envir = "data"),
      error = function(e) invisible(NULL)
    )
  }
}

source_data_dir <- function(source) {

  if (is_src_config(source)) {
    source <- get_source(source)
  }

  assert_that(is.string(source))

  if (grepl("_demo$", source)) {

    pkg <- sub("_demo$", ".demo", source)

    if (is_pkg_available(pkg)) {
      system.file("extdata", package = pkg)
    } else {
      data_dir(source, create = FALSE)
    }

  } else {
    data_dir(source, create = FALSE)
  }
}
