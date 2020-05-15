
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

  src <- get_src_name(x)
  dat <- data_env()

  delayedAssign(src, setup_src_env(x, new.env(parent = dat), dir),
                assign.env = dat)

  makeActiveBinding(src, get_from_data_env(src), assign_env)

  invisible(NULL)
}

#' @param data_env,aux_env Environments where data tables and auxiliary tables
#' are assigned
#'
#' @rdname attach_source
#'
#' @export
#'
setup_src_env <- function(x, env, dir = NULL) {
  UseMethod("setup_src_env", x)
}

#' @rdname attach_source
#' @export
setup_src_env.src_config <- function(x, env, dir = NULL) {

  if (is.null(dir)) {
    dir <- source_data_dir(x)
  }

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

  more_arg <- list(id_cols = get_id_cols(x), src_name = get_src_name(x),
                   src_env = env)
  dat_tbls <- Map(new_data_src, files, as_col_defaults(x),
                  MoreArgs = more_arg)

  list2env(dat_tbls, envir = env)
}

#' @rdname attach_source
#' @export
data <- new.env()

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

pkg_env <- function() asNamespace(methods::getPackageName())

data_env <- function() get("data", envir = pkg_env(), mode = "environment")

get_from_data_env <- function(source) {

  source <- force(source)

  function(value) {

    assert_that(missing(value),
      msg = paste0("Cannot update read-only data source `", source, "`")
    )

    tryCatch(get_src_env(source), error = function(e) invisible(NULL))
  }
}

source_data_dir <- function(source) {

  if (is_src_config(source)) {
    source <- get_src_name(source)
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

new_data_src <- function(files, defaults, id_cols, src_name, src_env) {

  assert_that(is_col_defaults(defaults), is_id_cols(id_cols),
              is.string(src_name), is.environment(src_env))

  res <- prt::new_prt(files)
  class(res) <- c(paste0(c(src_name, "data"), "_src"), class(res))

  attr(res, "defaults") <- defaults
  attr(res, "id_cols")  <- id_cols
  attr(res, "src_env") <- src_env

  res
}

is_data_src <- function(x) inherits(x, "data_src")

#' @export
get_src_name.data_src <- function(x) {
  sub("_src$", "", class(x)[1L])
}

#' @rdname attach_source
#' @export
get_src_env <- function(x) UseMethod("get_src_env", x)

#' @rdname attach_source
#' @export
get_src_env.character <- function(x) {

  assert_that(length(x) == 1L)

  res <- get0(x, envir = data_env(), mode = "environment", ifnotfound = NULL)

  if (is.null(res)) {
    stop("Source `", x, "` not found in ", envir, " environment. For ",
         "further information on how to set up a data source, refer to ",
         "`?attach_datasource`.")
  }

  res
}

#' @rdname attach_source
#' @export
get_src_env.default <- function(x) get_src_env(get_src_name(x))

#' @rdname attach_source
#' @export
get_src_env.data_src <- function(x) attr(x, "src_env")
