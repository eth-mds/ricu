
#' Data attach utilities
#'
#' Attaching a data source sets up an environment containing [prt::new_prt()]
#' files using [base::delayedAssign()].
#'
#' @param x Data source to attach
#' @param ... Forwarded to further calls to `attach_src()`
#'
#' @export
#'
attach_src <- function(x, ...) UseMethod("attach_src", x)

#' @param dir Directory used to look for [fst::fst()] files; `NULL` calls
#' [data_dir()] using the source name as `subdir` argument
#' @param assign_env Environment in which the data source will become available
#'
#' @rdname attach_src
#'
#' @export
#'
attach_src.src_cfg <- function(x, dir = src_data_dir(x),
                               assign_env = .GlobalEnv, ...) {

  assert_that(is.string(dir), is.environment(assign_env), ...length() == 0L)

  src <- src_name(x)
  dat <- data_env()

  delayedAssign(src, setup_src_env(x, new.env(parent = dat), dir),
                assign.env = dat)

  makeActiveBinding(src, get_from_data_env(src), assign_env)

  invisible(NULL)
}

#' @inheritParams read_src_cfg
#' @rdname attach_src
#' @export
attach_src.character <- function(x, name = "data-sources", file = NULL, ...) {

  for (cfg in read_src_cfg(x, name, file)) {
    attach_src(cfg, ...)
  }

  invisible(NULL)
}

#' @param env Environment where data proxy objects are created
#'
#' @rdname attach_src
#'
#' @export
#'
setup_src_env <- function(x, env, dir = NULL) {
  UseMethod("setup_src_env", x)
}

#' @rdname attach_src
#' @export
setup_src_env.src_cfg <- function(x, env, dir = src_data_dir(x)) {

  assert_that(is.string(dir))

  tbl <- as_tbl_spec(x)

  files <- Map(file.path, dir, fst_names(tbl))
  names(files) <- names(tbl)

  missing <- lgl_ply(files, all_fun, Negate(file.exists))

  if (any(missing)) {

    todo <- names(files[missing])

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

    download_src(x, dir, tables = todo)
    import_src(x, dir, cleanup = TRUE)

    done <- lgl_ply(files, all_fun, file.exists)

    assert_that(all(done), msg = paste0("Not all required tables were ",
      "successfully downloaded and imported: ", concat(names(files[!done])),
      " are still missing")
    )
  }

  more_arg <- list(id_cfg = as_id_cfg(x), src_name = src_name(x),
                   src_env = env)
  dat_tbls <- Map(new_data_src, files, as_col_cfg(x),
                  MoreArgs = more_arg)

  list2env(dat_tbls, envir = env)
}

#' @rdname attach_src
#' @export
data <- new.env()

#' @name mimic
#' @rdname attach_src
#' @export mimic
#'
NULL

#' @name mimic_demo
#' @rdname attach_src
#' @export mimic_demo
#'
NULL

#' @name eicu
#' @rdname attach_src
#' @export eicu
#'
NULL

#' @name eicu_demo
#' @rdname attach_src
#' @export eicu_demo
#'
NULL

#' @name hirid
#' @rdname attach_src
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

src_data_dir <- function(source) {

  if (!is.string(source)) {
    source <- src_name(source)
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

new_data_src <- function(files, col_cfg, id_cfg, src_name, src_env) {

  assert_that(is.string(src_name), is.environment(src_env))

  res <- prt::new_prt(files)
  class(res) <- c(paste0(c(src_name, "data"), "_src"), class(res))

  attr(res, "col_cfg") <- as_col_cfg(col_cfg)
  attr(res, "id_cfg")  <- as_id_cfg(id_cfg)
  attr(res, "src_env") <- src_env

  res
}

is_data_src <- function(x) inherits(x, "data_src")

#' @export
src_name.data_src <- function(x) {
  sub("_src$", "", class(x)[1L])
}

#' @param tbl String valued table name
#' @param src Object passed to `get_src_env()` or an environment
#'
#' @rdname attach_src
#' @export
get_data_src <- function(tbl, src) {

  assert_that(is.string(tbl))

  if (is.environment(src)) {
    env <- src
  } else {
    env <- get_src_env(src)
  }

  res <- get0(tbl, envir = env, ifnotfound = NULL)

  if (is.null(res)) {

    if (is.environment(src)) {
      src <- sub("environment:", "", format(src))
    } else {
      src <- paste0("<", src, ">")
    }

    stop("Table `", tbl, "` not found in environment ", src, ". Available ",
      "are:\n    * ", paste0("`", ls(envir = env), "`", collapse = "\n    * "),
      "\n  For further information on how to set up a data source, refer to ",
      "`?attach_src`."
    )
  }

  res
}

#' @rdname attach_src
#' @export
get_src_env <- function(x) UseMethod("get_src_env", x)

#' @rdname attach_src
#' @export
get_src_env.character <- function(x) {

  assert_that(is.string(x))

  env <- data_env()

  res <- get0(x, envir = env, mode = "environment", ifnotfound = NULL)

  if (is.null(res)) {
    stop("Source `", x, "` not found in ", env, " environment. For ",
         "further information on how to set up a data source, refer to ",
         "`?attach_src`.")
  }

  res
}

#' @rdname attach_src
#' @export
get_src_env.default <- function(x) get_src_env(src_name(x))

#' @rdname attach_src
#' @export
get_src_env.data_src <- function(x) attr(x, "src_env")
