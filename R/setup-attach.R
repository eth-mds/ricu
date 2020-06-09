
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

  warn_dots(...)

  assert_that(is.string(dir), is.environment(assign_env))

  src <- src_name(x)

  dat_env <- data_env()
  src_env <- new_src_env(src, x, env = new.env(parent = dat_env))

  delayedAssign(src, setup_src_env(x, src_env, dir), assign.env = dat_env)
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

  assert_that(is_src_env(env), is.string(dir))

  tbl <- as_tbl_cfg(x)

  fst_files <- lapply(tbl, fst_names)
  fst_paths <- Map(file.path, dir, fst_files)

  ensure_dirs(
    unique(dirname(unlist(fst_paths, recursive = FALSE)))
  )

  tables  <- chr_ply(tbl, tbl_name)
  missing <- lgl_ply(fst_paths, all_fun, Negate(file.exists))

  if (any(missing)) {

    todo <- tables[missing]

    msg <- paste("\nThe following tables are missing from", dir,
                 paste(todo, collapse = "\n  "), sep = "\n  ")

    if (interactive()) {

      message(msg)
      resp <- readline("Download now (Y/n)? ")

      if (!identical(resp, "Y")) {
        stop("Cannot continue without missing tables for `", src_name(x), "`.")
      }

    } else {

      stop(msg)
    }

    tmp <- ensure_dirs(tempfile())
    on.exit(unlink(tmp, recursive = TRUE))

    download_src(x, tmp, tables = todo)
    import_src(x, tmp)

    done <- Map(file.rename, Map(file.path, tmp, fst_files[missing]),
                fst_paths[missing])

    assert_that(all_fun(done, all), msg = paste0(
      "The following tables could be moved to the required location: ",
      concat(todo[!done]))
    )

    done <- lgl_ply(fst_paths, all_fun, file.exists)

    assert_that(all(done), msg = paste0(
      "Not all required tables were successfully downloaded and imported: ",
      concat(tables[!done])," are still missing")
    )
  }

  dat_tbls <- Map(new_src_tbl, fst_paths, as_col_cfg(x),
                  MoreArgs = list(src_env = env))
  names(dat_tbls) <- tables

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

  err_fun <- function(e) {

    msg <- conditionMessage(e)

    if (any(!grepl("^\nThe following tables are missing from\n", msg))) {
      message(msg)
    }

    invisible(NULL)
  }

  source <- force(source)

  function(value) {

    assert_that(missing(value),
      msg = paste0("Cannot update read-only data source `", source, "`")
    )

    tryCatch(as_src_env(source), error = err_fun)
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

new_src_tbl <- function(files, col_cfg, src_env) {

  assert_that(is_src_env(src_env))

  name <- src_name(src_env)

  res <- prt::new_prt(files)
  class(res) <- c(paste0(c(name, "src"), "_tbl"), class(res))

  attr(res, "col_cfg") <- as_col_cfg(col_cfg)
  attr(res, "src_env") <- src_env

  res
}

is_src_tbl <- function(x) inherits(x, "src_tbl")

#' @export
src_name.src_tbl <- function(x) {
  sub("_tbl$", "", class(x)[1L])
}

#' @rdname attach_src
#' @export
as_src_tbl <- function(x, ...) UseMethod("as_src_tbl", x)

#' @rdname attach_src
#' @export
as_src_tbl.src_tbl <- function(x, ...) {
  warn_dots(...)
  x
}

#' @param tbl String valued table name
#'
#' @rdname attach_src
#' @export
as_src_tbl.src_env <- function(x, tbl, ...) {

  warn_dots(...)

  assert_that(is.string(tbl))

  res <- get0(tbl, envir = x, ifnotfound = NULL)

  if (is.null(res)) {
    stop("Table `", tbl, "` not found for `", src_name(x), "`. Available ",
      "are:\n    * ", paste0("`", ls(envir = x), "`", collapse = "\n    * "),
      "\n  For further information on how to set up a data source, refer to ",
      "`?attach_src`."
    )
  }

  res
}

#' @param env Object passed to `as_src_env()` or an environment
#'
#' @rdname attach_src
#' @export
as_src_tbl.character <- function(x, env, ...) {
  as_src_tbl(as_src_env(env), x, ...)
}

new_src_env <- function(src_name, id_cfg, env = new.env(parent = data_env())) {

  assert_that(is.string(src_name), is.environment(env))

  structure(env, class = paste0(c(src_name, "src"), "_env"),
            src_name = src_name, id_cfg = as_id_cfg(id_cfg))
}

is_src_env <- function(x) inherits(x, "src_env")

#' @export
src_name.src_env <- function(x) attr(x, "src_name")

#' @rdname attach_src
#' @export
as_src_env <- function(x) UseMethod("as_src_env", x)

#' @rdname attach_src
#' @export
as_src_env.src_env <- function(x) x

#' @rdname attach_src
#' @export
as_src_env.character <- function(x) {

  assert_that(is.string(x))

  env <- data_env()

  res <- get0(x, envir = env, mode = "environment", ifnotfound = NULL)

  if (is.null(res)) {
    stop("Source `", x, "` not found in ", format(env), ". For ",
         "further information on how to set up a data source, refer to ",
         "`?attach_src`.")
  }

  res
}

#' @rdname attach_src
#' @export
as_src_env.default <- function(x) as_src_env(src_name(x))

#' @rdname attach_src
#' @export
as_src_env.src_tbl <- function(x) attr(x, "src_env")
