
#' Data attach utilities
#'
#' Making a dataset available to `ricu` consists of 3 steps: downloading
#' ([download_src()]), importing ([import_src()]) and attaching
#' ([attach_src()]). While downloading and importing are one-time procedures,
#' attaching of the dataset is repeated every time the package is loaded.
#' Briefly, downloading loads the raw dataset from the internet (most likely
#' in `.csv` format), importing consists of some preprocessing to make the
#' data available more efficiently and attaching sets up the data for use by
#' the package.
#'
#' @details
#' Attaching a dataset sets up two types of S3 classes: a single `src_env`
#' object, containing as many `src_tbl` objects as tables are associated with
#' the dataset. A `src_env` is an environment with an `id_cfg` attribute, as
#' well as sub-classes as specified by the data source `class_prefix`
#' configuration setting (see [load_src_cfg()]). All `src_env` objects created
#' by calling `attach_src()` represent environments that are direct
#' descendants of the `data` environment and are bound to the respective
#' dataset name within that environment. For more information on `src_env` and
#' `src_tbl` objects, refer to [new_src_tbl()].
#'
#' If set up correctly, it is not necessary for the user to directly call
#' `attach_src()`. When the package is loaded, the default data sources (see
#' [auto_attach_srcs()]) are attached automatically. This default can be
#' controlled by setting as environment variable `RICU_SRC_LOAD` a comma
#' separated list of data source names before loading the library. Setting
#' this environment variable as
#'
#' ```
#' Sys.setenv(RICU_SRC_LOAD = "mimic_demo,eciu_demo")
#' ```
#'
#' will change the default of loading both MIMIC-III and eICU, alongside the
#' respective demo datasets, as well as HiRID and AUMC, to just the two demo
#' datasets. For setting an environment variable upon startup of the R
#' session, refer to [base::.First.sys()].
#'
#' Attaching a dataset during package namespace loading will both instantiate
#' a corresponding `src_env` in the `data` environment and for convenience
#' also assign this object into the package namespace, such that for example
#' the MIMIC-III demo dataset not only is available as
#' `ricu::data::mimic_demo`, but also as `ricu::mimic_demo` (or if the package
#' namespace is attached, simply as `mimic_demo`). Dataset attaching using
#' `attach_src()` does not need to happen during namespace loading, but can be
#' triggered by the user at any time. If such a convenience link as described
#' above is desired by the user, an environment such as `.GlobalEnv` has to be
#' passed as `assign_env` to `attach_src()`.
#'
#' Data sets are set up as `src_env` objects irrespectively of whether all (or
#' any) of the required data is available. If some (or all) data is missing,
#' the user is asked for permission to download in interactive sessions and an
#' error is thrown in non-interactive sessions. Downloading demo datasets
#' requires no further information but access to full-scale datasets (even
#' though they are publicly available) is guarded by access credentials (see
#' [download_src()]).
#'
#' While `attach_src()` provides the main entry point, `src_env` objects are
#' instantiated by the S3 generic function `setup_src_env()` and the wrapping
#' function serves to catch errors that might be caused by config file parsing
#' issues as to not break attaching of the package namespace. Apart form this,
#' `attach_src()` also provides the convenience linking into the package
#' namespace (or a user-specified environment) described above.
#'
#' A `src_env` object created by `setup_src_env()` does not directly contain
#' `src_tbl` objects bound to names, but rather an active binding (see
#' [base::makeActiveBinding()]) per table. These active bindings check for
#' availability of required files and evaluate to corresponding `src_tbl`
#' objects if these checks are passed and ask for user input otherwise. As
#' `src_tbl` objects are intended to be read-only, assignment is not possible
#' except for the value `NULL` which resets the internally cached `src_tbl`
#' that is created on first successful access.
#'
#' @examples
#' \dontrun{
#'
#' Sys.setenv(RICU_SRC_LOAD = "")
#' library(ricu)
#'
#' ls(envir = data)
#' exists("mimic_demo")
#'
#' attach_src("mimic_demo", assign_env = .GlobalEnv)
#'
#' ls(envir = data)
#' exists("mimic_demo")
#'
#' mimic_demo
#'
#' }
#'
#' @param x Data source to attach
#' @param ... Forwarded to further calls to `attach_src()`
#'
#' @return Both `attach_src()` and `setup_src_env()` are called for side
#' effects and therefore return invisibly. While `attach_src()` returns `NULL`,
#' `setup_src_env()` returns the newly created `src_env` object.
#'
#' @export
#'
attach_src <- function(x, ...) UseMethod("attach_src", x)

#' @param assign_env,link_env Environment in which the data source will become
#' available
#' @param data_dir Directory used to look for [fst::fst()] files; `NULL` calls
#' [data_dir()] using the source name as `subdir` argument
#'
#' @rdname attach_src
#'
#' @export
#'
attach_src.src_cfg <- function(x, assign_env = NULL,
                               data_dir = src_data_dir(x), ...) {

  warn_dots(...)

  src <- src_name(x)

  tryCatch({

    assert_that(is.string(data_dir))

    setup_src_env(x, data_dir, assign_env)

  }, error = function(err) {

    msg <- conditionMessage(err)

    warn_ricu(
      c("Failed to attach source `{src}` with error:", msg),
      class = "src_attach_error", indent = c(0L, rep_along(2L, msg)),
      exdent = c(0L, rep_along(2L, msg))
    )

    if (is.environment(assign_env) && identical(assign_env, pkg_env())) {
      assign(src, NULL, envir = assign_env)
    }
  })

  invisible(NULL)
}

#' @rdname attach_src
#' @export
attach_src.character <- function(x, assign_env = NULL,
                                 data_dir = src_data_dir(x), ...) {

  read_err <- function(err) {

    msg <- conditionMessage(err)

    warn_ricu(
      c("Failed to read source configuration with error:", msg),
      class = "src_cfg_read_error", indent = c(0L, rep_along(2L, msg)),
      exdent = c(0L, rep_along(2L, msg))
    )

    NULL
  }

  try_parse <- function(src, cfg, dir, env) {

    parse_err <- function(err) {

      msg <- conditionMessage(err)

      warn_ricu(
        c("Failed to parse source configuration for source `{src}` with
           error:", msg),
        class = "src_cfg_parse_error", indent = c(0L, rep_along(2L, msg)),
        exdent = c(0L, rep_along(2L, msg))
      )

      if (is.environment(env) && identical(env, pkg_env())) {
        assign(src, NULL, envir = env)
      }

      NULL
    }

    cfg <- tryCatch(parse_src_cfg(cfg), error = parse_err)

    if (not_null(cfg)) {
      attach_src(cfg, env, dir)
    }

    invisible(NULL)
  }

  cfgs <- tryCatch(read_src_cfg(x, ...), error = read_err)

  if (has_name(cfgs, x)) {

    Map(try_parse, x, cfgs[x], data_dir, MoreArgs = list(env = assign_env))

  } else {

    warn_ricu("Failed to read source configuration for source{?s}
               {setdiff(x, names(cfg))}", class = "src_cfg_read_error")
  }

  invisible(NULL)
}

#' @export
attach_src.default <- function(x, ...) stop_generic(x, .Generic)

#' @rdname attach_src
#' @export
detach_src <- function(x) {

  if (is.character(x)) {
    envs <- lapply(x, as_src_env)
  } else {
    envs <- list(as_src_env(x))
  }

  for (env in envs) {

    lnk <- attr(env, "link")
    src <- src_name(env)

    if (is.environment(lnk) && identical(lnk, pkg_env())) {
      assign(src, NULL, envir = env)
    } else if (is.environment(lnk)) {
      rm(list = src, envir = lnk)
    }

    rm(list = src, envir = data_env())
  }

  invisible(NULL)
}

#' @rdname attach_src
#' @export
setup_src_env <- function(x, ...) {
  UseMethod("setup_src_env", x)
}

#' @rdname attach_src
#' @export
setup_src_env.src_cfg <- function(x, data_dir = src_data_dir(x),
                                  link_env = NULL, ...) {

  tbl_setup <- function(tbl, col, fil, src, env, dir) {

    fil <- file.path(dir, fil)

    src_tbl_cache <- NULL

    function(value) {

      if (!missing(value)) {

        if (is.null(value)) {
          src_tbl_cache <<- NULL
        } else {
          warn_ricu("Cannot update read-only table `{tbl_name(tbl)}` of data
                     source `{src_name(src)}`.", "assign_src_tbl")
        }
      }

      if (all(file.exists(fil))) {

        if (is.null(src_tbl_cache)) {
          src_tbl_cache <<- new_src_tbl(fil, col, tbl, src_prefix(src), env)
        }

        return(src_tbl_cache)
      }

      msg_ricu("Data for `{src_name(src)}` is missing", "miss_tbl_msg")

      if (is_interactive()) {

        resp <- read_line("Setup now (Y/n)? ")

        if (identical(resp, "Y")) {
          setup_src_data(src, data_dir = dir)
        }
      }

      if (all(file.exists(fil))) {

        src_tbl_cache <<- new_src_tbl(fil, col, tbl, src_prefix(src), env)

        return(src_tbl_cache)
      }

      stop_ricu("Cannot continue with missing data for `{src_name(src)}`. Data
                 can be downloaded and set up using `setup_src_data()`.",
                "miss_tbl_err")
    }
  }

  assert_that(is.string(data_dir))

  tbls <- as_tbl_cfg(x)
  cols <- as_col_cfg(x)

  src_env <- new_src_env(x, env = new.env(parent = data_env()),
                         link = link_env)

  tbl_fun <- Map(
    tbl_setup, vec_chop(tbls), vec_chop(cols), fst_file_names(tbls),
    MoreArgs = list(src = x, env = src_env, dir = data_dir)
  )

  Map(makeActiveBinding, names(tbls), tbl_fun, MoreArgs = list(env = src_env))

  invisible(src_env)
}

#' @export
setup_src_env.default <- function(x, ...) stop_generic(x, .Generic)

#' Data setup
#'
#' Making a dataset available to `ricu` consists of 3 steps: downloading
#' ([download_src()]), importing ([import_src()]) and attaching
#' ([attach_src()]). While downloading and importing are one-time procedures,
#' attaching of the dataset is repeated every time the package is loaded.
#' Briefly, downloading loads the raw dataset from the internet (most likely
#' in `.csv` format), importing consists of some preprocessing to make the
#' data available more efficiently and attaching sets up the data for use by
#' the package. The download and import steps can be combined using
#' `setup_src_data()`.
#'
#' @details
#' If `setup_src_data()` is called on data sources that have all data available
#' with `force = FALSE`, nothing happens apart of a message being displayed. If
#' only a subset of tables is missing, only these tables are downloaded
#' (whenever possible) and imported. Passing `force = TRUE` attempts to re-
#' download and import the entire data set. If the data source is available
#' as a data package (as is the case for the two demo datasets), data is not
#' downloaded and imported, but this package is installed.
#'
#' In most scenarios, `setup_src_data()` does not need to be called by users,
#' as upon package loading, all configured data sources are set up in a way
#' that enables download of missing data upon first access (and barring user
#' consent). However, instead of accessing all data sources where data
#' missingness should be resolved one by one, `setup_src_data()` is exported
#' for convenience.
#'
#' @inheritParams import_src
#' @param ... Forwarded to [load_src_cfg()] if `x` is a character vector
#'
#' @return Called for side effects and returns `NULL` invisibly.
#'
#' @export
setup_src_data <- function(x, ...) UseMethod("setup_src_data", x)

#' @export
setup_src_data.src_cfg <- function(x, data_dir = src_data_dir(x),
                                   force = FALSE, ...) {

  warn_dots(...)

  tbl <- as_tbl_cfg(x)

  fst_files <- fst_file_names(tbl)
  fst_paths <- Map(file.path, data_dir, fst_files)

  tables  <- tbl_name(tbl)

  if (force) {
    missing <- rep(TRUE, length(tables))
  } else {
    missing <- lgl_ply(fst_paths, all_fun, Negate(file.exists))
  }

  if (!any(missing)) {
    msg_ricu("all required tables for `{src_name(x)}` are available from
             {data_dir}")

    return(invisible(NULL))
  }

  todo <- tables[missing]

  if (data_pkg_avail(x)) {

    src_dir <- src_data_dir(x)

    if (!identical(data_dir, src_dir)) {
      warn_ricu("data will be saved to {src_dir} instead of {data_dir}",
                class = "pkg_src_dir")
    }

    install_data_pkgs(x)

  } else {

    ensure_dirs(
      unique(dirname(unlist(fst_paths, recursive = FALSE)))
    )

    download_src(x, data_dir, tables = todo, force = force)
    import_src(x, data_dir, tables = todo, force = force, cleanup = TRUE)
  }

  done <- lgl_ply(fst_paths, all_fun, file.exists)

  if (!all(done)) {
    stop_ricu(
      c("The following {qty(sum(!done))} table{?s} were not successfully
         downloaded and imported:", bullet(quote_bt(todo[!done]))),
      class = "tbl_dl_err", exdent = c(0L, rep(2L, sum(!done)))
    )
  }

  invisible(NULL)
}

#' @export
setup_src_data.character <- function(x, data_dir = src_data_dir(x),
                                     force = FALSE, ...) {

  Map(setup_src_data, load_src_cfg(x, ...), data_dir,
      MoreArgs = list(force = force))

  invisible(NULL)
}

#' @export
setup_src_env.default <- function(x, ...) stop_generic(x, .Generic)
