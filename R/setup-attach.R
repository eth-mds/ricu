
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
#' well as sub-calsses as specified by the data source `class_prefix`
#' configuration setting (see [load_src_cfg()]). All `src_env` objects created
#' by calling `attach_src()` represent environments that are direct
#' descendants of the `data` environment and are bound to the respective
#' dataset name within that environment. While `attach_src()` does not
#' immediately instantiate a `src_env` object, it rather creates a promise
#' using [base::delayedAssign()] which evaluates to a `src_env` upon first
#' access. This allows for data sources to be set up where the data is missing
#' in a way that prompts the user to download and import the data when first
#' accessed.
#'
#' Additionally, `attach_src()` creates an active binding using
#' [base::makeActiveBinding()], binding a function to the dataset name within
#' the environment passed as `assign_env`, which retrieves the respective
#' `src_env` from the `data` environment. This shortcut is set up for
#' convenience, such that for example the MIMIC-III demo dataset not only is
#' available as `ricu::data::mimic_demo`, but also as `ricu::mimic_demo` (or if
#' the package namespace is attached, simply as `mimic_demo`). The `ricu`
#' namespace contains objects `mimic`, `mimic_demo`, `eicu`, etc. which are
#' used as such links when loading the package. However, new data sets can be
#' set up an accessed in the same way.
#'
#' If set up correctly, it is not necessary for the user to directly call
#' `attach_src()`. When the package is loaded, the default data sources are
#' attached automatically. This default can be controlled by setting as
#' environment variable `RICU_SRC_LOAD` a comma separated list of data source
#' names before loading the library. Setting this environment variable as
#'
#' ```
#' Sys.setenv(RICU_SRC_LOAD = "mimic_demo,eciu_demo")
#' ```
#'
#' will change the default of loading both MIMIC-III and eICU, alongside the
#' respective demo datasets, and HiRID, to just the two demo datasets. For
#' setting an enviroment variable upon startup of the R session, refer to
#' [base::.First.sys()].
#'
#' The `src_env` promise for each data source is created using the S3 generic
#' function `setup_src_env()`. This function checks if all required files are
#' available from `data_dir`. If files are missing the user is prompted for
#' download in interactive sessions and an error is thrown otherwise. As soon
#' as all required data is available, a `src_tbl` object is created per table
#' and assigned to the `src_env`.
#'
#' The S3 class `src_tbl` inherits from [`prt`][prt::new_prt()], which
#' represents a partitioned [`fst`][fst::fst()] file. In addition to the `prt`
#' object, meta data in the form of `col_cfg` and `tbl_cfg` is associated with
#' a `src_tbl` object (see [load_src_cfg()]). Furthermore, as with `src_env`,
#' sub-classes are added as specified by the source configuration
#' `class_prefix` entry. This allows certain functionality, for example data
#' loading, to be adapted to data source-specific requirements.
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
#' attach_src("mimic_demo")
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
#' @export
#'
attach_src <- function(x, ...) UseMethod("attach_src", x)

#' @param data_dir Directory used to look for [fst::fst()] files; `NULL` calls
#' [data_dir()] using the source name as `subdir` argument
#' @param assign_env Environment in which the data source will become available
#'
#' @rdname attach_src
#'
#' @export
#'
attach_src.src_cfg <- function(x, assign_env = .GlobalEnv,
                               data_dir = src_data_dir(x), ...) {

  warn_dots(...)

  src <- src_name(x)

  tryCatch({

    assert_that(is.string(data_dir), is.environment(assign_env))

    dat_env <- data_env()
    src_env <- new_src_env(x, env = new.env(parent = dat_env))

    delayedAssign(src, setup_src_env(x, src_env, data_dir),
                  assign.env = dat_env)
    makeActiveBinding(src, get_from_data_env(src), assign_env)

  }, error = function(err) {

    warn_ricu({
      cli_text("Failed to attach source `{src}` with error:")
      cli_div(theme = list(body = list("margin-left" = 2)))
      cli_verbatim(conditionMessage(err))
    }, class = "src_attach_error")

    assign(src, NULL, envir = assign_env)
  })

  invisible(NULL)
}

#' @rdname attach_src
#' @export
attach_src.character <- function(x, assign_env = .GlobalEnv,
                                 data_dir = src_data_dir(x), ...) {

  read_err <- function(err) {

    warn_ricu({
      cli_text("Failed to read source configuration with error:")
      cli_div(theme = list(body = list("margin-left" = 2)))
      cli_verbatim(conditionMessage(err))
    }, class = "src_cfg_read_error")

    NULL
  }

  try_parse <- function(src, cfg, dir, env) {

    parse_err <- function(err) {

      warn_ricu({
        cli_text("Failed to parse source configuration for source `{src}`
                  with error:")
        cli_div(theme = list(body = list("margin-left" = 2)))
        cli_verbatim(conditionMessage(err))
      }, class = "src_cfg_parse_error")


      assign(src, NULL, envir = assign_env)

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

#' @param env Environment where data proxy objects are created
#'
#' @rdname attach_src
#'
#' @export
#'
setup_src_env <- function(x, env, ...) {
  UseMethod("setup_src_env", x)
}

#' @rdname attach_src
#' @export
setup_src_env.src_cfg <- function(x, env, data_dir = src_data_dir(x), ...) {

  assert_that(is_src_env(env), is.string(data_dir))

  tbl <- as_tbl_cfg(x)

  fst_files <- lapply(tbl, fst_file_names)
  fst_paths <- Map(file.path, data_dir, fst_files)

  ensure_dirs(
    unique(dirname(unlist(fst_paths, recursive = FALSE)))
  )

  tables  <- chr_ply(tbl, tbl_name)
  missing <- lgl_ply(fst_paths, all_fun, Negate(file.exists))

  if (any(missing)) {

    todo <- tables[missing]

    if (interactive()) {

      msg_ricu({
        cli_text("The following {qty(length(todo))} table{?s} {?is/are}
                  missing from directory {data_dir}:")
        cli_ul(quote_bt(todo))
      }, "miss_tbl_msg", tbl_ok = setNames(!missing, tables))

      resp <- readline("Download now (Y/n)? ")

      if (!identical(resp, "Y")) {
        stop_ricu("Cannot continue with missing tables for `{src_name(x)}`",
                  class = "tbl_dl_abort")
      }

    } else {

      stop_ricu({
        cli_text("The following {qty(length(todo))} table{?s} {?is/are}
                  missing from directory {data_dir}:")
        cli_ul(quote_bt(todo))
      }, class = "miss_tbl_err")
    }

    tmp <- ensure_dirs(tempfile())
    on.exit(unlink(tmp, recursive = TRUE))

    download_src(x, tmp, tables = todo)
    import_src(x, tmp)

    done <- Map(file.rename, Map(file.path, tmp, fst_files[missing]),
                fst_paths[missing])
    done <- lgl_ply(done, all)

    if (!all(done)) {
      stop_ricu({
        cli_text("The following {qty(sum(!done))} table{?s} could be moved to
                  directory {data_dir}:")
        cli_ul(quote_bt(todo[!done]))
      }, class = "tbl_mv_err")
    }

    done <- lgl_ply(fst_paths, all_fun, file.exists)

    if (!all(done)) {
      stop_ricu({
        cli_text("The following {qty(sum(!done))} table{?s} were not
                  successfully downloaded and imported:")
        cli_ul(quote_bt(todo[!done]))
      }, class = "tbl_mv_err")
    }
  }

  dat_tbls <- Map(new_src_tbl, fst_paths, as_col_cfg(x), as_tbl_cfg(x),
                  MoreArgs = list(prefix = x[["prefix"]], src_env = env))
  names(dat_tbls) <- tables

  list2env(dat_tbls, envir = env)
}

#' @export
setup_src_env.default <- function(x, ...) stop_generic(x, .Generic)
