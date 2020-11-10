
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
#' setting an environment variable upon startup of the R session, refer to
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
#' @return The constructors `new_src_env()`/`new_src_tbl()` as well as coercion
#' functions `as_src_env()`/`as_src_tbl()` return `src_env` and `src_tbl`
#' objects respectively. The function `attach_src()` is called for side
#' effects and returns `NULL` invisibly, while `setup_src_env()` instantiates
#' and returns a `src_env` object.
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
attach_src.src_cfg <- function(x, assign_env = NULL,
                               data_dir = src_data_dir(x), ...) {

  warn_dots(...)

  src <- src_name(x)

  tryCatch({

    assert_that(is.string(data_dir), null_or(assign_env, is.environment))

    dat_env <- data_env()
    src_env <- new_src_env(x, env = new.env(parent = dat_env))

    delayedAssign(src, setup_src_env(x, src_env, data_dir),
                  assign.env = dat_env)

    if (is.environment(assign_env)) {
      makeActiveBinding(src, get_from_data_env(src), assign_env)
    }

  }, error = function(err) {

    msg <- conditionMessage(err)

    warn_ricu(
      c("Failed to attach source `{src}` with error:", msg),
      class = "src_attach_error", indent = c(0L, rep_along(2L, msg)),
      exdent = c(0L, rep_along(2L, msg))
    )

    if (is.environment(assign_env)) {
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

      if (is.environment(env)) {
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

  tables  <- chr_ply(tbl, tbl_name)
  missing <- lgl_ply(fst_paths, all_fun, Negate(file.exists))

  if (any(missing)) {

    todo <- tables[missing]

    if (is_interactive()) {

      msg_ricu(
        c("The following {qty(length(todo))} table{?s} {?is/are} missing from
           directory {data_dir}:", bullet(quote_bt(todo))), "miss_tbl_msg",
        exdent = c(0L, rep_along(2L, todo)),
        tbl_ok = setNames(!missing, tables)
      )

      resp <- readline("Download now (Y/n)? ")

      if (!identical(resp, "Y")) {
        stop_ricu("Cannot continue with missing tables for `{src_name(x)}`",
                  "tbl_dl_abort")
      }

    } else {

      stop_ricu(
        c("The following {qty(length(todo))} table{?s} {?is/are}
           missing from directory {data_dir}:", bullet(quote_bt(todo))),
        class = "miss_tbl_err", exdent = c(0L, rep_along(2L, todo))
      )
    }

    setup_src_data(x, data_dir = data_dir)
  }

  dat_tbls <- Map(new_src_tbl, fst_paths, as_col_cfg(x), as_tbl_cfg(x),
                  MoreArgs = list(prefix = x[["prefix"]], src_env = env))
  names(dat_tbls) <- tables

  list2env(dat_tbls, envir = env)
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
#' with `force = FALSE`, nothing happens apart of a message being raised. If
#' only a subset of tables is missing, only these tables are downloaded
#' (whenever possible) and imported. Finally, if the data source is available
#' as a data package.
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
setup_src_data.src_env <- function(x, data_dir = src_data_dir(x),
                                   force = FALSE, ...) {

  warn_dots(...)

  tbl <- as_tbl_cfg(x)

  fst_files <- lapply(tbl, fst_file_names)
  fst_paths <- Map(file.path, data_dir, fst_files)

  tables  <- chr_ply(tbl, tbl_name)

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

    tmp <- ensure_dirs(tempfile())
    on.exit(unlink(tmp, recursive = TRUE))

    download_src(x, tmp, tables = todo, force = force)
    import_src(x, tmp, force = force)

    done <- Map(file.rename, Map(file.path, tmp, fst_files[missing]),
                fst_paths[missing])
    done <- lgl_ply(done, all)

    if (!all(done)) {
      stop_ricu(
        c("The following {qty(sum(!done))} table{?s} could be moved to
           directory {data_dir}:", bullet(quote_bt(todo[!done]))),
        class = "tbl_mv_err", exdent = c(0L, rep(2L, sum(!done)))
      )
    }
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

#' @param files File names of `fst` files that will be used to create a `prt`
#' object (see also [prt::new_prt()])
#' @param col_cfg Coerced to `col_cfg` by calling [as_col_cfg()]
#' @param tbl_cfg Coerced to `tbl_cfg` by calling [as_tbl_cfg()]
#' @param prefix Character vector valued data source name(s) (used as class
#' prefix)
#' @param src_env The data source environment (as `src_env` object)
#'
#' @rdname attach_src
#' @export
new_src_tbl <- function(files, col_cfg, tbl_cfg, prefix, src_env) {

  assert_that(is_src_env(src_env))

  res <- prt::new_prt(files)
  class(res) <- c(paste0(c(prefix, "src"), "_tbl"), class(res))

  attr(res, "col_cfg") <- as_col_cfg(col_cfg)
  attr(res, "tbl_cfg") <- as_tbl_cfg(tbl_cfg)
  attr(res, "src_env") <- src_env

  res
}

is_src_tbl <- is_type("src_tbl")

#' @export
id_vars.src_tbl <- function(x) {
  coalesce(id_vars(as_col_cfg(x)), id_vars(as_id_cfg(x)))
}

#' @export
index_var.src_tbl <- function(x) index_var(as_col_cfg(x))

#' @export
time_vars.src_tbl <- function(x) time_vars(as_col_cfg(x))

#' @importFrom tibble tbl_sum
#' @export
tbl_sum.src_tbl <- function(x) {

  out <- setNames(dim_brak(x), paste0("<", class(x)[2L], ">"))

  if (not_null(prt <- part_desc(x))) {
    out <- c(out, Partitions = prt)
  }

  ids <- id_var_opts(x)

  if (length(ids) > 1L && any(ids %in% colnames(x))) {
    out <- c(out, "ID options" = id_desc(x))
  }

  c(out, "Defaults" = def_desc(x), "Time vars" = tim_desc(x))
}

tim_desc <- function(x) {

  x <- time_vars(x)

  if (has_length(x)) {
    concat(x)
  } else {
    NULL
  }
}

def_desc <- function(x) {

  x <- as_col_cfg(x)

  res <- c(id = id_vars(x), index = index_var(x), value = val_var(x),
           unit = unit_var(x))

  if (has_length(res)) {
    paste0(res, " (", names(res), ")", collapse = ", ")
  } else {
    NULL
  }
}

id_desc <- function(x) {
  id <- sort(as_id_cfg(x))
  paste0(id_var_opts(id), " (", names(id), ")", id_cfg_op(id), collapse = " ")
}

part_desc <- function(x) {

  part_nrow <- prt::part_nrow(x)

  if (length(part_nrow) > 1L) {
    paste0("[", concat(chr_ply(part_nrow, big_mark)), "] rows")
  } else {
    NULL
  }
}

#' @export
src_name.src_tbl <- function(x) src_name(as_col_cfg(x))

#' @export
tbl_name.src_tbl <- function(x) tbl_name(as_col_cfg(x))

#' @rdname attach_src
#' @export
as_src_tbl <- function(x, ...) UseMethod("as_src_tbl", x)

#' @export
as_src_tbl.src_tbl <- function(x, ...) warn_dot_ident(x, ...)

#' @export
as_src_tbl.src_env <- function(x, tbl, ...) {

  warn_dots(...)

  assert_that(is.string(tbl))

  res <- get0(tbl, envir = x, inherits = FALSE, ifnotfound = NULL)

  if (is.null(res)) {

    opts <- ls(envir = x)

    stop_ricu(
      c("Table `{tbl}` not found for `{src_name(x)}`. Available are:",
        bullet(quote_bt(opts))),
      class = "src_tbl_not_found", exdent = c(0L, rep_along(2L, opts))
    )
  }

  res
}

#' @export
as_src_tbl.character <- function(x, env, ...) {
  as_src_tbl(as_src_env(env), x, ...)
}

#' @export
as_src_tbl.default <- function(x, ...) stop_generic(x, .Generic)

#' @rdname attach_src
#' @export
new_src_env <- function(x, env = new.env(parent = data_env())) {

  assert_that(is_src_cfg(x), is.environment(env))

  structure(env, class = paste0(c(x[["prefix"]], "src"), "_env"),
            src_name = src_name(x), id_cfg = as_id_cfg(x))
}

is_src_env <- is_type("src_env")

#' @export
print.src_env <- function(x, ...) {
  cat_line("<", class(x)[1L], "[", length(x), "]>")
  print(setNames(format(x), names(x)), quote = FALSE)
  invisible(x)
}

#' @export
format.src_env <- function(x, ...) {
  chr_ply(eapply(x, dim_brak), identity)
}

dim_desc <- function(x) {
  paste0(vapply(dim(x), big_mark, character(1L)),
         collapse = paste0(" ", symbol$cross, " "))
}

dim_brak <- function(x) paste0("[", dim_desc(x), "]")

#' @importFrom utils ls.str
#' @export
str.src_env <- function(object, ...) ls.str(object)

#' @export
src_name.src_env <- function(x) attr(x, "src_name")

#' @rdname attach_src
#' @export
as_src_env <- function(x) UseMethod("as_src_env", x)

#' @export
as_src_env.src_env <- function(x) x

#' @export
as_src_env.character <- function(x) {

  assert_that(is.string(x))

  env <- data_env()

  res <- get0(x, envir = env, mode = "environment", ifnotfound = NULL)

  if (is.null(res)) {
    stop_ricu("Source `{x}` not found in {format(env)}",
              class = "src_env_not_found")
  }

  res
}

#' @export
as_src_env.default <- function(x) as_src_env(src_name(x))

#' @export
as_src_env.src_tbl <- function(x) attr(x, "src_env")
