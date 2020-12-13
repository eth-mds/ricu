
#' File system utilities
#'
#' Determine the location where to place data meant to persist between
#' individual sessions.
#'
#' For data, the default location depends on the operating system as
#'
#' | **Platform** | **Location**                         |
#' | ------------ | -------------------------------------|
#' | Linux        | `~/.local/share/ricu`                |
#' | macOS        | `~/Library/Application Support/ricu` |
#' | Windows      | `%LOCALAPPDATA%/ricu`                |
#'
#' If the default storage directory does not exists, it will only be created
#' upon user consent (requiring an interactive session).
#'
#' The environment variable `RICU_DATA_PATH` can be used to overwrite the
#' default location. If desired, this variable can be set in an R startup file
#' to make it apply to all R sessions. For example, it could be set within:
#'
#' - A project-local `.Renviron`;
#' - The user-level `.Renviron`;
#' - A file at `$(R RHOME)/etc/Renviron.site`.
#'
#' Any directory specified as environment variable will recursively be created.
#'
#' Data source directories typically are sub-directories to `data_dir()` named
#' the same as the respective dataset. For demo datasets corresponding to
#' `mimic` and `eicu`, file location however deviates from this scheme. The
#' function `src_data_dir()` is used to determine the expected data location
#' of a given dataset.
#'
#' Configuration files used both for data source configuration, as well as for
#' dictionary definitions potentially involve multiple files that are read and
#' merged. For that reason, `get_config()` will iterate over directories
#' passed as `cfg_dirs` and look for the specified file (with suffix `.json`
#' appended and might be missing in some of the queried directories). All
#' found files are read by [jsonlite::read_json()] and the resulting lists are
#' combined by reduction with the binary function passed as `combine_fun`.
#'
#' With default arguments, `get_config()` will simply concatenate lists
#' corresponding to files found in the default config locations as returned by
#' `config_paths()`: first the directory specified by the environment variable
#' `RICU_CONFIG_PATH` (if set), followed by the directory at
#'
#' ```
#' system.file("extdata", "config", package = "ricu")
#' ```
#'
#' Further arguments are passed to [jsonlite::read_json()], which is called
#' with slightly modified defaults: `simplifyVector = TRUE`,
#' `simplifyDataFrame = FALSE` and `simplifyMatrix = FALSE`.
#'
#' The utility function `set_config()` writes the list passed as `x` to file
#' `dir/name.json`, using [jsonlite::write_json()] also with slightly modified
#' defaults (which can be overridden by passing arguments as `...`): `null =
#' "null"`, `auto_unbox = TRUE` and `pretty = TRUE`.
#'
#' Whenever the package namespace is attached, a summary of dataset
#' availability is printed using the utility functions `auto_attach_srcs()`
#' and `src_data_avail()`. While the former simply returns a character vector
#' of data sources that are configures for automatically being set up on
#' package loading, the latter returns a summary of the number of available
#' tables per dataset.m Finally, `is_data_avail()` returns a named logical
#' vector indicating which data sources have all required data available.
#'
#' @param subdir A string specifying a directory that will be made sure to
#' exist below the data directory.
#' @param create Logical flag indicating whether to create the specified
#' directory
#'
#' @rdname file_utils
#'
#' @return Functions `data_dir()`, `src_data_dir()` and `config_paths()` return
#' file paths as character vectors, `auto_attach_srcs()` returns a character
#' vector of data source names, `src_data_avail()` returns a `data.frame`
#' describing availability of data sources and `is_data_avail()` a named
#' logical vector. Configuration utilities `get_config()` and `set_config()`
#' read and write list objects to/from JSON format.
#'
#' @examples
#' Sys.setenv(RICU_DATA_PATH = tempdir())
#' identical(data_dir(), tempdir())
#'
#' dir.exists(file.path(tempdir(), "some_subdir"))
#' some_subdir <- data_dir("some_subdir")
#' dir.exists(some_subdir)
#'
#' cfg <- get_config("concept-dict")
#'
#' identical(
#'   cfg,
#'   get_config("concept-dict",
#'              system.file("extdata", "config", package = "ricu"))
#' )
#'
#' @export
#'
data_dir <- function(subdir = NULL, create = TRUE) {

  assert_that(is.flag(create))

  res <- Sys.getenv("RICU_DATA_PATH", unset = NA_character_)

  if (is.na(res)) {

    res <- switch(
      Sys.info()[["sysname"]],
      Darwin  = Sys.getenv("XDG_DATA_HOME", "~/Library/Application Support"),
      Windows = Sys.getenv("LOCALAPPDATA", Sys.getenv("APPDATA")),
      Sys.getenv("XDG_DATA_HOME", "~/.local/share")
    )

    res <- file.path(res, "ricu")

  }

  if (!is.null(subdir)) {
    assert_that(is.string(subdir))
    res <- file.path(res, subdir)
  }

  if (create) {
    res <- ensure_dirs(res)
  }

  res
}

#' @param srcs Character vector of data source names, an object for which an
#' `src_name()` method is defined or an arbitrary-length list thereof.
#'
#' @rdname file_utils
#' @export
src_data_dir <- function(srcs) {

  if (is.object(srcs)) {
    srcs <- list(srcs)
  }

  if (!is.character(srcs)) {
    srcs <- chr_ply(srcs, src_name)
  }

  if (length(srcs) > 1L) {
    return(chr_ply(srcs, src_data_dir))
  }

  assert_that(is.string(srcs))

  pkg <- sub("_", ".", srcs)

  if (is_pkg_installed(pkg)) {
    system.file("extdata", package = pkg)
  } else if (data_pkg_avail(pkg)) {
    file.path(.libPaths()[1L], pkg, "extdata")
  } else {
    data_dir(srcs)
  }
}

#' @importFrom utils packageDescription available.packages
data_pkg_avail <- function(src) {

  if (!is.string(src)) {
    src <- src_name(src)
  }

  repos <- packageDescription("ricu", fields = "Additional_repositories")
  pkgs  <- available.packages(repos = repos)

  sub("_", ".", src) %in% pkgs[, "Package"]
}

#' @importFrom utils install.packages
install_data_pkgs <- function(srcs = c("mimic_demo", "eicu_demo")) {

  if (!is.character(srcs)) {
    srcs <- src_name(srcs)
  }

  repos <- packageDescription("ricu", fields = "Additional_repositories")

  install.packages(sub("_", ".", srcs), repos = repos)
}

#' @rdname file_utils
#' @export
auto_attach_srcs <- function() {

  res <- Sys.getenv("RICU_SRC_LOAD", unset = NA_character_)

  if (is.na(res)) {
    c("mimic", "mimic_demo", "eicu", "eicu_demo", "hirid", "aumc")
  } else {
    strsplit(res, ",")[[1L]]
  }
}

ensure_dirs <- function(paths) {

  uq_paths <- unique(paths)

  is_dir <- file.info(uq_paths, extra_cols = FALSE)[["isdir"]]
  is_no_dir <- lgl_ply(is_dir, identical, FALSE)

  if (any(is_no_dir)) {
    stop_ricu(
      c("The following {qty(sum(is_no_dir))} path{?s} {?exists/exist} but not
         as director{?y/ies}:", bullet(uq_paths[is_no_dir])),
      class = "path_exists_not_dir", exdent = c(0L, rep(2L, sum(is_no_dir)))
    )
  }

  dirs_to_create <- uq_paths[is.na(is_dir)]

  if (length(dirs_to_create) > 0L) {

    res <- lgl_ply(dirs_to_create, dir.create, recursive = TRUE)

    if (!all(res)) {
      stop_ricu(
        c("The following {qty(sum(!res))} director{?y/ies} could not be
           created:", bullet(dirs_to_create[!res])),
        class = "dir_create_fail", exdent = c(0L, rep(2L, sum(!res)))
      )
    }
  }

  invisible(paths)
}

default_config_path <- function() {
  system.file("extdata", "config", package = pkg_name())
}

user_config_path <- function() {

  res <- Sys.getenv("RICU_CONFIG_PATH", unset = NA_character_)

  if (is.na(res)) {
    NULL
  } else {
    strsplit(res, ",")[[1L]]
  }
}

#' @rdname file_utils
#' @export
config_paths <- function() c(user_config_path(), default_config_path())

#' @param name File name of the configuration file (`.json` will be appended)
#' @param cfg_dirs Character vector of directories searched for config files
#' @param combine_fun If multiple files are found, a function for combining
#' returned lists
#' @param ... Passed to [jsonlite::read_json()] or [jsonlite::write_json()]
#'
#' @rdname file_utils
#'
#' @export
#'
get_config <- function(name, cfg_dirs = config_paths(), combine_fun = c, ...) {

  read_if_exists <- function(x, ...) {

    if (isTRUE(file.exists(x))) {
      read_json(x, ...)
    } else {
      NULL
    }
  }

  assert_that(is.string(name), has_length(cfg_dirs), all_fun(cfg_dirs, is.dir),
              null_or(combine_fun, is.function))

  res <- lapply(file.path(cfg_dirs, paste0(name, ".json")),
                read_if_exists, ...)

  if (is.null(combine_fun)) {
    res
  } else {
    Reduce(combine_fun, res, NULL)
  }
}

read_json <- function(path, simplifyVector = TRUE, simplifyDataFrame = FALSE,
                      simplifyMatrix = FALSE, ...) {

  assert_that(file.exists(path))

  jsonlite::read_json(path, simplifyVector = simplifyVector,
                      simplifyDataFrame = simplifyDataFrame,
                      simplifyMatrix = simplifyMatrix, ...)
}

#' @param x Object to be written
#' @param dir Directory to write the file to (created if non-existent)
#'
#' @rdname file_utils
#'
#' @export
#'
set_config <- function(x, name, dir = file.path("inst", "extdata", "config"),
                       ...) {

  assert_that(is.string(name))

  file <- paste0(name, ".json")

  if (!is.null(dir)) {

    assert_that(is.string(dir))

    file <- file.path(ensure_dirs(dir), file)
  }

  write_json(x, file, ...)
}

write_json <- function(x, path, null = "null", auto_unbox = TRUE,
                       pretty = TRUE, ...) {

  jsonlite::write_json(x, path, null = null, auto_unbox = auto_unbox,
                       pretty = pretty, ...)
}
