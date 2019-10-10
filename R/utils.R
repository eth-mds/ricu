
#' File system utilities
#'
#' Determine the location where to place data meant to persist between
#' individual sessions. The default location depends on the operating system
#' as
#'
#' | **Platform** | **Location**                          |
#' | ------------ | ------------------------------------- |
#' | Linux        | `~/.local/share/sepsr`                |
#' | macOS        | `~/Library/Application Support/sepsr` |
#' | Windows      | `%LOCALAPPDATA%/sepsr`                |
#'
#' If the default storage directory does not exists, it will only be created
#' upon user consent (requiring an interactive session).
#'
#' The environment variable `SEPSR_DATA_PATH` can be used to overwrite the
#' default location. If desired, this variable can be set in an R startup file
#' to make it apply to all R sessions. For example, it could be set within:
#'
#' - A project-local `.Renviron`;
#' - The user-level `.Renviron`;
#' - A file at `$(R RHOME)/etc/Renviron.site`.
#'
#' Any directory specified as environment variable will recursively be created.
#'
#' @param subdir A string specifying a directory that will be made sure to
#' exist below the data directory.
#'
#' @examples
#' Sys.setenv(SEPSR_DATA_PATH = tempdir())
#' identical(data_dir(), tempdir())
#'
#' dir.exists(file.path(tempdir(), "some_subdir"))
#' some_subdir <- data_dir("some_subdir")
#' dir.exists(some_subdir)
#'
#' @export
#'
data_dir <- function(subdir = NULL) {

  res <- sepsr_data_path()

  if (!is.null(subdir)) {
    assert_that(is.string(subdir))
    res <- ensure_dir(file.path(res, subdir))
  }

  res
}

sepsr_default_data_path <- function() {

  root <- switch(
    Sys.info()[["sysname"]],
    Darwin  = Sys.getenv("XDG_DATA_HOME", "~/Library/Application Support"),
    Windows = Sys.getenv("LOCALAPPDATA", Sys.getenv("APPDATA")),
    Sys.getenv("XDG_DATA_HOME", "~/.local/share")
  )

  root <- normalize_path(root)
  root <- file.path(root, "sepsr")

  if (!dir.exists(root)) {
    message("Attempting to set up data directory at\n  ", root,
            "\nPlease confirm (Y/n): ", appendLF = FALSE)
    if (identical(readline(), "Y")) dir.create(root)
  }

  if (!dir.exists(root)) {
    stop("Failed to set up data directory at\n  ", root)
  }

  root
}

normalize_path <- function(path, winslash = "/", mustWork = FALSE) {

  if (Sys.info()[["sysname"]] == "Windows") {
    path <- utils::shortPathName(path.expand(path))
  }

  normalizePath(path, winslash = winslash, mustWork = mustWork)
}

ensure_dir <- function(paths) {

  is_dir <- file.info(paths, extra_cols = FALSE)[["isdir"]]
  is_no_dir <- vapply(is_dir, identical, logical(1L), FALSE)

  if (any(is_no_dir)) {
    stop("The following path(s) exist(s) but not as directory:\n  ",
         paste(paths[is_no_dir], collapse = "\n  "))
  }

  dirs_to_create <- paths[is.na(is_dir)]

  res <- dir.create(dirs_to_create, recursive = TRUE)

  if (!all(res)) {
    stop("The following directorie(s) could not be created:\n  ",
         paste(dirs_to_create[!res], collapse = "\n  "))
  }

  invisible(paths)
}

sepsr_data_path <- function() {

  res <- ensure_dir(
    Sys.getenv("SEPSR_DATA_PATH", unset = sepsr_default_data_path())
  )

  res
}
