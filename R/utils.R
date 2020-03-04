
#' File system utilities
#'
#' Determine the location where to place data meant to persist between
#' individual sessions.
#'
#' For data, the default location depends on the operating system as
#'
#' | **Platform** | **Location**                          |
#' | ------------ | ------------------------------------- |
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
#' @param subdir A string specifying a directory that will be made sure to
#' exist below the data directory.
#'
#' @rdname file_utils
#'
#' @examples
#' Sys.setenv(RICU_DATA_PATH = tempdir())
#' identical(data_dir(), tempdir())
#'
#' dir.exists(file.path(tempdir(), "some_subdir"))
#' some_subdir <- data_dir("some_subdir")
#' dir.exists(some_subdir)
#'
#' @export
#'
data_dir <- function(subdir = NULL) {

  res <- data_dir_path()

  if (!is.null(subdir)) {
    assert_that(is.string(subdir))
    res <- ensure_dir(file.path(res, subdir))
  }

  res
}

default_data_path <- function() {

  root <- switch(
    Sys.info()[["sysname"]],
    Darwin  = Sys.getenv("XDG_DATA_HOME", "~/Library/Application Support"),
    Windows = Sys.getenv("LOCALAPPDATA", Sys.getenv("APPDATA")),
    Sys.getenv("XDG_DATA_HOME", "~/.local/share")
  )

  root <- file.path(root, "ricu")

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

ensure_dir <- function(paths) {

  is_dir <- file.info(paths, extra_cols = FALSE)[["isdir"]]
  is_no_dir <- lgl_ply(is_dir, identical, FALSE)

  if (any(is_no_dir)) {
    stop("The following path(s) exist(s) but not as directory:\n  ",
         paste(paths[is_no_dir], collapse = "\n  "))
  }

  dirs_to_create <- paths[is.na(is_dir)]

  if (length(dirs_to_create) > 0L) {

    res <- dir.create(dirs_to_create, recursive = TRUE)

    if (!all(res)) {
      stop("The following directorie(s) could not be created:\n  ",
           paste(dirs_to_create[!res], collapse = "\n  "))
    }
  }

  invisible(paths)
}

data_dir_path <- function() {

  env_var <- Sys.getenv("RICU_DATA_PATH", unset = NA_character_)

  if (is.na(env_var)) default_data_path()
  else                ensure_dir(env_var)
}

default_config_path <- function() {
  system.file("extdata", "config", package = methods::getPackageName())
}

config_dir_path <- function() {

  env_var <- Sys.getenv("RICU_CONFIG_PATH", unset = NA_character_)

  if (is.na(env_var)) default_config_path()
  else                ensure_dir(env_var)
}

#' For configuration files, the default location is `extdata/config` and the
#' environment variable `RICU_CONFIG_PATH` can be used to overwrite the
#' default location. Files are first searched for in the user-specified
#' directory and if not found there, the default dir ist taken into account.
#' Additionally, `get_config()` has a `dir` argument which takes highest
#' precedence.
#'
#' @param name File name of the configuration file (without file ending).
#' @param dir (Optional) directory name where to look for the specified file.
#' @param ... Passed to [jsonlite::read_json].
#'
#' @rdname file_utils
#'
#' @examples
#' cfg <- get_config("eicu-demo")
#' identical(
#'   cfg,
#'   get_config("eicu-demo",
#'              system.file("extdata", "config", package = "ricu"))
#' )
#'
#' @export
#'
get_config <- function(name, dir = NULL, ...) {

  assert_that(is.string(name))

  file <- paste0(name, ".json")

  if (is.null(dir)) {

    usr_file <- file.path(config_dir_path(), file)

    if (file.exists(usr_file)) file <- usr_file
    else                       file <- file.path(default_config_path(), file)

  } else {

    assert_that(is.dir(dir))
    file <- file.path(dir, file)
  }

  read_json(file, ...)
}

read_json <- function(file, simplifyVector = TRUE, simplifyDataFrame = FALSE,
                      simplifyMatrix = FALSE, ...) {

  assert_that(file.exists(file))

  jsonlite::read_json(file, simplifyVector = simplifyVector,
                      simplifyDataFrame = simplifyDataFrame,
                      simplifyMatrix = simplifyMatrix, ...)
}

is_pkg_available <- function(pkg) requireNamespace(pkg, quietly = TRUE)

get_table <- function(table, source) {

  if (is.character(source)) {
    source <- get0(source, data)
  }

  assert_that(is.string(table), is.environment(source))

  res <- get0(table, source)

  assert_that(prt::is_prt(res))

  res
}

agg_or_na <- function(agg_fun) {
  function(x) {
    if (all(is.na(x))) return(x[1L])
    res <- agg_fun(x, na.rm = TRUE)
    if(is.na(res)) x[1L] else res
  }
}

#' @export
min_or_na <- agg_or_na(min)

#' @export
max_or_na <- agg_or_na(max)

#' @export
sum_or_na <- agg_or_na(sum)

#' @export
secs <- function(x) as.difftime(x, units = "secs")

#' @export
mins <- function(x) as.difftime(x, units = "mins")

#' @export
hours <- function(x) as.difftime(x, units = "hours")

#' @export
days <- function(x) as.difftime(x, units = "days")

#' @export
weeks <- function(x) as.difftime(x, units = "weeks")

reduce <- function(f, x, ...) Reduce(function(x, y) f(x, y, ...), x)

round_to <- function(x, to = 1) if (to == 1) trunc(x) else to * trunc(x / to)

is_val <- function(x, val) !is.na(x) & x == val
is_true <- function(x) !is.na(x) & x

last_elem <- function(x) x[length(x)]
first_elem <- function(x) x[1L]

cat_line <- function(...) {
  line <- trimws(paste0(...), "right")
  cat(paste0(line, "\n"), sep = "")
}

str_in_vec_once <- function(str, vec) identical(sum(vec %in% str), 1L)

substitute_q <- function(expr, env) {
  substitute(substitute(x, env), list(x = expr))
}

null_or_subs <- function(x, where = parent.frame(1L)) {
  if (missing(x)) NULL else do.call("substitute", list(substitute(x), where))
}

carry_backwards <- function(x) {

  res <- x[length(x)]

  if (is.na(res)) {
    not_na <- !is.na(x)
    if (any(not_na)) return(tail(x[not_na], n = 1L))
  }

  res
}

replace_na <- function(x, val) replace(x, is.na(x), val)

split_indices <- function(len, n_chunks) {

  assert_that(is.count(len), is.count(n_chunks))

  if (len == 1L || n_chunks == 1L) {

    rep.int(1L, len)

  } else {

    i <- seq_len(len)

    fuzz <- min((len - 1L) / 1000, 0.4 * len / n_chunks)
    breaks <- seq(1 - fuzz, len + fuzz, length.out = n_chunks + 1L)
    bins <- cut(i, breaks)

    as.integer(bins)
  }
}

as_src <- function(x) {
  assert_that(is.string(x))
  sub("_demo$", "", x)
}

force_numeric <- function(x) {
  res <- suppressWarnings(as.numeric(x))
  new_na <- is.na(res) & !is.na(x)
  if (sum(is.na(new_na)) > 0L) {
    message("lost ", round(mean(new_na) * 100, digits = 2),
            "% of entries due to `force_numeric()`")
  }
  res
}

new_names <- function(old_names = character(0L), n = 1L,
                      chars = c(letters, LETTERS, 0L:9L), length = 15L) {

  if (inherits(old_names, "data.frame")) {
    old_names <- colnames(old_names)
  }

  assert_that(
    is.null(old_names) || is.character(old_names),
    is.count(n), is.count(length),
    is.character(chars), length(chars) >= 1L
  )

  repeat{
    res <- replicate(n, paste(sample(chars, length), collapse = ""))
    if (length(res) == length(unique(res)) && !any(res %in% old_names)) break
  }

  res
}

chr_ply <- function(x, fun, ..., length = 1L, use_names = FALSE) {
  vapply(x, fun, character(length), ..., USE.NAMES = use_names)
}

lgl_ply <- function(x, fun, ..., length = 1L, use_names = FALSE) {
  vapply(x, fun, logical(length), ..., USE.NAMES = use_names)
}
