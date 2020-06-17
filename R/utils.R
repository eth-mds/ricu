
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
#' @param subdir A string specifying a directory that will be made sure to
#' exist below the data directory.
#' @param create Logical flag indicating whether to create the specified
#' directory
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

ensure_dirs <- function(paths) {

  uq_paths <- unique(paths)

  is_dir <- file.info(uq_paths, extra_cols = FALSE)[["isdir"]]
  is_no_dir <- lgl_ply(is_dir, identical, FALSE)

  if (any(is_no_dir)) {
    stop("The following path(s) exist(s) but not as directory:\n  ",
         paste(uq_paths[is_no_dir], collapse = "\n  "))
  }

  dirs_to_create <- uq_paths[is.na(is_dir)]

  if (length(dirs_to_create) > 0L) {

    res <- lgl_ply(dirs_to_create, dir.create, recursive = TRUE)

    if (!all(res)) {
      stop("The following directorie(s) could not be created:\n  ",
           paste(dirs_to_create[!res], collapse = "\n  "))
    }
  }

  invisible(paths)
}

default_config_path <- function() {
  system.file("extdata", "config", package = methods::getPackageName())
}

config_dir_path <- function() {

  res <- Sys.getenv("RICU_CONFIG_PATH", unset = NA_character_)

  if (is.na(res)) {
    res <- default_config_path()
  }

  res
}

#' For configuration files, the default location is `extdata/config` and the
#' environment variable `RICU_CONFIG_PATH` can be used to overwrite the
#' default location. Files are first searched for in the user-specified
#' directory and if not found there, the default dir ist taken into account.
#' Additionally, `get_config()` has a `dir` argument which takes highest
#' precedence.
#'
#' @param name File name of the configuration file (without file ending)
#' @param dir (Optional) directory name where to look for the specified file
#' @param ... Passed to [jsonlite::read_json()] or [jsonlite::write_json()]
#'
#' @rdname file_utils
#'
#' @examples
#' cfg <- get_config("concept-dict")
#' identical(
#'   cfg,
#'   get_config("concept-dict",
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

    if (file.exists(usr_file)) {
      file <- usr_file
    } else {
      file <- file.path(default_config_path(), file)
    }

  } else {

    assert_that(is.dir(dir))
    file <- file.path(dir, file)
  }

  read_json(file, ...)
}

read_json <- function(path, simplifyVector = TRUE, simplifyDataFrame = FALSE,
                      simplifyMatrix = FALSE, ...) {

  assert_that(file.exists(path))

  jsonlite::read_json(path, simplifyVector = simplifyVector,
                      simplifyDataFrame = simplifyDataFrame,
                      simplifyMatrix = simplifyMatrix, ...)
}

#' @param x Object to be written
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

    file <- file.path(dir, file)
  }

  write_json(x, file, ...)
}

write_json <- function(x, path, null = "null", auto_unbox = TRUE,
                       pretty = TRUE, ...) {

  jsonlite::write_json(x, path, null = null, auto_unbox = auto_unbox,
                       pretty = pretty)
}

is_pkg_available <- function(pkg) requireNamespace(pkg, quietly = TRUE)

agg_or_na <- function(agg_fun) {
  function(x) {
    if (all(is.na(x))) return(x[1L])
    res <- agg_fun(x, na.rm = TRUE)
    if(is.na(res)) x[1L] else res
  }
}

#' @rdname utils
#' @export
#'
min_or_na <- agg_or_na(min)

#' @rdname utils
#' @export
#'
max_or_na <- agg_or_na(max)

#' @rdname utils
#' @export
#'
sum_or_na <- agg_or_na(sum)

#' Difftime utilities
#'
#' Utility functions for working with [base::difftime()] objects.
#'
#' @param x Object to test/coerce
#'
#' @rdname difftime
#' @export
#'
secs <- function(x) as.difftime(x, units = "secs")

#' @rdname difftime
#' @export
#'
mins <- function(x) as.difftime(x, units = "mins")

#' @rdname difftime
#' @export
#'
hours <- function(x) as.difftime(x, units = "hours")

#' @rdname difftime
#' @export
#'
days <- function(x) as.difftime(x, units = "days")

#' @rdname difftime
#' @export
#'
weeks <- function(x) as.difftime(x, units = "weeks")

is_difftime <- function(x) inherits(x, "difftime")

is_one_min <- function(x) all_equal(x, mins(1L))

#' @param allow_neg Logical flag indicating whether to allow negative values
#'
#' @rdname difftime
#' @export
#'
is_time <- function(x, allow_neg = TRUE) {
  is_difftime(x) && length(x) == 1L && (allow_neg || all(x >= 0))
}

#' @rdname difftime
#' @export
#'
is_time_vec <- function(x, allow_neg = TRUE) {
  is_difftime(x) && (allow_neg || all(x >= 0))
}

re_time <- function(x, interval) {
  round_to(`units<-`(x, units(interval)), as.double(interval))
}

reduce <- function(f, x, ...) Reduce(function(x, y) f(x, y, ...), x)

round_to <- function(x, to = 1) {
  if (all_equal(to, 1)) trunc(x) else to * trunc(x / to)
}

#' Utility functions
#'
#' Various utility functions
#'
#' @param x Object to test
#' @param val Value to compare against
#'
#' @rdname utils
#' @export
#'
is_val <- function(x, val) !is.na(x) & x == val

#' @rdname utils
#' @export
#'
not_val <- function(x, val) !is.na(x) & x != val

val_or_na <- function(x, val) is.na(x) | x == val

#' @rdname utils
#' @export
#'
is_true <- function(x) !is.na(x) & x

#' @rdname utils
#' @export
#'
last_elem <- function(x) x[length(x)]

#' @rdname utils
#' @export
#'
first_elem <- function(x) x[1L]

cat_line <- function(...) {
  line <- trimws(paste0(...), "right")
  cat(paste0(line, "\n"), sep = "")
}

big_mark <- function(x, ...) {
  mark <- if (identical(getOption("OutDec"), ",")) "." else ","
  formatC(x, big.mark = mark, format = "d", ...)
}

times <- function(fancy = l10n_info()$`UTF-8`) if (fancy) "\u00d7" else "x"

ellipsis <- function(fancy = l10n_info()$`UTF-8`) {
  if (fancy) "\u2026" else "..."
}

quote_bt <- function(x) encodeString(x, quote = "`")

concat <- function(...) paste0(..., collapse = ", ")

prcnt <- function(x, tot = sum(x)) {
  paste0(round(x / tot * 100, digits = 2), "%")
}

str_in_vec_once <- function(str, vec) identical(sum(vec %in% str), 1L)

null_or_subs <- function(x, where = parent.frame(1L)) {
  if (missing(x)) NULL else do.call("substitute", list(substitute(x), where))
}

carry_backwards <- function(x) {

  res <- x[length(x)]

  if (is.na(res)) {
    no_na <- not_na(x)
    if (any(no_na)) return(tail(x[no_na], n = 1L))
  }

  res
}

#' @rdname utils
#' @export
#'
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

force_numeric <- function(x) {
  res <- suppressWarnings(as.numeric(x))
  new_na <- sum(is.na(res) & !is.na(x))
  if (new_na > 0L) {
    message("    lost ", new_na, " (", prcnt(new_na, length(x)),
            ") entries due to `force_numeric()`")
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

int_ply <- function(x, fun, ..., length = 1L, use_names = FALSE) {
  vapply(x, fun, integer(length), ..., USE.NAMES = use_names)
}

dbl_ply <- function(x, fun, ..., length = 1L, use_names = FALSE) {
  vapply(x, fun, double(length), ..., USE.NAMES = use_names)
}

lst_xtr <- function(x, i) lapply(x, `[[`, i)

chr_xtr <- function(x, i, length = 1L) chr_ply(x, `[[`, i, length = length)

chr_xtr_null <- function(x, i, length = 1L) {
  chr_ply(x, xtr_null, i, rep(NA_character_, length), length = length)
}

lgl_xtr <- function(x, i, length = 1L) lgl_ply(x, `[[`, i, length = length)

lgl_xtr_null <- function(x, i, length = 1L) {
  lgl_ply(x, xtr_null, i, rep(NA, length), length = length)
}

int_xtr <- function(x, i, length = 1L) int_ply(x, `[[`, i, length = length)

int_xtr_null <- function(x, i, length = 1L) {
  int_ply(x, xtr_null, i, rep(NA_integer_, length), length = length)
}

dbl_xtr <- function(x, i, length = 1L) dbl_ply(x, `[[`, i, length = length)

dbl_xtr_null <- function(x, i, length = 1L) {
  dbl_ply(x, xtr_null, i, rep(NA_real_, length), length = length)
}

xtr_null <- function(x, i, null_val) {
  if (is.null(res <- x[[i]])) null_val else res
}

mul_xtr <- function(x, i) {
  res <- lapply(x, `[`, i)
  if (is.character(i)) lapply(res, setNames, i) else res
}

map <- function(f, ...) Map(f, ..., USE.NAMES = FALSE)

do_call <- function(x, fun, args = NULL) {
  if (is.null(args)) do.call(fun, x)
  else do.call(fun, unname(x[args]))
}

progr_init <- function(len = NULL, msg = "loading", ...) {

  if (interactive() && is_pkg_available("progress") && len > 1L) {

    res <- progress::progress_bar$new(format = ":what [:bar] :percent",
                                      total = len, ...)

    if (not_null(msg)) {
      res$message(msg)
    }

  } else {

    message(msg)
    res <- NULL
  }

  res
}

progr_iter <- function(name, pb = NULL, len = 1L) {

  assert_that(is.string(name))

  if (is.null(pb)) {

    message("  * `", name, "`")

  } else {

    if (nchar(name) > 15L) {
      name <- paste0(substr(name, 1L, 15L - nchar(ellipsis())), ellipsis())
    } else {
      name <- sprintf("%-15s", name)
    }

    pb$tick(len = len, tokens = list(what = name))
  }
}

warn_dots <- function(...) {

  if (...length() > 0L) {
    warning("Not expecting any arguments passed as `...`")
  }

  invisible(NULL)
}

warn_dot_ident <- function(x, ...)  {

  warn_dots(...)

  x
}

wrap_null <- function(...) {

  objs <- setNames(list(...), as.character(substitute(...)))

  objs[lgl_ply(objs, is.null)] <- list(list(NULL))

  list2env(objs, parent.frame())

  invisible(NULL)
}

all_equal <- function(x, y, ...) isTRUE(all.equal(x, y, ...))

coalesce <- function(...) {
  for (x in list(...)) if (is.null(x)) next else return(x)
}
