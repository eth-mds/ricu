
#' File system utilities
#'
#' Determine the location where to place data meant to persist between
#' individual sessions.
#'
#' For data, the default location depends on the operating system as
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
#' @rdname file_utils
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

ensure_dir <- function(paths) {

  is_dir <- file.info(paths, extra_cols = FALSE)[["isdir"]]
  is_no_dir <- vapply(is_dir, identical, logical(1L), FALSE)

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

  env_var <- Sys.getenv("SEPSR_DATA_PATH", unset = NA_character_)

  if (is.na(env_var)) default_data_path()
  else                ensure_dir(env_var)
}

default_config_path <- function() {
  system.file("extdata", "config", package = methods::getPackageName())
}

config_dir_path <- function() {

  env_var <- Sys.getenv("SEPSR_CONFIG_PATH", unset = NA_character_)

  if (is.na(env_var)) default_config_path()
  else                ensure_dir(env_var)
}

#' For configuration files, the default location is `extdata/config` and the
#' environment variable `SEPSR_CONFIG_PATH` can be used to overwrite the
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
#'              system.file("extdata", "config", package = "sepsr"))
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

get_table <- function(table, envir) {

  if (is.character(envir)) {
    envir <- get0(envir, data)
  }

  assert_that(is.string(table), is.environment(envir))

  res <- get0(table, envir)

  assert_that(prt::is_prt(res))

  res
}

expand_limits <- function(x, min_col = "min", max_col = "max", step_size = 1L,
                          id_cols = NULL, new_col = "hadm_time") {

  make_seq <- function(lwr, upr, step, unit) {
    seqs <- Map(seq, lwr, upr, MoreArgs = list(by = step))
    list(as.difftime(unlist(seqs), units = unit))
  }

  assert_that(
    is_dt(x), is.string(min_col), is.string(max_col),
    min_col %in% colnames(x), max_col %in% colnames(x),
    units(x[[min_col]]) == units(x[[max_col]])
  )

  unit <- units(x[[min_col]])

  res <- x[, make_seq(get(min_col), get(max_col), step_size, unit),
           by = id_cols]
  data.table::setnames(res, c(id_cols, new_col))
  data.table::setattr(res[[new_col]], "step_size", step_size)

  res
}

make_regular <- function(x, time_col = "hadm_time", id_cols = "hadm_id",
                         limits = NULL, step_size = NULL, ...) {

  assert_that(is_unique(x, c(id_cols, time_col)))

  if (is.null(step_size)) {
    step_size <- attr(x[[time_col]], "step_size")
  }

  if (is.null(limits)) {
    limits <- x[, list(min = min(get(time_col)), max = max(get(time_col))),
                       by = id_cols]
    join <- expand_limits(limits, "min", "max", step_size, id_cols, time_col)
  } else {
    join <- expand_limits(limits, ..., step_size = step_size,
                          id_cols = id_cols, new_col = time_col)
  }

  assert_that(is_regular(join, id_cols, time_col))

  tmp <- x[join, on = c(id_cols, time_col)]
}

is_regular <- function(x, id_cols = "hadm_id", time_col = "hadm_time") {

  check_time_col <- function(time, step) {
    tmp <- as.numeric(time)
    identical(tmp, seq(min(tmp), max(tmp), by = step))
  }

  assert_that(is_dt(x), has_cols(x, c(id_cols, time_col)))

  step <- attr(x[[time_col]], "step_size")
  res <- x[, check_time_col(get(time_col), step), by = id_cols]

  all(res[[setdiff(colnames(res), id_cols)]])
}

window_fun <- function(tbl, expr, ...) window_quo(tbl, substitute(expr), ...)

window_quo <- function(tbl, expr, id_cols = "hadm_id", time_col = "hadm_time",
                       full_window = FALSE,
                       window_length = as.difftime(24L, units = "hours")) {

  assert_that(is_dt(tbl), has_cols(tbl, c(id_cols, time_col)),
              is_unique(tbl, c(id_cols, time_col)), is.flag(full_window),
              is_difftime(window_length, allow_neg = FALSE))

  units(window_length) <- units(tbl[[time_col]])

  join <- tbl[,
    c(mget(id_cols), list(get(time_col)), list(get(time_col) - window_length))
  ]
  join <- data.table::setnames(join, c(id_cols, "cur_time", "min_time"))

  if (full_window) {
    join <- join[, win_time := window_length <= (cur_time - min(cur_time)),
                 by = id_cols]
    join <- join[(win_time), ]
  }

  on_clauses <- c(
    id_cols, paste(time_col, "<= cur_time"), paste(time_col, ">= min_time")
  )

  tmp <- tbl[join, eval(expr), on = on_clauses, by = .EACHI]

  tmp <- data.table::setnames(tmp, make.unique)
  tmp <- tmp[, hadm_time.1 := NULL]

  merge(tbl, tmp, by = c(id_cols, time_col), all = TRUE)
}

agg_or_na <- function(agg_fun) {
  function(x) {
    if (all(is.na(x))) return(x[1L])
    res <- agg_fun(x, na.rm = TRUE)
    if(is.na(res)) x[1L] else res
  }
}

min_or_na <- agg_or_na(min)
max_or_na <- agg_or_na(max)
sum_or_na <- agg_or_na(sum)

reduce <- function(f, x, ...) {
  Reduce(function(x, y) f(x, y, ...), x)
}

round_to <- function(x, to = 1) if (to == 1) round(x) else to * round(x / to)

time_unit_as_int <- function(x) {
  match(x, c("secs", "mins", "hours", "days", "weeks"))
}

is_val <- function(x, val) !is.na(x) & x == val
is_true <- function(x) !is.na(x) & x

last_elem <- function(x) x[length(x)]
first_elem <- function(x) x[1L]

cat_line <- function(...) {
  line <- trimws(paste0(...), "right")
  cat(paste0(line, "\n"), sep = "")
}
