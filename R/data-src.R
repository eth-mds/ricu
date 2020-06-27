
#' @rdname attach_src
#' @export
data <- new.env()

#' @name mimic
#' @rdname attach_src
NULL

#' @name mimic_demo
#' @rdname attach_src
NULL

#' @name eicu
#' @rdname attach_src
NULL

#' @name eicu_demo
#' @rdname attach_src
NULL

#' @name hirid
#' @rdname attach_src
NULL

pkg_env <- function() asNamespace(methods::getPackageName())

data_env <- function() get("data", envir = pkg_env(), mode = "environment")

get_from_data_env <- function(source) {

  err_fun <- function(err) {

    if (!inherits(err, "miss_tbl_err")) stop(err)

    invisible(NULL)
  }

  warn_fun <- function(warn) {

    msg <- warn[["message"]]

    if (identical(msg, "restarting interrupted promise evaluation")) {
      invokeRestart("muffleWarning")
    }
  }

  source <- force(source)

  function(value) {

    assert_that(missing(value),
      msg = paste0("Cannot update read-only data source `", source, "`")
    )

    tryCatch(
      withCallingHandlers(as_src_env(source), warning = warn_fun),
      error = err_fun
    )
  }
}

new_src_tbl <- function(files, col_cfg, prefix, src_env) {

  assert_that(is_src_env(src_env))

  res <- prt::new_prt(files)
  class(res) <- c(paste0(c(prefix, "src"), "_tbl"), class(res))

  attr(res, "col_cfg") <- as_col_cfg(col_cfg)
  attr(res, "src_env") <- src_env

  res
}

is_src_tbl <- function(x) inherits(x, "src_tbl")

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

  out <- setNames(dim_desc(dim(x)), paste0("<", class(x)[2L], ">"))

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

#' @rdname attach_src
#' @export
as_src_tbl.src_tbl <- function(x, ...) warn_dot_ident(x, ...)

#' @param tbl String valued table name
#'
#' @rdname attach_src
#' @export
as_src_tbl.src_env <- function(x, tbl, ...) {

  warn_dots(...)

  assert_that(is.string(tbl))

  res <- get0(tbl, envir = x, inherits = FALSE, ifnotfound = NULL)

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

new_src_env <- function(x, env = new.env(parent = data_env())) {

  assert_that(is_src_cfg(x), is.environment(env))

  structure(env, class = paste0(c(x[["prefix"]], "src"), "_env"),
            src_name = src_name(x), id_cfg = as_id_cfg(x))
}

is_src_env <- function(x) inherits(x, "src_env")

#' @export
print.src_env <- function(x, ...) {
  cat_line("<", class(x)[1L], "[", length(x), "]>")
  print(setNames(format(x), names(x)), quote = FALSE)
  invisible(x)
}

#' @export
format.src_env <- function(x, ...) {
  chr_ply(eapply(x, dim), dim_desc)
}

dim_desc <- function(x) {
  paste0("[", big_mark(x[1L]), " ", times(), " ", big_mark(x[2L]), "]")
}

#' @importFrom utils ls.str
#' @export
str.src_env <- function(object, ...) ls.str(object, ...)

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
