
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
attach_src.src_cfg <- function(x, assign_env = .GlobalEnv,
                               dir = src_data_dir(x), ...) {

  warn_dots(...)

  tryCatch({

    assert_that(is.string(dir), is.environment(assign_env))

    src <- src_name(x)

    dat_env <- data_env()
    src_env <- new_src_env(x, env = new.env(parent = dat_env))

    delayedAssign(src, setup_src_env(x, src_env, dir), assign.env = dat_env)
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

#' @inheritParams load_src_cfg
#' @rdname attach_src
#' @export
attach_src.character <- function(x, name = "data-sources", dir = NULL,
                                 assign_env = .GlobalEnv, ...) {

  cfgs <- tryCatch(read_src_cfg(x, name, dir), error = function(err) {

    warn_ricu({
      cli_text("Failed to read source configuration with error:")
      cli_div(theme = list(body = list("margin-left" = 2)))
      cli_verbatim(conditionMessage(err))
    }, class = "src_cfg_read_error")

    NULL
  })

  for (src in x) {

    cfg <- tryCatch(parse_src_cfg(cfgs[[src]]), error = function(err) {

      warn_ricu({
        cli_text("Failed to parse source configuration for source `{src}`
                  with error:")
        cli_div(theme = list(body = list("margin-left" = 2)))
        cli_verbatim(conditionMessage(err))
      }, class = "src_cfg_parse_error")


      assign(src, NULL, envir = assign_env)
    })

    if (not_null(cfg)) {
      attach_src(cfg, assign_env = assign_env, ...)
    }
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

    if (interactive()) {

      msg_ricu({
        cli_text("The following {qty(length(todo))} table{?s} {?is/are}
                  missing from directory {dir}:")
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
                  missing from directory {dir}:")
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
                  directory {dir}:")
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

  dat_tbls <- Map(new_src_tbl, fst_paths, as_col_cfg(x),
                  MoreArgs = list(prefix = x[["prefix"]], src_env = env))
  names(dat_tbls) <- tables

  list2env(dat_tbls, envir = env)
}

#' @export
setup_src_env.default <- function(x, ...) stop_generic(x, .Generic)
