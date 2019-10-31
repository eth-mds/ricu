
#' Data attach utilities
#'
#' @rdname data_attach
#'
#' @param demo Logical flag indicating whether to import the respective demo
#' of full dataset. Only used for determining the correct default arguments
#' passed on to `attach_datasource()`.
#' @param dir Data directory where the imported `.fst` files are located (see
#' [import_datasource()] for further information).
#' @param config Import configuration specified as nested list (see
#' [import_datasource()] for further information).
#' @param envir (Optional) environment that serves as proxy for the environment
#' holding references to tables. Passing `NULL` foregoes setting up this
#' forwarding (implemented as active bindings, see
#' [base::makeActiveBinding()]).
#'
#' @export
attach_mimic <- function(demo = FALSE, dir = mimic_data_dir(demo),
                         config = mimic_config(demo),
                         envir = if (demo) mimic_demo else mimic) {

  attach_datasource(dir, config)

  if (!is.null(envir)) {
    setup_proxy_env(config, envir)
  }

  invisible(NULL)
}

#' @rdname data_attach
#'
#' @export
#'
attach_eicu <- function(demo = FALSE, dir = eicu_data_dir(demo),
                        config = eicu_config(demo),
                        envir = if (demo) eicu_demo else eicu) {

  attach_datasource(dir, config)


  if (!is.null(envir)) {
    setup_proxy_env(config, envir)
  }

  invisible(NULL)
}

#' @rdname data_attach
#'
#' @export
#'
data <- new.env()

#' @rdname data_attach
#'
#' @export
#'
mimic <- new.env()

#' @rdname data_attach
#'
#' @export
#'
mimic_demo <- new.env()

#' @rdname data_attach
#'
#' @export
#'
eicu <- new.env()

#' @rdname data_attach
#'
#' @export
#'
eicu_demo <- new.env()

#' @rdname data_attach
#'
#' @export
#'
attach_datasource <- function(dir, config) {

  create_data_env <- function(dir, cfg) {

    assert_that(is.dir(dir))

    missing <- !table_exists_as_fst(cfg, dir)

    if (any(missing)) {

      todo <- names(cfg[["tables"]])[missing]

      msg <- paste("The following tables are missing from", dir,
                   paste(names(missing)[missing], collapse = "\n  "),
                   sep = "\n  ")

      if (interactive()) {

        message(msg)
        resp <- read_line("Download now (Y/n)? ")

        if (!identical(resp, "Y")) {
          warning("Cannot continue without the missing data.")
          return(NULL)
        }

      } else {
        warning(msg)
        return(NULL)
      }

      download_datasource(dir, cfg, table_sel = todo)
      import_datasource(dir, cfg, cleanup = TRUE)
    }

    files <- cfg_to_fst_names(cfg, dir)

    assert_that(all(file.exists(unlist(files))))

    prt <- lapply(files, prt::new_prt)
    list2env(prt, envir = NULL, parent = data)
  }

  assert_that(is.string(config[["name"]]))

  delayedAssign(config[["name"]], create_data_env(dir, config),
                assign.env = data)

  invisible(NULL)
}

#' @rdname data_attach
#'
#' @export
#'
setup_proxy_env <- function(config, envir = new.env()) {

  info <- paste("For further information on how to set up a data source,",
                "refer to `?attach_datasource`.")

  get_data_env <- function(name) {

    if (!exists(name, envir = data) ||
        is.null(res <- get(name, envir = data))) {
      stop("Data not found for `", name, "`. ", info)
    }

    res
  }

  get_table_from_env <- function(table, data) {
    env <- get_data_env(data)
    if (exists(table, envir = env)) get(table, envir = env)
    else stop("Table `", table, "` not found for `", data, "`. ", info)
  }

  create_binding <- function(table_name, dataset_name, envir) {
    get_table <- function() get_table_from_env(table_name, dataset_name)
    makeActiveBinding(table_name, get_table, envir)
  }


  data_name <- config[["name"]]
  tables <- cfg_to_table_name(config)

  assert_that(is.string(data_name), is.character(tables),
              is.environment(envir))

  lapply(tables, create_binding, data_name, envir)

  envir
}

cfg_to_table_name <- function(cfg) {
  assert_that(has_name(cfg, "tables"), !is.null(names(cfg[["tables"]])))
  sub("\\.csv(\\.gz)?", "", tolower(names(cfg[["tables"]])))
}

cfg_to_fst_names <- function(cfg, dir) {

  to_filenames <- function(csv_name, tbl_name) {

    if (has_name(tbls[[csv_name]], "partitioning")) {
      n_part <- length(tbls[[csv_name]][["partitioning"]][[1L]]) - 1L
      res <- file.path(dir, tbl_name, seq_len(n_part))
    } else {
      res <- file.path(dir, tbl_name)
    }

    paste0(res, ".fst")
  }

  assert_that(has_name(cfg, "tables"))

  tbls <- cfg[["tables"]]
  tbl_nms <- cfg_to_table_name(cfg)

  files <- Map(to_filenames, names(tbls), tbl_nms)
  names(files) <- cfg_to_table_name(cfg)

  files
}

table_exists_as_fst <- function(cfg, dir) {
  all_exists <- function(x) all(file.exists(x))
  files <- cfg_to_fst_names(cfg, dir)
  vapply(files, all_exists, logical(1L))
}

mimic_data_dir <- function(demo) {
  if (demo) {
    if (is_pkg_available("mimic.demo")) {
      system.file("extdata", package = "mimic.demo")
    } else {
      data_dir("mimic-demo")
    }
  } else {
    data_dir("mimic")
  }
}

mimic_config <- function(demo) {
  if (demo) {
    get_config("mimic-demo")
  } else {
    get_config("mimic-setup")
  }
}

eicu_data_dir <- function(demo) {
  if (demo) {
    if (is_pkg_available("eicu.demo")) {
      system.file("extdata", package = "eicu.demo")
    } else {
      data_dir("eicu-demo")
    }
  } else {
    data_dir("eicu")
  }
}

eicu_config <- function(demo) {
  if (demo) {
    get_config("eicu-demo")
  } else {
    get_config("eicu-setup")
  }
}
