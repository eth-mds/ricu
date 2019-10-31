
#' Data attach utilities
#' @export
attach_mimic <- function(demo = FALSE,
  dir = if (demo) data_dir("mimic-demo") else data_dir("mimic-data"),
  cfg = if (demo) get_config("mimic-demo") else get_config("mimic-setup"),
  env = if (demo) mimic_demo else mimic) {

  attach_datasource(dir, cfg)
  create_env_link(cfg, env)
}

#' @export
attach_eicu <- function(demo = FALSE,
  dir = if (demo) data_dir("eicu-demo") else data_dir("eicu-data"),
  cfg = if (demo) get_config("eicu-demo") else get_config("eicu-setup"),
  env = if (demo) eicu_demo else eicu) {

  attach_datasource(dir, cfg)
  create_env_link(cfg, env)
}

#' @export
data <- new.env()

#' @export
mimic <- new.env()

#' @export
mimic_demo <- new.env()

#' @export
eicu <- new.env()

#' @export
eicu_demo <- new.env()

attach_datasource <- function(dir, cfg) {

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
        resp <- read_line("Download now? (Y/n)")

        if (!identical(resp, "Y")) {
          warning("Cannot continue without the missing data.")
          return(NULL)
        }

      } else {
        warning(msg)
        return(NULL)
      }

      download_physionet_dataset(dir, cfg, table_sel = todo)
      import_datasource(dir, cfg, cleanup = TRUE)
    }

    files <- cfg_to_fst_names(cfg, dir)

    assert_that(all(file.exists(unlist(files))))

    prt <- lapply(files, prt::new_prt)
    list2env(prt, envir = NULL, parent = data)
  }

  assert_that(is.string(cfg[["name"]]))

  delayedAssign(cfg[["name"]], create_data_env(dir, cfg), assign.env = data)

  invisible(NULL)
}

create_env_link <- function(cfg, env) {

  info <- paste("For further information on how to set up a data source,",
                "refer to `?attach_datasource`."

  get_data_env <- function(name) {
    if (exists(name, envir = data)) get(name, envir = data)
    else stop("Data not found for `", name, "`. ", info)
  }

  get_table_from_env <- function(table, data) {
    env <- get_data_env(data)
    if (exists(table, envir = env)) get(table, envir = env)
    else stop("Table `", table, "` not found for `", data, "`. ", info)
  }

  create_binding <- function(table_name, dataset_name) {
    get_table <- function() get_table_from_env(table_name, dataset_name)
    makeActiveBinding(tbl, get_table, env)
  }


  data_name <- cfg[["name"]]
  tables <- cfg_to_table_name(cfg)

  assert_that(is.string(data_name), is.character(tables), is.environment(env))

  lapply(tables, create_binding, data_name)

  invisible(NULL)
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
