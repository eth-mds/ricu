
#' @export
attach_mimic <- function(demo = FALSE,
  dir = if (demo) data_dir("mimic-demo") else data_dir("mimic-data"),
  cfg = if (demo) get_config("mimic-demo") else get_config("mimic-setup"),
  env_name = if (demo) "mimic_demo" else "mimic", ...) {

  attach_datasource(dir, cfg, env_name)
}

#' @export
attach_eicu <- function(demo = FALSE,
  dir = if (demo) data_dir("eicu-demo") else data_dir("eicu-data"),
  cfg = if (demo) get_config("eicu-demo") else get_config("eicu-setup"),
  env_name = if (demo) "eicu_demo" else "eicu", ...) {

  attach_datasource(dir, cfg, env_name)
}

#' @export
data <- new.env()

attach_datasource <- function(dir, cfg, env_name, link_global = TRUE) {

  sub_csv <- function(repl, x) sub("\\.csv(\\.gz)?", repl, tolower(x))

  cfg_to_filename <- function(name) {
    if (has_name(cfg[[name]], "partitioning")) {
      n_part <- length(cfg[[name]][["partitioning"]][[1L]]) - 1L
      file.path(dir, sub_csv("", name), paste0(seq_len(n_part), ".fst"))
    } else {
      file.path(dir, sub_csv(".fst", name))
    }
  }

  assert_that(is.flag(link_global))

  files <- lapply(names(cfg), cfg_to_filename)
  names(files) <- sub_csv("", names(cfg))

  assert_that(all(file.exists(unlist(files))))

  envir <- new.env(parent = data)
  list2env(lapply(files, prt::new_prt), envir)
  assign(env_name, envir, envir = data)
  if (link_global) assign(env_name, envir, envir = .GlobalEnv)

  invisible(NULL)
}
