
#' Data download utilities
#'
#' The [Laboratory for Computational Physiology
#' ](https://lcp.mit.edu/index.html) (LCP) at MIT hosts two large-scale
#' databases of hospital intensive care units (ICUs), both of which can be
#' either downloaded in full or as demo subsets. While both databases are
#' publicly available, full download requires credentialed access which can be
#' gained by applying for up an account with [PhysioNet
#' ](https://physionet.org).
#'
#' The Medical Information Mart for Intensive Care (MIMIC) database holds
#' detailed clinical data from roughly 60,000 patient stays in Beth Israel
#' Deaconess Medical Center (BIDMC) intensive care units between 2001 and 2012.
#' The database includes information such as demographics, vital sign
#' measurements made at the bedside (~1 data point per hour), laboratory test
#' results, procedures, medications, caregiver notes, imaging reports, and
#' mortality (both in and out of hospital). For further information, please
#' refer to the [MIMIC-III documentation
#' ](https://mimic.physionet.org/about/mimic).
#'
#' More recently, Philips Healthcare and LCP began assembling the eICU
#' Collaborative Research Database as a multi-center resource for ICU data.
#' Combining data of several critical care units throughout the continental
#' United States from the years 2014 and 2015, this database contains
#' deidentified health data associated with over 200,000 admissions, including
#' vital sign measurements, care plan documentation, severity of illness
#' measures, diagnosis information, and treatment information. For further
#' information, please refer to the [eICU documentation
#' ](https://eicu-crd.mit.edu/about/eicu).
#'
#' In case the keyring package is available, credentials used to log onto
#' PhysioNet are stored using [keyring::key_set()], therefore allowing for non-
#' interactive querying of the PhysioNet service. Furthermore, if the openssl
#' package is available, SHA256 hashes of downloaded files are verified using
#' [openssl::sha256()].
#'
#' @references
#' MIMIC-III, a freely accessible critical care database. Johnson AEW, Pollard
#' TJ, Shen L, Lehman L, Feng M, Ghassemi M, Moody B, Szolovits P, Celi LA,
#' and Mark RG. Scientific Data (2016). DOI: 10.1038/sdata.2016.35.
#'
#' The eICU Collaborative Research Database, a freely available multi-center
#' database for critical care research. Pollard TJ, Johnson AEW, Raffa JD,
#' Celi LA, Mark RG and Badawi O. Scientific Data (2018). DOI:
#' http://dx.doi.org/10.1038/sdata.2018.178.
#'
#' @param x Object of which a [get_src_config()] method is defined
#' @param dir Destination directory where the downloaded data is written to.
#' @param tables Character vector specifying the tables to download. If
#' `NULL`, all available tables are downloaded.
#' @param ... Passed onto keyring, for example [keyring::key_set_with_value()].
#'
#' @examples
#' \dontrun{
#'
#' dir <- tempdir()
#' list.files(dir)
#'
#' download_datasource("mimic_demo", dir = dir)
#' list.files(dir)
#'
#' unlink(dir, recursive = TRUE)
#'
#' }
#'
#' @export
#'
download_source <- function(x, dir = NULL, tables = NULL, ...) {

  x <- get_src_config(x)

  if (is.null(dir)) {
    dir <- source_data_dir(x)
  }

  assert_that(is.dir(dir))

  tbl_files <- unlist(file_names(x))

  if (not_null(tables)) {

    assert_that(is.character(tables), length(tables) > 0L)

    tbl_files <- tbl_files[table_names(x) %in% tables]
  }

  tbl_files <- setdiff(tbl_files, list.files(dir))

  if (length(tbl_files) == 0L) {
    message("All required files are already available")
    return(invisible(NULL))
  }

  message("downloading `", get_src_name(x), "`")

  download_check_data(dir, tbl_files, get_url(x), ...)
}

read_line <- function(prompt = "", mask_input = FALSE) {

  if (!interactive()) return(NULL)

  if (mask_input && is_pkg_available("getPass")) {
    getPass::getPass(prompt)
  } else {
    readline(prompt)
  }
}

get_set_physionet_creds <- function(username = NULL, password = NULL,
                                    service = "physionet", keyring = NULL) {

  if (is.null(username) || is.null(password)) {

    if (is_pkg_available("keyring")) {

      hits <- keyring::key_list(service, keyring)

      if (nrow(hits) > 0L && (is.null(username) ||
          isTRUE(username %in% hits[["username"]]))) {

        if (is.null(username)) {
          username <- hits[1L, "username"]
        }

        password <- keyring::key_get(service, username, keyring)

      } else {

        message("set up credentials for physionet access")

        if (is.null(username)) {

          username <- read_line("username: ")
          if (is.null(password)) password <- read_line("password: ", TRUE)

        } else if (is.null(password)) {

          password <- read_line(paste0("password for user ", username, ": "),
                                TRUE)
        }

        keyring::key_set_with_value(service, username, password, keyring)
      }

    } else {

      if (is.null(username)) {
        username <- read_line("username: ")
        password <- read_line("password: ", TRUE)
      } else {
        password <- read_line(paste0("password for user ", username, ": "),
                              TRUE)
      }
    }
  }

  assert_that(is.string(username), is.string(password))

  list(username = username, password = password)
}

download_pysionet_file <- function(url, dest = NULL, username = NULL,
                                   password = NULL) {

  assert_that(is.string(url))

  handle <- curl::new_handle(useragent = "Wget/")

  if (is.null(username) || is.null(password)) {

    assert_that(is.null(username), is.null(password))

  } else {

    handle <- curl::handle_setopt(handle,
      username = username, password = password
    )
  }

  if (is.null(dest)) {

    res <- curl::curl_fetch_memory(url, handle)

  } else {

    assert_that(is.string(dest))

    if (file.exists(dest)) {
      handle <- curl::handle_setopt(handle,
        timevalue = file.mtime(dest), timecondition = TRUE
      )
    }

    tmp <- tempfile()
    on.exit(unlink(tmp))

    res <- curl::curl_fetch_disk(url, tmp, handle = handle)

    if (res[["status_code"]] == 304) {
      message("Skipped download of ", basename(url))
      return(invisible(NULL))
    }
  }

  if (res[["status_code"]] == 401) {

    stop("Access to the requested resource was denied. Please set up an ",
         "account at https://physionet.org/ and apply for data access.")

  } else if (res[["status_code"]] != 200) {

    stop(rawToChar(res[["content"]]))
  }

  message("successfully downloaded ", basename(url))

  if (is.null(dest)) {

    res[["content"]]

  } else {

    file.rename(res[["content"]], dest)
    invisible(NULL)
  }
}

get_sha256 <- function(url, username = NULL, password = NULL) {

  res <- download_pysionet_file(
    file.path(url, "SHA256SUMS.txt", fsep = "/"), dest = NULL,
    username = username, password = password
  )

  con <- rawConnection(res)
  on.exit(close(con))

  res <- readLines(con)
  strsplit(res, " ")
}

check_file_sha256 <- function(file, val) {
  isTRUE(as.character(openssl::sha256(file(file, raw = TRUE))) == val)
}

download_check_data <- function(dest_folder, tables, url, username,
                                password, ...) {

  if (missing(username) || missing(password)) {

    if (missing(username)) username <- NULL

    cred <- get_set_physionet_creds(username, ...)
    username <- cred[["username"]]
    password <- cred[["password"]]

  } else {

    username <- NULL
    password <- NULL
  }

  chksums <- get_sha256(url, username, password)
  avail_tbls <- vapply(chksums, `[[`, character(1L), 2L)

  assert_that(all(tables %in% avail_tbls))

  todo <- chksums[avail_tbls %in% tables]

  files <- vapply(todo, `[[`, character(1L), 2L)
  chksums <- vapply(todo, `[[`, character(1L), 1L)

  Map(download_pysionet_file,
    file.path(url, files, fsep = "/"),
    file.path(dest_folder, files),
    MoreArgs = list(username = username, password = password)
  )

  if (is_pkg_available("openssl")) {
    checks <- unlist(
      Map(check_file_sha256, file.path(dest_folder, files), chksums)
    )
    if (!all(checks)) {
      warning("The following files have the wrong checksum:\n  ",
        paste(basename(names(checks))[!checks], collapse = "\n  ")
      )
    }
  } else {
    message("The package openssl is required for checking file hashes.")
  }

  invisible(NULL)
}
