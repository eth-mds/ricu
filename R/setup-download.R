
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
#' Physionet credentials can either be entered in an interactive session,
#' passed as function arguments `user`/`pass` or as environment
#' variables `RICU_PHYSIONET_USER`/`RICU_PHYSIONET_PASS`. If the openssl
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
#' @param x Object specifying the source configuration
#' @param ... Generic consistency
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
#' @rdname download
#' @export
#'
download_src <- function(x, ...) UseMethod("download_src", x)

#' @param dir Destination directory where the downloaded data is written to.
#' @param tables Character vector specifying the tables to download. If
#' `NULL`, all available tables are downloaded.
#' @param force Logical flag; if `TRUE`, existing data will be re-downloaded
#' @param user,pass Physionet credentials; if `NULL` and environment
#' variables `RICU_PHYSIONET_USER`/`RICU_PHYSIONET_PASS` are not set, user
#' input is required
#'
#' @rdname download
#' @export
download_src.src_cfg <- function(x, dir = src_data_dir(x), tables = NULL,
                                 force = FALSE, user = NULL, pass = NULL,
                                 ...) {

  all_avail <- function(y) all(file.exists(file.path(dir, y)))

  assert_that(is.dir(dir), is.flag(force), ...length() == 0L)

  tbl <- as_tbl_spec(x)

  if (is.null(tables)) {
    tables <- names(tbl)
  }

  assert_that(is.character(tables), all(tables %in% names(tbl)))

  if (!force) {

    avail <- names(tbl)[
      lgl_ply(fst_names(tbl), all_avail) | lgl_ply(file_names(tbl), all_avail)
    ]

    tables <- setdiff(tables, avail)
  }

  if (length(tables) == 0L) {
    message("The requested tables have already been downloaded")
    return(invisible(NULL))
  }

  tbl <- tbl[tables]

  pba <- progr_init(sum(n_rows(tbl)),
    paste0("Downloading ", length(tbl), " tables for ", quote_bt(src_name(x)))
  )

  download_check_data(dir, file_names(tbl), src_url(x), user, pass, pba)

  if (!(is.null(pba) || pba$finished)) {
    pba$update(1)
    pba$terminate()
  }

  invisible(NULL)
}

#' @inheritParams read_src_cfg
#' @rdname download
#' @export
download_src.character <- function(x, name = "data-sources", file = NULL,
                                   ...) {

  for (cfg in read_src_cfg(x, name, file)) {
    download_src(cfg, ...)
  }

  invisible(NULL)
}

read_line <- function(prompt = "", mask_input = FALSE) {

  assert_that(interactive(), msg = "User input is required")

  if (mask_input && is_pkg_available("getPass")) {
    getPass::getPass(prompt)
  } else {
    readline(prompt)
  }
}

download_pysionet_file <- function(url, dest = NULL, user = NULL,
                                   pass = NULL) {

  assert_that(is.string(url))

  handle <- curl::new_handle(useragent = "Wget/")

  if (is.null(user) || is.null(pass)) {

    assert_that(is.null(user), is.null(pass))

  } else {

    handle <- curl::handle_setopt(handle, username = user, password = pass)
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

  if (is.null(dest)) {

    res[["content"]]

  } else {

    file.rename(res[["content"]], dest)
    invisible(NULL)
  }
}

get_sha256 <- function(url, user = NULL, pass = NULL) {

  res <- download_pysionet_file(
    file.path(url, "SHA256SUMS.txt", fsep = "/"), dest = NULL,
    user = user, pass = pass
  )

  con <- rawConnection(res)
  on.exit(close(con))

  res <- readLines(con)
  strsplit(res, " ")
}

check_file_sha256 <- function(file, val) {
  isTRUE(as.character(openssl::sha256(file(file, raw = TRUE))) == val)
}

get_cred <- function(x, env, msg, hide = FALSE) {

  assert_that(is.string(env), is.string(msg), is.flag(hide))

  if (is.null(x)) {
    x <- Sys.getenv(env, unset = NA_character_)
    if (is.na(x)) {
      x <- read_line(msg, hide)
    }
  }

  assert_that(is.string(x))

  x
}

download_check_data <- function(dest_folder, tables, url, user, pass,
                                prog = NULL) {

  check_cred <- function(e) {
    if (grepl("^Access.+access\\.$", conditionMessage(e))) NULL else stop(e)
  }

  dl_one <- function(file, path) {

    progr_iter(file, prog)

    download_pysionet_file(file.path(url, file, fsep = "/"), path,
                           user = user, pass = pass)

    invisible(NULL)
  }

  chksums <- tryCatch(get_sha256(url, user, pass), error = check_cred)

  if (is.null(chksums)) {

    user <- get_cred(user, "RICU_PHYSIONET_USER", "username: ")
    pass <- get_cred(pass, "RICU_PHYSIONET_PASS", "password: ", TRUE)

    chksums <- get_sha256(url, user, pass)
  }

  avail_tbls <- vapply(chksums, `[[`, character(1L), 2L)

  assert_that(all(tables %in% avail_tbls))

  todo <- chksums[avail_tbls %in% tables]

  files   <- vapply(todo, `[[`, character(1L), 2L)
  chksums <- vapply(todo, `[[`, character(1L), 1L)
  paths   <- file.path(dest_folder, files)

  Map(dl_one, files, paths)

  if (is_pkg_available("openssl")) {

    checks <- mapply(check_file_sha256, paths, chksums)

    if (!all(checks)) {
      warning("The following files have the wrong checksum:\n  ",
              paste(files[!checks], collapse = "\n  "))
    }

  } else {

    message("The package openssl is required for checking file hashes.")
  }

  invisible(NULL)
}
