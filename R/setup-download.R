
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
#' @importFrom utils untar
#' @importFrom curl new_handle handle_setopt parse_headers
#' @importFrom curl curl_fetch_disk curl_fetch_stream curl_fetch_memory
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

  warn_dots(...)

  assert_that(is.string(dir), is.flag(force))

  tmp <- tempfile()
  on.exit(unlink(tmp, recursive = TRUE))

  ensure_dirs(c(tmp, dir))

  tbl <- as_tbl_cfg(x)

  if (is.null(tables)) {
    tables <- names(tbl)
  }

  assert_that(is.character(tables), all(tables %in% names(tbl)))

  if (!force) {

    avail <- names(tbl)[lgl_ply(lapply(tbl, fst_names), all_avail) |
                        lgl_ply(lapply(tbl, dst_files), all_avail)]

    tables <- setdiff(tables, avail)
  }

  if (length(tables) == 0L) {
    message("The requested tables have already been downloaded")
    return(invisible(NULL))
  }

  tbl <- tbl[tables]

  to_dl <- unique(
    unlist(lapply(tbl, src_files), recursive = FALSE, use.names = FALSE)
  )

  download_check_data(tmp, to_dl, src_url(x), user, pass, src_name(x))

  to_unpck <- grepl("\\.tar(\\.gz)?$", to_dl)

  if (any(to_unpck)) {
    for (file in file.path(tmp, to_dl[to_unpck])) {
      untar(file, exdir = tmp)
    }
  }

  to_mv <- unlist(lapply(tbl, dst_files), recursive = FALSE, use.names = FALSE)
  targ  <- file.path(dir, to_mv)

  ensure_dirs(unique(dirname(targ)))

  file.rename(file.path(tmp, to_mv), targ)

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
                                   pass = NULL, head_only = FALSE,
                                   progress = NULL) {

  assert_that(is.string(url), is.flag(head_only))

  handle <- new_handle(useragent = "Wget/")

  if (is.null(user) || is.null(pass)) {

    assert_that(is.null(user), is.null(pass))

  } else {

    handle <- handle_setopt(handle, username = user, password = pass)
  }

  if (is.null(dest)) {

    assert_that(is.null(progress))

    if (head_only) {
      return(curl_fetch_memory(url, handle_setopt(handle, nobody = TRUE)))
    }

    res <- curl_fetch_memory(url, handle)

  } else {

    assert_that(is.string(dest), !head_only)

    if (file.exists(dest)) {
      handle <- handle_setopt(handle,
        timevalue = file.mtime(dest), timecondition = TRUE
      )
    }

    if (is.null(progress)) {

      res <- curl_fetch_disk(url, dest, handle = handle)

    } else {

      con <- file(dest, "ab", blocking = FALSE)
      on.exit(close(con))

      prog_fun <- function(x) {
        progr_iter(basename(url), progress, length(x))
        writeBin(x, con)
      }

      res <- curl_fetch_stream(url, prog_fun, handle = handle)
    }

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

get_file_size <- function(url, user, pass) {

  resp <- download_pysionet_file(url, NULL, user, pass, TRUE)
  resp <- parse_headers(resp$headers)

  hit <- grepl("^Content-Length:", resp)

  if (sum(hit) != 1L) {
    return(NULL)
  }

  as.numeric(sub("^Content-Length: ", "", resp[hit]))
}

download_check_data <- function(dest_folder, files, url, user, pass, src) {

  check_cred <- function(e) {
    if (grepl("^Access.+access\\.$", conditionMessage(e))) NULL else stop(e)
  }

  dl_one <- function(url, size, path, prog) {

    if (is.null(prog)) {
      progr_iter(basename(url), NULL, size)
    }

    download_pysionet_file(url, path, user, pass, progress = prog)

    invisible(NULL)
  }

  chksums <- tryCatch(get_sha256(url, user, pass), error = check_cred)

  if (is.null(chksums)) {

    user <- get_cred(user, "RICU_PHYSIONET_USER", "username: ")
    pass <- get_cred(pass, "RICU_PHYSIONET_PASS", "password: ", TRUE)

    chksums <- get_sha256(url, user, pass)
  }

  avail_tbls <- vapply(chksums, `[[`, character(1L), 2L)

  assert_that(all(files %in% avail_tbls))

  todo <- chksums[match(files, avail_tbls)]

  files   <- vapply(todo, `[[`, character(1L), 2L)
  chksums <- vapply(todo, `[[`, character(1L), 1L)
  paths   <- file.path(dest_folder, files)
  urls    <- file.path(url, files, fsep = "/")

  ensure_dirs(dirname(paths))

  sizes <- dbl_ply(urls, get_file_size, user, pass)

  pba <- progr_init(sum(sizes),
    paste0("Downloading ", length(files), " files for ", quote_bt(src))
  )

  Map(dl_one, urls, sizes, paths, MoreArgs = list(prog = pba))

  if (!(is.null(pba) || pba$finished)) {
    pba$update(1)
    pba$terminate()
  }

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
