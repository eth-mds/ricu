
download_mimic <- function(version = "1.4", demo = FALSE,
  dest = if (demo) data_dir("mimic-demo") else data_dir("mimic"), ...) {

  if (demo) {

    message("downloading mimic-iii v", version, " demo")

    download_pysionet_data(dest,
      paste0("https://physionet.org/files/mimiciii-demo/", version),
      username = NULL, password = NULL
    )

  } else {

    message("downloading mimic-iii v", version, " data")

    download_pysionet_data(dest,
      paste0("https://physionet.org/files/mimiciii/", version), ...
    )
  }
}

download_eicu <- function(version = "2.0", demo = FALSE,
  dest = if (demo) data_dir("eicu-demo") else data_dir("eicu"), ...) {

  if (demo) {

    message("downloading eicu v", version, " demo")

    download_pysionet_data(dest,
      paste0("https://physionet.org/files/eicu-crd-demo/", version),
      username = NULL, password = NULL
    )

  } else {

    message("downloading eicu v", version, " data")

    download_pysionet_data(dest,
      paste0("https://physionet.org/files/eicu-crd/", version), ...
    )
  }
}

get_physionet_creds <- function(username = NULL, service = "physionet",
                                keyring = NULL) {

  if (requireNamespace("getPass", quietly = TRUE)) {
    get_pass <- getPass::getPass
  } else {
    get_pass <- readline
  }

  if (requireNamespace("keyring", quietly = TRUE)) {

    hits <- keyring::key_list(service, keyring)

    if (nrow(hits) > 0L && (is.null(username) ||
        isTRUE(username %in% hits[["username"]]))) {

      if (is.null(username)) username <- hits[1L, "username"]
      password <- keyring::key_get(service, username, keyring)

    } else {

      message("set up credentials for physionet access")

      if (is.null(username)) {
        username <- readline("username: ")
        password <- get_pass("password: ")
      } else {
        password <- get_pass(paste0("password for user ", username, ": "))
      }

      keyring::key_set_with_value(service, username, password, keyring)
    }

  } else {

    if (is.null(username)) {
      username <- readline("username: ")
      password <- get_pass("password: ")
    } else {
      password <- get_pass(paste0("password for user ", username, ": "))
    }
  }

  list(username = username, password = password)
}

download_pysionet_data <- function(dest_folder, url, username, password, ...) {

  check_hash <- function(file, val) {

    if (requireNamespace("openssl", quietly = TRUE)) {

      isTRUE(as.character(openssl::sha256(file(file, raw = TRUE))) == val)

    } else {

      TRUE
    }

  }

  fetch_file <- function(x) {

    file <- file.path(dest_folder, x[2L])

    if (file.exists(file)) {
      handle <- curl::handle_setopt(handle,
        timevalue = file.mtime(file), timecondition = TRUE
      )
    } else {
      handle <- curl::handle_setopt(handle, timecondition = FALSE)
    }

    tmp <- tempfile()
    on.exit(unlink(tmp))

    res <- curl::curl_fetch_disk(paste0(url, "/", x[2L]), tmp,
                                 handle = handle)

    if (res[["status_code"]] == 304) {

      assert_that(check_hash(file, x[1L]))

      message("skipped download of ", x[2L])

    } else if (res[["status_code"]] == 200) {

      file.rename(tmp, file)

      assert_that(check_hash(file, x[1L]))

      message("successfully downloaded ", x[2L])

    } else {

      stop("download of ", x[2L], " failed with status code ",
           res[["status_code"]])
    }
  }

  if (missing(username) || missing(password)) {

    if (missing(username)) username <- NULL

    cred <- get_physionet_creds(username, ...)

    handle <- curl::new_handle(
      useragent = "Wget/", username = cred[["username"]],
      password = cred[["password"]]
    )

  } else {

    handle <- curl::new_handle(useragent = "Wget/")
  }

  chksums <- curl::curl_fetch_memory(file.path(url, "SHA256SUMS.txt"), handle)

  assert_that(chksums[["status_code"]] == 200)

  con <- rawConnection(chksums$content)
  on.exit(close(con))

  chksums <- readLines(con)
  chksums <- strsplit(chksums, " ")

  chksums <- chksums[
    grepl("\\.csv.gz", vapply(chksums, `[[`, character(1L), 2L))
  ]

  lapply(chksums, fetch_file)

  invisible(NULL)
}
