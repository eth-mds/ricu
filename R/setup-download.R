
#' Data download utilities
#'
#' Making a dataset available to `ricu` consists of 3 steps: downloading
#' ([download_src()]), importing ([import_src()]) and attaching
#' ([attach_src()]). While downloading and importing are one-time procedures,
#' attaching of the dataset is repeated every time the package is loaded.
#' Briefly, downloading loads the raw dataset from the internet (most likely
#' in `.csv` format), importing consists of some preprocessing to make the
#' data available more efficiently (by converting it to [`.fst`][fst::fst()]
#' format) and attaching sets up the data for use by the package.
#'
#' @details
#' Downloads by `ricu` are focused data hosted by
#' [PhysioNet](https://physionet.org) and tools are currently available for
#' downloading the datasets
#' [MIMIC-III](https://physionet.org/content/mimiciii/1.4/),
#' [eICU](https://physionet.org/content/eicu-crd/2.0/) and
#' [HiRID](https://physionet.org/content/hirid/1.0/) (see [data]). While
#' credentials are required for downloading any of the three datasets, demo
#' dataset for both MIMIC-III and eICU are available without having to log in.
#' Even though access to full dataset is credentialed, the datasets are in
#' fact publicly available. For setting up an account, please refer to [the
#' registration form](https://physionet.org/register/).
#'
#' PhysioNet credentials can either be entered in an interactive session,
#' passed as function arguments `user`/`pass` or as environment
#' variables `RICU_PHYSIONET_USER`/`RICU_PHYSIONET_PASS`. For setting
#' environment variables on session startup, refer to [base::.First.sys()] and
#' for setting environment variables in general, refer to [base::Sys.setenv()]
#' If the openssl package is available, SHA256 hashes of downloaded files are
#' verified using [openssl::sha256()].
#'
#' Demo datasets
#' [MIMIC-III demo](https://physionet.org/content/mimiciii-demo/1.4/) and
#' [eiCU demo](https://physionet.org/content/eicu-crd-demo/2.0/) can either be
#' installed as R packages directly by running
#'
#' ```
#' install.packages(
#'   c("mimic.demo", "eicu.demo"),
#'   repos = "https://septic-tank.github.io/physionet-demo"
#' )
#' ```
#'
#' or downloaded and imported using [download_src()] and [import_src()].
#' Furthermore, `ricu` specifies `mimic.demo` and `eicu.demo` as `Suggests`
#' dependencies therefore, passing `dependencies = TURE` when calling
#' [install.packages()] for installing `ricu`, this will automatically install
#' the demo datasets as well.
#'
#' While the included data downloaders are intended for data hosted by
#' PhysioNet, `download_src()` is an S3 generic function that can be extended
#' to new classes. Method dispatch is intended to occur on objects that
#' inherit from or can be coerced to `src_cfg`. For more information on data
#' source configuration, refer to [load_src_cfg()].
#'
#' As such, with the addition of the AmsterdamUMCdb dataset, which
#' unfortunately is not hosted on PhysioNet, A separate downloader for that
#' dataset is available as well. Currently this requires both availability of
#' the CRAN package `xml2`, as well as the command line utility 7zip.
#' Furthermore, data access has to be [requested
#' ](https://amsterdammedicaldatascience.nl/#amsterdamumcdb) and for
#' non-interactive download the download token has to be made available as
#' environment variable `RICU_AUMC_TOKEN` or passed as `token` argument to
#' `download_src()`. The download token can be retrieved from the URL provided
#' when granted access as by extracting the string followed by `token=`:
#'
#' ```
#' https://example.org/?s=download&token=0c27af59-72d1-0349-aa59-00000a8076d9
#' ```
#'
#' would translate to
#'
#' ```{r, eval = FALSE}
#' Sys.setenv(RICU_AUMC_TOKEN = "0c27af59-72d1-0349-aa59-00000a8076d9")
#' ```
#'
#' If the dependencies outlined above are not fulfilled, download and archive
#' extraction can be carried out manually into the corresponding folder and
#' [import_src()] can be run.
#'
#' @param x Object specifying the source configuration
#' @param data_dir Destination directory where the downloaded data is written
#' to.
#' @param ... Generic consistency
#'
#' @importFrom curl new_handle handle_setopt parse_headers
#'
#' @return Called for side effects and returns `NULL` invisibly.
#'
#' @examples
#' \dontrun{
#'
#' dir <- tempdir()
#' list.files(dir)
#'
#' download_datasource("mimic_demo", data_dir = dir)
#' list.files(dir)
#'
#' unlink(dir, recursive = TRUE)
#'
#' }
#'
#' @rdname download
#' @export
#'
download_src <- function(x, data_dir = src_data_dir(x), ...) {

  assert_that(has_length(ensure_dirs(data_dir)))

  UseMethod("download_src", x)
}

#' @param tables Character vector specifying the tables to download. If
#' `NULL`, all available tables are downloaded.
#' @param force Logical flag; if `TRUE`, existing data will be re-downloaded
#'
#' @rdname download
#' @export
download_src.src_cfg <- function(x, data_dir = src_data_dir(x), tables = NULL,
                                 force = FALSE, ...) {

  tbl <- determine_tables(x, data_dir, tables, force)

  if (length(tbl) == 0L) {
    msg_ricu("The requested tables have already been downloaded",
             class = "no_dl_required")
    return(invisible(NULL))
  }

  files <- unlst_str(raw_file_names(tbl))

  download_check_data(data_dir, files, src_url(x), src_name(x), ...)

  invisible(NULL)
}

#' @export
download_src.hirid_cfg <- function(x, data_dir = src_data_dir(x),
                                   tables = NULL, force = FALSE, ...) {

  tbl <- determine_tables(x, data_dir, tables, force)

  if (length(tbl) == 0L) {
    msg_ricu("The requested tables have already been downloaded",
             class = "no_dl_required")
    return(invisible(NULL))
  }

  todo <- field(tbl, "zip_file")
  fils <- unique(unlst_str(todo))

  tmp <- ensure_dirs(tempfile())
  on.exit(unlink(tmp, recursive = TRUE))

  download_check_data(tmp, fils, src_url(x), src_name(x), ...)

  res <- Map(ricu_untar, Map(file.path, tmp, todo), raw_file_names(tbl),
             MoreArgs = list(exdir = data_dir))

  assert_that(all_fun(res, identical, 0L))

  invisible(NULL)
}

ricu_untar <- function(...) utils::untar(...)

#' @param token Download token for AmsterdamUMCdb (see 'Details')
#' @param verbose Logical flag indicating whether to print progress information
#'
#' @rdname download
#' @importFrom utils unzip
#' @export
download_src.aumc_cfg <- function(x, data_dir = src_data_dir(x),
                                  tables = NULL, force = FALSE, token = NULL,
                                  verbose = TRUE, ...) { # nocov start

  warn_dots(..., ok_args = c("user", "pass"))

  if (!requireNamespace("xml2", quietly = TRUE)) {
    stop_ricu("Download of `aumc` data requires the `xml2` package. If
               unavailable, download and unzip the data manually
               to {data_dir}", class = "aumc_dl")
  }

  uzp <- getOption("unzip")

  if (identical(uzp, "internal")) {
    stop_ricu("Download of `aumc` data requires a path to an `unzip`
               binary returned by `getOption(\"unzip\")` in order to unzip
               files that are >4GB", class = "aumc_dl")
  }

  tbl <- determine_tables(x, data_dir, tables, force)

  if (length(tbl) == 0L) {
    msg_ricu("The requested tables have already been downloaded",
             class = "no_dl_required")
    return(invisible(NULL))
  }

  tok <- get_cred(token, "RICU_AUMC_TOKEN", "token: ")

  url <- paste0("https://filesender.surf.nl/?s=download&token=", tok)
  res <- download_file(url)

  if (res[["status_code"]] == 200) {

    info <- xml2::read_html(rawToChar(res[["content"]]))

    size <- xml2::xml_find_first(info,
                                 "//div[contains(@class, 'general box')]")
    size <- as.numeric(xml2::xml_attr(size, "data-transfer-size"))

    info <- xml2::xml_find_first(info,
      "//div[contains(@class, 'files box')]/div[contains(@class, 'file')]"
    )

    name <- xml2::xml_attr(info, "data-name")
    url  <- xml2::xml_attr(xml2::xml_child(info, "a"), "href")

    if (!is.na(size) && !is.na(name) && !is.na(info)) {

      tmp <- ensure_dirs(tempfile())
      on.exit(unlink(tmp, recursive = TRUE))

      fil <- file.path(tmp, name)

      if (isTRUE(verbose)) {
        prg <- progress_init(size, msg = "Donwloading `aumc` data",
                             what = FALSE)
      } else {
        prg <- FALSE
      }

      res <- download_file(url, dest = fil, progr = prg)

      if (res[["status_code"]] == 200) {

        unzip(fil, files = chr_ply(tbl, raw_file_name),
              exdir = normalizePath(data_dir), overwrite = TRUE, unzip = uzp)

        return(invisible(NULL))
      }
    }
  }

  stop_ricu("Could not successfully download `aumc` data. Please download and
             extract the corresponding .zip archive manually to {data_dir}.",
            class = "aumc_dl")
} # nocov end

#' @param user,pass PhysioNet credentials; if `NULL` and environment
#' variables `RICU_PHYSIONET_USER`/`RICU_PHYSIONET_PASS` are not set, user
#' input is required
#'
#' @rdname download
#' @export
download_src.character <- function(x, data_dir = src_data_dir(x),
                                   tables = NULL, force = FALSE,
                                   user = NULL, pass = NULL, verbose = TRUE,
                                   ...) {

  if (is.null(tables)) {
    tables <- list(tables)
  }

  Map(download_src, load_src_cfg(x, ...), data_dir, tables,
    MoreArgs = list(force = force, user = user, pass = pass, verbose = verbose)
  )

  invisible(NULL)
}

#' @export
download_src.default <- function(x, ...) stop_generic(x, .Generic)

determine_tables <- function(x, dir, tables, force) {

  x <- as_tbl_cfg(x)

  if (is.null(tables)) {
    tables <- names(x)
  }

  assert_that(is.character(tables), are_in(tables, names(x)),
              is.dir(dir), is.flag(force))

  if (!force) {
    avail  <- names(x)[src_file_exist(x, dir, "fst") |
                       src_file_exist(x, dir, "raw")]
    tables <- setdiff(tables, avail)
  }

  x[tables]
}

download_file <- function(url, handle = new_handle(), dest = NULL,
                          progr = NULL) {

  if (is.null(dest)) {
    return(curl::curl_fetch_memory(url, handle))
  }

  if (is.null(progr)) {
    return(curl::curl_fetch_disk(url, dest, handle = handle))
  }

  con <- file(dest, "ab", blocking = FALSE)
  on.exit(close(con))

  prog_fun <- function(x) {
    progress_tick(NULL, progr, length(x))
    writeBin(x, con)
  }

  curl::curl_fetch_stream(url, prog_fun, handle = handle)
}

download_pysionet_file <- function(url, dest = NULL, user = NULL,
                                   pass = NULL, head_only = FALSE,
                                   progress = NULL) {

  assert_that(is.string(url), null_or(dest, is.string), is.flag(head_only))

  handle <- new_handle(useragent = "Wget/")

  if (is.null(user) || is.null(pass)) {

    assert_that(is.null(user), is.null(pass))

  } else {

    handle <- handle_setopt(handle, username = user, password = pass)
  }

  if (is.null(dest) && head_only) {

    handle <- handle_setopt(handle, nobody = TRUE)

  } else if (not_null(dest) && file.exists(dest)) {

    handle <- handle_setopt(handle,
      timevalue = file.mtime(dest), timecondition = TRUE
    )
  }

  res <- download_file(url, handle, dest, progress)

  status <- res[["status_code"]]

  if (status == 304) {

    msg_ricu("Skipped download of {basename(url)}")
    return(invisible(NULL))

  } else if (status == 401) {

    stop_ricu("Access to the requested resource was denied. Please set up an
               account at https://physionet.org/ and apply for data access.",
              class = "physionet_401")

  } else if (status != 200) {

    stop_ricu(rawToChar(res[["content"]]),
              class = paste0("physionet_", status))
  }

  if (head_only) {

    res

  } else if (is.null(dest)) {

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

get_cred <- function(x, env, msg) {

  assert_that(is.string(env), is.string(msg))

  if (is.null(x)) {

    x <- sys_env(env, unset = NA_character_)

    if (is.na(x)) {

      if (!is_interactive()) {
        stop_ricu("User input is required")
      }

      x <- read_line(msg)
    }
  }

  assert_that(is.string(x))

  x
}

read_line <- function(prompt = "") readline(prompt)

get_file_size <- function(url, user, pass) {

  resp <- download_pysionet_file(url, NULL, user, pass, TRUE)
  resp <- parse_headers(resp$headers)

  hit <- grepl("^Content-Length:", resp, ignore.case = TRUE)

  if (sum(hit) != 1L) {
    return(NULL)
  }

  as.numeric(sub("^Content-Length: ", "", resp[hit], ignore.case = TRUE))
}

download_check_data <- function(dest_folder, files, url, src,
                                user = NULL, pass = NULL, verbose = TRUE,
                                ...) {

  dl_one <- function(url, size, path, prog) {

    progress_tick(basename(url), prog, 0L)

    download_pysionet_file(url, path, user, pass, progress = prog)

    invisible(NULL)
  }

  warn_dots(..., ok_args = "token")

  chksums <- tryCatch(
    get_sha256(url, user, pass),
    physionet_401 = function(err) NULL
  )

  if (is.null(chksums)) {

    user <- get_cred(user, "RICU_PHYSIONET_USER", "username: ")
    pass <- get_cred(pass, "RICU_PHYSIONET_PASS", "password: ")

    chksums <- get_sha256(url, user, pass)
  }

  avail_tbls <- vapply(chksums, `[[`, character(1L), 2L)

  assert_that(are_in(files, avail_tbls))

  todo <- chksums[match(files, avail_tbls)]

  files   <- vapply(todo, `[[`, character(1L), 2L)
  chksums <- vapply(todo, `[[`, character(1L), 1L)
  paths   <- file.path(dest_folder, files)
  urls    <- file.path(url, files, fsep = "/")

  ensure_dirs(dirname(paths))
  unlink(paths)

  sizes <- dbl_ply(urls, get_file_size, user, pass)

  if (isTRUE(verbose)) {
    pba <- progress_init(sum(sizes),
      msg = "Downloading {length(files)} file{?s} for {quote_bt(src)}"
    )
  } else {
    pba <- FALSE
  }

  with_progress(
    Map(dl_one, urls, sizes, paths, MoreArgs = list(prog = pba)),
    progress_bar = pba
  )

  if (isTRUE(verbose)) {
    msg_ricu("Comparing checksums")
  }

  checks <- mapply(check_file_sha256, paths, chksums)

  if (!all(checks)) {
    warn_ricu(
      c("Checksum mismatch for {qty(sum(!checks))} file{?s}:",
        bullet(files[!checks])),
      class = "checksum_mismatch", exdent = c(0L, rep(2L, sum(!checks)))
    )
  }

  invisible(NULL)
}
