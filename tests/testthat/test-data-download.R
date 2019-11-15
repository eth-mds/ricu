
test_that("credential management", {

  dat.fr <- function(...) data.frame(..., stringsAsFactors = FALSE)

  creds <- with_mock(
    `keyring::key_set_with_value` = function(...) invisible(NULL),
    get_set_physionet_creds("foo", "bar")
  )

  expect_identical(creds, list(username = "foo", password = "bar"))

  creds <- with_mock(
    `keyring::key_list` = function(...) {
      dat.fr(service = "physionet", username = "foo")
    },
    `keyring::key_get` = function(...) "bar",
    `keyring::key_set_with_value` = function(...) invisible(NULL),
    get_set_physionet_creds()
  )

  expect_identical(creds, list(username = "foo", password = "bar"))

  creds <- with_mock(
    `keyring::key_set_with_value` = function(...) invisible(NULL),
    `ricu:::read_line` = function(...) "bar",
    get_set_physionet_creds("foo")
  )

  expect_identical(creds, list(username = "foo", password = "bar"))

  creds <- with_mock(
    `ricu:::is_pkg_available` = function(...) FALSE,
    `ricu:::read_line` = function(x, ...) {
      if (grepl("user", x)) "foo" else if (grepl("pass", x)) "bar" else "baz"
    },
    get_set_physionet_creds()
  )

  expect_identical(creds, list(username = "foo", password = "bar"))

  expect_error(
    with_mock(
      `ricu:::is_pkg_available` = function(...) FALSE,
      get_set_physionet_creds()
    )
  )
})

curl_mock_fm <- function(url, handle) {

  file <- system.file("testdata", paste0(basename(url), ".rds"),
                      package = "ricu")

  if (identical(file, "")) stop("file ", basename(url), "not found")

  readRDS(file)
}

curl_mock_fd <- function(url, path, handle) {

  if (!dir.exists(path)) dir.create(path)

  dat <- curl_mock_fm(url, handle)

  dest_file <- file.path(path, basename(url))

  # if e.g. timecondition opt can be queried from handle, a 304 could be
  # returned to mimic the no-change scenario

  writeBin(dat[["content"]], dest_file)
  dat[["content"]] <- dest_file

  dat
}

tmp <- tempfile()

setup(dir.create(tmp))
teardown(unlink(tmp, recursive = TRUE))

test_that("file download", {

  dat_mem <- with_mock(
    `curl::curl_fetch_memory` = curl_mock_fm,
    `curl::curl_fetch_disk` = function(...) stop("error"),
    download_pysionet_file("foo/bar/SHA256SUMS.txt")
  )

  expect_is(dat_mem, "raw")
  expect_length(list.files(tmp), 0L)

  dat_disk <- with_mock(
    `curl::curl_fetch_memory` = function(...) stop("error"),
    `curl::curl_fetch_disk` = curl_mock_fd,
    download_pysionet_file("foo/bar/SHA256SUMS.txt",
                           file.path(tmp, "SHA256SUMS.txt"))
  )

  expect_is(dat_disk, "NULL")
  expect_length(list.files(tmp, "SHA256SUMS.txt"), 1L)
})

test_that("hash checking", {

  sha <- with_mock(
    `curl::curl_fetch_memory` = curl_mock_fm,
    `curl::curl_fetch_disk` = function(...) stop("error"),
    get_sha256("foo/bar")
  )

  expect_is(sha, "list")
  for (x in sha) {
    expect_is(x, "character")
    expect_length(x, 2L)
  }

  with_mock(
    `curl::curl_fetch_memory` = function(...) stop("error"),
    `curl::curl_fetch_disk` = curl_mock_fd,
    download_pysionet_file("foo/bar/patients.csv",
                           file.path(tmp, sha[[2L]][2L]))
  )

  expect_true(
    check_file_sha256(file.path(tmp, sha[[2L]][2L]), sha[[2L]][1L])
  )
  expect_false(
    check_file_sha256(file.path(tmp, sha[[2L]][2L]), sha[[1L]][1L])
  )
})
