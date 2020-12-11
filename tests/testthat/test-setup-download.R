
curl_mock_fm <- function(url, handle, ...) {

  file <- system.file("testdata", paste0(basename(url), ".rds"),
                      package = "ricu")

  if (identical(file, "")) stop("file ", basename(url), "not found")

  readRDS(file)
}

curl_mock_fd <- function(url, path, handle, ...) {

  dat <- curl_mock_fm(url, handle)

  # if e.g. timecondition opt can be queried from handle, a 304 could be
  # returned to mimic the no-change scenario

  writeBin(dat[["content"]], path)

  dat
}

curl_mock_st <- function(url, fun, handle, ...) {

  dat <- curl_mock_fm(url, handle)

  fun(dat[["content"]])

  dat
}

tmp <- tempfile()

setup(dir.create(tmp))

teardown({
  unlink(tmp, recursive = TRUE)
  Sys.unsetenv("FOO_VAR")
})

test_that("credentials", {

  Sys.setenv(FOO_VAR = "bar_val")

  expect_silent(crd <- get_cred("foo_val", "FOO_VAR", "input"))
  expect_identical(crd, "foo_val")

  expect_silent(crd <- get_cred(NULL, "FOO_VAR", "input"))
  expect_identical(crd, "bar_val")

  Sys.unsetenv("FOO_VAR")

  crd <- with_mock(
    readline = function(prompt = "") {
      stopifnot(identical(prompt, "input"))
      "baz_val"
    },
    get_cred(NULL, "BAR_VAR", "input")
  )

  expect_identical(crd, "baz_val")
})

test_that("file size", {

  pat_siz <- with_mock(
    `curl::curl_fetch_memory` = curl_mock_fm,
    `curl::curl_fetch_disk` = function(...) stop("error"),
    `curl::curl_fetch_stream` = function(...) stop("error"),
    get_file_size("foo/bar/PATIENTS.csv", NULL, NULL)
  )

  expect_is(pat_siz, "numeric")
  expect_length(pat_siz, 1L)
})

test_that("hash checking", {

  sha <- with_mock(
    `curl::curl_fetch_memory` = curl_mock_fm,
    `curl::curl_fetch_disk` = function(...) stop("error"),
    `curl::curl_fetch_stream` = function(...) stop("error"),
    get_sha256("foo/bar")
  )

  expect_is(sha, "list")

  for (x in sha) {
    expect_is(x, "character")
    expect_length(x, 2L)
  }

  res <- with_mock(
    `curl::curl_fetch_memory` = function(...) stop("error"),
    `curl::curl_fetch_disk` = curl_mock_fd,
    `curl::curl_fetch_stream` = function(...) stop("error"),
    download_pysionet_file("foo/bar/PATIENTS.csv",
                           file.path(tmp, sha[[2L]][2L]))
  )

  expect_null(res)

  expect_false(
    check_file_sha256(file.path(tmp, sha[[2L]][2L]), sha[[2L]][1L])
  )
  expect_true(
    check_file_sha256(file.path(tmp, sha[[2L]][2L]), sha[[1L]][1L])
  )
})

test_that("file download", {

  unlink(list.files(tmp, full.names = TRUE))

  dat_mem <- with_mock(
    `curl::curl_fetch_memory` = curl_mock_fm,
    `curl::curl_fetch_disk` = function(...) stop("error"),
    `curl::curl_fetch_stream` = function(...) stop("error"),
    download_pysionet_file("foo/bar/SHA256SUMS.txt")
  )

  expect_is(dat_mem, "raw")
  expect_identical(list.files(tmp), character(0L))

  unlink(list.files(tmp, full.names = TRUE))

  dat_disk <- with_mock(
    `curl::curl_fetch_memory` = function(...) stop("error"),
    `curl::curl_fetch_disk` = curl_mock_fd,
    `curl::curl_fetch_stream` = function(...) stop("error"),
    download_pysionet_file("foo/bar/SHA256SUMS.txt",
                           file.path(tmp, "SHA256SUMS.txt"))
  )

  expect_is(dat_disk, "NULL")
  expect_identical(list.files(tmp, "SHA256SUMS.txt"), "SHA256SUMS.txt")

  unlink(list.files(tmp, full.names = TRUE))

  tables <- c("PATIENTS.csv", "SERVICES.csv")

  with_mock_curl <- function(...) {
    with_mock(
      `curl::curl_fetch_memory` = curl_mock_fm,
      `curl::curl_fetch_disk` = curl_mock_fd,
      `curl::curl_fetch_stream` = curl_mock_st,
      ...
    )
  }

  res <- with_mock_curl(
    download_check_data(tmp, tables, "foo/bar", NULL, NULL, "foo")
  )

  expect_null(res)
  expect_setequal(list.files(tmp), tables)

  expect_warning(
    with_mock_curl(
      check_file_sha256 = function(...) FALSE,
      download_check_data(tmp, tables, "foo/bar", NULL, NULL, "foo")
    ), class = "checksum_mismatch"
  )

  expect_error(
    with_mock_curl(
      download_check_data(tmp, toupper(tables), "foo/bar", NULL, NULL, "foo")
    ), class = "are_in_assert"
  )
})

test_that("src download", {

  srcs <- c("mimic_demo", "eicu_demo")
  dirs <- file.path(tmp, srcs)

  expect_true(all(lgl_ply(dirs, dir.create)))

  with_mock_dl_check <- function(...) {
    with_mock(
      download_check_data = function(dest_folder, files, url, user, pass,
                                     src) {
        assert_that(
          is.character(files), has_length(files), is.string(dest_folder),
          is.string(url)
        )
        invisible(NULL)
      },
      ...
    )
  }

  expect_invisible(res <- with_mock_dl_check(download_src(srcs, dirs)))
  expect_null(res)

  src <- "hirid"
  dir <- file.path(tmp, src)

  expect_true(dir.create(dir))

  res <- with_mock_dl_check(
    untar = function(tarfile, files, exdir, ...) {
      assert_that(
        is.string(tarfile), is.character(files), has_length(files),
        is.string(exdir)
      )
      invisible(0L)
    },
    download_src(src, dir)
  )

  expect_null(res)
})
