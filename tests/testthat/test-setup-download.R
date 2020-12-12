
mock_fetch_memory <- function(url, handle, ...) {

  file <- system.file("testdata", paste0(basename(url), ".rds"),
                      package = "ricu")

  if (identical(file, "")) {
    stop("file ", file, "not found")
  }

  readRDS(file)
}

mock_fetch_disk <- function(url, path, handle, ...) {

  dat <- mock_fetch_memory(url, handle)

  # if e.g. timecondition opt can be queried from handle, a 304 could be
  # returned to mimic the no-change scenario

  writeBin(dat[["content"]], path)

  dat
}

mock_fetch_stream <- function(url, fun, handle, ...) {

  dat <- mock_fetch_memory(url, handle)

  fun(dat[["content"]])

  dat
}

wrong_fun <- mockthat::mock(stop("called wrong fun"))

test_that("credentials", {

  withr::local_envvar(FOO_VAR = "bar_val")

  expect_silent(crd <- get_cred("foo_val", "FOO_VAR", "input"))
  expect_identical(crd, "foo_val")

  expect_silent(crd <- get_cred(NULL, "FOO_VAR", "input"))
  expect_identical(crd, "bar_val")

  withr::local_envvar(FOO_VAR = NA)

  res <- mockthat::with_mock(
    is_interactive = function() TRUE,
    read_line = function(...) "baz_val",
    get_cred(NULL, "BAR_VAR", "input")
  )

  expect_identical(res, "baz_val")
})

test_that("file size", {

  pat_siz <- mockthat::with_mock(
    `curl::curl_fetch_memory` = mock_fetch_memory,
    `curl::curl_fetch_disk` = wrong_fun,
    `curl::curl_fetch_stream` = wrong_fun,
    get_file_size("foo/PATIENTS.csv", NULL, NULL)
  )

  expect_type(pat_siz, "double")
  expect_length(pat_siz, 1L)
})

test_that("hash checking", {

  sha <- mockthat::with_mock(
    `curl::curl_fetch_memory` = mock_fetch_memory,
    `curl::curl_fetch_disk` = wrong_fun,
    `curl::curl_fetch_stream` = wrong_fun,
    get_sha256("foo")
  )

  expect_type(sha, "list")

  for (x in sha) {
    expect_type(x, "character")
    expect_length(x, 2L)
  }

  tmp <- withr::local_tempdir()
  tbl <- "PATIENTS.csv"

  res <- mockthat::with_mock(
    `curl::curl_fetch_memory` = wrong_fun,
    `curl::curl_fetch_disk` = mock_fetch_disk,
    `curl::curl_fetch_stream` = wrong_fun,
    download_pysionet_file(
      "foo/PATIENTS.csv", file.path(tmp, tbl)
    )
  )

  expect_null(res)

  sha <- setNames(chr_xtr(sha, 1L), chr_xtr(sha, 2L))

  expect_true(check_file_sha256(file.path(tmp, tbl), sha[[tbl]]))
})

test_that("file download", {

  tmp <- withr::local_tempdir()

  dat_mem <- mockthat::with_mock(
    `curl::curl_fetch_memory` = mock_fetch_memory,
    `curl::curl_fetch_disk` = wrong_fun,
    `curl::curl_fetch_stream` = wrong_fun,
    download_pysionet_file("foo/SHA256SUMS.txt")
  )

  expect_type(dat_mem, "raw")
  expect_identical(list.files(tmp), character(0L))

  tmp <- withr::local_tempdir()

  dat_disk <- mockthat::with_mock(
    `curl::curl_fetch_memory` = wrong_fun,
    `curl::curl_fetch_disk` = mock_fetch_disk,
    `curl::curl_fetch_stream` = wrong_fun,
    download_pysionet_file(
      "foo/SHA256SUMS.txt", file.path(tmp, "SHA256SUMS.txt")
    )
  )

  expect_type(dat_disk, "NULL")
  expect_identical(list.files(tmp), "SHA256SUMS.txt")

  tmp <- withr::local_tempdir()

  tables <- c("PATIENTS.csv", "SERVICES.csv")

  res <- mockthat::with_mock(
    `curl::curl_fetch_memory` = mock_fetch_memory,
    `curl::curl_fetch_disk` = mock_fetch_disk,
    `curl::curl_fetch_stream` = mock_fetch_stream,
    download_check_data(tmp, tables, "foo", "foo", NULL, NULL, FALSE)
  )

  expect_null(res)
  expect_setequal(list.files(tmp), tables)

  expect_warning(
    mockthat::with_mock(
      `curl::curl_fetch_memory` = mock_fetch_memory,
      `curl::curl_fetch_disk` = mock_fetch_disk,
      `curl::curl_fetch_stream` = mock_fetch_stream,
      check_file_sha256 = function(...) FALSE,
      download_check_data(tmp, tables, "foo", "foo", NULL, NULL, FALSE)
    ),
    class = "checksum_mismatch"
  )

  expect_error(
    mockthat::with_mock(
      `curl::curl_fetch_memory` = mock_fetch_memory,
      `curl::curl_fetch_disk` = mock_fetch_disk,
      `curl::curl_fetch_stream` = mock_fetch_stream,
      download_check_data(tmp, tolower(tables), "foo", "foo", NULL, NULL,
                          FALSE)
    ),
    class = "are_in_assert"
  )
})

test_that("src download", {

  mock_dl_check <- function(dest_folder, files, url, user, pass, src) {

    assert_that(
      is.character(files), has_length(files), is.string(dest_folder),
      is.string(url)
    )

    invisible(NULL)
  }

  mock_untar <- function(tarfile, files, exdir, ...) {

    assert_that(
      is.string(tarfile), is.character(files), has_length(files),
      is.string(exdir)
    )

    invisible(0L)
  }

  tmp <- withr::local_tempdir()

  srcs <- c("mimic_demo", "eicu_demo")
  dirs <- file.path(tmp, srcs)

  expect_true(all(lgl_ply(dirs, dir.create)))

  mk_dl <- mockthat::local_mock(download_check_data = NULL)

  expect_invisible(res <- download_src(srcs, dirs))

  expect_null(res)
  expect_true(dir.exists(mockthat::mock_arg(mk_dl, "dest_folder")))

  src <- "hirid"
  dir <- file.path(tmp, src)

  expect_true(dir.create(dir))

  expect_invisible(
    res <- mockthat::with_mock(
      download_check_data = NULL,
      untar = 0L,
      download_src(src, dir)
    )
  )

  expect_null(res)
})
