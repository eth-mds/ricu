
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
    `read_line` = function(...) "bar",
    get_set_physionet_creds("foo")
  )

  expect_identical(creds, list(username = "foo", password = "bar"))

  creds <- with_mock(
    is_pkg_available = function(...) FALSE,
    read_line = function(x, ...) {
      if (grepl("user", x)) "foo" else if (grepl("pass", x)) "bar" else "baz"
    },
    get_set_physionet_creds()
  )

  expect_identical(creds, list(username = "foo", password = "bar"))

  expect_error(
    with_mock(
      is_pkg_available = function(...) FALSE,
      get_set_physionet_creds()
    )
  )
})
