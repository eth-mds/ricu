
expect_all_identical <- function(object, expected = object[[1L]], ...,
                                 info = NULL, label = NULL,
                                 expected_label = NULL) {

  act <- quasi_label(rlang::enquo(object), label, arg = "object")
  exp <- quasi_label(rlang::enquo(expected), expected_label,
                     arg = "expected")

  if (length(exp$val) == 1L) {
    ident <- vapply(act$val, identical, logical(1L), exp$val, ...)
  } else {
    ident <- mapply(identical, act$val, exp$val, MoreArgs = list(...))
  }

  if (all(ident)) {

    msg <- ""

  } else {

    if (length(exp$val) == 1L) {
      comp <- lapply(act$val[!ident], compare, exp$val)
    } else {
      comp <- Map(compare, act$val[!ident], exp$val[!ident])
    }

    msgs <- lapply(comp, function(co) {
      if (co$equal) {
        msg <- "Objects equal but not identical"
      } else {
        msg <- co$message
      }
    })

    msg <- paste(msgs, collapse = "\n  ")
  }

  expect(all(ident), sprintf("%s not identical to %s.\n  %s", act$lab,
                             exp$lab, msg), info = info)

  invisible(act$val)
}

test_that("expect_all_identical", {

  expect_success(expect_all_identical("foo", "foo"))
  expect_success(expect_all_identical(c("foo", "foo"), "foo"))
  expect_failure(expect_all_identical("foo", "bar"))

  expect_success(expect_all_identical(c("foo", "bar"), c("foo", "bar")))
  expect_failure(expect_all_identical(c("foo", "bar"), c("foo", "foo")))
  expect_success(expect_all_identical("foo", c("foo", "foo")))
  expect_failure(expect_all_identical("foo", c("foo", "bar")))

  expect_success(expect_all_identical(c("foo", "foo")))
  expect_failure(expect_all_identical(c("foo", "bar")))
})


expect_all_equal <- function(object, expected = object[[1L]], ..., info = NULL,
                             label = NULL, expected_label = NULL) {

  act <- quasi_label(rlang::enquo(object), label, arg = "object")
  exp <- quasi_label(rlang::enquo(expected), expected_label,
                     arg = "expected")

  if (length(exp$val) == 1L) {
    comp <- lapply(act$val, compare, exp$val)
  } else {
    comp <- Map(compare, act$val, exp$val)
  }

  equ <- vapply(comp, `[[`, logical(1L), "equal")
  msg <- vapply(comp, `[[`, character(1L), "message")

  msg <- paste(msg[!equ], collapse = "\n  ")

  expect(all(equ), sprintf("%s not identical to %s.\n  %s", act$lab,
                           exp$lab, msg), info = info)

  invisible(act$val)
}

test_that("expect_all_equal", {

  expect_success(expect_all_equal("foo", "foo"))
  expect_success(expect_all_equal(c("foo", "foo"), "foo"))
  expect_failure(expect_all_equal("foo", "bar"))

  expect_success(expect_all_equal(c("foo", "bar"), c("foo", "bar")))
  expect_failure(expect_all_equal(c("foo", "bar"), c("foo", "foo")))
  expect_success(expect_all_equal("foo", c("foo", "foo")))
  expect_failure(expect_all_equal("foo", c("foo", "bar")))

  expect_success(expect_all_equal(c("foo", "foo")))
  expect_failure(expect_all_equal(c("foo", "bar")))
})

expect_all <- function(object, info = NULL, label = NULL) {

  act <- quasi_label(rlang::enquo(object), label, arg = "object")

  expect(all(as.logical(act$val)),
         sprintf("%s contains non-true entries.", act$lab), info = info)

  invisible(act$val)
}

test_that("expect_all_equal", {

  expect_success(expect_all(TRUE))
  expect_failure(expect_all(FALSE))
  expect_success(expect_all(c(TRUE, TRUE)))
  expect_failure(expect_all(c(TRUE, FALSE)))
})

expect_fsetequal <- function(object, expected, ...) {

  act <- quasi_label(rlang::enquo(object))
  exp <- quasi_label(rlang::enquo(expected))

  expect(
    data.table::fsetequal(act$val, exp$val, ...),
    sprintf("%s is not fsetequal to %s.", act$lab, exp$lab)
  )

  invisible(act$val)
}

test_that("expect_fsetequal", {

  dt1 <- data.table::data.table(a = letters[1:10], b = 1:10)
  dt2 <- data.table::data.table(a = LETTERS[1:10], b = 1:10)

  expect_success(expect_fsetequal(dt1, dt1))
  expect_success(expect_fsetequal(dt1, dt1[sample(1:10), ]))
  expect_failure(expect_fsetequal(dt1, dt2))
  expect_failure(expect_fsetequal(dt1, dt1[-1L, ]))
})

cpy <- data.table::copy
assign("ident_cb", function(x, ...) x, envir = .GlobalEnv)

skip_if_srcs_missing <- function(srcs) {

  avail <- is_data_avail(srcs)
  skip  <- !all(avail)

  if (skip) {
    msg <- fmt_msg("Data source{?s} {quote_bt(srcs[!avail])} {?is/are} missing
                    but required for tests")
  } else {
    msg <- NULL
  }

  skip_if(skip, msg)
}
