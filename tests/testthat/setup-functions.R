
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

stubs <- function(where, ..., depth = 1L, envir = parent.frame()) {

  call <- match.call()

  newc <- call[seq_len(2L)]
  newc[[1L]] <- quote(mockery::stub)

  for (i in seq_len(...length()) + 2L) {
    newc[[3L]] <- names(call)[i]
    newc[[4L]] <- call[[i]]
    eval(newc, envir)
  }

  invisible(NULL)
}

reset_stubs <- function(where, what, depth = 1L, envir = parent.frame()) {

  stopifnot(is.character(where), length(where) == 1L,
            is.character(what),  length(what)  >= 1L)

  args <- lapply(what, as.symbol)
  names(args) <- what

  args <- c(list(where = as.symbol(where)), args,
            list(depth = depth, envir = envir))

  do.call("stubs", args)
}

with_stubs <- function(where, ..., depth = 1L, envir = parent.frame()) {

  call <- match.call()
  ndot <- ...length()
  last <- ndot + 2L

  wher <- deparse(substitute(where))
  funs <- names(call)[seq(3L, last - 1L)]

  stopifnot(identical(nzchar(names(call)[last]), FALSE), ndot >= 1L)

  code <- call[[last]]

  call[[1L]] <- quote(stubs)
  call[[last]] <- NULL

  on.exit(reset_stubs(wher, funs, depth = depth, envir = envir))

  eval(call, envir)
  eval(code, envir)
}

local_stubs <- function(where, ..., depth = 1L, envir = parent.frame()) {

  call <- match.call()
  wher <- deparse(substitute(where))
  funs <- names(call)[seq(3L, ...length() + 2L)]

  call[[1L]] <- quote(stubs)

  res <- eval(call, envir)

  defer(reset_stubs(wher, funs, depth = depth, envir = envir), envir = envir)

  invisible(res)
}

test_that("stubs", {

  fun_f <- function(x) x + 10
  fun_g <- function(y) y + 20
  fun_h <- function(z) fun_f(z) + fun_g(z)

  stubs(fun_h, fun_f = 300, fun_g = 500)

  expect_equal(fun_h(1), 800)

  reset_stubs("fun_h", c("fun_f", "fun_g"))

  expect_equal(fun_h(1), 32)
})

test_that("with_stubs", {

  fun_f <- function(x) x + 10
  fun_g <- function(y) y + 20
  fun_h <- function(z) fun_f(z) + fun_g(z)

  with_stubs(fun_h, fun_f = 300, fun_g = 500, {
    expect_equal(fun_h(1), 800)
  })

  expect_equal(fun_h(1), 32)
})

stub_test_fun_f <- function(x) x + 10
stub_test_fun_g <- function(y) y + 20
stub_test_fun_h <- function(z) stub_test_fun_f(z) + stub_test_fun_g(z)

test_that("local_stubs 1", {

  expect_equal(stub_test_fun_h(1), 32)

  local_stubs(stub_test_fun_h, stub_test_fun_f = 300, stub_test_fun_g = 500)

  expect_equal(stub_test_fun_h(1), 800)
})

test_that("local_stubs 2", {
  expect_equal(stub_test_fun_h(1), 32)
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
