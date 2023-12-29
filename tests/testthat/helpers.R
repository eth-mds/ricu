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

expect_all <- function(object, info = NULL, label = NULL) {

  act <- quasi_label(rlang::enquo(object), label, arg = "object")

  expect(all(as.logical(act$val)),
         sprintf("%s contains non-true entries.", act$lab), info = info)

  invisible(act$val)
}

expect_fsetequal <- function(object, expected, ...) {

  act <- quasi_label(rlang::enquo(object))
  exp <- quasi_label(rlang::enquo(expected))

  expect(
    data.table::fsetequal(act$val, exp$val, ...),
    sprintf("%s is not fsetequal to %s.", act$lab, exp$lab)
  )

  invisible(act$val)
}

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

skip_if_no_local_testdata <- function() {

  skip_if(
    identical(system.file("local_testdata", package = "ricu"), ""),
    "No local testdata is available"
  )
}

with_src <- function(src = "mimic_test", env = parent.frame()) {

  stopifnot(!src %in% attached_srcs(), src %in% c("mimic_test", "eicu_test"))

  attach_src(src, data_dir = src_data_dir(sub("_test", "_demo", src)),
             cfg_dirs = system.file("testdata", package = "ricu"))

  withr::defer(detach_src(src), envir = env)

  as_src_env(src)
}
