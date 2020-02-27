
expect_length <- function(object, n) {

  act <- quasi_label(rlang::enquo(object), arg = "object")

  act$n <- length(act$val)
  expect(
    act$n == n,
    sprintf("%s has length %i, not length %i.", act$lab, act$n, n)
  )

  invisible(act$val)
}

expect_equal_df <- function(object, expected, ..., info = NULL, label = NULL,
                            expected.label = NULL) {

  act <- quasi_label(rlang::enquo(object), label, arg = "object")
  exp <- quasi_label(rlang::enquo(expected), expected.label, arg = "expected")

  x <- data.table::setDF(data.table::copy(act$val))
  rownames(x) <- NULL
  y <- data.table::setDF(data.table::copy(exp$val))
  rownames(y) <- NULL

  comp <- compare(x, y, ...)

  expect(
    comp$equal,
    sprintf("%s not equal to %s.\n%s", act$lab, exp$lab, comp$message),
    info = info
  )

  invisible(act$val)
}

cpy <- data.table::copy
