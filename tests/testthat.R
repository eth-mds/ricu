library(testthat)
library(ricu)

if (requireNamespace("xml2")) {
  test_check(
    "ricu",
    reporter = MultiReporter$new(
      reporters = list(JunitReporter$new(file = "test-results.xml"),
                       CheckReporter$new()
      )
    )
  )
} else {
  test_check("ricu")
}
