library(testthat)
library(sepsr)

if (requireNamespace("xml2")) {
  test_check(
    "sepsr",
    reporter = MultiReporter$new(
      reporters = list(JunitReporter$new(file = "test-results.xml"),
                       CheckReporter$new()
      )
    )
  )
} else {
  test_check("sepsr")
}
