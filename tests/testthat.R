
library(testthat)

if (requireNamespace("xml2")) {
  reporter <- MultiReporter$new(
    reporters = list(JunitReporter$new(file = "test-results.xml"),
                     CheckReporter$new()
    )
  )
} else {
  reporter <- check_reporter()
}

test_package("ricu", reporter = reporter)
