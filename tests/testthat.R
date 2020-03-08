
pkg_env <- function(package, rm) {

  res <- as.list(getNamespace(package), all.names = TRUE)

  if (length(rm)) {
    res[rm] <- NULL
  }

  list2env(res, parent = parent.env(getNamespace(package)))
}

maybe_root_dir <- function(path) {
  tryCatch(pkgload::pkg_path(path), error = function(...) path)
}

run_tests <- function(package,
                      filter = NULL,
                      reporter = check_reporter(),
                      rm = NULL,
                      ...,
                      stop_on_failure = TRUE,
                      stop_on_warning = FALSE,
                      wrap = TRUE) {
  library(testthat)
  require(package, character.only = TRUE)

  env_test <- getFromNamespace("env_test", "testthat")

  assign("in_test", TRUE, envir = env_test)
  assign("package", package, envir = env_test)

  on.exit({
    assign("in_test", FALSE, envir = env_test)
    assign("package", NULL, envir = env_test)
  })

  test_path <- "testthat"
  if (!utils::file_test("-d", test_path)) {
    stop("No tests found for ", package, call. = FALSE)
  }

  env <- pkg_env(package, rm)
  withr::local_options(list(topLevelEnvironment = env))

  withr::local_envvar(list(
    TESTTHAT_PKG = package,
    TESTTHAT_DIR = maybe_root_dir(test_path)
  ))

  test_dir(
    path = test_path,
    reporter = reporter,
    env = env,
    filter = filter,
    ...,
    stop_on_failure = stop_on_failure,
    stop_on_warning = stop_on_warning,
    wrap = wrap
  )
}

if (getRversion() < "4.0.0") {
  to_rm <- c("cbind.ts_tbl", "rbind.ts_tbl")
} else {
  to_rm <- NULL
}

if (requireNamespace("xml2")) {
  run_tests(
    "ricu",
    reporter = MultiReporter$new(
      reporters = list(JunitReporter$new(file = "test-results.xml"),
                       CheckReporter$new()
      )
    ),
    rm = to_rm
  )
} else {
  run_tests("ricu", rm = to_rm)
}
