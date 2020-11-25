
test_that("attach", {

  srcs <- c("mimic_demo", "eicu_demo")

  withr::local_envvar(RICU_SRC_LOAD = paste(srcs, collapse = ","))

  suppressPackageStartupMessages(
    library("ricu", character.only = TRUE, quietly = TRUE)
  )

  auto_srcs <- auto_attach_srcs()

  expect_setequal(auto_srcs, srcs)
})
