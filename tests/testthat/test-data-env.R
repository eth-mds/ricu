
skip_if_srcs_missing(c("mimic_demo", "eicu_demo"))

test_that("demo envs", {

  expect_s3_class(mimic_demo, c("mimic_demo_env", "mimic_env", "src_env"))
  expect_s3_class(eicu_demo, c("eicu_demo_env", "eicu_env", "src_env"))

  expect_snapshot(print(mimic_demo))
  expect_snapshot(print(eicu_demo))

  mi_tbl <- mimic_demo[[names(mimic_demo)[1L]]]
  ei_tbl <- eicu_demo[[names(eicu_demo)[1L]]]

  expect_s3_class(mi_tbl, c("mimic_demo_tbl", "mimic_tbl", "src_tbl", "prt"))
  expect_s3_class(ei_tbl, c("eicu_demo_tbl", "eicu_tbl", "src_tbl", "prt"))

  expect_snapshot(tbl_sum(mi_tbl))
  expect_snapshot(tbl_sum(ei_tbl))
})

test_that("src env", {

  some_env <- new.env()
  link_env <- new.env()

  cfg <- load_src_cfg("mimic_demo")[["mimic_demo"]]

  res <- new_src_env(cfg, some_env)

  expect_s3_class(res, c("mimic_demo_env", "mimic_env", "src_env"))
  expect_identical(res, some_env)
  expect_length(res, 0L)

  res <- new_src_env(cfg, some_env, link_env)

  expect_identical(res, link_env$mimic_demo)

  expect_warning(
    new_src_env(cfg, some_env, link_env),
    class = "src_name_bound_in_link_env"
  )
})
