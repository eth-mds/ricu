
test_that("config classes", {

  cfg <- load_src_cfg(c("mimic_demo", "eicu_demo"))

  expect_type(cfg, "list")
  expect_length(cfg, 2L)

  mi <- cfg[["mimic_demo"]]
  ei <- cfg[["eicu_demo"]]

  expect_s3_class(mi, c("mimic_demo_cfg", "mimic_cfg", "src_cfg"))
  expect_s3_class(ei, c("eicu_demo_cfg", "eicu_cfg", "src_cfg"))

  expect_identical(as_src_cfg(mi), mi)

  id <- as_id_cfg(mi)
  co <- as_col_cfg(mi)
  tb <- as_tbl_cfg(mi)

  expect_s3_class(id, c("mimic_demo_ids", "mimic_ids", "id_cfg"))
  expect_identical(as_id_cfg(id), id)

  expect_s3_class(co, c("mimic_demo_cols", "mimic_cols", "col_cfg"))
  expect_identical(as_col_cfg(co), co)

  expect_s3_class(tb, c("mimic_demo_tbl", "mimic_tbl", "tbl_cfg"))
  expect_identical(as_tbl_cfg(tb), tb)

  expect_identical(length(co), length(tb))
})
