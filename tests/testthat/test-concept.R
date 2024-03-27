

test_that("load hirid items", {

  skip_if_not_installed("mockthat")

  gluc <- mockthat::with_mock(
    get_hirid_ids = id_tbl(id = c(20005110L, 24000523L, 24000585L),
                           unit = rep("mmol/l", 3L)),
    as_src_tbl = structure(list(),
      class = c("hirid_tbl", "src_tbl"),
      col_cfg = new_col_cfg("hirid", "observations", index_var = "datetime",
                            time_vars = c("datetime", "entertime"),
                            val_var = "value")
    ),
    parse_dictionary(read_dictionary("concept-dict"), "hirid", "glu")
  )

  expect_identical(n_tick(gluc), 2L)

  gluc <- as_item(gluc)[[1L]]
  gluc <- try_add_vars(gluc, id_var = "patientid", index_var = "datetime",
                       type = "meta_vars")

  dat <- mockthat::with_mock(
    load_ts = ts_tbl(
      patientid = rep(seq_len(2L), each = 10L),
      datetime = hours(rep(seq_len(10L), 2L)),
      variableid = rep(24000585L, 20L),
      value = rnorm(20L)
    ),
    do_itm_load(gluc)
  )

  expect_s3_class(dat, "ts_tbl")
  expect_true(is_ts_tbl(dat))
  expect_identical(data_vars(dat), c("variableid", "value", "unit"))
  expect_equal(interval(dat), hours(1L))

  expect_identical(id_vars(gluc), id_vars(dat))
  expect_identical(index_var(gluc), index_var(dat))
  expect_identical(meta_vars(gluc), meta_vars(dat))

  expect_identical(dat, do_callback(gluc, dat))
})

skip_if_srcs_missing("mimic_demo")

test_that("load concepts", {

  gluc <- concept("gluc",
    item("mimic_demo", "labevents", "itemid", list(c(50809L, 50931L)))
  )

  dat1 <- load_concepts(gluc, verbose = FALSE)

  expect_s3_class(dat1, "ts_tbl")
  expect_true(is_ts_tbl(dat1))
  expect_identical(colnames(dat1), c("icustay_id", "charttime", "gluc"))
  expect_equal(interval(dat1), hours(1L))

  expect_snapshot(print(gluc))

  albu <- concept("albu", item("mimic_demo", "labevents", "itemid", 50862L))
  glal <- c(albu, gluc)

  dat2 <- load_concepts(glal, verbose = FALSE)

  expect_setequal(data_vars(dat2), c("gluc", "albu"))

  expect_snapshot(print(albu))
  expect_snapshot(print(glal))

  dat3 <- load_concepts(gluc, merge_data = FALSE, verbose = FALSE)

  expect_type(dat3, "list")
  expect_length(dat3, 1L)

  dat3 <- dat3[[1L]]

  expect_true(is_ts_tbl(dat3))
  expect_identical(colnames(dat3), c("icustay_id", "charttime", "gluc"))
  expect_equal(interval(dat3), hours(1L))

  dat4 <- load_concepts(gluc, aggregate = FALSE, merge_data = FALSE,
                        verbose = FALSE)

  expect_type(dat4, "list")
  expect_length(dat4, 1L)

  dat4 <- dat4[[1L]]

  expect_true(is_ts_tbl(dat4))
  expect_identical(colnames(dat4), c("icustay_id", "charttime", "gluc"))
  expect_gt(nrow(dat4), nrow(dat3))

  expect_identical(dat3,
    dat4[, list(gluc = median(gluc)), by = c(meta_vars(dat4))]
  )

  dat5 <- load_concepts(gluc, aggregate = identity, merge_data = FALSE,
                        verbose = FALSE)

  expect_type(dat5, "list")
  expect_length(dat5, 1L)
  expect_identical(dat4, dat5[[1L]], ignore_attr = TRUE)

  expect_error(
    load_concepts(gluc, aggregate = "identity", verbose = FALSE)
  )

  static <- load_concepts(c("sex", "age"), "mimic_demo", verbose = FALSE)

  expect_s3_class(static, "id_tbl")
  expect_true(is_id_tbl(static))
  expect_setequal(colnames(static), c("icustay_id", "sex", "age"))
  expect_type(static[["age"]], "double")
  expect_type(static[["sex"]], "character")

  expect_error(
    concept("gluc",
      item("mimic_demo", "labevents", "itemid", c(50809L, 50931L))
    )
  )

  gluc2 <- concept("gluc",
    list(item("mimic_demo", "labevents", "itemid", c(50809L, 50931L)))
  )

  dat6 <- load_concepts(gluc2, verbose = FALSE)

  expect_identical(dat1, dat6, ignore_attr = TRUE)

  gcs_con <- load_dictionary(concepts = "gcs")
  gcs_raw <- concept("gcs_raw", gcs_con, sed_impute = "none",
                     class = "rec_cncpt")

  dat7 <- load_concepts(gcs_raw, "mimic_demo", verbose = FALSE)

  expect_s3_class(dat7, "ts_tbl")
  expect_true(is_ts_tbl(dat7))
  expect_identical(colnames(dat7), c("icustay_id", "charttime", "gcs_raw"))
  expect_equal(interval(dat7), hours(1L))

  expect_snapshot(print(gcs_con))
  expect_snapshot(print(gcs_raw))
})

test_that("load concepts", {

  dat1 <- load_concepts(4144235L, "mimic_demo", verbose = FALSE)

  expect_s3_class(dat1, "ts_tbl")
  expect_true(is_ts_tbl(dat1))
  expect_identical(colnames(dat1),
                   c("icustay_id", "charttime", "omop_4144235"))
  expect_equal(interval(dat1), hours(1L))

  dat2 <- load_concepts(c(4144235, 4017497), "mimic_demo", verbose = FALSE)

  expect_s3_class(dat2, "ts_tbl")
  expect_true(is_ts_tbl(dat2))
  expect_identical(
    colnames(dat2),
    c("icustay_id", "charttime", "omop_4144235", "omop_4017497")
  )
  expect_equal(interval(dat2), hours(1L))

  expect_warning(
    load_concepts(c(4144235, 123), "mimic_demo", verbose = FALSE),
    class = "omop_miss_id"
  )
})

skip_if_srcs_missing("eicu_demo")

test_that("load concepts multi src", {

  ids <- list(eicu_demo  = c(141765, 143870),
              mimic_demo = c(293280, 298685))

  dat <- load_concepts("glu", c("mimic_demo", "eicu_demo"),
                       patient_ids = ids, verbose = FALSE)

  expect_s3_class(dat, "ts_tbl")
  expect_true(is_ts_tbl(dat))
  expect_length(id_vars(dat), 2L)

  uqe <- unique(dat[, id_vars(dat), with = FALSE])

  expect_identical(nrow(uqe), 4L)
  expect_identical(dat,
    load_concepts("glu", c("mimic_demo", "eicu_demo"), patient_ids = uqe,
                  verbose = FALSE)
  )

  uqe <- as.data.frame(uqe)

  expect_identical(dat,
    load_concepts("glu", c("mimic_demo", "eicu_demo"), patient_ids = uqe,
                  verbose = FALSE)
  )
})

test_that("load external dictionary", {

  srcs <- c("mimic_demo", "eicu_demo")
  conc <- c("age", "alb", "glu", "gcs", "esr", "fgn")
  dict <- load_dictionary(srcs, conc)

  expect_s3_class(dict, "concept")
  expect_length(dict, 6L)
  expect_named(dict, conc)

  expect_snapshot(print(dict))

  itms <- as_item(dict)

  expect_s3_class(itms, "item")
  expect_length(itms, 21L)
  expect_setequal(names(itms), srcs)

  expect_snapshot(print(itms))

  avail <- concept_availability(dict)

  expect_snapshot_value(avail, style = "json2")
  expect_snapshot_value(concept_availability(dict, include_rec = TRUE),
                        style = "json2")
  expect_snapshot_value(explain_dictionary(dict), style = "json2")

  conc <- c("age", "esr", "fgn", "foo")
  extr <- load_dictionary(srcs, conc,
                          cfg_dirs = system.file("testdata", package = "ricu"))

  expect_s3_class(extr, "concept")
  expect_length(extr, 4L)
  expect_named(extr, conc)

  expect_false(avail["esr", "eicu_demo"])
  expect_true(concept_availability(extr)["esr", "eicu_demo"])

  itms <- as_item(extr["foo"])

  expect_s3_class(itms, "item")
  expect_length(itms, 2L)
  expect_named(itms, srcs, ignore.order = TRUE)

  expect_snapshot(print(itms))

  itm <- subset_src(as_item(extr["fgn"]), "mimic_demo")
  tmp <- item("mimic_demo", "labevents", "itemid", 12345L,
               unit_var = "valueuom", target = "ts_tbl")

  expect_s3_class(itm, "item")
  expect_length(itm, 1L)
  expect_named(itm, "mimic_demo")
  expect_identical(itm, tmp)

  expect_snapshot(print(itm))
})
