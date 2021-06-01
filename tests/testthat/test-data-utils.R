
skip_if_srcs_missing(c("mimic_demo", "eicu_demo"))

test_that("stay windows", {

  wins <- stay_windows(c("mimic_demo", "eicu_demo"))

  expect_s3_class(wins, "id_tbl")
  expect_length(id_vars(wins), 2L)
  expect_true("source" %in% id_vars(wins))
  expect_setequal(data_vars(wins), c("start", "end"))

  hrd <- mockthat::with_mock(
    load_src = function(x, src, ...) {
      if (identical(x, "observations")) {
        data.table::data.table(
          patientid = rep(seq_len(5L), each = 2L),
          datetime = as.POSIXct(runif(10L, 1e4, 1e6), origin = "2100-01-01")
        )
      } else {
        data.table::data.table(
          patientid = rep(seq_len(5L), each = 2L) + 1L,
          admissiontime = as.POSIXct(runif(10L, 0, 1e5), origin = "2100-01-01")
        )
      }
    },
    id_windows("hirid")
  )

  expect_s3_class(hrd, "id_tbl")
  expect_identical(nrow(hrd), 8L)
  expect_identical(id_vars(hrd), "patientid")
  expect_setequal(data_vars(hrd), c("patientid_start", "patientid_end"))

  aum <- mockthat::with_mock(
    as_src_tbl = function(...) {
      data.table::data.table(
        patientid = integer(0L), admissionid = integer(0L),
        admittedat = numeric(0L), dischargedat = numeric(0L),
        dateofdeath = numeric(0L)
      )
    },
    load_src = function(...) {
      data.table::data.table(
        admissionid = seq_len(10L),
        patientid = seq_len(10L),
        admittedat = rep(0, 10L),
        dischargedat = runif(10L, 1e8, 1e9),
        dateofdeath = sample(c(rep(NA_real_, 4L), runif(6L, 1e8, 1e9)))
      )
    },
    id_windows("aumc")
  )

  expect_s3_class(aum, "id_tbl")
  expect_identical(nrow(aum), 10L)
  expect_identical(id_vars(aum), "patientid")
  expect_setequal(data_vars(aum),
    c("admissionid", "admissionid_start", "patientid_start", "admissionid_end",
      "patientid_end")
  )
})

test_that("id up/downgrades", {

  tbl  <- as_src_tbl("labevents", "mimic_demo")
  alb <- load_ts(tbl, is_val(itemid, 50862L), "value",
                 "icustay_id", "charttime")

  expect_s3_class(alb, "ts_tbl")

  tbl  <- as_src_tbl("lab", "eicu_demo")
  alb <- load_ts(tbl, is_val(labname, "albumin"), "labresult",
                 "patienthealthsystemstayid", "labresultoffset")

  expect_s3_class(alb, "ts_tbl")

  tbl  <- as_src_tbl("patients", "mimic_demo")
  sex <- load_id(tbl, cols = "gender", id_var = "hadm_id")

  expect_s3_class(sex, "id_tbl")
})
