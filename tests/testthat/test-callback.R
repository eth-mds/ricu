
test_that("aumc callbacks", {

  skip_if_not_installed("mockthat")

  set.seed(11)

  env <- with_src()

  eos <- ts_tbl(
    admissionid = sample(seq_len(50), 100, TRUE),
    measuredat = hours(sample(seq(-24, 48, 2), 100, TRUE)),
    itemid = rep(9967L, 100), unit = rep("10^9/l", 100), value = rnorm(100),
    interval = hours(1L)
  )

  id <- sample(1:100, 75)

  wbc <- ts_tbl(
    admissionid = c(eos$admissionid[id], sample(seq_len(100), 25, TRUE)),
    measuredat = c(eos$measuredat[id], hours(sample(seq(-24, 48), 25, TRUE))),
    wbc = rnorm(100),
    interval = hours(1L)
  )

  res <- mockthat::with_mock(
    load_concepts = wbc,
    id_name_to_type = "icustay",
    blood_cell_ratio(eos, "value", "unit", env)
  )

  expect_true(all(c("value", "unit") %in% data_vars(res)))

  ids <- sample(seq_len(10), 100, TRUE)
  tim <- hours(sample(seq(-24, 48, 2), 100, TRUE))

  epi <- ts_tbl(
    admissionid = ids, start = tim,
    doserateunit = sample(c("uur", NA_character_), 100, replace = TRUE),
    doserateperkg = rep(FALSE, 100),
    stop = tim + hours(sample(seq_len(24), 100, TRUE)),
    itemid = rep(6818L, 100),
    doseunit = sample(c("mg", "\u00b5g"), 100, replace = TRUE),
    dose = rnorm(100),
    orderid = seq.int(100),
    index_var = "start", interval = hours(1L)
  )

  wei <- id_tbl(
    admissionid = seq.int(4, 15),
    weight = runif(12, 50, 100)
  )

  res <- mockthat::with_mock(
    load_concepts = wei,
    id_name_to_type = "icustay",
    aumc_rate_kg(epi, "dose", "doseunit", "doserateperkg", "doserateunit",
                 "stop", env)
  )

  expect_true(all(c("dose", "doseunit") %in% data_vars(res)))

  res <- aumc_dur(epi, "dose", "stop", "orderid")

  expect_true(all(c("dose", "orderid") %in% data_vars(res)))
  expect_s3_class(res$dose, "difftime")

  adh <- ts_tbl(
    admissionid = ids, start = tim,
    doserateunit = sample(c("dag", "uur", NA_character_), 100, replace = TRUE),
    stop = tim + hours(sample(seq_len(24), 100, TRUE)),
    itemid = rep(12467L, 100),
    doseunit = sample(c("mg", "\u00b5g"), 100, replace = TRUE),
    dose = rnorm(100),
    orderid = seq.int(100),
    index_var = "start", interval = hours(1L)
  )

  fun <- aumc_rate_units(0.5)
  res <- fun(epi, "dose", "doseunit", "doserateunit", "stop", env)

  expect_true(all(c("dose", "doseunit") %in% data_vars(res)))
  expect_type(res$dose, "double")
  expect_type(res$doseunit, "character")

  dea <- ts_tbl(
    admissionid = ids, dateofdeath = tim,
    dischargedat = tim + hours(sample(seq_len(100), 100, TRUE)),
    index_var = "dateofdeath", interval = hours(1L)
  )

  res <- aumc_death(dea, "dischargedat")

  expect_true("dischargedat" %in% data_vars(res))
  expect_type(res$dischargedat, "logical")

  sig <- sample(c("-", "NUL", NA_character_), 100, TRUE)
  bxs <- ts_tbl(
    admissionid = ids, measuredat = tim, tag = sig,
    itemid = rep(9994L, 100), unit = rep("mmol/l", 100),
    value = -runif(100) * is_true(sig == "-"),
    index_var = "measuredat", interval = hours(1L)
  )

  res <- aumc_bxs(bxs, "value", "tag")

  expect_true(all(c("value", "unit") %in% data_vars(res)))
  expect_gte(min(res$value), 0)

  ras <- sample(
    c("-1 slaperig", "-2 lichte sedatie", "-3 matige sedatie",
      "-4 diepe sedatie", "-5 niet wekbaar", "+1 onrustig", "+2 geagiteerd",
      "+3 erg geagiteerd", "+4 strijdlustig", "0 alert en kalm"
    ), 100, TRUE
  )

  res <- aumc_rass(ras)

  expect_type(res, "integer")
  expect_setequal(res, -5:4)
})

test_that("hirid callbacks", {

  skip_if_not_installed("mockthat")

  set.seed(11)

  env <- with_src()

  ids <- sample(seq_len(10), 100, TRUE)
  tim <- hours(sample(seq(-24, 48, 2), 100, TRUE))
  var <- sample(c(110L, 200L), 100, TRUE)

  dea <- ts_tbl(
    patientid = ids, datetime = tim, variableid = var,
    value = sample(50:150, 100, TRUE),
    unit = ifelse(var == 110, "mmHg", "/min"),
    index_var = "datetime", interval = hours(1L)
  )

  sta <- sample(c("alive", "dead"), 10, TRUE)
  res <- mockthat::with_mock(
    load_id = id_tbl(
      patientid = 3:12, discharge_status = sta
    ),
    hirid_death(dea, "value", "variableid", env)
  )

  expect_true("value" %in% data_vars(res))
  expect_type(res$value, "logical")

  epi <- ts_tbl(
    patientid = ids, givenat = tim,
    infusionid = ids * sample(1:3 * 10, 100, TRUE),
    doseunit = rep("\u00b5g", 100),
    givendose = runif(100, 10, 100),
    index_var = "givenat", interval = hours(1L)
  )

  wei <- id_tbl(
    admissionid = seq.int(4, 15),
    weight = runif(12, 50, 100)
  )

  res <- mockthat::with_mock(
    load_concepts = wei,
    id_name_to_type = "icustay",
    hirid_rate_kg(epi, "givendose", "doseunit", "infusionid", env)
  )

  expect_true("givendose" %in% data_vars(res))
  expect_type(res$givendose, "double")

  res <- hirid_duration(epi, "givendose", "infusionid", env)

  expect_true("givendose" %in% data_vars(res))
  expect_s3_class(res$givendose, "difftime")

  epi <- epi[, doseunit := "U"]

  res <- hirid_rate(epi, "givendose", "doseunit", "infusionid", env)

  expect_true(all(c("givendose", "doseunit") %in% data_vars(res)))
  expect_type(res$givendose, "double")
  expect_identical(unique(res$doseunit), "U/min")

  uri <- ts_tbl(
    patientid = ids, givenat = tim,
    value = runif(100, 100, 500), unit = rep("cummulative", 100),
    index_var = "givenat", interval = hours(1L)
  )

  uri <- uri[, value := cumsum(value), by = "patientid"]
  res <- hirid_urine(uri, "value", "unit")

  expect_true(all(c("value", "unit") %in% data_vars(res)))
  expect_type(res$value, "double")
  expect_identical(unique(res$unit), "mL")
})

test_that("misc itm callbacks", {

  fun <- aggregate_fun("sum", "new")

  expect_type(fun, "closure")
  expect_named(formals(fun), c("x", "val_var", "unit_var", "..."))

  col <- rnorm(12)
  dat <- ts_tbl(a = rep(1:2, each = 6), b = hours(rep(1:3, 4)), c = col,
                d = rep("units", 12))

  res <- fun(dat, "c", "d")

  expect_identical(nrow(res), 6L)
  expect_identical(res$d, rep("new", 6L))
  expect_identical(res$c, dbl_ply(split(dat$c, rep(1:6, each = 2)), sum))
})

skip_if_srcs_missing("mimic_demo")

test_that("mimic itm callbacks", {

  cnc <- c("ins", "phn_rate", "los_hosp")
  dat <- load_concepts(cnc, "mimic_demo", verbose = FALSE)

  expect_s3_class(dat, c("ts_tbl", "id_tbl"))
  expect_setequal(data_vars(dat), cnc)
  expect_gt(nrow(dat), 0L)
})

test_that("mimic cncpt callbacks", {

  dat <- load_concepts("safi", "mimic_demo", verbose = FALSE,
                       mode = "extreme_vals")

  expect_s3_class(dat, c("ts_tbl", "id_tbl"))
  expect_identical(data_vars(dat), "safi")
  expect_gt(nrow(dat), 0L)

  cnc <- c("vaso_ind", "bmi", "norepi_equiv")
  dat <- load_concepts(cnc, "mimic_demo", verbose = FALSE)

  expect_s3_class(dat, c("ts_tbl", "id_tbl"))
  expect_setequal(data_vars(dat), cnc)
  expect_gt(nrow(dat), 0L)
})

skip_if_srcs_missing("eicu_demo")

test_that("eicu itm callbacks", {

  cnc <- c("adm", "age", "adh_rate")
  dat <- load_concepts(cnc, "eicu_demo", verbose = FALSE)

  expect_s3_class(dat, c("ts_tbl", "id_tbl"))
  expect_setequal(data_vars(dat), cnc)
  expect_gt(nrow(dat), 0L)
})

test_that("susp_inf", {

  abx <- ts_tbl(hadm_id = rep(100012L, 13L), startdate = hours(c(48, 52, 57,
    61, 72, 80, 84, 92, 93, 104, 115, 192, 216)), abx = rep(TRUE, 13L),
    interval = hours(1L)
  )

  samp <- ts_tbl(hadm_id = rep(100012L, 3L), chartdate = hours(c(1, 48, 70)),
    samp = rep(TRUE, 3L), interval = hours(1L)
  )

  expected <- ts_tbl(hadm_id = rep(100012L, 6L), chartdate = hours(c(1, 48, 52,
    57, 61, 70)), susp_inf = rep(TRUE, 6L), interval = hours(1L)
  )

  expect_identical(susp_inf(abx, samp), expected)
})
