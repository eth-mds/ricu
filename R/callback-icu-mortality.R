# ==============================================================================
# 
# Refined ICU mortality callbacks
#
# based on YAIB: https://github.com/rvandewater/YAIB-cohorts/tree/main/ricu-extensions/callbacks
# ==============================================================================

aumc_death_icu <- function (x, val_var, death_date, ...) {
  # Identify ICU mortality in AUMCdb via the discharge destination field. Use
  # discharge time from the ICU as death time, as date of death sometimes only
  # contain the date part and no time (i.e., 00:00:00).
  #
  # See discussions here:
  # https://github.com/AmsterdamUMC/AmsterdamUMCdb/issues/56
  # https://github.com/AmsterdamUMC/AmsterdamUMCdb/issues/61
  idx <- index_var(x)
  x[, `:=`(c(val_var), ricu:::is_true(get(val_var) == "Overleden"))]
  x[get(death_date) - get(idx) > hours(24L), `:=`(c(val_var), FALSE)]
  x
}

hirid_death_icu <- function (x, val_var, sub_var, env, ...) {
  dis <- "discharge_status"
  idx <- index_var(x)
  idc <- id_vars(x)
  res <- dt_gforce(x, "last", by = idc, vars = idx)
  tmp <- load_id(env[["general"]], cols = dis)
  res <- merge(res, tmp[ricu:::is_true(get(dis) == "dead"), ])
  res <- res[, `:=`(c(val_var, dis), list(TRUE, NULL))]
  res
}

mi_death_icu <- function(x, transfers, icu_wards, ...) {
  # Look for all hospital deaths in which the last careunit was an ICU.
  # See discussion here: https://github.com/MIT-LCP/mimic-code/issues/874
  id <- id_vars(transfers)
  lead <- function(x) data.table:::shift(x, type = "lead")

  transfers[, is_last := ricu:::is_true(lead(eventtype) == "discharge")]
  last_ward <- transfers[, .(ward = ward[is_last]), by = c(id)]
  last_ward[, "is_icu" := .(ricu:::is_true(ward %in% icu_wards))]

  dat <- data_var(x)
  x[(last_ward[is_icu == FALSE]), c(dat) := 0L]
  x[, c(dat) := ricu:::is_true(get(dat) == 1L)]
  x
}

mimic_death_icu <- function(x, env, ...){
  icu_wards <- sort(unique(env[["icustays"]]$first_careunit))
  transfers <- load_ts(env[["transfers"]], id_var = "hadm_id", index_var = "intime", interval = mins(1L))
  transfers <- change_id(transfers, "icustay", as_src_cfg(env), id_type = TRUE)
  rename_cols(transfers, "ward", "curr_careunit", by_ref = TRUE)
  mi_death_icu(x, transfers, icu_wards, ...)
}

miiv_death_icu <- function(x, env, ...){
  icu_wards <- sort(unique(env[["icustays"]]$first_careunit))
  transfers <- load_ts(env[["transfers"]], index_var = "intime")
  rename_cols(transfers, "ward", "careunit", by_ref = TRUE)
  mi_death_icu(x, transfers, icu_wards, ...)
}

