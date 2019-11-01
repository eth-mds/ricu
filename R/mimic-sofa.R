
mimic_sofa_pa <- function(lab_item = 50821L, unit_handler = fix_case_allow_na,
                          agg_fun = min, ...) {

  message("fetching pao2")

  lab <- mimic_get_items(c(pao2 = lab_item), item_table = "d_labitems",
                         unit_handler = unit_handler, ...)

  assert_that(has_unit(lab, "pao2", "mm Hg"))

  aggregate_vals(lab, agg_fun, by_cols = NULL, val_col = "pao2")
}

mimic_sofa_fi <- function(lab_item = 50816L, chart_item = 223835L,
                          unit_handler = fix_percent,
                          agg_fun = max, ...) {

  message("fetching fio2")

  lab <- mimic_get_items(lab_item, item_table = "d_labitems",
                         unit_handler = unit_handler, ...)
  lab <- aggregate_vals(lab, agg_fun, by_cols = c("hadm_id", "rel_time"),
                        new_col = "fi_lab")
  chart <- mimic_get_items(chart_item, item_table = "d_items",
                           unit_handler = unit_handler, ...)
  chart <- aggregate_vals(chart, agg_fun,
                          by_cols = c("hadm_id", "icustay_id", "rel_time"),
                          new_col = "fi_chart")

  assert_that(
    has_unit(lab, "fi_lab", "%"),
    has_unit(chart, "fi_chart", "%")
  )

  res <- merge(lab, chart, by = c("hadm_id", "rel_time"), all = TRUE)
  res <- res[, fio2 := ifelse(is.na(fi_lab), fi_chart, fi_lab)]
  res <- res[, c("fi_lab", "icustay_id", "fi_chart") := NULL]

  res

}

mimic_sofa_pafi <- function(...,
                            pa = mimic_sofa_pa(...), fi = mimic_sofa_fi(...),
                            win_length = as.difftime(2L, units = "hours"),
                            mode = c("match_vals", "extreme_vals")) {

  assert_that(is_dt(pa), is_dt(fi),
              has_cols(pa, c("hadm_id", "rel_time", "pao2")),
              has_cols(fi, c("hadm_id", "rel_time", "fio2")),
              is_difftime(win_length, allow_neg = FALSE))

  mode <- match.arg(mode)

  fi <- fi[fio2 <= 1, fio2 := fio2 * 100]
  fi <- fi[fio2 > 0, ]

  if (mode == "match_vals") {

    res <- rbind(
      fi[pa, on = c("hadm_id", "rel_time"), roll = win_length],
      pa[fi, on = c("hadm_id", "rel_time"), roll = win_length]
    )
    res <- unique(res)

  } else {

    res <- merge(pa, fi, all = TRUE, by = c("hadm_id", "rel_time"))
    res <- make_regular(res, time_col = "rel_time", id_cols = "hadm_id")
    res <- window_quo(res,
      substitute(list(min_pa = min_fun(pao2), max_fi = max_fun(fio2)),
                 list(min_fun = min_or_na, max_fun = max_or_na)),
      full_window = FALSE, window_length = win_length
    )
    res <- res[, c("pao2", "fio2") := NULL]
    res <- data.table::setnames(res, c("min_pa", "max_fi"), c("pao2", "fio2"))
  }

  res <- res[, pafi := 100 * pao2 / fio2]
  res <- res[, c("pao2", "fio2") := NULL]

  res
}

mimic_sofa_vent <- function(
  start_items = c(223848L, 223849L, 220339L, 224419L, 224684L:224687L,
                  224695L:224697L, 224700L:224707L, 224709L, 224738L,
                  224746L, 224747L, 224750L, 226873L, 227187L),
  stop_items = list(extub = c(227194L, 225468L, 225477L),
                    o2_therapy = 226732L),
  win_length = as.difftime(4L, units = "hours"),
  min_length = as.difftime(10L, units = "mins"),
  value_col = "value",
  id_cols = "hadm_id",
  na_rm = "hadm_id",
  time_scale = "hours",
  round_fun = round,
  ...) {

  combine_itms <- function(x, ...) {
    res <- lapply(x, mimic_get_items, value_col = value_col, id_cols = id_cols,
                  na_rm = na_rm, split_items = TRUE, time_scale = "mins", ...)
    res <- lapply(do.call("c", res),
                  function(y) y[, c("hadm_id", "rel_time"), with = FALSE])
    res <- unique(data.table::rbindlist(res))
    data.table::setkeyv(res, c("hadm_id", "rel_time"))
  }

  final_units <- function(x) {
    units(x) <- time_scale
    round_fun(x)
  }

  assert_that(!identical(time_scale, "secs"))

  message("fetching mechanical ventilation info")

  units(win_length) <- "mins"
  units(min_length) <- "mins"

  start <- combine_itms(list(start_items), ...)[, start_time := rel_time]
  stop <- combine_itms(stop_items, ...)[, stop_time := rel_time]

  merged <- stop[start, roll = -win_length]
  merged <- merged[is.na(stop_time), stop_time := start_time + win_length]
  merged <- merged[stop_time - start_time >= min_length, ]
  merged <- merged[, c("start_time", "stop_time") := list(
    final_units(start_time), final_units(stop_time)
  )]

  res <- unique(
    expand_limits(merged, min_col = "start_time", max_col = "stop_time",
                  id_cols = "hadm_id")
  )
  res <- res[, vent := TRUE]

  res
}

mimic_sofa_coag <- function(platelets = 51265L, unit_handler = allow_na,
                            agg_fun = min, ...) {

  message("fetching platelet counts")

  res <- mimic_get_items(items = c(coag = platelets),
                         item_table = "d_labitems",
                         unit_handler = unit_handler, ...)

  assert_that(has_unit(res, "coag", "K/uL"))

  aggregate_vals(res, agg_fun, by_cols = c("hadm_id", "rel_time"),
                 val_col = "coag")
}

mimic_sofa_bili <- function(bilirubin = 50885L, unit_handler = allow_na,
                            agg_fun = max, ...) {

  message("fetching bilirubin measurements")

  res <- mimic_get_items(items = c(bili = bilirubin),
                         item_table = "d_labitems",
                         unit_handler = unit_handler, ...)

  assert_that(has_unit(res, "bili", "mg/dL"))

  aggregate_vals(res, agg_fun, by_cols = c("hadm_id", "rel_time"),
                 val_col = "bili",)
}

mimic_sofa_map <- function(items = c(220052L, 220181L), agg_fun = min, ...) {

  message("fetching map measurements")

  res <- mimic_get_items(items = items, split_items = FALSE, ...)
  res <- aggregate_vals(res, agg_fun, new_col = "map")

  assert_that(has_unit(res, "map", "mmHg"))

  res[, icustay_id := NULL]
  res
}

mimic_sofa_vaso <- function(norepi = 221906L, epi = 221289L, dopa = 221662L,
                            dobu = 221653L, agg_fun = max,
                            unit_col = "rateuom", value_col = "rate", ...) {

  message("fetching vasopressor dosages")

  vaso <- mimic_get_items(
    items = c(norepi = norepi, epi = epi, dopa = dopa, dobu = dobu),
    unit_col = unit_col, value_col = value_col, ...
  )

  vaso <- lapply(vaso, aggregate_vals, fun = agg_fun)

  res <- reduce(merge, vaso, by = c("hadm_id", "icustay_id", "rel_time"),
                all = TRUE)

  assert_that(
    has_unit(res, "dopa", "mcg/kg/min"),
    has_unit(res, "norepi", "mcg/kg/min"),
    has_unit(res, "dobu", "mcg/kg/min"),
    has_unit(res, "epi", "mcg/kg/min")
  )

  res[, icustay_id := NULL]
  res
}

gcs_callback <- function(x, sed_id = 223900L, sed_val = "No Response-ETT") {

  err <- x[["error"]] == 1L
  if (any(err)) {
    message("removing ", sum(err), " rows due to error == 1L")
    x <- x[!err, ]
  }

  assert_that(!any(x[["valuenum"]] == 0))

  sed <- x[["itemid"]] == sed_id & x[["value"]] == sed_val
  if (any(sed)) {
    message("setting ", sum(sed), " sedation instances to zero")
    x <- x[sed, valuenum := 0]
  }

  assert_that(!any(is.na(x[["valuenum"]])))

  x
}

mimic_sofa_gcs <- function(verbal = 223900L, motor = 223901L, eye = 220739L,
                           callback = gcs_callback, agg_fun = min,
                           win_length = as.difftime(6L, units = "hours"),
                           set_na_max = TRUE, ...) {

  repl_na <- function(x, val) {

    repl <- is.na(x)
    if (any(repl)) {
      message("replacing ", sum(repl), " values with ", val)
      x[repl] <- val
    }

    x
  }

  impute_na_prev <- function(x) {

    res <- x[length(x)]

    if (is.na(res)) {
      not_na <- !is.na(x)
      if (any(not_na)) return(tail(x[not_na], n = 1L))
    }

    res
  }

  is_zero <- function(x) !is.na(x) & x == 0

  message("fetching gcs scores")

  gcs_cols <- c("eye", "verbal", "motor")

  res <- mimic_get_items(
    items = c(verbal = verbal, motor = motor, eye = eye),
    callback = callback, ...
  )

  res <- lapply(res, aggregate_vals, fun = agg_fun)
  res <- reduce(merge, res, by = c("hadm_id", "icustay_id", "rel_time"),
                all = TRUE)

  assert_that(
    has_unit(res, "eye", NA_character_),
    has_unit(res, "verbal", NA_character_),
    has_unit(res, "motor", NA_character_)
  )

  zeros <- reduce(`|`, res[, lapply(.SD, is_zero), .SDcols = gcs_cols])

  if (any(zeros)) {
    message("Setting ", sum(zeros), " rows to max gcs values due to a zero.")
    res <- res[zeros, (gcs_cols) := list(4, 5, 6)]
  }

  res <- window_quo(res,
    substitute(list(eye_imp = fun(eye), verb_imp = fun(verbal),
                    mot_imp = fun(motor)),
               list(fun = impute_na_prev)),
    id_cols = c("hadm_id", "icustay_id"), window_length = win_length
  )

  if (set_na_max) {
    res <- res[, (gcs_cols) := list(
      repl_na(eye_imp, 4), repl_na(verb_imp, 5), repl_na(mot_imp, 6)
    )]
  }

  res <- res[, gcs := eye_imp + verb_imp + mot_imp]

  res <- data.table::set(res,
    j = c(gcs_cols, "eye_imp", "verb_imp", "mot_imp", "icustay_id"),
    value = NULL
  )

  res
}

mimic_sofa_crea <- function(items = 50912L, unit_handler = allow_na,
                            agg_fun = max, ...) {

  message("fetching creatinine measurements")

  res <- mimic_get_items(items = c(crea = items), item_table = "d_labitems",
                         unit_handler = unit_handler, ...)

  assert_that(has_unit(res, "crea", "mg/dL"))

  aggregate_vals(res, agg_fun, by_cols = c("hadm_id", "rel_time"),
                 new_col = "crea")
}

mimic_sofa_urine <- function(
  items = c(226557L:226561L, 226563L:226567L, 227510L, 226584L),
  agg_fun = sum,
  icu_limits = mimic_get_icu_stays("min", "max", NULL, data_env = data_env),
  win_fun = sum_or_na,
  full_win = TRUE,
  data_env = "mimic",
  ...) {

  message("fetching urine measurements")

  res <- mimic_get_items(items = items, split_items = FALSE,
                         data_env = data_env, ...)
  res <- aggregate_vals(res, agg_fun, new_col = "urine")

  assert_that(has_unit(res, "urine", "mL"))

  res <- make_regular(res, time_col = "rel_time",
                      id_cols = c("hadm_id", "icustay_id"),
                      limits = icu_limits)

  res <- window_quo(res,
    substitute(list(urine_24 = win_agg_fun(urine)),
               list(win_agg_fun = win_fun)),
    id_cols = c("hadm_id", "icustay_id"), full_window = full_win
  )

  data.table::setattr(res[["urine_24"]], "units",
                      attr(res[["urine"]], "units"))
  res <- res[, c("icustay_id", "urine") := NULL]

  res
}

mimic_sofa_vars <- function(data_env = "mimic", ...,
  pafi = mimic_sofa_pafi(data_env = data_env, ...),
  vent = mimic_sofa_vent(data_env = data_env, ...),
  coag = mimic_sofa_coag(data_env = data_env, ...),
  bili = mimic_sofa_bili(data_env = data_env, ...),
  map  = mimic_sofa_map(data_env = data_env, ...),
  vaso = mimic_sofa_vaso(data_env = data_env, ...),
  gcs  = mimic_sofa_gcs(data_env = data_env, ...),
  crea = mimic_sofa_crea(data_env = data_env, ...),
  urine = mimic_sofa_urine(data_env = data_env, ...)) {

  tables <- list(pafi, vent, coag, bili, map, vaso, gcs, crea, urine)

  assert_that(
    all(vapply(tables, is_dt, logical(1L)))
  )

  dat <- reduce(merge, tables, by = c("hadm_id", "rel_time"), all = TRUE)

  limits <- dat[, list(min = min(rel_time), max = max(rel_time)),
                by = "hadm_id"]

  hadm <- mimic_get_icu_stays(NULL, NULL, data_env = data_env)
  hadm <- hadm[, list(disch = max(disch)), by = "hadm_id"]

  limits <- merge(limits, hadm, by = "hadm_id", all.x = TRUE)
  limits <- limits[, list(min = min(as.difftime(0, units = "hours"), min),
                          max = max(max, disch, na.rm = TRUE)), by = "hadm_id"]

  make_regular(dat, time_col = "rel_time", id_cols = "hadm_id",
               limits = limits)
}
