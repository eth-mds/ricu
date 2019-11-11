
mimic_get_data_items <- function(items, item_table = "d_items",
  data_env = "mimic", item_col = "itemid", id_cols = "hadm_id", ...) {

  table <- determine_event_table(items, item_table, data_env)

  fun <- switch(table,
    labevents = mimic_get_lab_items,
    outputevents = mimic_get_out_items,
    chartevents = mimic_get_chart_items,
    inputevents_cv = mimic_get_incv_items,
    inputevents_mv = mimic_get_inmv_items,
    procedureevents_mv = mimic_get_procmv_items,
    stop("No matching function for table `", table, "`.")
  )

  fun(items, data_env, mimic_admit_difftime, item_col, id_cols, ...)
}

mimic_get_lab_items <- function(items, data_env, difftime_fun, item_col,
  id_cols, time_col = "charttime", value_cols = "valuenum",
  unit_cols = "valueuom", ...) {

  get_data_items(items, "labevents", data_env, difftime_fun, item_col, id_cols,
    time_col, value_cols, unit_cols, ...)
}

mimic_get_out_items <- function(items, data_env, difftime_fun, item_col,
  id_cols, time_col = "charttime", value_cols = "value",
  unit_cols = "valueuom", ...) {

  get_data_items(items, "outputevents", data_env, difftime_fun, item_col,
    id_cols, time_col, value_cols, unit_cols, ...)
}

mimic_get_chart_items <- function(items, data_env, difftime_fun, item_col,
  id_cols, time_col = "charttime", value_cols = "valuenum",
  unit_cols = "valueuom", ...) {

  get_data_items(items, "chartevents", data_env, difftime_fun, item_col,
    id_cols, time_col, value_cols, unit_cols, ...)
}

mimic_get_incv_items <- function(items, data_env, difftime_fun, item_col,
  id_cols, time_col = "charttime", value_cols = "amount",
  unit_cols = "amountuom", ...) {

  get_data_items(items, "inputevents_cv", data_env, difftime_fun, item_col,
    id_cols, time_col, value_cols, unit_cols, ...)
}

mimic_get_inmv_items <- function(items, data_env, difftime_fun, item_col,
  id_cols, time_col = "starttime", value_cols = "amount",
  unit_cols = "amountuom", ...) {

  get_data_items(items, "inputevents_mv", data_env, difftime_fun, item_col,
    id_cols, time_col, value_cols, unit_cols, ...)
}

mimic_get_procmv_items <- function(items, data_env, difftime_fun, item_col,
  id_cols, time_col = "starttime", value_cols = "value",
  unit_cols = "valueuom", ...) {

  get_data_items(items, "procedureevents_mv", data_env, difftime_fun, item_col,
    id_cols, time_col, value_cols, unit_cols, ...)
}

mimic_admit_difftime <- function(dat, data_env = "mimic",
  time_col = "charttime", time_name = "hadm_time", time_scale = "hours",
  step_size = 1L) {

  adm <- mimic_get_admissions(data_env = data_env)

  nrow_before <- nrow(dat)
  dat <- merge(dat, adm, by = "hadm_id", all = FALSE)
  nrow_rm <- nrow_before - nrow(dat)

  if (nrow_rm > 0L) {
    message("Lost ", nrow_rm, " rows determining `", time_name, "`.")
  }

  dat <- dat[, c(time_name) := round_to(
    difftime(eval(as.name(time_col)), admittime, units = time_scale), step_size
  )]

  data.table::setattr(dat[[time_name]], "step_size", step_size)

  dat
}

determine_event_table <- function(items,
  item_table = c("d_items", "d_labitems"), data_env = "mimic") {

  item_table <- match.arg(item_table)

  assert_that(length(items) > 0L)

  lookup <- get_table(item_table, data_env)
  assert_that(all(items %in% lookup[["itemid"]]))

  if (item_table == "d_labitems") {
    return("labevents")
  }

  lookup <- lookup[, c("itemid", "linksto")]

  res <- unique(lookup[itemid %in% items, ][["linksto"]])
  assert_that(length(res) == 1L)

  res
}
