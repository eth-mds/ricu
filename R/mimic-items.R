
mimic_get_items <- function(items,
  item_table = "d_items", data_env = "mimic", ...) {

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

  fun(items, data_env, ...)
}

mimic_get_lab_items <- function(items, data_env,
  time_col = "charttime", unit_col = "valueuom", value_col = "valuenum",
  id_cols = "hadm_id", ...) {

  mimic_get_event_items(items, "labevents", time_col, unit_col, value_col,
                        id_cols, data_env, ...)
}

mimic_get_out_items <- function(items, data_env,
  time_col = "charttime", unit_col = "valueuom", value_col = "value",
  id_cols = c("hadm_id", "icustay_id"), ...) {

  mimic_get_event_items(items, "outputevents", time_col, unit_col, value_col,
                        id_cols, data_env, ...)
}

mimic_get_chart_items <- function(items, data_env,
  time_col = "charttime", unit_col = "valueuom", value_col = "valuenum",
  id_cols = c("hadm_id", "icustay_id"), ...) {

  mimic_get_event_items(items, "chartevents", time_col, unit_col, value_col,
                        id_cols, data_env, ...)
}

mimic_get_incv_items <- function(items, data_env,
  time_col = "charttime", unit_col = "amountuom", value_col = "amount",
  id_cols = c("hadm_id", "icustay_id"), ...) {

  mimic_get_event_items(items, "inputevents_cv", time_col, unit_col, value_col,
                        id_cols, data_env, ...)
}

mimic_get_inmv_items <- function(items, data_env,
  time_col = "starttime", unit_col = "amountuom", value_col = "amount",
  id_cols = c("hadm_id", "icustay_id"), ...) {

  mimic_get_event_items(items, "inputevents_mv", time_col, unit_col, value_col,
                        id_cols, data_env, ...)
}

mimic_get_procmv_items <- function(items, data_env,
  time_col = "starttime", unit_col = "valueuom", value_col = "value",
  id_cols = c("hadm_id", "icustay_id"), ...) {

  mimic_get_event_items(items, "procedureevents_mv", time_col, unit_col,
                        value_col, id_cols, data_env, ...)
}

mimic_get_event_items <- function(items, table_name, time_col, unit_col,
  value_col, id_cols, data_env, na_rm = c(id_cols, value_col),
  item_col = "itemid", subset_expr = NULL, rel_time_col = "rel_time",
  time_scale = "hours", round_fun = round, split_items = length(items) > 1L,
  unit_handler = NULL, callback = NULL) {

  proc_each <- function(x) {

    if (nrow(x) > 0L) {

      x <- handle_unit(x, value_col, unit_col, unit_handler)

      if (!is.null(callback)) {
        x <- callback(x)
      }
    }

    x[, c(id_cols, rel_time_col, value_col), with = FALSE]
  }

  dat <- mimic_do_get_events(items, table_name, data_env, na_rm, item_col,
                             subset_expr)
  dat <- mimic_admit_difftime(dat, data_env, time_col, rel_time_col,
                              time_scale, round_fun)

  if (split_items && length(items) > 1L) {
    dat <- dat[, c(item_col) := factor(get(item_col), levels = items)]
    res <- split(dat, by = item_col)
  } else {
    res <- list(dat)
  }

  res <- lapply(res, proc_each)

  if (!is.null(names(items)) && (length(items) == 1L || length(res) > 1L)) {

    if (length(items) == 1L) {
      new_names <- list(c(id_cols, rel_time_col, names(items)))
    } else {
      new_names <- names(items)[match(names(res), items)]
      new_names <- lapply(new_names, function(x) c(id_cols, rel_time_col, x))
    }

    res <- Map(data.table::setnames, res, new_names)
  }

  if (split_items) {
    res
  } else {
    res[[1L]]
  }
}

mimic_do_get_events <- function(items, table_name, data_env, na_rm = NULL,
                                item_col = "itemid", subset_expr = NULL) {

  assert_that(length(items) > 0L, is.string(item_col))

  if (length(items) == 1L) {
    row_expr <- substitute(item_col == items,
      list(items = items, item_col = as.name(item_col))
    )
  } else {
    row_expr <- substitute(item_col %in% items,
      list(items = items, item_col = as.name(item_col))
    )
  }

  if (!is.null(subset_expr)) {
    row_expr <- substitute((item_subset) & (extra_subset),
      list(item_subset = row_expr, extra_subset = subset_expr)
    )
  }

  dat <- get_table(table_name, data_env)
  dat <- prt::subset_quo(dat, row_expr)

  if (!is.null(na_rm)) {
    nrow_before <- nrow(dat)
    dat <- na.omit(dat, na_rm)
    nrow_rm <- nrow_before - nrow(dat)
    if (nrow_rm > 0L) {
      message("Removed ", nrow_rm, " rows from `", table_name,
              "` due to `NA` in columns\n  ",
              paste0("`", na_rm, "`", collapse = ", "))
    }
  }

  dat
}

mimic_admit_difftime <- function(dat, data_env = "mimic",
  time_col = "charttime", rel_time_col = "rel_time", time_scale = "hours",
  round_fun = round) {

  adm <- mimic_get_admissions(data_env = data_env)

  dat <- merge(dat, adm, by = "hadm_id", all = FALSE)

  dat <- dat[, c(rel_time_col) := round_fun(
    difftime(eval(as.name(time_col)), admittime, units = time_scale)
  )]

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
