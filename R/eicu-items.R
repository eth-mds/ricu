
eicu_get_lab_items <- function(items, data_env = "eicu",
  time_col = "labresultoffset", unit_col = "labmeasurenamesystem",
  value_col = "labresult",
  id_cols = c(hadm_id = "patienthealthsystemstayid",
              icustay_id = "patientunitstayid"),
  na_rm = c("patientunitstayid", value_col), item_col = "labname", ...) {

  eicu_get_event_items(items, "lab", time_col, unit_col, value_col,
                       id_cols, data_env, na_rm, item_col, ...)
}

eicu_get_event_items <- function(items, table_name, time_col, unit_col,
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

    x <- x[, c(id_cols, rel_time_col, value_col), with = FALSE]
    x <- data.table::setnames(x, old = id_cols, new = names(id_cols))
  }

  assert_that(!is.null(names(id_cols)))

  dat <- do_get_items(items, table_name, data_env, item_col, na_rm,
                      subset_expr)
  dat <- eicu_admit_difftime(dat, data_env, time_col, rel_time_col,
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
      new_names <- list(c(names(id_cols), rel_time_col, names(items)))
    } else {
      new_names <- names(items)[match(names(res), items)]
      new_names <- lapply(new_names,
                          function(x) c(names(id_cols), rel_time_col, x))
    }

    res <- Map(data.table::setnames, res, new_names)
  }

  if (split_items) {
    res
  } else {
    res[[1L]]
  }
}

eicu_admit_difftime <- function(dat, data_env = "eicu",
  time_col = "labresultoffset", rel_time_col = "rel_time",
  time_scale = "hours", round_fun = round) {

  fix_rel_time <- function(col, offset, ts, rfun) {
    tmp <- as.difftime(col - offset, units = "mins")
    units(tmp) <- time_scale
    round_fun(tmp)
  }

  adm <- eicu_get_admissions(data_env = data_env)

  dat <- merge(dat, adm, by = "patientunitstayid", all = FALSE)

  dat <- dat[, c(rel_time_col) := fix_rel_time(
    get(time_col), hospitaladmitoffset
  )]

  dat
}

