
get_data_items <- function(items, table_name, data_env, difftime_fun,
  item_col, id_cols, time_col, value_cols, unit_cols = NULL, extra_cols = NULL,
  value_names = NULL, regex = FALSE, na_rm = c(id_cols, time_col, value_cols),
  time_scale = "hours", step_size = 1L,
  split_items = regex || length(items) > 1L,
  unit_handler = NULL, expected_unit = NULL, agg_fun = NULL,
  id_names = "hadm_id", time_name = "hadm_time") {

  if (length(value_cols) > 1L) assert_that(!split_items)

  need_cols <- c(item_col, id_cols, time_col, value_cols, unit_cols,
                 extra_cols)

  dat <- read_data_items(items, table_name, data_env, need_cols, item_col,
                         regex, na_rm)

  dat <- difftime_fun(dat, data_env, time_col, time_name, time_scale,
                      step_size)

  drop_cols <- setdiff(colnames(dat), c(item_col, id_cols, time_name,
                                        value_cols, unit_cols, extra_cols))
  dat <- data.table::set(dat, j = drop_cols, value = NULL)

  assert_that(inherits(dat[[time_name]], "difftime"),
              identical(units(dat[[time_name]]), time_scale),
              all(as.numeric(dat[[time_name]]) %% step_size == 0))

  if (split_items) {
    dat <- split_data_items(dat, item_col, value_cols,
                            if (regex) NULL else items)
  } else if (length(value_cols) > 1L) {
    browser()
  } else {
    dat <- list(dat)
  }

  dat <- lapply(dat, add_data_item_units, value_cols, unit_cols, unit_handler,
                expected_unit)
  dat <- lapply(dat, data.table::set, j = c(item_col, unit_cols, extra_cols),
                value = NULL)

  if (is.null(value_names)) {
    value_names <- rep(list(NULL), length(dat))
  } else if (length(value_names) == 1L) {
    value_names <- rep(value_names, length(dat))
  } else {
    assert_that(length(value_names) == length(dat),
                setequal(names(dat), items))
    value_names <- value_names[match(names(dat), items)]
  }

  dat <- Map(rename_data_items, dat, value_names,
             MoreArgs = list(id_cols, id_names, time_name))

  dat <- lapply(dat, new_ts_tbl, id_names, time_name, step_size)

  if (!is.null(agg_fun)) {
    dat <- lapply(dat, make_unique, agg_fun)
  }

  if (missing(agg_fun) && !all(vapply(dat, is_unique, logical(1L)))) {
    warning("Non-unique id/value combinations found. Consider `agg_fun`.")
  }

  if (split_items) dat
  else             dat[[1L]]
}

read_data_items <- function(items, table_name, data_env, cols, item_col,
                            regex = FALSE, na_rm = NULL) {

  assert_that(is.string(item_col), is.flag(regex))

  if (regex) {

    assert_that(is.string(items))

    row_expr <- substitute(grepl(items, item_col),
      list(items = items, item_col = as.name(item_col))
    )
  } else if (length(items) == 1L) {
    row_expr <- substitute(!is.na(item_col) & item_col == items,
      list(items = items, item_col = as.name(item_col))
    )
  } else {

    assert_that(length(items) > 1L)

    row_expr <- substitute(item_col %in% items,
      list(items = items, item_col = as.name(item_col))
    )
  }

  dat <- get_table(table_name, data_env)
  dat <- prt::subset_quo(dat, row_expr, cols)

  if (!is.null(na_rm)) {

    assert_that(all(na_rm %in% colnames(dat)))

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

split_data_items <- function(dat, item_col, value_cols, items = NULL) {

  if (!is.null(items)) {
    dat <- dat[, c(item_col) := factor(get(item_col), levels = items)]
  }

  split(dat, by = item_col)
}

add_data_item_units <- function(dat, value_col, unit_col, handler = NULL,
                                expected = NULL) {

  if (is.null(unit_col) || nrow(dat) == 0L) return(dat)

  unit <- table(dat[[unit_col]], useNA = "ifany")

  if (!is.null(handler)) {

    assert_that(is.function(handler))

    dat <- handler(dat, unit_col = unit_col, val_col = value_col)
    unit <- table(dat[[unit_col]], useNA = "ifany")
  }

  if (length(unit) != 1L) {
    warning("Multiple units detected: ",
      paste0(names(unit), " (", round(unit / nrow(dat) * 100, 2L), "%)",
             collapse = ", ")
    )
  }

  unit <- names(which.max(unit))

  if (!is.null(expected)) {
    assert_that(is.string(expected), identical(unit, expected))
  }

  data.table::setattr(dat[[value_col]], "units", unit)

  dat
}

rename_data_items <- function(dat, val_name, id_cols, id_names, time_name) {

  assert_that(ncol(dat) == length(id_cols) + 2L,
              all(id_cols %in% colnames(dat)),
              time_name %in% colnames(dat),
              length(id_cols) == length(id_names))

  val_col <- setdiff(colnames(dat), c(id_cols, time_name))
  if (is.null(val_name)) val_name <- val_col

  dat <- data.table::setcolorder(dat, c(id_cols, time_name, val_col))
  dat <- data.table::setnames(dat, c(id_names, time_name, val_name))

  dat
}
