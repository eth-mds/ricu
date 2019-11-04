
do_get_items <- function(items, table_name, data_env, item_col, na_rm = NULL,
                         subset_expr = NULL) {

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

aggregate_vals <- function(tbl, fun = mean,
                           by_cols = c("hadm_id", "icustay_id", "rel_time"),
                           val_col = setdiff(colnames(tbl), by_cols),
                           new_col = val_col, ...) {

  if (is.null(val_col)) val_col <- setdiff(colnames(tbl), by_cols)
  if (is.null(by_cols)) by_cols <- setdiff(colnames(tbl), val_col)

  assert_that(
    data.table::is.data.table(tbl), is.function(fun), is.string(val_col),
    is.character(by_cols), length(by_cols) > 0L,
    all(c(val_col, by_cols) %in% colnames(tbl))
  )

  if (nrow(tbl) == 0) return(tbl)

  units <- attr(tbl[[val_col]], "units")

  tbl <- tbl[, setNames(lapply(.SD, fun, ...), new_col), .SDcols = val_col,
             by = by_cols]

  if (!is.null(units)) {
    data.table::setattr(tbl[[new_col]], "units", units)
  }

  tbl
}
