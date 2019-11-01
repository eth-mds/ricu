
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

  units <- attr(tbl[[val_col]], "units")

  tbl <- tbl[, setNames(lapply(.SD, fun, ...), new_col), .SDcols = val_col,
             by = by_cols]

  if (!is.null(units)) {
    data.table::setattr(tbl[[new_col]], "units", units)
  }

  tbl
}
