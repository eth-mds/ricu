
#' @export
mimic_prescr <- function(row_expr, ...) {
  mimic_prescr_(null_or_subs(row_expr), ...)
}

#' @export
mimic_prescr_ <- function(row_quo = NULL, extra_cols = character(0L),
                          id_cols = "hadm_id", time_col = "startdate",
                          id_names = id_cols, time_name = "hadm_time",
                          interval = hours(1L), envir = "mimic",
                          date_extra = days(1L) - interval) {

  add_delta <- function(x) x + date_extra

  assert_that(is.character(extra_cols))

  res <- mimic_tbl_("prescriptions", row_quo,
                    unique(c(id_cols, time_col, extra_cols)), interval, envir)

  time_cols <- intersect(colnames(res), c("startdate", "enddate"))
  time_delta <- paste0(time_cols, "_ub")

  res <- res[, c(time_delta) := lapply(.SD, add_delta), .SDcols = time_cols]

  res <- setnames(res, c(id_cols, time_col), c(id_names, time_name))

  new_ts_tbl(res, id_names, time_name, as.numeric(interval))
}
