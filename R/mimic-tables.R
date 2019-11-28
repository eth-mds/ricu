
#' @export
mimic_admissions <- function(cols = character(0L), rows = NULL,
                             id_cols = "hadm_id", time_col = "dischtime",
                             ...) {

  mimic_ts_quo("admissions", rows, cols, id_cols, time_col, ...)
}

#' @export
mimic_microbio <- function(cols = character(0L), rows = NULL,
                           id_cols = "hadm_id", time_col = "charttime",
                           ...) {

  mimic_ts_date_time_quo("microbiologyevents", rows, cols, id_cols, time_col,
                         ..., date_cols = "chartdate", time_cols = "charttime")
}

#' @export
mimic_prescriptions <- function(cols = character(0L), rows = NULL,
                                id_cols = "hadm_id", time_col = "startdate",
                                ...) {

  delta <- function(n) list(as.difftime(rep.int(24L, n), units = "hours"))

  res <- mimic_ts_unit_quo("prescriptions", rows, cols, id_cols,
                           time_col, ..., val_cols = "dose_val_rx",
                           unit_cols = "dose_unit_rx")

  time_cols <- intersect(colnames(res), c("startdate", "enddate"))
  time_delta <- paste0(time_cols, "_win")

  res <- res[, c(time_delta) := rep(delta(.N), length(time_delta))]

  update_ts_def(res, new_ts_window(time_cols, time_delta))
}

#' @export
mimic_input_mv <- function(cols = character(0L), rows = NULL,
                           id_cols = "hadm_id", time_col = "starttime", ...) {

  val_cols  <- c("amount",    "rate",    "totalamount")
  unit_cols <- c("amountuom", "rateuom", "totalamountuom")

  mimic_ts_unit_quo("inputevents_mv", rows, cols, id_cols, time_col,
                     ..., val_cols = val_cols, unit_cols = unit_cols)
}
