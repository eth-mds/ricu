
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

  res <- mimic_ts_unit_quo("prescriptions", rows, cols, id_cols,
                           time_col, ..., val_cols = "dose_val_rx",
                           unit_cols = "dose_unit_rx")

  date_cols <- intersect(colnames(res), c("startdate", "enddate"))
  date_inds <- paste0(date_cols, "_date")

  update_ts_def(res, new_ts_date(date_cols))
}

#' @export
mimic_input_mv <- function(cols = character(0L), rows = NULL,
                           id_cols = "hadm_id", time_col = "starttime", ...) {

  val_cols  <- c("amount",    "rate",    "totalamount")
  unit_cols <- c("amountuom", "rateuom", "totalamountuom")

  mimic_ts_unit_quo("inputevents_mv", rows, cols, id_cols, time_col,
                     ..., val_cols = val_cols, unit_cols = unit_cols)
}
