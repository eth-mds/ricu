
#' @export
eicu_lab <- function(cols = character(0L), rows = NULL,
                     id_cols = "patienthealthsystemstayid",
                     time_col = "labresultoffset", ...) {

  # it might make sense to pool the cols `labmeasurenamesystem` and
  # `labmeasurenameinterface` for units

  eicu_ts_unit_quo("lab", rows, cols, id_cols, time_col, ...,
                   val_cols = "labresult",
                   unit_cols = "labmeasurenameinterface")
}

#' @export
eicu_medication <- function(cols = character(0L), rows = NULL,
                            id_cols = "patienthealthsystemstayid",
                            time_col = "drugstartoffset", ...) {

  eicu_ts_quo("medication", rows, cols, id_cols, time_col, ...)
}

#' @export
eicu_patient <- function(cols = character(0L), rows = NULL,
                         id_cols = "patienthealthsystemstayid",
                         time_col = "unitadmitoffset", ...) {

  eicu_ts_quo("patient", rows, cols, id_cols, time_col, ...)
}
