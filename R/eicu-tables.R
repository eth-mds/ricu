
#' @export
eicu_inf_drug <- function(cols = character(0L), rows = NULL,
                          id_cols = "patienthealthsystemstayid",
                          time_col = "infusionoffset", ...) {

  eicu_ts_quo("infusiondrug", rows, cols, id_cols, time_col, ...)
}

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
eicu_resp_chart <- function(cols = character(0L), rows = NULL,
                            id_cols = "patienthealthsystemstayid",
                            time_col = "respchartoffset", ...) {

  eicu_ts_quo("respiratorycharting", rows, cols, id_cols, time_col, ...)
}

#' @export
eicu_patient <- function(cols = character(0L), rows = NULL,
                         id_cols = "patienthealthsystemstayid",
                         time_col = "unitadmitoffset", ...) {

  eicu_ts_quo("patient", rows, cols, id_cols, time_col, ...)
}

#' @export
eicu_resp_care <- function(cols = character(0L), rows = NULL,
                           id_cols = "patienthealthsystemstayid",
                           time_col = "respcarestatusoffset", ...) {

  eicu_ts_quo("respiratorycare", rows, cols, id_cols, time_col, ...)
}

#' @export
eicu_vital_period <- function(cols = character(0L), rows = NULL,
                              id_cols = "patienthealthsystemstayid",
                              time_col = "observationoffset", ...) {

  eicu_ts_quo("vitalperiodic", rows, cols, id_cols, time_col, ...)
}
