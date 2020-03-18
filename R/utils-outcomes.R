
#' @export
outcome <- function(source, type = c("mortality", "extended_stay"), ...) {

  type <- match.arg(type)

  switch(
    match.arg(type),
    mortality = switch(
      as_src(source),
      mimic = mimic_mortality(source, ...),
      eicu  = eicu_mortality(source, ...),
      hirid = stop("TODO"),
      stop("Data source not recognized.")
    ),
    extended_stay = stop("TODO")
  )

}

mimic_mortality <- function(source, id_col = "hadm_id") {

  if (!identical(id_col, "hadm_id")) stop("TODO")

  exp_flag <- "hospital_expire_flag"

  res <- data_tbl(source, "admissions", cols = c(id_col, exp_flag))

  res <- set(res, j = exp_flag, value = as.logical(res[[exp_flag]]))
  res <- rename_cols(res, "expired", exp_flag)

  res
}

eicu_mortality <- function(source, id_col = "patienthealthsystemstayid") {

  assert_that(is.string(id_col),
              id_col %in% c("patienthealthsystemstayid", "patientunitstayid"))

  status <- "hospitaldischargestatus"

  res <- data_tbl(source, "patient", cols = c(id_col, status))

  res <- set(res, j = status, value = res[[status]] == "Expired")
  res <- rename_cols(res, "expired", status)

  res[, list(expired = any(get("expired"))), by = id_col]
}
