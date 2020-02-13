
#' @export
outcome <- function(source, type = c("mortality", "extended_stay"), ...) {

  type <- match.arg(type)

  switch(
    match.arg(type),
    mortality = switch(
      as_src(source),
      mimic = mimic_mortality(...),
      eicu  = stop("TODO"),
      hirid = stop("TODO"),
      stop("Data source not recognized.")
    ),
    extended_stay = stop("TODO")
  )

}

mimic_mortality <- function() {
  res <- mimic_tbl("admissions", cols = c("hadm_id", "hospital_expire_flag"))
  res[, `:=`(c("expired", "hospital_expire_flag"),
             list(as.logical(hospital_expire_flag), NULL))][]
}
