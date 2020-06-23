
#' SIRS score label
#'
#' The SIRS (Systemic Inflammatory Response Syndrome) score is a commonly used
#' assessment tool used to track a patient's well-being in an ICU.
#'
#' @param temperature, heart_rate, respiratory_rate, pa_co2,white_blood_cells, bands Data input used for SIRS score evaluation
#' @param interval Time series interval (only used for checking consistency
#' of input data)
#'
#' @rdname label_sirs
#' @export
#'
sirs_score <- function(temperature, heart_rate, respiratory_rate, pa_co2,
                       white_blood_cells, bands,
                       interval = ricu::interval(temperature)) {

  temp <- function(x) fifelse(x < 36 | x > 38, 1L, 0L)
  hrat <- function(x) fifelse(x > 90, 1L, 0L)

  resp <- function(re, pa) fifelse(re > 20 | pa < 32, 1L, 0L)
  wbcn <- function(wb, ba) fifelse(wb < 4 | wb > 12 | ba > 10, 1L, 0L)

  temperature <- temperature[, c("temperature") := temp(get("temperature"))]
  heart_rate  <- heart_rate[,  c("heart_rate")  := hrat(get("heart_rate"))]

  respiratory <- merge(respiratory_rate, pa_co2, all = TRUE)
  respiratory <- respiratory[,
    c("respiratory", "respiratory_rate", "pa_co2") := list(
      resp(get("respiratory_rate"), get("pa_co2")), NULL, NULL
    )
  ]

  wbc <- merge(white_blood_cells, bands, all = TRUE)
  wbc <- wbc[, c("wbc", "white_blood_cells", "bands") := list(
    wbcn(get("white_blood_cells"), get("bands")), NULL, NULL
  )]

  res <- reduce(merge, list(temperature, heart_rate, respiratory, wbc),
                all = TRUE)

  res <- res[, c("sirs_score") := rowSums(.SD, na.rm = TRUE),
    .SDcols = c("temperature", "heart_rate", "respiratory", "wbc")
  ]

  rm_cols(res, c("temperature", "heart_rate", "respiratory", "wbc"),
          by_ref = TRUE)
}
