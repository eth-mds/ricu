
#' SIRS score label
#'
#' The SIRS (Systemic Inflammatory Response Syndrome) score is a commonly used
#' assessment tool used to track a patient's well-being in an ICU.
#'
#' @param ... Data input used for score
#' evaluation
#' @param win_length Window used for carry forward
#' @param keep_components Logical flag indicating whether to return the
#' individual components alongside the aggregated score
#' @param interval Time series interval (only used for checking consistency
#' of input data)
#'
#' @rdname label_sirs
#' @export
#'
sirs_score <- function(..., win_length = hours(24L), keep_components = FALSE,
                       interval = NULL) {

  tmpe <- function(x) fifelse(x < 36 | x > 38, 1L, 0L)
  hrat <- function(x) fifelse(x > 90, 1L, 0L)

  rspi <- function(re, pa) fifelse(re > 20 | pa < 32, 1L, 0L)
  wbcn <- function(wb, ba) fifelse(wb < 4 | wb > 12 | ba > 10, 1L, 0L)

  win_length <- as_interval(win_length)

  assert_that(is.flag(keep_components))

  cnc <- c("temp", "hr", "resp", "wbc", "pco2", "bnd")
  res <- collect_dots(cnc, interval, ..., merge_dat = TRUE)

  exp <- substitute(lapply(.SD, fun), list(fun = locf))
  res <- slide(res, !!exp, before = win_length, .SDcols = cnc)

  res <- res[, c(cnc) := list(
    tmpe(get("temp")), hrat(get("hr")), rspi(get("resp"), get("pco2")),
    wbcn(get("wbc"), get("bnd")), NULL, NULL)
  ]

  cnc <- head(cnc, n = 4L)
  res <- res[, c("sirs") := rowSums(.SD, na.rm = TRUE), .SDcols = cnc]

  if (isTRUE(keep_components)) {
    res <- rename_cols(res, paste0(cnc, "_comp"), cnc, by_ref = TRUE)
  } else {
    res <- rm_cols(res, cnc, by_ref = TRUE)
  }

  res
}

#' @rdname label_sirs
#' @export
qsofa_score <- function(..., win_length = hours(24L), keep_components = FALSE,
                        interval = NULL) {

  gte <- function(x, val) is_true(x >= val)
  lte <- function(x, val) is_true(x <= val)

  win_length <- as_interval(win_length)

  assert_that(is.flag(keep_components))

  cnc <- c("gcs", "sbp", "resp")
  res <- collect_dots(cnc, interval, ..., merge_dat = TRUE)

  exp <- substitute(lapply(.SD, fun), list(fun = locf))
  res <- slide(res, !!exp, before = win_length, .SDcols = cnc)

  res <- res[, c(cnc) := list(
    lte(get("gcs"), 13), lte(get("sbp"), 100), gte(get("resp"), 22)
  )]

  res <- res[, c("qsofa") := rowSums(.SD, na.rm = TRUE), .SDcols = cnc]

  if (isTRUE(keep_components)) {
    res <- rename_cols(res, paste0(cnc, "_comp"), cnc, by_ref = TRUE)
  } else {
    res <- rm_cols(res, cnc, by_ref = TRUE)
  }

  res
}

#' @rdname label_sirs
#' @export
news_score <- function(..., win_length = hours(24L), keep_components = FALSE,
                       interval = NULL) {

  resp   <- map_vals(c(3L,     1L, 0L, 2L, 3L    ), c( 8,  11,  20,  24))
  o2_sat <- map_vals(c(3L, 2L, 1L, 0L            ), c(91,  93,  95))
  temp   <- map_vals(c(3L,     1L, 0L, 1L, 2L    ), c(35,  36,  38,  39))
  sys_bp <- map_vals(c(3L, 2L, 1L, 0L,         3L), c(90, 100, 110, 219))
  heart  <- map_vals(c(3L,     1L, 0L, 1L, 2L, 3L), c(40,  50,  90, 110, 130))

  suppo2 <- function(x) fifelse(x, 2L, 0L)
  avpu   <- function(x) fifelse(x == "A", 0L, 3L)

  win_length <- as_interval(win_length)

  assert_that(is.flag(keep_components))

  cnc <- c("hr", "avpu", "supp_o2", "o2sat", "temp", "sbp", "resp")
  res <- collect_dots(cnc, interval, ..., merge_dat = TRUE)
  res <- res[is.na(get("supp_o2")), c("supp_o2") := FALSE]

  exp <- substitute(lapply(.SD, fun), list(fun = locf))
  res <- slide(res, !!exp, before = win_length, .SDcols = cnc)

  res <- res[, c(cnc) := list(
    heart(get("hr")), avpu(get("avpu")), suppo2(get("supp_o2")),
    o2_sat(get("o2sat")), temp(get("temp")),
    sys_bp(get("sbp")), resp(get("resp")))
  ]

  res <- res[, c("news") := rowSums(.SD, na.rm = TRUE), .SDcols = cnc]

  if (isTRUE(keep_components)) {
    res <- rename_cols(res, paste0(cnc, "_comp"), cnc, by_ref = TRUE)
  } else {
    res <- rm_cols(res, cnc, by_ref = TRUE)
  }

  res
}

#' @rdname label_sirs
#' @export
mews_score <- function(..., win_length = hours(24L), keep_components = FALSE,
                       interval = NULL) {

  sys_bp <- map_vals(c(3L, 2L, 1L, 0L,     2L    ), c(70, 80, 100, 199))
  heart  <- map_vals(c(    2L, 1L, 0L, 1L, 2L, 3L), c(40, 50, 100, 110, 129))
  resp   <- map_vals(c(    2L,     0L, 1L, 2L, 3L), c( 9, 14,  20,  29))
  temp   <- map_vals(c(    2L,     0L,     2L    ), c(35, 38.4))

  avpu <- function(x) setNames(c(0L, 1L, 2L, 3L), c("A", "V", "P", "U"))[x]

  win_length <- as_interval(win_length)

  assert_that(is.flag(keep_components))

  cnc <- c("hr", "avpu", "temp", "sbp", "resp")
  res <- collect_dots(cnc, interval, ..., merge_dat = TRUE)

  exp <- substitute(lapply(.SD, fun), list(fun = locf))
  res <- slide(res, !!exp, before = win_length, .SDcols = cnc)

  res <- res[, c(cnc) := list(
    heart(get("hr")), avpu(get("avpu")), temp(get("temp")), sys_bp(get("sbp")),
    resp(get("resp")))
  ]

  res <- res[, c("mews") := rowSums(.SD, na.rm = TRUE), .SDcols = cnc]

  if (isTRUE(keep_components)) {
    res <- rename_cols(res, paste0(cnc, "_comp"), cnc, by_ref = TRUE)
  } else {
    res <- rm_cols(res, cnc, by_ref = TRUE)
  }

  res
}
