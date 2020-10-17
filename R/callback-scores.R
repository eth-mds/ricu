
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

  assert_that(is.flag(keep_components), is_interval(win_length))

  cnc <- c("temp", "hr", "resp", "pco2", "wbc", "bnd")
  res <- collect_dots(cnc, interval, ..., merge_dat = TRUE)

  expr <- substitute(
    list(temp = fun(temp), hr = fun(hr), resp = fun(resp), pco2 = fun(pco2),
         wbc = fun(wbc), bnd = fun(bnd)),
    list(fun = locf)
  )

  res <- slide(res, !!expr, before = win_length)

  res <- res[, c(cnc) := list(
    tmpe(get("temp")), hrat(get("hr")),
    rspi(get("resp"), get("pco2")), NULL,
    wbcn(get("wbc"), get("bnd")), NULL)
  ]

  cnc <- data_vars(res)

  res <- setnafill(res, fill = 0L, cols = cnc)
  res <- res[, c("sirs") := rowSums(.SD), .SDcols = cnc]

  if (!keep_components) {
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

  assert_that(is_interval(win_length), is.flag(keep_components))

  cnc <- c("hr", "avpu", "supp_o2", "o2sat", "temp", "sbp", "resp")
  res <- collect_dots(cnc, interval, ..., merge_dat = TRUE)
  res <- res[is.na(get("supp_o2")), c("supp_o2") := FALSE]

  expr <- substitute(
    list(resp = fun(resp), avpu = fun(avpu),
         o2sat = fun(o2sat), supp_o2 = fun(supp_o2),
         temp = fun(temp), hr = fun(hr),
         sbp = fun(sbp)), list(fun = locf)
  )

  res <- slide(res, !!expr, before = win_length)

  res <- res[, c(cnc) := list(
    heart(get("hr")), avpu(get("avpu")), suppo2(get("supp_o2")),
    o2_sat(get("o2sat")), temp(get("temp")),
    sys_bp(get("sbp")), resp(get("resp")))
  ]

  res <- setnafill(res, fill = 0L, cols = cnc)
  res <- res[, c("news") := rowSums(.SD), .SDcols = cnc]

  if (!keep_components) {
    res <- rm_cols(res, cnc, by_ref = TRUE)
  }

  res
}

#' @importFrom data.table setnafill
#' @rdname label_sirs
#' @export
mews_score <- function(..., win_length = hours(24L), keep_components = FALSE,
                       interval = NULL) {

  sys_bp <- map_vals(c(3L, 2L, 1L, 0L,     2L    ), c(70, 80, 100, 199))
  heart  <- map_vals(c(    2L, 1L, 0L, 1L, 2L, 3L), c(40, 50, 100, 110, 129))
  resp   <- map_vals(c(    2L,     0L, 1L, 2L, 3L), c( 9, 14,  20,  29))
  temp   <- map_vals(c(    2L,     0L,     2L    ), c(35, 38.4))

  avpu <- function(x) setNames(c(0L, 1L, 2L, 3L), c("A", "V", "P", "U"))[x]

  assert_that(is_interval(win_length), is.flag(keep_components))

  cnc <- c("hr", "avpu", "temp", "sbp", "resp")
  res <- collect_dots(cnc, interval, ..., merge_dat = TRUE)

  expr <- substitute(
    list(hr = fun(hr), avpu = fun(avpu), temp = fun(temp), sbp = fun(sbp),
         resp = fun(resp)), list(fun = locf)
  )

  res <- slide(res, !!expr, before = win_length)

  res <- res[, c(cnc) := list(
    heart(get("hr")), avpu(get("avpu")), temp(get("temp")), sys_bp(get("sbp")),
    resp(get("resp")))
  ]

  res <- setnafill(res, fill = 0L, cols = cnc)
  res <- res[, c("mews") := rowSums(.SD), .SDcols = cnc]

  if (!keep_components) {
    res <- rm_cols(res, cnc, by_ref = TRUE)
  }

  res
}
