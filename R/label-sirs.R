
#' SIRS score label
#'
#' The SIRS (Systemic Inflammatory Response Syndrome) score is a commonly used
#' assessment tool used to track a patient's well-being in an ICU.
#'
#' @param respiratory_rate,vent_ind,fi_o2,gcs,... Data input used for score
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
sirs_score <- function(respiratory_rate, ..., win_length = hours(24L),
                       keep_components = FALSE,
                       interval = ricu::interval(respiratory_rate)) {

  temp <- function(x) fifelse(x < 36 | x > 38, 1L, 0L)
  hrat <- function(x) fifelse(x > 90, 1L, 0L)

  resp <- function(re, pa) fifelse(re > 20 | pa < 32, 1L, 0L)
  wbcn <- function(wb, ba) fifelse(wb < 4 | wb > 12 | ba > 10, 1L, 0L)

  res <- reduce(merge, c(list(respiratory_rate), list(...)),
                all = TRUE)

  comps <- c("temperature", "heart_rate", "respiratory_rate", "pa_co2",
             "white_blood_cells", "bands")

  assert_that(has_interval(res, interval), is_interval(win_length),
              is.flag(keep_components), has_name(res, comps))

  expr <- substitute(
    list(temperature = fun(temperature), heart_rate = fun(heart_rate),
         respiratory_rate = fun(respiratory_rate), pa_co2 = fun(pa_co2),
         white_blood_cells = fun(white_blood_cells), bands = fun(bands)),
    list(fun = locf)
  )

  res <- slide(res, !!expr, before = win_length)

  res <- res[, c(comps) := list(
    temp(get("temperature")), hrat(get("heart_rate")),
    resp(get("respiratory_rate"), get("pa_co2")), NULL,
    wbcn(get("white_blood_cells"), get("bands")), NULL)
  ]

  comps <- data_vars(res)

  res <- setnafill(res, fill = 0L, cols = comps)
  res <- res[, c("sirs_score") := rowSums(.SD), .SDcols = comps]

  if (!keep_components) {
    res <- rm_cols(res, comps, by_ref = TRUE)
  }

  res
}

#' @rdname label_sirs
#' @export
supp_o2 <- function(vent_ind, fi_o2, interval = ricu::interval(vent_ind)) {

  res <- merge(vent_ind, fi_o2, all = TRUE)

  assert_that(has_interval(res, interval))

  res <- res[, c("supp_o2", "vent_ind", "fi_o2") := list(
    get("vent_ind") | get("fi_o2") > 21, NULL, NULL
  )]

  res
}

map_vals <- function(pts, vals) {
  function(x) pts[findInterval(x, vals, left.open = TRUE) + 1]
}

#' @param by_ref Logical flag indicating whether to change the input by
#' reference
#' @rdname label_sirs
#' @export
avpu <- function(gcs, interval = ricu::interval(gcs), by_ref = TRUE) {

  avpu_map <- map_vals(c(NA, "U", "P", "V", "A", NA), c(2, 3, 9, 13, 15))

  assert_that(has_interval(gcs, interval))

  if (isFALSE(by_ref)) {
    gcs <- copy(gcs)
  }

  gcs <- gcs[, c("avpu", "gcs") := list(avpu_map(get("gcs")), NULL)]

  gcs
}

#' @rdname label_sirs
#' @export
news_score <- function(respiratory_rate, ..., win_length = hours(24L),
                       keep_components = FALSE,
                       interval = ricu::interval(respiratory_rate)) {

  resp   <- map_vals(c(3L,     1L, 0L, 2L, 3L    ), c( 8,  11,  20,  24))
  o2_sat <- map_vals(c(3L, 2L, 1L, 0L            ), c(91,  93,  95))
  temp   <- map_vals(c(3L,     1L, 0L, 1L, 2L    ), c(35,  36,  38,  39))
  sys_bp <- map_vals(c(3L, 2L, 1L, 0L,         3L), c(90, 100, 110, 219))
  heart  <- map_vals(c(3L,     1L, 0L, 1L, 2L, 3L), c(40,  50,  90, 110, 130))

  supp_o2 <- function(x) fifelse(x, 2L, 0L)
  avpu    <- function(x) fifelse(x == "A", 0L, 3L)

  res <- reduce(merge, c(list(respiratory_rate), list(...)),
                all = TRUE)
  res <- res[is.na(get("supp_o2")), c("supp_o2") := FALSE]

  comps <- c("heart_rate", "avpu", "supp_o2", "o2_saturation", "temperature",
             "systolic_bp", "respiratory_rate")

  assert_that(has_interval(res, interval), is_interval(win_length),
              is.flag(keep_components), has_name(res, comps))

  expr <- substitute(
    list(respiratory_rate = fun(respiratory_rate), avpu = fun(avpu),
         o2_saturation = fun(o2_saturation), supp_o2 = fun(supp_o2),
         temperature = fun(temperature), heart_rate = fun(heart_rate),
         systolic_bp = fun(systolic_bp)), list(fun = locf)
  )

  res <- slide(res, !!expr, before = win_length)

  res <- res[, c(comps) := list(
    heart(get("heart_rate")), avpu(get("avpu")), supp_o2(get("supp_o2")),
    o2_sat(get("o2_saturation")), temp(get("temperature")),
    sys_bp(get("systolic_bp")), resp(get("respiratory_rate")))
  ]

  res <- setnafill(res, fill = 0L, cols = comps)
  res <- res[, c("news_score") := rowSums(.SD), .SDcols = comps]

  if (!keep_components) {
    res <- rm_cols(res, comps, by_ref = TRUE)
  }

  res
}

#' @importFrom data.table setnafill
#' @rdname label_sirs
#' @export
mews_score <- function(respiratory_rate, ..., win_length = hours(24L),
                       keep_components = FALSE,
                       interval = ricu::interval(respiratory_rate)) {

  sys_bp <- map_vals(c(3L, 2L, 1L, 0L,     2L    ), c(70, 80, 100, 199))
  heart  <- map_vals(c(    2L, 1L, 0L, 1L, 2L, 3L), c(40, 50, 100, 110, 129))
  resp   <- map_vals(c(    2L,     0L, 1L, 2L, 3L), c( 9, 14,  20,  29))
  temp   <- map_vals(c(    2L,     0L,     2L    ), c(35, 38.4))

  avpu <- function(x) setNames(c(0L, 1L, 2L, 3L), c("A", "V", "P", "U"))[x]

  res <- reduce(merge, c(list(respiratory_rate), list(...)), all = TRUE)

  comps <- c("heart_rate", "avpu", "temperature", "systolic_bp",
             "respiratory_rate")

  assert_that(has_interval(res, interval), is_interval(win_length),
              is.flag(keep_components), has_name(res, comps))

  expr <- substitute(
    list(heart_rate = fun(heart_rate), avpu = fun(avpu),
         temperature = fun(temperature), systolic_bp = fun(systolic_bp),
         respiratory_rate = fun(respiratory_rate)), list(fun = locf)
  )

  res <- slide(res, !!expr, before = win_length)

  res <- res[, c(comps) := list(
    heart(get("heart_rate")), avpu(get("avpu")), temp(get("temperature")),
    sys_bp(get("systolic_bp")), resp(get("respiratory_rate")))
  ]

  res <- setnafill(res, fill = 0L, cols = comps)
  res <- res[, c("mews_score") := rowSums(.SD), .SDcols = comps]

  if (!keep_components) {
    res <- rm_cols(res, comps, by_ref = TRUE)
  }

  res
}
