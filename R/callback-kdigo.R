# ==============================================================================
# 
# KDIGO Callbacks
#
# based on YAIB: https://github.com/rvandewater/YAIB-cohorts/tree/main/ricu-extensions/callbacks
# ==============================================================================
kdigo_crea <- function(..., keep_components = FALSE, interval = NULL) {
  cnc <- c("crea")
  crea <- ricu:::collect_dots(cnc, interval, ...)

  id <- id_vars(crea)
  ind <- index_var(crea)

  min_over_period <- function(dur = hours(1L)) {
    cdur <- as.character(dur)
    summ <- slide(
      crea,
      list(crea = min(get("crea"), na.rm = TRUE)),
      dur,
      left_closed = FALSE
    )
    rename_cols(summ, paste0("crea_", cdur, "hr"), "crea")
  }

  res <- lapply(hours(2 * 24, 7 * 24), min_over_period)
  res <- merge_lst(c(list(crea), res))
  res[, kdigo_crea := data.table::fcase(
      crea >= 3 * crea_168hr                         , 3L,
      crea >= 4 &
        (crea - crea_48hr >= 0.3 |
         crea >= 1.5 * crea_168hr)                   , 3L,
      crea >= 2 * crea_168hr                         , 2L,
      crea >= crea_48hr + 0.3                        , 1L,
      crea >= 1.5 * crea_168hr                       , 1L,
      default = 0L
  )]

  cols_rm <- c("crea_48hr", "crea_168hr")
  if (!keep_components) {
    cols_rm <- c(cols_rm, "crea")
  }
  res <- rm_cols(res, cols_rm, skip_absent = TRUE, by_ref = TRUE)
  res
}


urine_rate <- function(x, max_gap = hours(24L), interval = NULL, id_type = "icustay") {
  # TODO: Does not currently work as a rec_cncpt. For example, currently keep_components = TRUE would lead to
  #       a situation in which `urine` and not `urine_rate` is passed back. This is likely because `fun_itm`,
  #       which is currently expects a table with a single column. If multiple are present, it chooses the first,
  #       which in this case is `urine`. Unhelpfully, this is then renamed to `urine_rate`, hiding this
  #       behaviour.
  #       Solution: remove keep_components for now and use only as `fun_itm`
  id <- id_var(x)
  ind <- index_var(x)

  res <- rename_cols(x, "urine", old = data_var(x))

  res[, tm := get(ind) - data.table::shift(get(ind)) + 1L, by = c(id)]
  res[, tm := ifelse(is.na(tm) | tm > max_gap, 1, tm)]
  res[, val_var := urine / tm]

  cols_rm <- c("tm", "urine")
  res <- rm_cols(res, cols_rm, skip_absent = TRUE, by_ref = TRUE)
  res
}


kdigo_urine <- function(..., keep_components = FALSE, interval = NULL) {
  cnc <- c("urine_rate", "weight")
  res <- ricu:::collect_dots(cnc, interval, ...)
  urine_rate <- res[["urine_rate"]]
  weight <- res[["weight"]]

  id <- id_vars(urine_rate)
  ind <- index_var(urine_rate)

  rate_over_period <- function(dur = hours(1L)) {
    name <- paste0("urine_rate_", as.character(dur), "hr")
    summ <- slide(urine_rate, list(urine_h = sum(get("urine_rate"), na.rm = TRUE)), dur, left_closed = FALSE)
    summ[weight, urine_h := urine_h / ifelse(is.na(weight), 75, weight), on = c(id)]
    summ <- rename_cols(summ, name, "urine_h")
    summ[, .SD, .SDcols = c(id, ind, name)]
  }

  res <- lapply(hours(6L, 12L, 24L), rate_over_period)
  res <- merge_lst(c(list(urine_rate, weight), res))
  res[, kdigo_urine := data.table::fcase( # TODO: make work with intervals other than
    get(ind) >= hours(24L) & urine_rate_24hr < 0.3, 3L,
    get(ind) >= hours(12L) & urine_rate_12hr == 0 , 3L,
    get(ind) >= hours(12L) & urine_rate_12hr < 0.5, 2L,
    get(ind) >= hours(6L)  & urine_rate_6hr  < 0.5, 1L,
    default = 0L
  )]

  cols_rm <- c(
    "urine_rate_6hr", "urine_rate_12hr", "urine_rate_24hr"
  )
  if (!keep_components) {
    cols_rm <- c(cols_rm, "urine_rate", "weight")
  }
  res <- rm_cols(res, cols_rm, skip_absent = TRUE, by_ref = TRUE)
  res
}


kdigo <- function(..., keep_components = FALSE, interval = NULL) {
  cnc <- c("kdigo_crea", "kdigo_urine")
  res <- ricu:::collect_dots(cnc, interval, ...)
  kdigo_crea <- res[["kdigo_crea"]]
  kdigo_urine <- res[["kdigo_urine"]]

  idc <- id_vars(kdigo_crea)
  indc <- index_var(kdigo_crea)
  idu <- id_vars(kdigo_urine)
  indu <- index_var(kdigo_urine)

  res <- merge(kdigo_crea, kdigo_urine, by.x = c(idc, indc), by.y = c(idu, indu), all = TRUE)
  res[, kdigo := pmax(kdigo_crea, kdigo_urine, na.rm = TRUE)]

  if (!keep_components) {
    cols_rm <- c("kdigo_crea", "kdigo_urine")
    res <- rm_cols(res, cols_rm, skip_absent = TRUE, by_ref = TRUE)
  }
  res
}


aki <- function(..., threshold = 1L, interval = NULL, keep_components = FALSE) {

  cnc <- c("kdigo")
  res <- ricu:::collect_dots(cnc, interval, ...)
  res[, aki := kdigo >= threshold]

  if (!keep_components) {
    res <- rm_cols(res, "kdigo", skip_absent = TRUE, by_ref = TRUE)
  }

  res[aki == TRUE]
}
