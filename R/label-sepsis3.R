
#' @export
delta_cummin <- function(x) {
  x - cummin(ifelse(is.na(x), .Machine$integer.max, x))
}

#' @export
delta_start <- function(x) x - x[!is.na(x)][1L]

#' @export
delta_min <- function(x, shifts = seq.int(0L, 23L)) {
  if (length(x) == 0L) x
  else {
    x - do.call(pmin.int, c(data.table::shift(x, shifts), list(na.rm = TRUE)))
  }
}

#' @export
sepsis_3 <- function(sofa, si, si_window = c("first", "last", "any"),
                     delta_fun = delta_cummin,
                     sofa_thresh = 2L) {

  assert_that(same_interval(sofa, si), same_key(sofa, si),
              has_col(sofa, "sofa_score"),
              has_time_cols(si, c("si_lwr", "si_upr")))

  si_window <- match.arg(si_window)
  key <- key(sofa)

  sofa <- set(sofa, j = "join_time1", value = time_col(sofa))
  sofa <- set(sofa, j = "join_time2", value = time_col(sofa))

  on.exit(rm_cols(sofa, c("join_time1", "join_time2")))

  join_clause <- c(key, "join_time1 >= si_lwr", "join_time2 <= si_upr")

  if (si_window == "first") si <- si[, head(.SD, n = 1L), by = key]
  if (si_window == "last")  si <- si[, tail(.SD, n = 1L), by = key]

  res <- sofa[si, c(list(delta_sofa = delta_fun(get("sofa_score"))),
                    mget(c(index(sofa), index(si)))),
              on = join_clause, by = .EACHI, nomatch = 0]

  res <- res[is_true(get("delta_sofa") >= get("sofa_thresh")), ]

  res <- rm_cols(res, c("join_time1", "join_time2", "delta_sofa"))
  res <- rename_cols(res, "sep3_time", index(res))

  res <- res[, head(.SD, n = 1L), by = key]

  res
}
