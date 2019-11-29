
si_compute <- function(abx, micro, win_lwr = hours(48L),
                       win_upr = hours(24L)) {

  min_fun <- function(x) if (length(x) == 0L) x else min(x)

  abx_win   <- ts_def(abx)[["ts_window"]]
  micro_win <- ts_def(micro)[["ts_window"]]

  assert_that(
    length(abx_win)   == 1L, is_unique(abx),
    length(micro_win) == 1L, is_unique(micro),
    is_time(win_lwr, allow_neg = FALSE),
    is_time(win_upr, allow_neg = FALSE)
  )

  browser()

  abx   <-   abx[, time_copy := get(index(abx))]
  micro <- micro[, time_copy := get(index(micro))]

  join_clause <- c("hadm_id", "win_end >= hadm_time", "time_copy <= hadm_time")

  message("determining si times")

  abx_micro <- abx[micro, list(si_time = min_fun(hadm_time)), on = join_clause,
                   by = .EACHI, nomatch = NULL]
  micro_abx <- micro[abx, list(si_time = min_fun(hadm_time)), on = join_clause,
                   by = .EACHI, nomatch = NULL]

  res_col <- c("hadm_id", "si_time")

  res <- unique(rbind(abx_micro[, res_col, with = FALSE],
                      micro_abx[, res_col, with = FALSE]))
  res <- data.table::setkeyv(res, res_col)

  res <- res[, c("si_lwr", "si_upr") := list(si_time - win_lwr,
                                             si_time + win_upr)]

  res
}
