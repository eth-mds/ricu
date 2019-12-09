
si_compute <- function(abx, micro, win_lwr = hours(48L),
                       win_upr = hours(24L)) {

  min_fun <- function(x) if (length(x) == 0L) x else min(x)

  assert_that(
    same_ts(abx, micro),
    has_time_col(abx, "win_end"), has_time_col(micro, "win_end"),
    is_time(win_lwr, allow_neg = FALSE),
    is_time(win_upr, allow_neg = FALSE)
  )

  key <- key(abx)
  ind <- index(abx)

  abx   <-   abx[, time_copy := get(ind)]
  micro <- micro[, time_copy := get(ind)]

  join_clause <- c(key, paste("win_end >=", ind), paste("time_copy <=", ind))

  message("determining si times")

  abx_micro <- abx[micro, list(si_time = min_fun(get(ind))),
                   on = join_clause, by = .EACHI, nomatch = NULL]
  micro_abx <- micro[abx, list(si_time = min_fun(get(ind))),
                   on = join_clause, by = .EACHI, nomatch = NULL]

  res <- unique(rbind(abx_micro[, c(key, "si_time"), with = FALSE],
                      micro_abx[, c(key, "si_time"), with = FALSE]))

  res <- res[, c("si_lwr", "si_upr") := list(si_time - win_lwr,
                                             si_time + win_upr)]

  as_ts_tbl(res, key, "si_time", interval(abx))
}
