
unique_time_win <- function(tbl, min_count = 1L,
                            count_win = as.difftime(24L, units = "hours")) {

  id <- c("hadm_id", "hadm_time")

  assert_that(is_dt(tbl), has_cols(tbl, c(id, "win_end")))

  time_step <- attr(tbl[["hadm_time"]], "step_size")
  tbl <- tbl[, list(count = .N, win_end = max(win_end)), by = id]
  data.table::setattr(tbl[["hadm_time"]], "step_size", time_step)

  if (min_count > 1L) {

    assert_that(is_difftime(count_win, allow_neg = FALSE))

    expr <- quote(list(sum_count = sum(count), max_win = max(win_end)))
    tbl <- window_quo(tbl, expr, window_length = count_win)
    tbl <- tbl[sum_count >= min_count, ]
    data.table::set(tbl, j = c("sum_count", "count"), value = NULL)
    data.table::setnames(tbl, "max_win", "win_end")

  } else {
    data.table::set(tbl, j = "count", value = NULL)
  }

  tbl
}

si_compute <- function(abx, micro, win_lwr = as.difftime(48L, units = "hours"),
                       win_upr = as.difftime(24L, units = "hours")) {

  id <- c("hadm_id", "hadm_time")

  min_fun <- function(x) if (length(x) == 0L) x else min(x)

  assert_that(
    is_dt(abx),   is_unique(abx,   id), has_cols(abx,   c(id, "win_end")),
    is_dt(micro), is_unique(micro, id), has_cols(micro, c(id, "win_end")),
    is_difftime(win_lwr, allow_neg = FALSE),
    is_difftime(win_upr, allow_neg = FALSE),
  )

  abx   <-   abx[, time_copy := hadm_time]
  micro <- micro[, time_copy := hadm_time]

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
