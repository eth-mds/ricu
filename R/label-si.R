
#' @export
si_data <- function(source, abx_count_win = hours(24L), abx_min_count = 1L,
                    positive_cultures = FALSE, interval = hours(1L),
                    patient_ids = NULL, col_cfg = get_col_config(source),
                    dictionary = read_dictionary("concept-dict")) {

  if (!identical(as_src(source), "mimic")) stop("TODO")

  assert_that(is.count(abx_min_count), is.flag(positive_cultures))

  if (positive_cultures) {
    samp_fun <- "sum"
  } else {
    samp_fun <- quote(list(fluid_sampling = .N))
  }

  funs <- c(antibiotics = "sum", fluid_sampling = samp_fun)
  dict <- dictionary[names(funs), source = source]

  dat <- load_concepts(source, dict, patient_ids, col_cfg, funs,
                       interval, merge_data = FALSE)
  names(dat) <- chr_ply(dat, data_cols)

  dat[["antibiotics"]] <- si_abx(dat[["antibiotics"]], abx_count_win,
                                 abx_min_count)

  dat[["fluid_sampling"]] <- data.table::set(
    dat[["fluid_sampling"]],
    j = "fluid_sampling",
    value = dat[["fluid_sampling"]][["fluid_sampling"]] > 0L
  )

  res <- merge(dat[["antibiotics"]], dat[["fluid_sampling"]], all = TRUE)
  res <- rename_cols(res, c("abx", "samp"), c("antibiotics", "fluid_sampling"))

  res
}

si_abx <- function(x, count_win, min_count) {

  if (min_count > 1L) {

    assert_that(is_time(count_win, allow_neg = FALSE))

    x <- slide_quo(x, quote(list(antibiotics = sum(antibiotics))),
                   before = count_win)
  }

  x <- data.table::set(x, j = "antibiotics",
                       value = x[["antibiotics"]] >= min_count)

  x
}

#' @export
si_windows <- function(tbl, abx_win = hours(24L), samp_win = hours(72L),
                       si_lwr = hours(48L), si_upr = hours(24L)) {

  span_win <- function(x, col, win) {
    x <- x[, c("win_end", "time_copy") := list(get(ind) + win, get(ind))]
    x[is_true(get(col)), ]
  }

  min_fun <- function(x) if (length(x) == 0L) x else min(x)

  win_args <- list(abx_win = abx_win, samp_win = samp_win, si_lwr = si_lwr,
                   si_upr = si_upr)

  assert_that(
    all(vapply(win_args, is_time, logical(1L), allow_neg = FALSE)),
    has_name(tbl, c("abx", "samp"))
  )

  win_args <- lapply(win_args, `units<-`, time_unit(tbl))
  list2env(win_args, environment())

  id <- id(tbl)
  ind <- index(tbl)

  dat <- Map(span_win, unmerge(tbl, c("abx", "samp")), c("abx", "samp"),
             win_args[c("abx_win", "samp_win")])

  join_clause <- c(id(tbl), paste("win_end >=", ind),
                             paste("time_copy <=", ind))

  abx_samp <- dat[["abx"]][dat[["samp"]], list(si_time = min_fun(get(ind))),
                          on = join_clause, by = .EACHI, nomatch = NULL]
  samp_abx <- dat[["samp"]][dat[["abx"]], list(si_time = min_fun(get(ind))),
                          on = join_clause, by = .EACHI, nomatch = NULL]

  res <- unique(rbind(abx_samp[, c(id, "si_time"), with = FALSE],
                      samp_abx[, c(id, "si_time"), with = FALSE]))

  res <- res[, c("si_lwr", "si_upr") := list(
    get("si_time") - win_args[["si_lwr"]],
    get("si_time") + win_args[["si_upr"]]
  )]

  as_ts_tbl(res, id, "si_time", interval(tbl))
}
