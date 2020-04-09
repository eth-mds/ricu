
#' @export
si_data <- function(source, abx_count_win = hours(24L), abx_min_count = 1L,
                    positive_cultures = FALSE,
                    dictionary = read_dictionary("concept-dict"), ...) {

  assert_that(is.count(abx_min_count), is.flag(positive_cultures))

  if (positive_cultures) {
    samp_fun <- "sum"
  } else {
    samp_fun <- quote(list(fluid_sampling = .N))
  }

  funs <- c(antibiotics = "sum", fluid_sampling = samp_fun)
  dict <- dictionary[names(funs), source = source]

  dat <- load_concepts(dict, aggregate = funs, merge_data = FALSE, ...)

  if (has_name(dat, "antibiotics")) {
    dat[["antibiotics"]] <- si_abx(dat[["antibiotics"]], abx_count_win,
                                   abx_min_count)
  }

  if (has_name(dat, "fluid_sampling")) {
    dat[["fluid_sampling"]] <- si_samp(dat[["fluid_sampling"]])
  }

  res <- reduce(merge, dat, all = TRUE)

  if ("antibiotics" %in% data_cols(res)) {
    res <- rename_cols(res, "abx", "antibiotics")
  } else {
    res <- res[, c("abx") := NA]
  }

  if ("fluid_sampling" %in% data_cols(res)) {
    res <- rename_cols(res, "samp", "fluid_sampling")
  } else {
    res <- res[, c("samp") := NA]
  }

  res
}

si_abx <- function(x, count_win, min_count) {

  if (min_count > 1L) {

    assert_that(is_time(count_win, allow_neg = FALSE))

    query <- quote(list(antibiotics = sum(antibiotics, na.rm = TRUE)))

    x <- slide_quo(x, query, before = hours(0L), after = count_win)
  }

  set(x, j = "antibiotics", value = x[["antibiotics"]] >= min_count)
}

si_samp <- function(x) {
  set(x, j = "fluid_sampling", value = x[["fluid_sampling"]] > 0L)
}

#' @export
si_windows <- function(tbl, mode = c("and", "or"), abx_win = hours(24L),
                       samp_win = hours(72L), si_lwr = hours(48L),
                       si_upr = hours(24L)) {

  span_win <- function(x, col, win) {
    x <- x[, c("win_end", "time_copy") := list(get(ind) + win, get(ind))]
    x[is_true(get(col)), ]
  }

  min_fun <- function(x) if (length(x) == 0L) x else min(x)

  mode <- match.arg(mode)

  win_args <- list(abx_win = abx_win, samp_win = samp_win, si_lwr = si_lwr,
                   si_upr = si_upr)

  assert_that(all(lgl_ply(win_args, is_time, allow_neg = FALSE)),
              has_name(tbl, c("abx", "samp")))

  win_args <- lapply(win_args, `units<-`, time_unit(tbl))
  list2env(win_args, environment())

  id  <- id(tbl)
  ind <- index(tbl)

  if (identical(mode, "and")) {

    dat <- Map(span_win, unmerge(tbl, c("abx", "samp")), c("abx", "samp"),
               win_args[c("abx_win", "samp_win")])

    join_clause <- c(id, paste(c("win_end >=", "time_copy <="), ind))

    abx_samp <- dat[["abx"]][dat[["samp"]], list(si_time = min_fun(get(ind))),
                            on = join_clause, by = .EACHI, nomatch = NULL]
    samp_abx <- dat[["samp"]][dat[["abx"]], list(si_time = min_fun(get(ind))),
                            on = join_clause, by = .EACHI, nomatch = NULL]

    res <- unique(rbind(abx_samp[, c(id, "si_time"), with = FALSE],
                        samp_abx[, c(id, "si_time"), with = FALSE]))

    res <- as_ts_tbl(res, id, id_opts(tbl), "si_time", interval(tbl))

  } else {

    res <- tbl[get("abx") | get("samp"), ]
    res <- rm_cols(res, data_cols(res))
    res <- rename_cols(res, "si_time", ind)
  }

  res <- res[, c("si_lwr", "si_upr") := list(
    get("si_time") - win_args[["si_lwr"]],
    get("si_time") + win_args[["si_upr"]]
  )]

  res
}
