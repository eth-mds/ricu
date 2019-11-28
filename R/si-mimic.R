
#' @export
mimic_abx_presc <- function(
  select_drugs = paste(
    "aztreonam|bactrim|cephalexin|chloramphenicol|cipro|flagyl|metronidazole",
    "nitrofurantoin|tazobactam|rifampin|sulfadiazine|timentin|trimethoprim",
    "(amika|gentami|vanco)cin|(amoxi|ampi|dicloxa|naf|oxa|peni|pipera)cillin",
    "(azithro|clarithro|erythro|clinda|strepto|tobra|vanco)mycin",
    "cef(azolin|tazidime|adroxil|epime|otetan|otaxime|podoxime|uroxime)",
    "(doxy|mino|tetra)cycline|(levofl|moxifl|ofl)oxacin|macro(bid|dantin)",
    "(una|zo)syn",
    sep = "|"
  ),
  drop_routes = "[ao][dsu]|eye|ear|tp",
  win_length = hours(24L),
  interval = hours(1L),
  envir = "mimic") {

  assert_that(is.string(select_drugs), is.string(drop_routes),
              is_time(win_length, allow_neg = FALSE))

  search_fun <- function(col) grepl(select_drugs, col, ignore.case = TRUE)

  message("Fetching abx from `presciriptions`.")

  row_expr <- substitute((fun(drug) | fun(drug_name_generic)) & !fun(route),
                         list(fun = search_fun))

  res <- mimic_prescriptions(rows = row_expr, interval = interval,
                             envir = envir)
  res <- res[, startdate_win := startdate_win + win_length]

  res
}

#' @export
mimic_abx_inmv <- function(select_cat = "antibiotics", win_length = hours(24L),
                           interval = hours(1L), envir = "mimic") {

  assert_that(is.string(select_cat), is_time(win_length, allow_neg = FALSE))

  message("Fetching abx from `inputevents_mv`.")

  row_expr <- substitute(grepl(pat, ordercategoryname, ignore.case = TRUE),
                         list(pat = select_cat))

  res <- mimic_input_mv(rows = row_expr, interval = interval,
                        envir = envir)

  res <- res[, starttime_win := starttime + win_length]
  update_ts_def(res, new_ts_window("starttime", "starttime_win"))

  res
}

#' @export
mimic_abx <- function(x = mimic_abx_presc(...), min_count = 1L,
                      count_win = hours(24L), ...) {

  assert_that(has_ts_meta(x, "ts_window"))

  win_col <- ts_def(x)[["ts_window"]][["time_cols"]]
  key_col <- key(x)

  assert_that(length(win_col) == 1L, length(key_col) == 1L)

  x <- x[, list(count = .N, time_win = max(get(win_col))),
         by = c(id_cols(x))]

  if (min_count > 1L) {

    assert_that(is_time(count_win, allow_neg = FALSE))

    expr <- quote(list(sum_count = sum(count), max_win = max(time_win)))
    x <- slide_quo(x, expr, before = count_win)

    x <- x[sum_count >= min_count, ]

    x <- rm_cols(x, "sum_count")
    x <- rename_cols(x, "time_win", "max_win")

  } else {
    x <- rm_cols(x, "count")
  }

  x <- rename_cols(x, c("hadm_id", "hadm_time"), c(key_col, index(x)))

  x
}

#' @export
mimic_micro <- function(win_length = hours(72L), interval = hours(1L),
                        envir = "mimic") {

  message("Fetching microbiology events.")

  res <- mimic_microbio(interval = interval, envir = envir)

  res <- res[, list(time_win = max(charttime_win)), by = c(id_cols(x))]
  res <- res[, time_win := time_win + win_length]

  res <- rename_cols(res, c("hadm_id", "hadm_time"), id_cols(x))
  res <- update_ts_def(res, new_ts_window("hadm_time", "time_win"))

  res
}

mimic_si <- function(abx = mimic_abx(...), micro = mimic_micro(...), ...) {
  si_compute(abx, micro)
}
