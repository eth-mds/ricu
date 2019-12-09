
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
  interval = hours(1L),
  envir = "mimic") {

  assert_that(is.string(select_drugs), is.string(drop_routes))

  search_fun <- function(col) grepl(select_drugs, col, ignore.case = TRUE)

  message("Fetching abx from `presciriptions`.")

  row_expr <- substitute((fun(drug) | fun(drug_name_generic)) & !fun(route),
                         list(fun = search_fun))

  mimic_prescriptions(rows = row_expr, interval = interval, envir = envir)
}

#' @export
mimic_abx_inmv <- function(select_cat = "antibiotics", interval = hours(1L),
                           envir = "mimic") {

  assert_that(is.string(select_cat))

  message("Fetching abx from `inputevents_mv`.")

  row_expr <- substitute(grepl(pat, ordercategoryname, ignore.case = TRUE),
                         list(pat = select_cat))

  mimic_input_mv(rows = row_expr, interval = interval, envir = envir)
}

#' @export
mimic_abx <- function(x = mimic_abx_presc(...), win_length = hours(24L),
                      date_extra = hours(24L), min_count = 1L,
                      count_win = hours(24L), ...) {

  x <- add_date_win(x, "win_end", win_length, date_extra)
  x <- make_unique_quo(x, quote(list(count = .N, win_end = max(win_end))))

  if (min_count > 1L) {

    assert_that(is_time(count_win, allow_neg = FALSE))

    expr <- quote(list(sum_count = sum(count), max_win = max(win_end)))
    x <- slide_quo(x, expr, before = count_win)

    x <- x[sum_count >= min_count, ]

    x <- rm_cols(x, "sum_count")
    x <- rename_cols(x, "win_end", "max_win")

  } else {
    x <- rm_cols(x, "count")
  }

  rename_cols(x, "hadm_time", index(x))
}

#' @export
mimic_micro <- function(win_length = hours(72L), date_extra = hours(24L),
                        rm_negative = FALSE, interval = hours(1L),
                        envir = "mimic") {

  assert_that(is.flag(rm_negative))

  message("Fetching microbiology events.")

  if (rm_negative) {
    row_expr <- quote(!is.na(org_itemid))
  } else {
    row_expr <- NULL
  }

  res <- mimic_microbio(rows = row_expr, interval = interval, envir = envir)
  res <- add_date_win(res, "win_end", win_length, date_extra)

  res <- make_unique_quo(res, quote(list(win_end = max(win_end))))

  rename_cols(res, "hadm_time", index(res))
}

add_date_win <- function(x, win_name, win_len, date_extra) {

  assert_that(is_ts_tbl(x), !has_col(x, win_name),
              is_time(win_len), is_time(date_extra))

  x <- x[, c(win_name) := get(index(x)) + win_len]

  if (has_ts_meta(x, "ts_date")) {
    x <- x[, c(win_name) := get(win_name) + fifelse(
      is_date(x, index(x)), date_extra, hours(0L)
    )]
  }

  x
}

#' @export
mimic_si <- function(abx = mimic_abx(...), micro = mimic_micro(...),
                     win_lwr = hours(48L), win_upr = hours(24L), ...) {
  si_compute(abx, micro, win_lwr, win_upr)
}
