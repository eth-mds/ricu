
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
  win_length = as.difftime(24L, units = "hours"),
  date_padding = as.difftime(24L, units = "hours"),
  time_scale = "hours",
  step_size = 1L,
  data_env = "mimic") {

  assert_that(is.string(select_drugs), is.string(drop_routes),
              is_difftime(win_length, allow_neg = FALSE),
              is_difftime(date_padding, allow_neg = FALSE))

  search_fun <- function(col) grepl(select_drugs, col, ignore.case = TRUE)

  message("Fetching abx from `presciriptions`.")

  row_expr <- substitute((fun(drug) | fun(drug_name_generic)) & !fun(route),
                         list(fun = search_fun))

  dat <- get_table("prescriptions", data_env)
  dat <- prt::subset_quo(dat, row_expr, c("hadm_id", "startdate"))

  res <- mimic_admit_difftime(dat, data_env, "startdate",
                              time_scale = time_scale, step_size = step_size)

  drop_cols <- setdiff(colnames(res), c("hadm_id", "hadm_time"))
  res <- data.table::set(res, j = drop_cols, value = NULL)
  res <- res[, win_end := hadm_time + win_length + date_padding]

  res
}

mimic_abx_inmv <- function(select_cat = "antibiotics",
                           win_length = as.difftime(24L, units = "hours"),
                           date_padding = as.difftime(24L, units = "hours"),
                           time_scale = "hours", step_size = 1L,
                           data_env = "mimic") {

  assert_that(is.string(select_cat),
              is_difftime(win_length, allow_neg = FALSE),
              is_difftime(date_padding, allow_neg = FALSE))

  message("Fetching abx from `inputevents_mv`.")

  res <- mimic_get_data_items(select_cat,
    item_col = "ordercategoryname",
    unit_cols = NULL,
    value_names = "abx_admission",
    regex = TRUE,
    time_scale = time_scale,
    step_size = step_size,
    split_items = FALSE,
    agg_fun = NULL
  )

  drop_cols <- setdiff(colnames(res), c("hadm_id", "hadm_time"))
  res <- data.table::set(res, j = drop_cols, value = NULL)
  res <- res[, win_end := hadm_time + win_length]

  res
}

mimic_abx <- function(abx = mimic_abx_presc(..., data_env = data_env),
                      min_count = 1L,
                      count_win = as.difftime(24L, units = "hours"),
                      ..., data_env = "mimic") {

  unique_time_win(abx, min_count, count_win)
}

mimic_micro <- function(win_length = as.difftime(72L, units = "hours"),
                        date_padding = as.difftime(24L, units = "hours"),
                        time_scale = "hours", step_size = 1L,
                        data_env = "mimic") {

  message("Fetching microbiology events.")

  dat <- get_table("microbiologyevents", data_env)
  dat <- dat[, c("hadm_id", "chartdate", "charttime")]
  dat <- dat[, is_date := ifelse(is.na(charttime), TRUE, FALSE)]
  dat <- dat[(is_date), charttime := chartdate]

  dat <- mimic_admit_difftime(dat, data_env, "charttime",
                              time_scale = time_scale, step_size = step_size)

  dat <- dat[, win_end := hadm_time + win_length]
  dat <- dat[(is_date),  win_end := win_end + date_padding]

  unique_time_win(dat, min_count = 1L)
}

mimic_si <- function(abx = mimic_abx(...), micro = mimic_micro(...), ...) {
  si_compute(abx, micro)
}
