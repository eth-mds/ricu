
#' @export
data_ts <- function(source, table, row_expr, ...) {
  data_ts_quo(source, table, null_or_subs(row_expr), ...)
}

#' @export
data_ts_quo <- function(source, table, row_quo = NULL, cols = NULL,
                        id_col = default_id_col(config = cfg),
                        time_col = default_time_col(table, config = cfg),
                        interval = hours(1L),
                        cfg = get_col_config(source, "all")) {

  tbl <- get_table(table, source)

  if (!id_col %in% colnames(tbl)) {

    candid <- get_col_config(NULL, "id_cols", cfg, "all")
    aux_id <- tail(candid[candid %in% colnames(tbl)], n = 1L)
    id_col <- candid[candid == id_col]

  } else {
    aux_id <- id_col
  }

  if (!is.null(cols)) {
    cols <- c(aux_id, time_col, cols)
  }

  res <- data_tbl_quo(source, table, row_quo, cols, interval,
                      get_col_config(NULL, "data_fun", cfg))
  res <- as_ts_tbl(res, aux_id, time_col, interval)

  if (!identical(id_col, aux_id)) {
    res <- rename_cols(res, names(aux_id), aux_id)
    res <- change_id(res, source, to = names(id_col), from = names(aux_id))
    res <- rename_cols(res, id_col, names(id_col))
  }

  res
}

#' @export
data_id <- function(source, table, row_expr, ...) {
  data_id_quo(source, table, null_or_subs(row_expr), ...)
}

#' @export
data_id_quo <- function(source, table, row_quo = NULL, cols = NULL,
                        id_col = default_id_col(config = cfg),
                        interval = hours(1L),
                        cfg = get_col_config(source, "all")) {

  tbl <- get_table(table, source)

  if (!id_col %in% colnames(tbl)) {

    candid <- get_col_config(NULL, "id_cols", cfg, "all")
    aux_id <- tail(candid[candid %in% colnames(tbl)], n = 1L)
    id_col <- candid[candid == id_col]

  } else {
    aux_id <- id_col
  }

  if (!is.null(cols)) {
    cols <- c(aux_id, cols)
  }

  res <- data_tbl_quo(source, table, row_quo, cols, interval,
                      get_col_config(NULL, "data_fun", cfg))
  res <- as_id_tbl(res, aux_id)

  if (!identical(id_col, aux_id)) {
    res <- rename_cols(res, names(aux_id), aux_id)
    res <- change_id(res, source, to = names(id_col), from = names(aux_id))
    res <- rename_cols(res, id_col, names(id_col))
  }

  res
}

#' @export
data_tbl <- function(source, table, row_expr, ...) {
  data_tbl_quo(source, table, null_or_subs(row_expr), ...)
}

#' @export
data_tbl_quo <- function(source, table, row_quo = NULL, cols = NULL,
                         interval = hours(1L),
                         data_fun = get_col_config(source, "data_fun")) {

  assert_that(is.string(source), is.string(table),
              null_or(row_quo, is.language),
              null_or(cols, is.character),
              is_time(interval, allow_neg = FALSE),
              is.function(data_fun))

  res <- data_fun(table = table, row_quo = row_quo, cols = cols,
                  interval = interval, source = source)

  assert_that(is_dt(res))

  if (!is.null(cols)) {
    assert_that(has_cols(res, cols))
  }

  res[]
}

#' @export
get_col_config <- function(source = NULL,
                           what = c("tables", "id_cols", "data_fun", "all"),
                           config = get_config("default-cols"), ...) {

  what <- match.arg(what)

  if (!is.null(source)) {
    assert_that(is.string(source), has_name(config, source))
    config <- config[[source]]
  }

  if (identical(what, "all")) {

    config

  } else {

    assert_that(has_name(config, what))

    switch(
      what,
      tables   = get_default_cols(config[[what]], ...),
      id_cols  = get_id_col(config[[what]], ...),
      data_fun = get_data_fun(config[[what]], ...),
    )
  }
}

get_data_fun <- function(cfg) {

  if (is.null(cfg)) {
    default_data_fun
  } else {
    get(cfg, mode = "function")
  }
}

get_id_col <- function(cfg, type = "hadm") {

  type <- match.arg(type, c(map_id_cols(), "all"))

  if (identical(type, "all")) {

    res <- unlist(Filter(Negate(is.na), cfg[map_id_cols()]))

    assert_that(length(res) > 0L)

  } else {

    assert_that(type %in% names(cfg))

    res <- cfg[[type]]

    if (is.null(res)) {
      stop("The selected data source does not allow for ", type, " ids to be ",
           "used.")
    }
  }

  res
}

get_default_cols <- function(cfg, table = NULL) {

  get_one <- function(x) {

    cfg_names <- c("time_col", "val_col", "unit_col")

    if (is.null(names(x))) {
      assert_that(identical(x, list()))
    }

    assert_that(all(names(x) %in% cfg_names))
    res <- setNames(x[cfg_names], cfg_names)
    assert_that(all(lgl_ply(res, is.string) | lgl_ply(res, is.null)))
    res
  }

  if (is.null(table)) {
    lapply(cfg, get_one)
  } else {
    assert_that(is.string(table), has_name(cfg, table))
    get_one(cfg[[table]])
  }
}


get_default_col <- function(name) {

  assert_that(is.string(name))

  function(table, source = NULL, ...) {
    get_col_config(source, "tables", ..., table = table)[[name]]
  }
}

#' @export
default_time_col <- get_default_col("time_col")

#' @export
default_val_col <- get_default_col("val_col")

#' @export
default_unit_col <- get_default_col("unit_col")

default_id_col <- function(source = NULL, ...) {
  get_col_config(source, "id_cols", ...)
}

upgrade_id_map <- function(src, to, from, interval = NULL,
                           shift_col = "shift") {

  reclock <- function(x, y) re_time(x - y, interval)

  mod_time <- function(x) {
    list(data.table::shift(x, fill = -Inf), replace(x, length(x), Inf))
  }

  assert_that(is.string(from), is.string(to),
              map_id_cols(from, TRUE) < map_id_cols(to, TRUE))

  if (is.null(interval)) {
    return(downgrade_id_map(src, from, to))
  }

  assert_that(is_time(interval, allow_neg = FALSE))

  orig <- map_in_cols(from)
  inn  <- map_in_cols(to)
  out  <- map_out_cols(to)
  cols <- c(from, to, inn, out, orig)

  map <- get_table("id_map", src, "aux")
  map <- map[, cols, with = FALSE]

  map <- map[, c("out", shift_col) := lapply(.SD, reclock, get(orig)),
             .SDcols = c(out, inn)]
  map <- rm_cols(map, c(orig, inn, out))

  if (map_id_cols(to, TRUE) < max(map_id_cols(as_factor = TRUE))) {
    map <- unique(map)
  }

  map <- data.table::setorderv(map, c(from, "out"))
  map <- map[, c("lwr", "upr") := mod_time(get("out")), by = c(from)]
  map <- rm_cols(map, "out")

  map
}

downgrade_id_map <- function(src, to, from, interval = NULL,
                             shift_col = "shift") {

  assert_that(is.string(from), is.string(to),
              map_id_cols(from, TRUE) > map_id_cols(to, TRUE))

  map <- get_table("id_map", src, "aux")

  if (is.null(interval)) {
    cols <- c(from, to)
  } else {
    orig <- map_in_cols(from)
    inn  <- map_in_cols(to)
    cols <- c(from, to, orig, inn)
  }

  map <- map[, cols, with = FALSE]

  if (!is.null(interval)) {
    map <- map[, c(shift_col) := re_time(get(inn) - get(orig), interval)]
    map <- rm_cols(map, c(orig, inn))
  }

  if (map_id_cols(to, TRUE) < max(map_id_cols(as_factor = TRUE))) {
    map <- unique(map)
  }

  map
}

change_id <- function(x, source, to, from) {

  assert_that(is.string(to), is.string(from), from != to)

  if (map_id_cols(from, TRUE) < map_id_cols(to, TRUE)) {
    upgrade_id(x, source, to, from)
  } else {
    downgrade_id(x, source, to, from)
  }
}

#' @export
upgrade_id.ts_tbl <- function(x, source, to, from) {

  sft <- new_names(x)
  map <- upgrade_id_map(source, to, from, interval(x), sft)

  tmp <- c("time_1", "time_2")
  x <- x[, c(tmp) := get(index(x))]
  on.exit(rm_cols(x, tmp))

  res <- x[map, on = c(from, paste(tmp, c(">= lwr", "< upr"))), nomatch = NULL]

  res <- set_id(res, to)
  res <- res[, c(index(x)) := get(index(x)) - get(sft)]
  res <- rm_cols(res, c(from, tmp, sft))

  res
}

#' @export
upgrade_id.id_tbl <- function(x, source, to, from) {

  map <- upgrade_id_map(source, to, from)
  res <- merge(x, map, by = from)
  res <- set_id(res, to)
  res <- rm_cols(res, from)

  res
}

#' @export
downgrade_id.ts_tbl <- function(x, source, to, from) {

  sft <- new_names(x)
  map <- downgrade_id_map(source, to, from, interval(x), sft)

  res <- merge(x, map, by = from)

  res <- set_id(res, to)
  res <- res[, c(index(x)) := get(index(x)) - get(sft)]
  res <- rm_cols(res, c(from, sft))

  res
}

#' @export
downgrade_id.id_tbl <- function(x, source, to, from) {

  map <- downgrade_id_map(source, to, from)
  res <- merge(x, map, by = from)
  res <- set_id(res, to)
  res <- rm_cols(res, from)

  res
}
