
#' Low level functions for loading data
#'
#' Depending on the desired return type (`ts_tbl`, `id_tbl` or plain
#' `data.table`) and on whether the row-subsetting argument is a quoted
#' expression (suffix `_quo`), this set of functions provides the basic
#' mechanisms for loading data.
#'
#' @param source String specifying the data source
#' @param table String specifying the table from which to load data
#' @param row_expr Expression to be quoted and used to define a row subsetting;
#' `NULL` corresponds to no row subsetting
#' @param ... Forwarded to the corresponding function that takes a quoted
#' expression and further to `change_id()`
#'
#' @rdname load_data
#'
#' @export
#'
data_ts <- function(source, table, row_expr, ...) {
  data_ts_quo(source, table, null_or_subs(row_expr), ...)
}

#' @param row_quo Quoted expression used for defining a row subsetting; `NULL`
#' corresponds to no row subsetting
#' @param cols Character vector, specifying the column to return (in addition
#' to potential columns specified as `id_col` and `time_col`); `NULL`
#' corresponds to all columns
#' @param id_col The column defining the id of `ts_tbl` and `id_tbl` objects
#' @param time_col The column defining the index of `ts_tbl` objects
#' @param interval The time interval used to discretize time stamps with,
#' specified as [base::difftime()] object
#' @param cfg Configuration list used for determining default options for
#' `id_col` and `time_col` arguments, id options as well as the `data_fun`
#' argument
#'
#' @rdname load_data
#'
#' @export
#'
data_ts_quo <- function(source, table, row_quo = NULL, cols = NULL,
                        id_col = default_id_col(config = cfg),
                        time_col = default_time_col(table, config = cfg),
                        interval = hours(1L),
                        cfg = get_col_config(source, "all"), ...) {

  tbl <- get_tbl(table, source)
  ids <- get_col_config(NULL, "id_cols", cfg, "all")

  if (!id_col %in% colnames(tbl)) {
    aux_id <- tail(ids[ids %in% colnames(tbl)], n = 1L)
  } else {
    aux_id <- id_col
  }

  id_col <- ids[ids == id_col]

  if (!is.null(cols)) {
    cols <- c(aux_id, time_col, cols)
  }

  res <- data_tbl_quo(source, table, row_quo, cols, interval,
                      get_col_config(NULL, "data_fun", cfg))
  res <- as_ts_tbl(res, aux_id, ids, time_col, interval)

  change_id(res, source, to = names(id_col), ...)
}

#' @rdname load_data
#'
#' @export
#'
data_id <- function(source, table, row_expr, ...) {
  data_id_quo(source, table, null_or_subs(row_expr), ...)
}

#' @rdname load_data
#'
#' @export
#'
data_id_quo <- function(source, table, row_quo = NULL, cols = NULL,
                        id_col = default_id_col(config = cfg),
                        interval = hours(1L),
                        cfg = get_col_config(source, "all"), ...) {

  tbl <- get_tbl(table, source)
  ids <- get_col_config(NULL, "id_cols", cfg, "all")

  if (!id_col %in% colnames(tbl)) {
    aux_id <- tail(ids[ids %in% colnames(tbl)], n = 1L)
  } else {
    aux_id <- id_col
  }

  id_col <- ids[ids == id_col]

  if (!is.null(cols)) {
    cols <- c(aux_id, cols)
  }

  res <- data_tbl_quo(source, table, row_quo, cols, interval,
                      get_col_config(NULL, "data_fun", cfg))
  res <- as_id_tbl(res, aux_id, ids)

  change_id(res, source, to = names(id_col), ...)
}

#' @rdname load_data
#'
#' @export
#'
data_tbl <- function(source, table, row_expr, ...) {
  data_tbl_quo(source, table, null_or_subs(row_expr), ...)
}

#' @param data_fun Function used for loading the data; the default uses
#' [prt::subset_quo()] and for non-default values, a function with arguments
#' `table`, `row_quo`, `cols`, `interval`, and `source` is expected
#'
#' @rdname load_data
#'
#' @export
#'
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

    res <- unlist(Filter(Negate(is.null), cfg[map_id_cols()]))

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

  map <- stay_windows(src, from, to, shift_col, "out", interval)

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

  map <- get_tbl("id_map", src, "aux")

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

#' @export
id_name <- function(x, col = id(x)) {
  opts <- id_opts(x)
  if (is.null(opts)) col
  else if (is.null(names(opts))) opts[match(col, opts)]
  else names(opts[match(col, opts)])
}

id_col <- function(x, name) {
  opts <- id_opts(x)
  if (is.null(opts)) name
  else if (is.null(names(opts))) opts[match(name, opts)]
  else unname(opts[match(name, names(opts))])
}

#' @export
next_id <- function(x, n = 1L) {
  names(id_opts(x)[id_pos(x) + n])
}

#' @export
prev_id <- function(x, n = 1L) next_id(x, -n)

id_pos <- function(x, id = id_name(x)) {
  opts <- id_opts(x)
  if (is.null(opts)) 1L
  else if (is.null(names(opts))) match(id, opts)
  else match(id, names(opts))
}

#' @export
stay_windows <- function(source, id_type = "icustay", win_type = "icustay",
                         in_time = "intime", out_time = "outtime",
                         interval = hours(1L)) {

  reclock <- function(x, y) re_time(x - y, interval)

  assert_that(is_time(interval, allow_neg = FALSE))

  map <- get_tbl("id_map", source, "aux", allow_null = TRUE)

  if (is.null(map)) {
    return(map)
  }

  orig <- map_in_cols(id_type)
  inn  <- map_in_cols(win_type)
  out  <- map_out_cols(win_type)

  cols <- unique(c(id_type, win_type, inn, out, orig))

  map <- map[, cols, with = FALSE]

  map <- map[, c(in_time, out_time) := lapply(.SD, reclock, get(orig)),
             .SDcols = c(inn, out)]
  map <- rm_cols(map, setdiff(colnames(map),
                              c(id_type, win_type, in_time, out_time)))

  as_id_tbl(map, id_type)
}

#' @export
change_id <- function(x, source, to, from = id_name(x), ...) {

  assert_that(is.string(to), is.string(from))

  if (id_pos(x, from) < id_pos(x, to)) {
    upgrade_id(x, source, to, from, ...)
  } else if (id_pos(x, from) > id_pos(x, to)) {
    downgrade_id(x, source, to, from, ...)
  } else {
    x
  }
}

#' @export
upgrade_id.ts_tbl <- function(x, source, to, from, ...) {

  sft <- new_names(x)
  map <- upgrade_id_map(source, to, from, interval(x), sft)

  tmp <- c("time_1", "time_2")
  x <- x[, c(tmp) := get(index(x))]
  on.exit(rm_cols(x, tmp))

  join <- c(paste(id_col(x, from), "==", from),
            paste(tmp, c(">= lwr", "< upr")))

  res  <- x[map, on = join, nomatch = NULL, ...]

  new <- id_col(x, to)
  res <- rename_cols(res, new, to)
  res <- set_id(res, new)

  res <- res[, c(index(x)) := get(index(x)) - get(sft)]
  res <- rm_cols(res, c(id_col(x, from), tmp, sft))

  res
}

#' @export
upgrade_id.id_tbl <- function(x, source, to, from, ...) {

  map <- upgrade_id_map(source, to, from)

  res <- merge(x, map, by.x = id_col(x, from), by.y = from, ...)

  new <- id_col(x, to)
  res <- rename_cols(res, new, to)
  res <- set_id(res, new)

  res <- rm_cols(res, id_col(x, from))

  res
}

#' @export
downgrade_id.ts_tbl <- function(x, source, to, from, ...) {

  sft <- new_names(x)
  map <- downgrade_id_map(source, to, from, interval(x), sft)

  res <- merge(x, map, by.x = id_col(x, from), by.y = from, ...)

  new <- id_col(x, to)
  res <- rename_cols(res, new, to)
  res <- set_id(res, new)

  res <- res[, c(index(x)) := get(index(x)) - get(sft)]
  res <- rm_cols(res, c(id_col(x, from), sft))

  res
}

#' @export
downgrade_id.id_tbl <- function(x, source, to, from, ...) {

  map <- downgrade_id_map(source, to, from)

  res <- merge(x, map, by.x = id_col(x, from), by.y = from, ...)

  new <- id_col(x, to)
  res <- rename_cols(res, new, to)
  res <- set_id(res, new)

  res <- rm_cols(res, id_col(x, from))

  res
}
