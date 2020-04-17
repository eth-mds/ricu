
new_src_config <- function(name, id_config, col_config,
                           data_fun = "default_data_fun") {

  id_config <- as_id_config(id_config)
  col_config <- as_col_config(col_config)

  assert_that(is.string(name), is_id_config(id_config),
              all_is(col_config, is_col_config), is.string(data_fun),
              exists(data_fun, mode = "function"))

  structure(list(name = name, id_config = id_config, col_config = col_config,
                 data_fun = data_fun), class = "src_config")
}

is_src_config <- function(x) inherits(x, "src_config")

#' @export
get_source.src_config <- function(x) x[["name"]]

new_id_config <- function(cfg) {

  cfg <- as.list(cfg)

  assert_that(is.list(cfg), length(cfg) > 0L, has_name(cfg, "icustay"),
              all_is(cfg, is.string), is_unique(c(unlist(cfg), names(cfg))))

  structure(
    cfg[intersect(c("patient", "hadm", "icustay"), names(cfg))],
    class = "id_config"
  )
}

is_id_config <- function(x) inherits(x, "id_config")

as_id_config <- function(x) UseMethod("as_id_config", x)

as_id_config.id_config <- function(x) x

as_id_config.list <- function(x) new_id_config(x)

new_col_config <- function(table, cfg) {

  opts <- c("id_col", "time_col", "val_col", "unit_col")

  cfg <- as.list(cfg)

  assert_that(is.list(cfg), all_is(cfg, is.string))

  if (length(cfg)) {
    assert_that(!is.null(names(cfg)), all(names(cfg) %in% opts))
  }

  cfg <- cfg[opts]
  names(cfg) <- opts

  structure(list(table = table, cols = cfg), class = "col_config")
}

is_col_config <- function(x) inherits(x, "col_config")

as_col_config <- function(x) UseMethod("as_col_config", x)

as_col_config.col_config <- function(x) x

as_col_config.list <- function(x) Map(new_col_config, names(x), x)

read_src_config <- function(name = "data-sources", file = NULL, ...) {

  if (!is.null(file)) {

    assert_that(missing(name), file.exists(file))

    cfg <- read_json(file, ...)

  } else {

    cfg <- get_config(name, ...)
  }

  def <- lapply(lapply(cfg, `[[`, "tables"), function(x) {
    setNames(lapply(x, `[[`, "defaults"), chr_ply(x, `[[`, "name"))
  })

  Map(new_src_config, chr_ply(cfg, `[[`, "name"), lapply(cfg, `[[`, "id_cols"),
      def, chr_ply(cfg, `[[`, "data_fun"))
}

get_src_config <- function(x, ...) UseMethod("get_src_config", x)

get_src_config.src_config <- function(x, ...) x

get_src_config.character <- function(x, ...) {

  assert_that(is.string(x))

  cfg <- read_src_config(...)

  assert_that(is.list(cfg), all_is(cfg, is_src_config))

  hit <- x == chr_ply(cfg, get_source)

  assert_that(sum(hit) == 1L)

  cfg[[which(hit)]]
}

get_src_config.default <- function(x, ...) {
  get_src_config(unique(get_source(x)), ...)
}

get_id_config <- function(x, id_type = NULL, ...) {

  if (is_id_config(x)) {
    cfg <- x
  } else {
    cfg <- get_src_config(x, ...)[["id_config"]]
    assert_that(is_id_config(cfg))
  }

  if (is.null(id_type)) {
    return(unlist(cfg))
  }

  assert_that(is.string(id_type), has_name(cfg, id_type))

  cfg[[id_type]]
}

get_col_config <- function(x, table = NULL, ...) {

  assert_that(null_or(table, is.string))

  if (is_col_config(x)) {

    res <- x[["cols"]]

    if (is.string(table)) {
      assert_that(identical(table, x[["table"]]))
    }

    return(res)
  }

  if (all_is(x, is_col_config)) {
    cfg <- x
  } else {
    cfg <- get_src_config(x, ...)[["col_config"]]
    assert_that(all_is(cfg, is_col_config))
  }

  res <- lapply(cfg, `[[`, "cols")

  null_id <- lgl_ply(lapply(res, `[[`, "id_col"), is.null)
  def_id  <- get_id_config(x, "icustay", ...)

  res[null_id] <- Map(`[[<-`, res[null_id], "id_col", def_id)

  if (is.null(table)) {
    return(res)
  }

  hit <- table == chr_ply(cfg, `[[`, "table")

  assert_that(sum(hit) == 1L)

  res[[which(hit)]]
}

get_data_fun <- function(...) {
  cfg <- get_src_config(...)[["data_fun"]]
  get(cfg, mode = "function")
}

default_id_col <- function(table, ...) {
  get_col_config(..., table = table)[["id_col"]]
}

default_time_col <- function(table, ...) {
  get_col_config(..., table = table)[["time_col"]]
}

default_val_col <- function(table, ...) {
  get_col_config(..., table = table)[["val_col"]]
}

default_unit_col <- function(table, ...) {
  get_col_config(..., table = table)[["unit_col"]]
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
