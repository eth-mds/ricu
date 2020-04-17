
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
