
#' @export
id_origin <- function(x, ...) UseMethod("id_origin", x)

#' @export
id_origin.id_cols <- function(x, id, env = get_src_env(x), ...) {

  assert_that(...length() == 0L)

  sel <- get_id_cols(x, id_name = id)

  if (!identical(sel, x)) {
    return(id_origin(sel, id, env))
  }

  assert_that(length(x) == 1L)

  x <- x[[1L]]

  id_orig_all(x[["table"]], x[["id"]], x[["start"]], env)
}

#' @export
id_origin.eicu_ids <- function(x, ...) {
  warning("Absolute ID origin times are not available for source `eicu`")
  NextMethod()
}

#' @export
id_origin.eicu_demo_ids <- function(x, ...) {
  warning("Absolute ID origin times are not available for source `eicu_demo`")
  NextMethod()
}

#' @export
id_origin.default <- function(x, id, env = get_src_env(x), ...) {
  id_origin(get_id_cols(x, id_name = id, ...), id = id, env = env)
}

id_orig_all <- memoise::memoise(
  function(tbl, id, start, env) {
    res <- get(tbl, envir = env)
    res <- res[, c(id, start)]
    res <- rename_cols(res, "origin", start)
    as_id_tbl(res, id = id)
  }
)

#' @export
id_windows <- function(x, ...) UseMethod("id_windows", x)

#' @export
id_windows.mimic_ids <- function(x, env = get_src_env(x), ...) {

  assert_that(...length() == 0L)

  id_wins_mimic(x, env)
}

#' @export
id_windows.mimic_demo_ids <- function(x, env = get_src_env(x), ...) {

  assert_that(...length() == 0L)

  id_wins_mimic(x, env)
}

#' @export
id_windows.eicu_ids <- function(x, env = get_src_env(x), ...) {

  assert_that(...length() == 0L)

  id_wins_eicu(x, env)
}

#' @export
id_windows.eicu_demo_ids <- function(x, env = get_src_env(x), ...) {

  assert_that(...length() == 0L)

  id_wins_eicu(x, env)
}

#' @export
id_windows.hirid_ids <- function(x, env = get_src_env(x), ...) {

  assert_that(...length() == 0L)

  id_wins_hirid(x, env)
}

order_rename <- function(x, id_nms, id_col, st_col, ed_col) {
  x <- setcolorder(x, c(id_col, st_col, ed_col))
  x <- rename_cols(x, c(id_nms, paste0(id_nms, "_start"),
                                paste0(id_nms, "_end")))
  as_id_tbl(x, id = id_nms[1L])
}

as_dt_min <- function(x, y) round(difftime(x, y, units = "mins"))

id_wins_mimic <- memoise::memoise(
  function(x, env) {

    merge_inter <- function(x, y) {
      merge(x, y, by = intersect(colnames(x), colnames(y)))
    }

    get_id_tbl <- function(tbl, id, start, end, aux) {
      get(tbl, envir = env)[, c(id, start, end, aux)]
    }

    x <- x[order(int_ply(x, `[[`, "pos"), decreasing = TRUE)]

    ids  <- chr_ply(x, `[[`, "id")
    sta <- chr_ply(x, `[[`, "start")
    end  <- chr_ply(x, `[[`, "end")

    res <- Map(get_id_tbl, chr_ply(x, `[[`, "table"), ids, sta,
               end, c(as.list(ids[-1L]), list(NULL)))
    res <- Reduce(merge_inter, res)

    res <- res[, c(sta, end) := lapply(.SD, as_dt_min, get(sta[1L])),
               .SDcols = c(sta, end)]

    order_rename(res, names(x), ids, sta, end)
  }
)

id_wins_eicu <- memoise::memoise(
  function(x, env) {

    x <- x[order(int_ply(x, `[[`, "pos"), decreasing = TRUE)]

    ids <- chr_ply(x, `[[`, "id")
    sta <- unlist(lapply(x, `[[`, "start"))
    end <- unlist(lapply(x, `[[`, "end"))

    res <- get("patient", env)[, c(ids, sta, end)]

    icu_in  <- "unitadmitoffset"
    sta <- c(sta, icu_in)
    res <- res[, c(icu_in) := 0L]

    res <- res[, c(sta, end) := lapply(.SD, as.difftime, units = "mins"),
               .SDcols = c(sta, end)]

    order_rename(res, names(x), ids, sta, end)
  }
)

id_wins_hirid <- memoise::memoise(
  function(x, env) {

    ind_fun <- function(id, index) {

      tmp <- data.table::setDT(list(id = id, index = index))

      ind <- tmp[, .I[which.max(get("index"))], by = "id"][["V1"]]

      res <- logical(nrow(tmp))
      res[ind] <- TRUE

      res
    }

    ids <- chr_ply(x, `[[`, "id")
    sta <- chr_ply(x, `[[`, "start")

    obs <- get("observations", env)
    obs <- subset(obs, .env$ind_fun(.data$patientid, .data$datetime),
                  c("patientid", "datetime"), part_safe = TRUE)
    tbl <- get(chr_ply(x, `[[`, "table"), env)[, c(ids, sta)]

    res <- merge(tbl, obs, by = ids)
    res <- res[, c(sta, "datetime") := lapply(.SD, as_dt_min, get(sta)),
               .SDcols = c(sta, "datetime")]

    order_rename(res, names(x), ids, sta, "datetime")
  }
)

#' @export
id_windows.default <- function(x, env = get_src_env(x), ...) {
  id_windows(get_id_cols(x, ...), env = env)
}

upgrade_id_map <- function(src, to, from, interval = NULL,
                           shift_col = "shift", win_cols = c("lwr", "upr")) {

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

  if (!is.null(win_cols)) {
    map <- data.table::setorderv(map, c(from, "out"))
    map <- map[, c(win_cols) := mod_time(get("out")), by = c(from)]
  }

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

#' @param col Name of an id column
#'
#' @rdname change_id
#' @export
#'
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

#' @param n Number of jumps in id ordering
#'
#' @rdname change_id
#' @export
#'
next_id <- function(x, n = 1L) {
  names(id_opts(x)[id_pos(x) + n])
}

#' @rdname change_id
#' @export
#'
prev_id <- function(x, n = 1L) next_id(x, -n)

id_pos <- function(x, id = id_name(x)) {
  opts <- id_opts(x)
  if (is.null(opts)) 1L
  else if (is.null(names(opts))) match(id, opts)
  else match(id, names(opts))
}

#' Stays
#'
#' For a given ID type, get all stays with corresponding start and end times.
#'
#' @param id_type Type of ID all returned times are relative to
#' @param win_type Type of ID for which the in/out times is returned
#' @param in_time,out_time column names of the returned in/out times
#'
#' @inheritParams data_ts_quo
#'
#' @export
#'
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

  if (is.null(in_time)) {
    inn  <- NULL
  } else {
    inn  <- map_in_cols(win_type)
  }

  if (is.null(out_time)) {
    inn  <- NULL
  } else {
    out  <- map_out_cols(win_type)
  }

  cols <- unique(c(id_type, win_type, inn, out, orig))

  map <- map[, cols, with = FALSE]

  map <- map[, c(in_time, out_time) := lapply(.SD, reclock, get(orig)),
             .SDcols = c(inn, out)]
  map <- rm_cols(map, setdiff(colnames(map),
                              c(id_type, win_type, in_time, out_time)))

  as_id_tbl(map, id_type)
}

#' Switch between id types
#'
#' In ICU settings, multiple ID types may be in use which present an ordering
#' or nested structure, for example patient id, hospital stay id and ICU stay
#' id. This function allows for converting from one ID to another.
#'
#' @param x `icu_tbl` object for which to make the id change
#' @param source Name of the data source
#' @param to The destination id type
#' @param from The current id type
#' @param ... Generic consistency
#'
#' @rdname change_id
#' @export
#'
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

#' @param cols Column names that require time-adjustment
#'
#' @rdname change_id
#' @export
#'
upgrade_id <- function(x, source, to = next_id(x), from = id_name(x),
                       cols = time_cols(x), ...) {

  UseMethod("upgrade_id", x)
}

#' @rdname change_id
#' @export
#'
downgrade_id <- function(x, source, to = prev_id(x), from = id_name(x),
                         cols = time_cols(x), ...) {

  UseMethod("downgrade_id", x)
}

#' @rdname change_id
#' @export
#'
upgrade_id.ts_tbl <- function(x, source, to = next_id(x), from = id_name(x),
                              cols = time_cols(x), ...) {

  assert_that(index(x) %in% cols)

  sft <- new_names(x)
  map <- upgrade_id_map(source, to, from, interval(x), sft, c("lwr", "upr"))

  tmp <- c("time_1", "time_2")
  x <- x[, c(tmp) := get(index(x))]
  on.exit(rm_cols(x, tmp))

  join <- c(paste(id_col(x, from), "==", from),
            paste(tmp, c(">= lwr", "< upr")))

  res  <- x[map, on = join, nomatch = NULL, allow.cartesian = TRUE]

  new <- id_col(x, to)
  res <- rename_cols(res, new, to)
  res <- set_id(res, new)

  res <- res[, c(cols) := lapply(.SD, `-`, get(sft)), .SDcols = cols]
  res <- rm_cols(res, c(id_col(x, from), tmp, sft))

  res
}

#' @param interval If `cols` is a non-zero length vector, a time interval has
#' to be supplied as scalar positive `difftime` object
#'
#' @rdname change_id
#' @export
#'
upgrade_id.id_tbl <- function(x, source, to = next_id(x), from = id_name(x),
                              cols = time_cols(x), interval = NULL, ...) {

  if (length(cols)) {
    assert_that(is_time(interval, allow_neg = FALSE))
  } else {
    interval <- NULL
  }

  sft <- new_names(x)
  map <- upgrade_id_map(source, to, from, interval, sft, NULL)

  res <- merge(x, map, by.x = id_col(x, from), by.y = from,
               allow.cartesian = TRUE)

  new <- id_col(x, to)
  res <- rename_cols(res, new, to)
  res <- set_id(res, new)

  if (length(cols)) {
    res <- res[, c(cols) := lapply(.SD, `-`, get(sft)), .SDcols = cols]
  }

  res <- rm_cols(res, c(id_col(x, from), sft))

  res
}

#' @rdname change_id
#' @export
#'
downgrade_id.ts_tbl <- function(x, source, to = next_id(x), from = id_name(x),
                                cols = time_cols(x), ...) {

  assert_that(index(x) %in% cols)

  sft <- new_names(x)
  map <- downgrade_id_map(source, to, from, interval(x), sft)

  res <- merge(x, map, by.x = id_col(x, from), by.y = from)

  new <- id_col(x, to)
  res <- rename_cols(res, new, to)
  res <- set_id(res, new)

  res <- res[, c(cols) := lapply(.SD, `-`, get(sft)), .SDcols = cols]
  res <- rm_cols(res, c(id_col(x, from), sft))

  res
}

#' @rdname change_id
#' @export
#'
downgrade_id.id_tbl <- function(x, source, to = next_id(x), from = id_name(x),
                                cols = time_cols(x), interval = NULL, ...) {

  if (length(cols)) {
    assert_that(is_time(interval, allow_neg = FALSE))
  } else {
    interval <- NULL
  }

  sft <- new_names(x)
  map <- downgrade_id_map(source, to, from, interval, sft)

  res <- merge(x, map, by.x = id_col(x, from), by.y = from)

  new <- id_col(x, to)
  res <- rename_cols(res, new, to)
  res <- set_id(res, new)

  if (length(cols)) {
    res <- res[, c(cols) := lapply(.SD, `-`, get(sft)), .SDcols = cols]
  }

  res <- rm_cols(res, c(id_col(x, from), sft))

  res
}
