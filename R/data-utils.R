
#' Data loading utilities
#'
#' Two important tools for smoothing out differences among used datasets are
#' `id_origin()` which returns origin times for a given ID and `id_windows()`
#' which defines stay windows and relationships between ID systems.
#'
#' @param x Object identify the ID system (passed to [as_src_env()])
#' @param id ID name for which to return origin times
#' @param origin_name String-valued name which will be used to label the origin
#' column
#' @param copy Logical flag indicating whether to return a copy of the memoized
#' `0data.table` for safety
#'
#' @rdname data_utils
#' @keywords internal
#' @export
id_origin <- function(x, id, origin_name = NULL, copy = TRUE) {

  assert_that(is.string(id), is.flag(copy))

  x <- as_src_env(x)

  key <- paste(src_name(x), id, sep = ":")
  res <- get0(key, envir = id_orig_env, mode = "list", inherits = FALSE,
              ifnotfound = NULL)

  if (is.null(res)) {
    res <- id_orig_helper(x, id)
    assign(key, res, id_orig_env)
  }

  assert_that(is_id_tbl(res), identical(id_vars(res), id))

  if (not_null(origin_name)) {
    res <- rename_cols(res, c(id, origin_name))
  } else if (copy) {
    res <- copy(res)
  }

  res
}

id_orig_env <- new.env()

#' @rdname data_utils
#' @export
id_orig_helper <- function(x, id) UseMethod("id_orig_helper", x)

#' @rdname data_utils
#' @export
id_orig_helper.src_env <- function(x, id) {

  assert_that(is.string(id))

  cfg <- as_id_cfg(x)[id == id_var_opts(x)]

  assert_that(length(cfg) == 1L)

  res  <- as_src_tbl(x, field(cfg, "table"))
  strt <- field(cfg, "start")

  if (strt %in% colnames(res)) {
    res <- res[, c(id, strt)]
  } else {
    res <- res[, id]
    res <- res[, c(strt) := 0]
  }

  as_id_tbl(unique(res), id, by_ref = TRUE)
}

#' @export
id_orig_helper.default <- function(x, ...) stop_generic(x, .Generic)

#' @rdname data_utils
#' @export
id_windows <- function(x, copy = TRUE) {

  col_obeys_interval <- function(col, x, ival) obeys_interval(x[[col]], ival)

  x <- as_src_env(x)

  key <- src_name(x)
  res <- get0(key, envir = id_win_env, mode = "list", inherits = FALSE,
              ifnotfound = NULL)

  if (is.null(res)) {

    res <- id_win_helper(x)
    ids <- field(as_id_cfg(x), "id")
    sec <- c(paste0(ids, "_start"), paste0(ids, "_end"))

    assert_that(
      is_id_tbl(res), has_name(res, ids),
      all_fun(sec, col_obeys_interval, res, mins(1L)),
      all_equal(range(res[[paste0(id_var(res), "_start")]]), mins(c(0, 0)))
    )

    assign(key, res, id_win_env)
  }

  assert_that(is_id_tbl(res))

  if (copy) {
    res <- copy(res)
  }

  res
}

id_win_env <- new.env()

#' @rdname data_utils
#' @export
id_win_helper <- function(x) UseMethod("id_win_helper", x)

#' @rdname data_utils
#' @export
id_win_helper.mimic_env <- function(x) {

  merge_inter <- function(x, y) {
    merge(x, y, by = intersect(colnames(x), colnames(y)))
  }

  get_id_tbl <- function(tbl, id, start, end, aux) {
    as_src_tbl(x, tbl)[, c(id, start, end, aux)]
  }

  cfg <- sort(as_id_cfg(x), decreasing = TRUE)

  ids  <- field(cfg, "id")
  sta <- field(cfg, "start")
  end  <- field(cfg, "end")

  res <- Map(get_id_tbl, field(cfg, "table"), ids, sta,
             end, c(as.list(ids[-1L]), list(NULL)))
  res <- Reduce(merge_inter, res)

  res <- res[, c(sta, end) := lapply(.SD, as_dt_min, get(sta[1L])),
             .SDcols = c(sta, end)]

  order_rename(res, ids, sta, end)
}

#' @rdname data_utils
#' @export
id_win_helper.eicu_env <- function(x) {

  cfg <- sort(as_id_cfg(x), decreasing = TRUE)

  ids <- field(cfg, "id")
  sta <- field(cfg, "start")
  end <- field(cfg, "end")

  tbl <- as_src_tbl(x, "patient")
  mis <- setdiff(sta, colnames(tbl))

  assert_that(length(mis) >= 1L)

  res <- tbl[, c(ids, intersect(sta, colnames(tbl)), end)]

  if (has_length(mis)) {
    res[, c(mis) := 0L]
  }

  res <- res[, c(sta, end) := lapply(.SD, as.difftime, units = "mins"),
             .SDcols = c(sta, end)]

  order_rename(res, ids, sta, end)
}

#' @importFrom rlang .data .env
#'
#' @rdname data_utils
#' @export
id_win_helper.hirid_env <- function(x) {

  ind_fun <- function(id, index) {

    tmp <- data.table::setDT(list(id = id, index = index))

    ind <- tmp[, .I[which.max(get("index"))], by = "id"][["V1"]]

    res <- logical(nrow(tmp))
    res[ind] <- TRUE

    res
  }

  cfg <- sort(as_id_cfg(x), decreasing = TRUE)

  ids <- field(cfg, "id")
  sta <- field(cfg, "start")

  obs <- as_src_tbl(x, "observations")
  obs <- subset(obs, .env$ind_fun(.data$patientid, .data$datetime),
                c("patientid", "datetime"), part_safe = TRUE)
  obs <- obs[, list(datetime = max(get("datetime"))), by = "patientid"]
  tbl <- as_src_tbl(x, field(cfg, "table"))[, c(ids, sta)]

  res <- merge(tbl, obs, by = ids)
  res <- res[, c(sta, "datetime") := lapply(.SD, as_dt_min, get(sta)),
             .SDcols = c(sta, "datetime")]

  order_rename(res, ids, sta, "datetime")
}

#' @export
id_win_helper.default <- function(x) stop_generic(x, .Generic)

order_rename <- function(x, id_var, st_var, ed_var) {
  x <- setcolorder(x, c(id_var, st_var, ed_var))
  x <- rename_cols(x, c(id_var, paste0(id_var, "_start"),
                                paste0(id_var, "_end")), by_ref = TRUE)
  as_id_tbl(x, id_var[1L], by_ref = TRUE)
}

as_dt_min <- function(x, y) round(difftime(x, y, units = "mins"))

#' @param id_var Type of ID all returned times are relative to
#' @param win_var Type of ID for which the in/out times is returned
#' @param in_time,out_time column names of the returned in/out times
#'
#' @rdname data_utils
#' @export
id_map <- function(x, id_var, win_var, in_time = NULL, out_time = NULL) {

  assert_that(is.string(id_var), is.string(win_var),
              null_or(in_time, is.string), null_or(out_time, is.string))

  x <- as_src_env(x)

  key <- paste(src_name(x), paste(id_var, win_var, sep = "_"), sep = ":")
  res <- get0(key, envir = id_map_env, mode = "list", inherits = FALSE,
              ifnotfound = NULL)

  if (is.null(res)) {
    res <- id_map_helper(x, id_var, win_var)
    assign(key, res, id_map_env)
  }

  assert_that(is_id_tbl(res), identical(id_vars(res), id_var))

  inn <- if (is.null(in_time)) NULL else paste0(win_var, "_start")
  out <- if (is.null(out_time)) NULL else paste0(win_var, "_end")

  res <- res[, unique(c(id_var, win_var, inn, out)), with = FALSE]

  if (not_null(coalesce(in_time, out_time))) {
    res <- rename_cols(res, c(in_time, out_time), c(inn, out), by_ref = TRUE)
  }

  res
}

id_map_env <- new.env()

#' @rdname data_utils
#' @export
id_map_helper <- function(x, id_var, win_var) UseMethod("id_map_helper", x)

#' @rdname data_utils
#' @export
id_map_helper.src_env <- function(x, id_var, win_var) {

  map    <- id_windows(x)
  map_id <- id_vars(map)

  if (identical(id_var, map_id)) {
    ori <- NULL
  } else {
    ori <- paste0(id_var, "_start")
  }

  inn <- paste0(win_var, "_start")
  out <- paste0(win_var, "_end")

  map <- map[, unique(c(id_var, win_var, inn, out, ori)), with = FALSE]

  if (not_null(ori)) {
    map <- map[, c(inn, out) := lapply(.SD, `-`, get(ori)),
               .SDcols = c(inn, out)]
    map <- rm_cols(map, ori, by_ref = TRUE)
  }

  as_id_tbl(unique(map), id_var, by_ref = TRUE)
}

#' @export
id_map_helper.default <- function(x, ...) stop_generic(x, .Generic)

#' Stays
#'
#' For a given ID type, get all stays with corresponding start and end times.
#'
#' @param x Passed to [as_id_cfg()] and [as_src_env()]
#' @param id_type Type of ID all returned times are relative to
#' @param win_type Type of ID for which the in/out times is returned
#' @param in_time,out_time column names of the returned in/out times
#' @param interval The time interval used to discretize time stamps with,
#' specified as [base::difftime()] object
#'
#' @export
#'
stay_windows <- function(x, id_type = "icustay", win_type = "icustay",
                         in_time = "start", out_time = "end",
                         interval = hours(1L)) {

  assert_that(is_interval(interval))

  cfg <- as_id_cfg(x)
  res <- id_map(x, id_vars(cfg[id_type]), id_vars(cfg[win_type]),
                in_time, out_time)

  if (!is_one_min(interval) && (not_null(in_time) || not_null(out_time))) {

    res <- res[, c(in_time, out_time) := lapply(.SD, re_time, interval),
               .SDcols = c(in_time, out_time)]
  }

  res
}

#' Switch between id types
#'
#' In ICU settings, multiple ID types may be in use which present an ordering
#' or nested structure, for example patient id, hospital stay id and ICU stay
#' id. This function allows for converting from one ID to another.
#'
#' @param x `icu_tbl` object for which to make the id change
#' @param target_id The destination id name
#' @param src Passed to [as_id_cfg()] and [as_src_env()]
#' @param ... Passed to `upgrade_id()`/`downgrade_id()`
#' @param keep_old_id Logical flag indicating whether to keep the previous ID
#' column
#'
#' @rdname change_id
#' @export
#'
change_id <- function(x, target_id, src, ..., keep_old_id = TRUE) {

  assert_that(is.string(target_id), is.flag(keep_old_id))

  orig_id <- id_var(x)

  if (identical(orig_id, target_id)) {
    return(x)
  }

  id_cfg <- as_id_cfg(src)

  opt <- id_var_opts(id_cfg)
  ori <- id_cfg[orig_id   == opt]
  fin <- id_cfg[target_id == opt]

  if (isTRUE(ori < fin)) {
    res <- upgrade_id(x, target_id, src, ...)
  } else if (isTRUE(ori > fin)) {
    res <- downgrade_id(x, target_id, src, ...)
  } else {
    stop_ricu("Cannot handle conversion of IDs with identical positions",
              class = "ident_pos_id_change")
  }

  if (isFALSE(keep_old_id)) {
    res <- rm_cols(res, orig_id, by_ref = TRUE)
  }

  res
}

#' @param cols Column names that require time-adjustment
#'
#' @rdname change_id
#' @export
#'
upgrade_id <- function(x, target_id, src, cols = time_vars(x), ...) {

  cfg <- as_id_cfg(src)
  opt <- id_var_opts(cfg)

  assert_that(cfg[id_vars(x) == opt] < cfg[target_id == opt])

  UseMethod("upgrade_id", x)
}

#' @rdname change_id
#' @export
#'
downgrade_id <- function(x, target_id, src, cols = time_vars(x), ...) {

  cfg <- as_id_cfg(src)
  opt <- id_var_opts(cfg)

  assert_that(cfg[id_vars(x) == opt] > cfg[target_id == opt])

  UseMethod("downgrade_id", x)
}

#' @rdname change_id
#' @export
#'
upgrade_id.ts_tbl <- function(x, target_id, src, cols = time_vars(x), ...) {

  assert_that(index_var(x) %in% cols)

  if (!is_one_min(interval(x))) {
    warn_ricu("Changing the ID of non-minute resolution data will change the
               interval to 1 minute", class = "non_min_id_change")
  }

  sft <- new_names(x)
  idx <- index_var(x)

  map <- id_map(src, id_vars(x), target_id, sft, idx)

  res <- map[x, on = meta_vars(x), roll = -Inf, rollends = TRUE]
  res <- res[, c(cols) := lapply(.SD, `-`, get(sft)), .SDcols = cols]

  res <- as_ts_tbl(res, target_id, idx, mins(1L), by_ref = TRUE)
  res <- rm_cols(res, sft, by_ref = TRUE)

  res
}

#' @rdname change_id
#' @export
#'
upgrade_id.id_tbl <- function(x, target_id, src, cols = time_vars(x), ...) {

  change_id_helper(x, target_id, src, cols, "up", ...)
}

#' @rdname change_id
#' @export
#'
downgrade_id.ts_tbl <- function(x, target_id, src, cols = time_vars(x),
                                ...) {

  assert_that(index_var(x) %in% cols)

  if (!is_one_min(interval(x))) {
    warn_ricu("Changing the ID of non-minute resolution data will change the
               interval to 1 minute", class = "non_min_id_change")
  }

  res <- change_id_helper(x, target_id, src, cols, "down", ...)

  change_interval(res, mins(1L), cols, by_ref = TRUE)
}

#' @export
upgrade_id.default <- function(x, ...) stop_generic(x, .Generic)

#' @rdname change_id
#' @export
#'
downgrade_id.id_tbl <- function(x, target_id, src, cols = time_vars(x),
                                ...) {

  change_id_helper(x, target_id, src, cols, "down", ...)
}

#' @export
downgrade_id.default <- function(x, ...) stop_generic(x, .Generic)

change_id_helper <- function(x, targ, src, cols, dir = c("down", "up"), ...) {

  dir <- match.arg(dir)
  idx <- id_vars(x)

  if (length(cols)) {
    sft <- new_names(x)
  } else {
    sft <- NULL
  }

  if (identical(dir, "down")) {
    map <- id_map(src, targ, idx, sft, NULL)
  } else {
    map <- id_map(src, idx, targ, sft, NULL)
  }

  res <- merge(x, map, by = idx, ...)

  if (length(cols)) {
    res <- res[, c(cols) := lapply(.SD, `-`, get(sft)), .SDcols = cols]
  }

  res <- set_id_vars(res, targ)
  res <- rm_cols(res, sft, by_ref = TRUE)

  res
}
