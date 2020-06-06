
#' Data loading utilities
#'
#' Two important tools for smoothing out differences among used datasets are
#' `id_origin()` which returns origin times for a given ID and `id_windows()`
#' which defines stay windows and relationships between ID systems.
#'
#' @param x Object to dispatch on
#' @param ... Generic consistency
#'
#' @rdname data_utils
#' @export
id_origin <- function(x, ...) UseMethod("id_origin", x)

#' @param id ID name for which to return origin times
#' @param env Data environment in which to look for tables to construct helper
#' tables
#'
#' @rdname data_utils
#' @export
id_origin.id_cfg <- function(x, id, env = as_src_env(x), ...) {

  assert_that(is.string(id), ...length() == 0L)

  sel <- select_ids(x, id)

  if (!identical(sel, x)) {
    return(id_origin(sel, id, env))
  }

  assert_that(length(x) == 1L)

  id_orig_all(field(x, "table"), field(x, "id"), field(x, "start"), env)
}

#' @rdname data_utils
#' @export
id_origin.eicu_ids <- function(x, ...) {
  warning("Absolute ID origin times are not available for source `eicu`")
  NextMethod()
}

#' @rdname data_utils
#' @export
id_origin.eicu_demo_ids <- function(x, ...) {
  warning("Absolute ID origin times are not available for source `eicu_demo`")
  NextMethod()
}

#' @rdname data_utils
#' @export
id_origin.default <- function(x, id, env = as_src_env(x), ...) {
  id_origin(as_id_cfg(x), id = id, env = env, ...)
}

id_orig_all <- memoise::memoise(
  function(tbl, id, start, env) {
    res <- get(tbl, envir = env)
    res <- res[, c(id, start)]
    res <- rename_cols(res, "origin", start, by_ref = TRUE)
    as_id_tbl(res, id, by_ref = TRUE)
  }
)

#' @rdname data_utils
#' @export
id_windows <- function(x, ...) UseMethod("id_windows", x)

#' @rdname data_utils
#' @export
id_windows.mimic_ids <- function(x, env = as_src_env(x), ...) {
  warn_dots(...)
  id_wins_mimic(x, env)
}

#' @rdname data_utils
#' @export
id_windows.mimic_demo_ids <- function(x, env = as_src_env(x), ...) {
  warn_dots(...)
  id_wins_mimic(x, env)
}

#' @rdname data_utils
#' @export
id_windows.eicu_ids <- function(x, env = as_src_env(x), ...) {
  warn_dots(...)
  id_wins_eicu(x, env)
}

#' @rdname data_utils
#' @export
id_windows.eicu_demo_ids <- function(x, env = as_src_env(x), ...) {
  warn_dots(...)
  id_wins_eicu(x, env)
}

#' @rdname data_utils
#' @export
id_windows.hirid_ids <- function(x, env = as_src_env(x), ...) {
  warn_dots(...)
  id_wins_hirid(x, env)
}

#' @rdname data_utils
#' @export
id_windows.default <- function(x, env = as_src_env(x), ...) {
  id_windows(as_id_cfg(x, ...), env = env)
}

order_rename <- function(x, id_col, st_col, ed_col) {
  x <- setcolorder(x, c(id_col, st_col, ed_col))
  x <- rename_cols(x, c(id_col, paste0(id_col, "_start"),
                                paste0(id_col, "_end")), by_ref = TRUE)
  as_id_tbl(x, id_col[1L], by_ref = TRUE)
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

    x <- sort(x, decreasing = TRUE)

    ids  <- field(x, "id")
    sta <- field(x, "start")
    end  <- field(x, "end")

    res <- Map(get_id_tbl, field(x, "table"), ids, sta,
               end, c(as.list(ids[-1L]), list(NULL)))
    res <- Reduce(merge_inter, res)

    res <- res[, c(sta, end) := lapply(.SD, as_dt_min, get(sta[1L])),
               .SDcols = c(sta, end)]

    order_rename(res, ids, sta, end)
  }
)

id_wins_eicu <- memoise::memoise(
  function(x, env) {

    x <- sort(x, decreasing = TRUE)

    ids <- field(x, "id")
    sta <- field(x, "start")
    end <- field(x, "end")

    if (anyNA(sta)) {
      sta <- sta[!is.na(sta)]
    }

    res <- get("patient", env)[, c(ids, sta, end)]

    icu_in  <- "unitadmitoffset"
    sta <- c(sta, icu_in)
    res <- res[, c(icu_in) := 0L]

    res <- res[, c(sta, end) := lapply(.SD, as.difftime, units = "mins"),
               .SDcols = c(sta, end)]

    order_rename(res, ids, sta, end)
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

    ids <- field(x, "id")
    sta <- field(x, "start")

    obs <- get("observations", env)
    obs <- subset(obs, .env$ind_fun(.data$patientid, .data$datetime),
                  c("patientid", "datetime"), part_safe = TRUE)
    tbl <- get(field(x, "table"), env)[, c(ids, sta)]

    res <- merge(tbl, obs, by = ids)
    res <- res[, c(sta, "datetime") := lapply(.SD, as_dt_min, get(sta)),
               .SDcols = c(sta, "datetime")]

    order_rename(res, ids, sta, "datetime")
  }
)

id_map_min <- function(x, id_name, win_id, in_time, out_time, ...) {

  x <- as_id_cfg(x, ...)

  map    <- id_windows(x)
  map_id <- id_vars(map)

  if (identical(id_name, map_id) || (is.null(in_time) && is.null(out_time))) {
    ori <- NULL
  } else {
    ori <- paste0(id_name, "_start")
  }

  inn <- if (is.null(in_time))  NULL else paste0(win_id, "_start")
  out <- if (is.null(out_time)) NULL else paste0(win_id, "_end")
  iot <- c(in_time, out_time)

  map <- map[, unique(c(id_name, win_id, inn, out, ori)), with = FALSE]

  if (not_null(ori)) {
    map <- map[, c(iot) := lapply(.SD, `-`, get(ori)),
               .SDcols = c(inn, out)]
  } else if (not_null(in_time) || not_null(out_time)) {
    map <- rename_cols(map, iot, c(inn, out), by_ref = TRUE)
  }

  map <- rm_cols(map, setdiff(colnames(map), c(id_name, win_id, iot)),
                 by_ref = TRUE)

  if (max(select_ids(x, c(id_name, win_id))) < max(x)) {
    map <- unique(map)
  }

  as_id_tbl(map, id_name, by_ref = TRUE)
}

#' Stays
#'
#' For a given ID type, get all stays with corresponding start and end times.
#'
#' @param x Object to dispatch on
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

  assert_that(is_time(interval, allow_neg = FALSE))

  cfg <- as_id_cfg(x)
  res <- id_map_min(cfg, get_id_col(cfg, id_type), get_id_col(cfg, win_type),
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
#' @param id_cfg An `id_cfg` object, specifying the id options
#' @param ... Passed to `upgrade_id()`/`downgrade_id()`
#'
#' @rdname change_id
#' @export
#'
change_id <- function(x, target_id, id_cfg, ...) {

  assert_that(is.string(target_id))

  orig_id <- id_vars(x)

  if (identical(orig_id, target_id)) {
    return(x)
  }

  if (select_ids(id_cfg, orig_id) < select_ids(id_cfg, target_id)) {
    upgrade_id(x, target_id, id_cfg, ...)
  } else if (select_ids(id_cfg, orig_id) > select_ids(id_cfg, target_id)) {
    downgrade_id(x, target_id, id_cfg, ...)
  } else {
    stop("Cannot handle conversion of IDs with identical positions")
  }
}

#' @param cols Column names that require time-adjustment
#'
#' @rdname change_id
#' @export
#'
upgrade_id <- function(x, target_id, id_cfg, cols = time_vars(x), ...) {

  assert_that(select_ids(id_cfg, id_vars(x)) < select_ids(id_cfg, target_id))

  UseMethod("upgrade_id", x)
}

#' @rdname change_id
#' @export
#'
downgrade_id <- function(x, target_id, id_cfg, cols = time_vars(x), ...) {

  assert_that(select_ids(id_cfg, id_vars(x)) > select_ids(id_cfg, target_id))

  UseMethod("downgrade_id", x)
}

#' @rdname change_id
#' @export
#'
upgrade_id.ts_tbl <- function(x, target_id, id_cfg, cols = time_vars(x), ...) {

  assert_that(index_var(x) %in% cols)

  if (!is_one_min(interval(x))) {
    warning("Changing the ID of non-minute resolution data will change the ",
            "interval to 1 minute")
  }

  sft <- new_names(x)
  idx <- index_var(x)

  map <- id_map_min(id_cfg, id_vars(x), target_id, sft, idx)

  res <- map[x, on = meta_vars(x), roll = -Inf, rollends = TRUE]
  res <- res[, c(cols) := lapply(.SD, `-`, get(sft)), .SDcols = cols]

  res <- as_ts_tbl(res, target_id, idx, mins(1L), by_ref = TRUE)
  res <- rm_cols(res, sft, by_ref = TRUE)

  res
}

#' @rdname change_id
#' @export
#'
upgrade_id.id_tbl <- function(x, target_id, id_cfg, cols = time_vars(x), ...) {

  change_id_helper(x, target_id, id_cfg, cols, "up", ...)
}

#' @rdname change_id
#' @export
#'
downgrade_id.ts_tbl <- function(x, target_id, id_cfg, cols = time_vars(x),
                                ...) {

  assert_that(index_var(x) %in% cols)

  if (!is_one_min(interval(x))) {
    warning("Changing the ID of non-minute resolution data will change the ",
            "interval to 1 minute")
  }

  res <- change_id_helper(x, target_id, id_cfg, cols, "down", ...)

  change_interval(res, mins(1L), cols, by_ref = TRUE)
}

#' @rdname change_id
#' @export
#'
downgrade_id.id_tbl <- function(x, target_id, id_cfg, cols = time_vars(x),
                                ...) {

  change_id_helper(x, target_id, id_cfg, cols, "down", ...)
}

change_id_helper <- function(x, targ, cfg, cols, dir = c("down", "up"), ...) {

  dir <- match.arg(dir)
  idx <- id_vars(x)

  if (length(cols)) {
    sft <- new_names(x)
  } else {
    sft <- NULL
  }

  if (identical(dir, "down")) {
    map <- id_map_min(cfg, targ, idx, sft, NULL)
  } else {
    map <- id_map_min(cfg, idx, targ, sft, NULL)
  }

  res <- merge(x, map, by = idx, ...)

  if (length(cols)) {
    res <- res[, c(cols) := lapply(.SD, `-`, get(sft)), .SDcols = cols]
  }

  res <- set_id_vars(res, targ)
  res <- rm_cols(res, sft, by_ref = TRUE)

  res
}
