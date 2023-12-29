#' Data loading utilities
#'
#' Two important tools for smoothing out differences among used datasets are
#' `id_origin()` which returns origin times for a given ID and `id_map()`
#' which returns a mapping between two ID systems alongside start and end
#' columns of the target ID system relative to the source ID system. As both
#' these function are called frequently during data loading and might involve
#' somewhat expensive operations, both rely on internal helper functions
#' (`id_orig_helper()` and `id_map_helper()`) which perform the heavy lifting
#' and wrap those helper functions, providing a memoization layer. When adding
#' a new data source, a class specific implementation of the S3 generic
#' function `id_map_helper()` might be required, as this is used during data
#' loading using [load_id()] and [load_ts()] via [change_id()].
#'
#' @details
#' For the internal datasets, `id_map_helper()` relies on yet another S3
#' generic function `id_windows()`, which provides a table containing all
#' available ID systems, as well as all ID windows for a given data source. As
#' for the other two functions, the same helper-function approach is in place,
#' with the data loading function `id_win_helper()`. The function
#' `id_map_helper()` is then implemented in a data source agnostic manner
#' (dispatching on the `src_env` class), providing subsetting of this larger
#' ID map table and ensuring timestamps are relative to the correct ID system.
#' For adding a new data source however, this layer can be forgone. Similarly
#' for `id_origin()`, this is used for the internal datasets in
#' [load_difftime()]. An implementation of [load_difftime()], specific to a
#' new data source can be provided that does not rely on `id_windows()`,
#' making this function irrelevant for this specific dataset.
#'
#' @param x Object identify the ID system (passed to [as_src_env()])
#' @param id ID name for which to return origin times
#' @param origin_name String-valued name which will be used to label the origin
#' column
#' @param copy Logical flag indicating whether to return a copy of the memoized
#' `0data.table` for safety
#'
#' @return
#' * `id_origin()`/`id_orig_helper()`: an `id_tbl` with admission time stamps
#'    corresponding to the selected ID
#' * `id_windows()`/`id_win_helper()`: an `id_tbl` holding all IDs and their
#'    respective start and end times
#' * `id_map()`/`id_map_helper()`: an `id_tbl` containing the selected IDs and
#'    depending on values passed as `in_time` and `out_time`, start and end
#'    times of the ID passed as `win_var`
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
id_orig_helper <- function(x, id) {

  assert_that(is.string(id))

  UseMethod("id_orig_helper", x)
}

#' @rdname data_utils
#' @export
id_orig_helper.src_env <- function(x, id) {

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

  res <- unique(res)

  as_id_tbl(res, id, by_ref = TRUE)
}

#' @rdname data_utils
#' @export
id_orig_helper.miiv_env <- function(x, id) {

  if (!identical(id, "subject_id")) {
    return(NextMethod())
  }

  cfg <- as_id_cfg(x)[id == id_var_opts(x)]

  assert_that(length(cfg) == 1L)

  sta <- field(cfg, "start")
  age <- "anchor_age"

  res <- as_src_tbl(x, field(cfg, "table"))
  res <- res[, c(id, sta, age)]
  res <- res[, c(sta, age) := shift_year(get(sta), get(age))]

  as_id_tbl(res, id, by_ref = TRUE)
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

#' @rdname data_utils
#' @export
id_win_helper.sic_env <- function(x) {
  
  sec_as_mins <- function(x) min_as_mins(as.integer(x / 60))
  
  cfg <- sort(as_id_cfg(x), decreasing = TRUE)
  
  ids <- field(cfg, "id")
  sta <- c(unique(field(cfg, "start")), "HospAdmTime")
  end <- field(cfg, "end")
  
  tbl <- as_src_tbl(x, unique(field(cfg, "table")))
  
  mis <- setdiff(sta, colnames(tbl))
  
  res <- load_src(tbl, cols = c(ids, intersect(sta, colnames(tbl)), end))
  
  if (length(mis) > 0L) {
    res[, c(mis) := 0L]
  }
  
  res <- res[, c(sta, end) := lapply(.SD, sec_as_mins), .SDcols = c(sta, end)]
  res <- setcolorder(res, c(ids, sta, end))
  res <- rename_cols(res, c(ids, paste0(ids, "_start"),
                            paste0(ids, "_end")), by_ref = TRUE)
  
  as_id_tbl(res, ids[2L], by_ref = TRUE)
}

#' @importFrom rlang .data .env
#'
#' @rdname data_utils
#' @export
id_win_helper.hirid_env <- function(x) {

  ind_fun <- function(id, index) {

    tmp <- setDT(list(id = id, index = index))

    ind <- tmp[, .I[which.max(get("index"))], by = "id"][["V1"]]

    res <- logical(nrow(tmp))
    res[ind] <- TRUE

    res
  }

  cfg <- sort(as_id_cfg(x), decreasing = TRUE)

  ids <- field(cfg, "id")
  sta <- field(cfg, "start")

  obs <- load_src(
    "observations", x, .env$ind_fun(.data$patientid, .data$datetime),
    c("patientid", "datetime")
  )
  obs <- obs[, list(datetime = max(get("datetime"))), by = "patientid"]

  tbl <- load_src(
    field(cfg, "table"), x, cols = c(ids, sta)
  )

  res <- merge(tbl, obs, by = ids)
  res <- res[, c(sta, "datetime") := lapply(.SD, as_dt_min, get(sta)),
             .SDcols = c(sta, "datetime")]

  order_rename(res, ids, sta, "datetime")
}

#' @rdname data_utils
#' @export
id_win_helper.aumc_env <- function(x) {

  cfg <- sort(as_id_cfg(x), decreasing = TRUE)

  ids <- field(cfg, "id")
  sta <- field(cfg, "start")
  end <- field(cfg, "end")

  tbl <- as_src_tbl(x, unique(field(cfg, "table")))

  mis <- setdiff(sta, colnames(tbl))

  assert_that(length(mis) >= 1L)

  res <- load_src(tbl, cols = c(ids, intersect(sta, colnames(tbl)), end))

  if (length(mis) > 0L) {
    res[, c(mis) := 0L]
  }

  res <- res[, c(sta, end) := lapply(.SD, ms_as_mins), .SDcols = c(sta, end)]

  res <- setcolorder(res, c(ids, sta, end))
  res <- rename_cols(res, c(ids, paste0(ids, "_start"),
                                 paste0(ids, "_end")), by_ref = TRUE)

  as_id_tbl(res, ids[2L], by_ref = TRUE)
}

#' @rdname data_utils
#' @export
id_win_helper.miiv_env <- function(x) {

  merge_inter <- function(x, y) {
    merge(x, y, by = intersect(colnames(x), colnames(y)))
  }

  get_id_tbl <- function(tbl, id, start, end, aux) {
    as_src_tbl(x, tbl)[, c(id, start, end, aux)]
  }

  age <- "anchor_age"

  cfg <- sort(as_id_cfg(x), decreasing = TRUE)

  ids  <- field(cfg, "id")
  sta <- field(cfg, "start")
  end  <- field(cfg, "end")

  res <- Map(get_id_tbl, field(cfg, "table"), ids, sta,
             end, c(as.list(ids[-1L]), age))
  res <- Reduce(merge_inter, res)

  res <- res[, c(tail(sta, n = 1L), age) := shift_year(get(tail(sta, n = 1L)),
                                                       get(age))]
  res <- res[, c(sta, end) := lapply(.SD, as_dt_min, get(sta[1L])),
             .SDcols = c(sta, end)]

  order_rename(res, ids, sta, end)
}

#' @export
id_win_helper.default <- function(x) stop_generic(x, .Generic)

shift_year <- function(x, y) list(as.POSIXct(paste0(x - y, "-01-01")), NULL)

order_rename <- function(x, id_var, st_var, ed_var) {
  x <- setcolorder(x, c(id_var, st_var, ed_var))
  x <- rename_cols(x, c(id_var, paste0(id_var, "_start"),
                                paste0(id_var, "_end")), by_ref = TRUE)
  as_id_tbl(x, id_var[1L], by_ref = TRUE)
}

as_dt_min <- function(x, y) floor(difftime(x, y, units = "mins"))

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

  io_vars <- paste0(win_var, c("_start", "_end"))

  if (!identical(id_var, map_id)) {
    ori <- new_names(map)
    map <- map[, c(ori) := get(paste0(id_var, "_start"))]
    map <- map[, unique(c(id_var, win_var, io_vars, ori)), with = FALSE]
    map <- map[, c(io_vars) := lapply(.SD, `-`, get(ori)),
               .SDcols = c(io_vars)]
  }

  kep <- setdiff(colnames(map), unique(c(id_var, win_var, io_vars)))
  map <- rm_cols(map, kep, by_ref = TRUE)
  map <- unique(map)

  as_id_tbl(map, id_var, by_ref = TRUE)
}

#' @export
id_map_helper.default <- function(x, ...) stop_generic(x, .Generic)

#' Stays
#'
#' Building on functionality offered by the (internal) function [id_map()],
#' stay windows as well as (in case of differing values being passed as
#' `id_type` and `win_type`) an ID mapping is computed.
#'
#' @seealso change_id
#'
#' @param x Data source (is coerced to `src_env` using `as_src_env()`)
#'
#' @return An `id_tbl` containing the selected IDs and depending on values
#' passed as `in_time` and `out_time`, start and end times of the ID passed as
#' `win_var`.
#'
#' @rdname stay_windows
#' @export
stay_windows <- function(x, ...) UseMethod("stay_windows", x)

#' @param id_type Type of ID all returned times are relative to
#' @param win_type Type of ID for which the in/out times is returned
#' @param in_time,out_time column names of the returned in/out times
#' @param interval The time interval used to discretize time stamps with,
#' specified as [base::difftime()] object
#' @param patient_ids Patient IDs used to subset the result
#' @param ... Generic consistency
#'
#' @rdname stay_windows
#' @export
stay_windows.src_env <- function(x, id_type = "icustay", win_type = id_type,
                                 in_time = "start", out_time = "end",
                                 interval = hours(1L), patient_ids = NULL,
                                 ...) {

  warn_dots(...)

  assert_that(is_interval(interval))

  cfg <- as_id_cfg(x)
  res <- id_map(x, id_vars(cfg[id_type]), id_vars(cfg[win_type]),
                in_time, out_time)

  if (!is_one_min(interval) && (not_null(in_time) || not_null(out_time))) {

    res <- res[, c(in_time, out_time) := lapply(.SD, re_time, interval),
               .SDcols = c(in_time, out_time)]
  }

  merge_patid(res, patient_ids)
}

#' @rdname stay_windows
#' @export
stay_windows.character <- function(x, ...) stay_windows(as.list(x), ...)

#' @rdname stay_windows
#' @export
stay_windows.list <- function(x, ..., patient_ids = NULL) {

  load_one <- function(x, ...) {

    src <- src_name(x)
    res <- stay_windows(x, ..., patient_ids = patient_ids[[src]])

    if (mulit_src) {
      res <- add_src_col(res, src)
    }

    res

  }

  x <- lapply(x, as_src_env)

  srcs <- unique(chr_ply(x, src_name))
  mulit_src <- length(srcs) > 1L

  patient_ids <- split_patid(patient_ids, srcs)

  rbind_lst(lapply(x, load_one, ...))
}

#' @rdname stay_windows
#' @export
stay_windows.default <- function(x, ...) stay_windows(as_src_env(x), ...)

#' Switch between id types
#'
#' ICU datasets such as MIMIC-III or eICU typically represent patients by
#' multiple ID systems such as patient IDs, hospital stay IDs and ICU
#' admission IDs. Even if the raw data is available in only one such ID
#' system, given a mapping of IDs alongside start and end times, it is
#' possible to convert data from one ID system to another. The function
#' `change_id()` provides such a conversion utility, internally either
#' calling `upgrade_id()` when moving to an ID system with higher cardinality
#' and `downgrade_id()` when the target ID system is of lower cardinality
#'
#' @details
#' In order to provide ID system conversion for a data source, the (internal)
#' function [id_map()] must be able to construct an ID mapping for that data
#' source. Constructing such a mapping can be expensive w.r.t. the frequency
#' it might be re-used and therefore, [id_map()] provides caching
#' infrastructure. The mapping itself is constructed by the (internal)
#' function [id_map_helper()], which is expected to provide source and
#' destination ID columns as well as start and end columns corresponding to
#' the destination ID, relative to the source ID system. In the following
#' example, we request for `mimic_demo`, with ICU stay IDs as source and
#' hospital admissions as destination IDs.
#'
#' ```{r, eval = is_data_avail("mimic_demo")}
#' id_map_helper(mimic_demo, "icustay_id", "hadm_id")
#' ```
#'
#' Both start and end columns encode the hospital admission windows relative
#' to each corresponding ICU stay start time. It therefore comes as no
#' surprise that most start times are negative (hospital admission typically
#' occurs before ICU stay start time), while end times are often days in the
#' future (as hospital discharge typically occurs several days after ICU
#' admission).
#'
#' In order to use the ID conversion infrastructure offered by `ricu` for a
#' new dataset, it typically suffices to provide an `id_cfg` entry in the
#' source configuration (see [load_src_cfg()]), outlining the available ID
#' systems alongside an ordering, as well as potentially a class specific
#' implementation of [id_map_helper()] for the given source class, specifying
#' the corresponding time windows in 1 minute resolution (for every possible
#' pair of IDs).
#'
#' While both up- and downgrades for `id_tbl` objects, as well as downgrades
#' for `ts_tbl` objects are simple merge operations based on the ID mapping
#' provided by [id_map()], ID upgrades for `ts_tbl` objects are slightly more
#' involved. As an example, consider the following setting: we have `data`
#' associated with `hadm_id` IDs and times relative to hospital admission:
#'
#' ```
#'                1      2       3        4       5       6        7      8
#' data        ---*------*-------*--------*-------*-------*--------*------*---
#'                3h    10h     18h      27h     35h     43h      52h    59h
#'
#'                                          HADM_1
#'             0h     7h                26h        37h             53h      62h
#' hadm_id     |-------------------------------------------------------------|
#' icustay_id         |------------------|          |---------------|
#'                    0h                19h         0h             16h
#'                            ICU_1                       ICU_2
#' ```
#'
#' The mapping of data points from `hadm_id` to `icustay_id` is created as
#' follows: ICU stay end times mark boundaries and all data that is recorded
#' after the last ICU stay ended is assigned to the last ICU stay. Therefore
#' data points 1-3 are assigned to `ICU_1`, while 4-8 are assigned to `ICU_2`.
#' Times have to be shifted as well, as timestamps are expected to be relative
#' to the current ID system. Data points 1-3 therefore are assigned to time
#' stamps -4h, 3h and 11h, while data points 4-8 are assigned to -10h, -2h,
#' 6h, 15h and 22h. Implementation-wise, the mapping is computed using an
#' efficient `data.table` rolling join.
#'
#' @param x `icu_tbl` object for which to make the id change
#' @param target_id The destination id name
#' @param src Passed to [as_id_cfg()] and [as_src_env()]
#' @param ... Passed to `upgrade_id()`/`downgrade_id()`
#' @param keep_old_id Logical flag indicating whether to keep the previous ID
#' column
#' @param id_type Logical flag indicating whether `target_id` is specified as
#' ID name (e.g. `icustay_id` on MIMIC) or ID type (e.g. `icustay`)
#'
#' @return An object of the same type as `x` with modified IDs.
#'
#' @examples
#' if (require(mimic.demo)) {
#' tbl <- mimic_demo$labevents
#' dat <- load_difftime(tbl, itemid == 50809, c("charttime", "valuenum"))
#' dat
#'
#' change_id(dat, "icustay_id", tbl, keep_old_id = FALSE)
#' }
#'
#' @encoding UTF-8
#' @rdname change_id
#' @export
#'
change_id <- function(x, target_id, src, ..., keep_old_id = TRUE,
                      id_type = FALSE) {

  assert_that(is.string(target_id), is.flag(keep_old_id))

  id_cfg <- as_id_cfg(src)

  if (isTRUE(id_type)) {
    target_id <- id_type_to_name(id_cfg, target_id)
  }

  orig_id <- id_var(x)

  if (identical(orig_id, target_id)) {
    return(x)
  }

  opt <- id_var_opts(id_cfg)

  assert_that(is_in(orig_id, opt), is_in(target_id, opt))

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
    fun <- `+`
  } else {
    map <- id_map(src, idx, targ, sft, NULL)
    fun <- `-`
  }

  res <- merge(x, map, by = idx, ...)

  if (length(cols)) {
    res <- res[, c(cols) := lapply(.SD, fun, get(sft)), .SDcols = cols]
  }

  res <- set_id_vars(res, targ)
  res <- rm_cols(res, sft, by_ref = TRUE)

  res
}
