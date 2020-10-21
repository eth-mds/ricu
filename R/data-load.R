
#' Low level functions for loading data
#'
#' Data loading involves a cascade of S3 generic functions, which can
#' individually be adapted to the specifics of individual data sources. A the
#' lowest level, `load_scr` is called, followed by `load_difftime()`.
#' Functions up the chain, are described in [load_id()].
#'
#' @details
#' A function extending the S3 generic `load_src()` is expected to load a
#' subset of rows/columns from a tabular data source. While the column
#' specification is provided as character vector of column names, the row
#' subsetting involves non-standard evaluation (NSE). Data-sets that are
#' included with `ricu` are represented by [`prt`][prt::new_prt()] objects,
#' which use [rlang::eval_tidy()] to evaluate NSE expressions. Furthermore,
#' `prt` objects potentially represent tabular data split into partitions and
#' row-subsetting expressions are evaluated per partition (see the `part_safe`
#' flag in [prt::subset.prt()]). The return value of `load_src()` is expected
#' to be of type `data.table`.
#'
#' Timestamps are represented differently among the included data sources:
#' while MIMIC-III and HiRID use absolute date/times, eICU provides temporal
#' information as minutes relative to ICU admission. Other data sources, such
#' as the ICU dataset provided by Amsterdam UMC, opt for relative times as
#' well, but not in minutes since admission, but in milliseconds. In order to
#' smoothen out such discrepancies, the next function in the data loading
#' hierarchy is `load_difftime()`. This function is expected to call
#' `load_src()` in order to load a subset of rows/columns from a table stored
#' on disk and convert all columns that represent timestamps (as specified by
#' the argument `time_vars`) into [base::difftime()] vectors using `mins` as
#' time unit.
#'
#' The returned object should be of type `id_tbl`, with the ID vars
#' identifying the ID system the times are relative to. If for example all
#' times are relative to ICU admission, the ICU stay ID should be returned as
#' ID column. The argument `id_hint` may suggest an ID type, but if in the raw
#' data, this ID is not available, `load_difftime()` may return data using a
#' different ID system. In MIMIC-III, for example, data in the `labevents`
#' table is available for `subject_id` (patient ID) pr `hadm_id` (hospital
#' admission ID). If data is requested for `icustay_id` (ICU stay ID), this
#' request cannot be fulfilled and data is returned using the ID system with
#' the highest cardinality (among the available ones). Utilities such as
#' [change_id()] can the later be used to resolve data to `icustay_id`.
#'
#' @param x Object for which to load data
#' @param ... Generic consistency
#'
#' @examples
#' if (require(mimic.demo)) {
#' tbl <- mimic_demo$labevents
#' col <- c("charttime", "value")
#'
#' load_src(tbl, itemid == 50809)
#'
#' colnames(
#'   load_src("labevents", "mimic_demo", itemid == 50809, cols = col)
#' )
#'
#' load_difftime(tbl, itemid == 50809)
#'
#' colnames(
#'   load_difftime(tbl, itemid == 50809, col)
#' )
#'
#' id_vars(
#'   load_difftime(tbl, itemid == 50809, id_hint = "icustay_id")
#' )
#'
#' id_vars(
#'   load_difftime(tbl, itemid == 50809, id_hint = "subject_id")
#' )
#' }
#'
#' @rdname load_src
#' @export
#'
load_src <- function(x, ...) UseMethod("load_src", x)

#' @param rows Expression used for row subsetting (NSE)
#' @param cols Character vector of column names
#'
#' @rdname load_src
#' @export
load_src.src_tbl <- function(x, rows, cols = colnames(x), ...) {

  warn_dots(...)

  assert_that(is.character(cols), length(cols) > 0L)

  cols <- unique(cols)

  subset(x, {{ rows }}, .env$cols, part_safe = TRUE)
}

#' @param src Passed to [as_src_tbl()] in order to determine the data source
#'
#' @rdname load_src
#' @export
load_src.character <- function(x, src, ...) {
  load_src(as_src_tbl(x, src), ...)
}

#' @export
load_src.default <- function(x, ...) stop_generic(x, .Generic)

#' @rdname load_src
#' @export
load_difftime <- function(x, ...) UseMethod("load_difftime", x)

#' @param id_hint String valued id column selection (not necessarily honored)
#' @param time_vars Character vector enumerating the columns to be treated as
#' timestamps and thus returned as [base::difftime()] vectors
#'
#' @rdname load_src
#' @export
load_difftime.mimic_tbl <- function(x, rows, cols = colnames(x),
                                    id_hint = id_vars(x),
                                    time_vars = ricu::time_vars(x), ...) {

  warn_dots(...)

  load_mihi(x, {{ rows }}, cols, id_hint, time_vars)
}

#' @rdname load_src
#' @export
load_difftime.eicu_tbl <- function(x, rows, cols = colnames(x),
                                   id_hint = id_vars(x),
                                   time_vars = ricu::time_vars(x), ...) {

  warn_dots(...)

  load_eicu(x, {{ rows }}, cols, id_hint, time_vars)
}

#' @rdname load_src
#' @export
load_difftime.hirid_tbl <- function(x, rows, cols = colnames(x),
                                    id_hint = id_vars(x),
                                    time_vars = ricu::time_vars(x), ...) {

  warn_dots(...)

  load_mihi(x, {{ rows }}, cols, id_hint, time_vars)
}

#' @rdname load_src
#' @export
load_difftime.character <- function(x, src, ...) {
  load_difftime(as_src_tbl(x, src), ...)
}

#' @export
load_difftime.default <- function(x, ...) stop_generic(x, .Generic)

resolve_id_hint <- function(tbl, hint) {

  assert_that(is.string(hint))

  if (hint %in% colnames(tbl)) {
    return(hint)
  }

  opts <- as_id_cfg(tbl)

  hits <- id_var_opts(opts) %in% colnames(tbl)

  assert_that(sum(hits) > 0L, msg = paste0("No overlap between configured id var options and available columns for table `", tbl_name(tbl), "` in ",
    src_name(tbl))
  )

  id_vars(opts[hits])
}

load_mihi <- function(x, rows, cols, id_hint, time_vars) {

  dt_round_min <- function(x, y) round_to(difftime(x, y, units = "mins"))

  id_col <- resolve_id_hint(x, id_hint)

  assert_that(is.string(id_col), id_col %in% colnames(x))

  if (!id_col %in% cols) {
    cols <- c(cols, id_col)
  }

  time_vars <- intersect(time_vars, cols)

  dat <- load_src(x, {{ rows }}, cols)

  if (length(time_vars)) {

    dat <- merge(dat, id_origin(x, id_col, origin_name = "origin"),
                 by = id_col)

    dat <- dat[,
      c(time_vars) := lapply(.SD, dt_round_min, get("origin")),
      .SDcols = time_vars
    ]
    dat <- dat[, c("origin") := NULL]
  }

  as_id_tbl(dat, id_vars = id_col, by_ref = TRUE)
}

load_eicu <- function(x, rows, cols, id_hint, time_vars) {

  id_col <- resolve_id_hint(x, id_hint)

  if (!id_col %in% cols) {
    cols <- c(id_col, cols)
  }

  time_vars <- intersect(time_vars, cols)

  dat <- load_src(x, {{ rows }}, cols)

  if (length(time_vars)) {

    assert_that(id_col %in% colnames(dat),
      msg = paste("In order to return relative times, a single ID var",
                  paste0("`", id_col, "`"), "is required.")
    )

    dat <- dat[, c(time_vars) := lapply(.SD, as.difftime, units = "mins"),
               .SDcols = time_vars]
  }

  as_id_tbl(dat, id_vars = id_col, by_ref = TRUE)
}

#' Load data as `id_tbl` or `ts_tbl` objects
#'
#' Building on functionality provided by [load_src()] and [load_difftime()],
#' `load_id()` and `load_ts()` load data from disk as `id_tbl` and `ts_tbl`
#' objects respectively. Over `load_difftime()` both `load_id()` and
#' `load_ts()` provide a way to specify [meta_vars()] (as `id_var` and
#' `index_var` arguments), as well as an interval size (as `interval`
#' argument) for time series data.
#'
#' @details
#' While for [load_difftime()] the ID variable can be suggested, the function
#' only returns a best effort at fulfilling this request. In some cases, where
#' the data does not allow for the desired ID type, data is returned using the
#' ID system (among all available ones for the given table) with highest
#' cardinality. Both `load_id()` and `load_ts()` are guaranteed to return an
#' object with [id_vars()] set as requested by the `id_var` argument.
#' Internally, the change of ID system is performed by [change_id()].
#'
#' Additionally, while times returned by [load_difftime()] are in 1 minute
#' resolution, the time series step size can be specified by the `interval`
#' argument when calling `load_id()` or `load_ts()`. This rounding and
#' potential change of time unit is performed by [change_interval()] on all
#' columns specified by the `time_vars` argument. All time stamps are relative
#' to the origin provided by the ID system. This means that for an `id_var`
#' corresponding to hospital IDs, times are relative to hospital admission.
#'
#' When `load_id()` (or `load_ts()`) is called on [`itm`][new_itm()] objects
#' instead of [`src_tbl`][new_src_tbl()] (or objects that can be coerced to
#' `src_tbl`), The row-subsetting is performed according the the specification
#' as provided by the `itm` object. Furthermore, at default settings, columns
#' are returned as required by the `itm` object and `id_var` (as well as
#' `index_var`) are set accordingly if specified by the `itm` or set to
#' default values for the given `src_tbl` object if not explicitly specified.
#'
#' @inheritParams load_difftime
#'
#' @param id_var The column defining the id of `ts_tbl` and `id_tbl` objects
#' @param interval The time interval used to discretize time stamps with,
#' specified as [base::difftime()] object
#'
#' @examples
#' if (require(mimic.demo)) {
#' load_id("admissions", "mimic_demo", cols = "admission_type")
#'
#' dat <- load_ts(mimic_demo$labevents, itemid %in% c(50809L, 50931L),
#'                cols = c("itemid", "valuenum"))
#'
#' glu <- new_itm(src = "mimic_demo", table = "labevents",
#'                sub_var = "itemid", ids = c(50809L, 50931L))
#'
#' identical(load_ts(glu), dat)
#' }
#'
#' @rdname load_tbl
#' @export
load_id <- function(x, ...) UseMethod("load_id", x)

#' @rdname load_tbl
#' @export
load_id.src_tbl <- function(x, rows, cols = colnames(x),
                            id_var = id_vars(x), interval = hours(1L),
                            time_vars = ricu::time_vars(x), ...) {

  warn_dots(...)

  res <- load_difftime(x, {{ rows }}, cols, id_var, time_vars)

  time_vars <- intersect(time_vars, colnames(res))

  res <- change_id(res, id_var, x, cols = time_vars, keep_old_id = FALSE)
  res <- change_interval(res, interval, time_vars, by_ref = TRUE)

  res
}

#' @rdname load_tbl
#' @export
load_id.character <- function(x, src, ...) {
  load_id(as_src_tbl(x, src), ...)
}

#' @rdname load_tbl
#' @export
load_id.itm <- function(x, cols = colnames(x), id_var = id_vars(x), ...) {
  load_id(as_src_tbl(x), !!prepare_query(x), cols, id_var, ...)
}

#' @rdname load_tbl
#' @export
load_id.fun_itm <- function(x, ...) stop_generic(x, .Generic)

#' @rdname load_tbl
#' @export
load_id.default <- function(x, ...) load_id(as_src_tbl(x), ...)

#' @param index_var The column defining the index of `ts_tbl` objects
#'
#' @rdname load_tbl
#' @export
load_ts <- function(x, ...) UseMethod("load_ts", x)

#' @rdname load_tbl
#' @export
load_ts.src_tbl <- function(x, rows, cols = colnames(x), id_var = id_vars(x),
                            index_var = ricu::index_var(x),
                            interval = hours(1L),
                            time_vars = ricu::time_vars(x), ...) {

  warn_dots(...)

  assert_that(is.string(index_var))

  if (!index_var %in% cols) {
    cols <- c(cols, index_var)
  }

  res <- load_difftime(x, {{ rows }}, cols, id_var, time_vars)
  res <- as_ts_tbl(res, id_vars(res), index_var, mins(1L), by_ref = TRUE)

  time_vars <- intersect(time_vars, colnames(res))

  res <- change_id(res, id_var, x, cols = time_vars, keep_old_id = FALSE)
  res <- change_interval(res, interval, time_vars, by_ref = TRUE)

  res
}

#' @rdname load_tbl
#' @export
load_ts.character <- function(x, src, ...) {
  load_ts(as_src_tbl(x, src), ...)
}

#' @rdname load_tbl
#' @export
load_ts.itm <- function(x, cols = colnames(x), id_var = id_vars(x),
                        index_var = ricu::index_var(x), ...) {

  load_ts(as_src_tbl(x), !!prepare_query(x), cols, id_var, index_var, ...)
}

#' @rdname load_tbl
#' @export
load_ts.fun_itm <- function(x, ...) stop_generic(x, .Generic)

#' @rdname load_tbl
#' @export
load_ts.default <- function(x, ...) load_ts(as_src_tbl(x), ...)
