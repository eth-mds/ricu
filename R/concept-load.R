
#' Load concept data
#'
#' Concept objects are used in `ricu` as a way to specify how a clinical
#' concept, such as heart rate can be loaded from a data source. Building on
#' this abstraction, `load_concepts()` powers concise loading of data with
#' data source specific pre-processing hidden away from the user, thereby
#' providing a data source agnostic interface to data loading. At default
#' value of the argument `merge_data`, a tabular data structure (either a
#' [`ts_tbl`][ts_tbl()] or an [`id_tbl`][id_tbl()], depending on what kind of
#' concepts are requested), inheriting from
#' [`data.table`][data.table::data.table], is returned, representing the data
#' in wide format (i.e. returning concepts as columns).
#'
#' @details
#' In order to allow for a large degree of flexibility (and extensibility),
#' which is much needed owing to considerable heterogeneity presented by
#' different data sources, several nested S3 classes are involved in
#' representing a concept and `load_concepts()` follows this hierarchy of
#' classes recursively when
#' resolving a concept. An outline of this hierarchy can be described as
#'
#' * `concept`: contains many `cncpt` objects (of potentially differing
#'   sub-types), each comprising of some meta-data and an `item` object
#' * `item`: contains many `itm` objects (of potentially differing
#'   sub-types), each encoding how to retrieve a data item.
#'
#' The design choice for wrapping a vector of `cncpt` objects with a container
#' class `concept` is motivated by the requirement of having several different
#' sub-types of `cncpt` objects (all inheriting from the parent type `cncpt`),
#' while retaining control over how this homogeneous w.r.t. parent type, but
#' heterogeneous w.r.t. sub-type vector of objects behaves in terms of S3
#' generic functions.
#'
#' @section Concept:
#' Top-level entry points are either a character vector, which is used to
#' subset a `concept` object or an entire [concept
#' dictionary][load_dictionary()], or a `concept` object. When passing a
#' character vector as first argument, the most important further arguments at
#' that level control from where the dictionary is taken (`dict_name` or
#' `dict_dirs`). At `concept` level, the most important additional arguments
#' control the result structure: data merging can be disabled using
#' `merge_data` and data aggregation is governed by the `aggregate` argument.
#'
#' Data aggregation is important for merging several concepts into a
#' wide-format table, as this requires data to be unique per observation (i.e.
#' by either id or combination of id and index). Several value types are
#' acceptable as `aggregate` argument, the most important being `FALSE`, which
#' disables aggregation, NULL, which auto-determines a suitable aggregation
#' function or a string which is ultimately passed to [dt_gforce()] where it
#' identifies a function such as `sum()`, `mean()`, `min()` or `max()`. More
#' information on aggregation is available as [aggregate()][rename_cols()].
#' If the object passed as `aggregate` is scalar, it is applied to all
#' requested concepts in the same way. In order to customize aggregation per
#' concept, a named object (with names corresponding to concepts) of the same
#' length as the number of requested concepts may be passed.
#'
#' Under the hood, a `concept` object comprises of several `cncpt` objects
#' with varying sub-types (for example `num_cncpt`, representing continuous
#' numeric data or `fct_cncpt` representing categorical data). This
#' implementation detail is of no further importance for understanding concept
#' loading and for more information, please refer to the
#' [`concept`][concept()] documentation. The only argument that is introduced
#' at `cncpt` level is `progress`, which controls progress reporting. If
#' called directly, the default value of `NULL` yields messages, sent to the
#' terminal. Internally, if called from `load_concepts()` at `concept` level
#' (with `verbose` set to `TRUE`), a [progress::progress_bar] is set up in a
#' way that allows nested messages to be captured and not interrupt progress
#' reporting (see [msg_progress()]).
#'
#' @section Item:
#' A single `cncpt` object contains an `item` object, which in turn is
#' composed of several `itm` objects with varying sub-types, the relationship
#' `item` to `itm` being that of `concept` to `cncpt` and the rationale for
#' this implementation choice is the same as previously: a container class
#' used representing a vector of objects of varying sub-types, all inheriting
#' form a common super-type. For more information on the `item` class, please
#' refer to the [relevant documentation][item]. Arguments introduced at `item`
#' level include `patient_ids`, `id_type` and `interval`. Acceptable values for
#' `interval` are scalar-valued [base::difftime()] objects (see also helper
#' functions such as [hours()]) and this argument essentially controls the
#' time-resolution of the returned time-series. Of course, the limiting factor
#' raw time resolution which is on the order of hours for data sets like
#' [MIMIC-III](https://physionet.org/content/mimiciii/) or
#' [eICU](https://physionet.org/content/eicu-crd) but can be much higher for a
#' data set like [HiRID](https://physionet.org/content/hirid/). The argument
#' `id_type` is used to specify what kind of id system should be used to
#' identify different time series in the returned data. A data set like
#' MIMIC-III, for example, makes possible the resolution of data to 3 nested
#' ID systems:
#'
#' * `patient` (`subject_id`): identifies a person
#' * `hadm` (`hadm_id`): identifies a hospital admission (several of which are
#'    possible for a given person)
#' * `icustay` (`icustay_id`): identifies an admission to an ICU and again has
#'    a one-to-many relationship to `hadm`.
#'
#' Acceptable argument values are strings that match ID systems as specified
#' by the [data source configuration][load_src_cfg()]. Finally, `patient_ids`
#' is used to define a patient cohort for which data can be requested. Values
#' may either be a vector of IDs (which are assumed to be of the same type as
#' specified by the `id_type` argument) or a tabular object inheriting from
#' `data.frame`, which must contain a column named after the data set-specific
#' ID system identifier (for MIIMIC-III and an `id_type` argument of `hadm`,
#' for example, that would be `hadm_id`).
#'
#' @section Extensions:
#' The presented hierarchy of S3 classes is designed with extensibility in
#' mind: while the current range of functionality covers settings encountered
#' when dealing with the included concepts and datasets, further data sets
#' and/or clinical concepts might necessitate different behavior for data
#' loading. For this reason, various parts in the cascade of calls to
#' `load_concepts()` can be adapted for new requirements by defining new sub-
#' classes to `cncpt` or `itm` and  providing methods for the generic function
#' `load_concepts()`specific to these new classes. At `cncpt` level, method
#' dispatch defaults to `load_concepts.cncpt()` if no method specific to the
#' new class is provided, while at `itm` level, no default function is
#' available.
#'
#' Roughly speaking, the semantics for the two functions are as follows:
#'
#' * `cncpt`: Called with arguments `x` (the current `cncpt` object),
#'   `aggregate` (controlling how aggregation per time-point and ID is
#'   handled), `...` (further arguments passed to downstream methods) and
#'   `progress` (controlling progress reporting), this function should be able
#'   to load and aggregate data for the given concept. Usually this involves
#'   extracting the `item` object and calling `load_concepts()` again,
#'   dispatching on the `item` class with arguments `x` (the given `item`),
#'   arguments passed as `...`, as well as `progress`.
#' * `itm`: Called with arguments `x` (the current object inheriting from
#'   `itm`, `patient_ids` (`NULL` or a patient ID selection), `id_type` (a
#'   string specifying what ID system to retrieve), and `interval` (the time
#'   series interval), this function actually carries out the loading of
#'   individual data items, using the specified ID system, rounding times to
#'   the correct interval and subsetting on patient IDs. As return value, on
#'   object of class as specified by the `target` entry is expected and all
#'   [data_vars()] should be named consistently, as data corresponding to
#'   multiple `itm` objects concatenated in row-wise fashion as in
#'   [base::rbind()].
#'
#' @param x Object specifying the data to be loaded
#' @param ... Passed to downstream methods
#'
#' @return An `id_tbl`/`ts_tbl` or a list thereof, depending on loaded
#' concepts and the value passed as `merge_data`.
#'
#' @examples
#' if (require(mimic.demo)) {
#' dat <- load_concepts("glu", "mimic_demo")
#'
#' gluc <- concept("gluc",
#'   item("mimic_demo", "labevents", "itemid", list(c(50809L, 50931L)))
#' )
#'
#' identical(load_concepts(gluc), dat)
#'
#' class(dat)
#' class(load_concepts(c("sex", "age"), "mimic_demo"))
#' }
#'
#' @rdname load_concepts
#' @export
load_concepts <- function(x, ...) UseMethod("load_concepts", x)

#' @param src A character vector, used to subset the `concepts`; `NULL`
#' means no subsetting
#' @param concepts The concepts to be used or `NULL` in which case
#' [load_dictionary()] is called
#' @param dict_name,dict_dirs In case not concepts are passed as `concepts`,
#' these are forwarded to [load_dictionary()] as `name` and `file` arguments
#'
#' @rdname load_concepts
#' @export
load_concepts.character <- function(x, src = NULL, concepts = NULL, ...,
                                    dict_name = "concept-dict",
                                    dict_dirs = NULL) {

  if (is.null(concepts)) {

    assert_that(not_null(src))

    load_concepts(
      load_dictionary(src, x, name = dict_name, cfg_dirs = dict_dirs),
      src = NULL, ...
    )

  } else {

    load_concepts(concepts[x], src, ...)
  }
}

#' @param aggregate Controls how data within concepts is aggregated
#' @param merge_data Logical flag, specifying whether to merge concepts into
#' wide format or return a list, each entry corresponding to a concept
#' @param verbose Logical flag for muting informational output
#'
#' @rdname load_concepts
#' @export
load_concepts.concept <- function(x, src = NULL, aggregate = NULL,
                                  merge_data = TRUE, verbose = TRUE, ...) {

  assert_that(is.flag(merge_data), is.flag(verbose))

  x <- subset_src(x, src)

  aggregate <- rep_arg(aggregate, names(x))

  if (isTRUE(merge_data) && any(lgl_ply(aggregate, isFALSE)) &&
      length(x) > 1L) {

    stop_ricu("
      Data aggregation cannot be disabled (i.e. passing an `aggregate` value
      of `FALSE` for at least one concept) when data merging is enabled.",
      "merge_no_agg"
    )
  }

  if (verbose) {
    pba <- progress_init(n_tick(x), "Loading {length(x)} concept{?s}")
  } else {
    pba <- FALSE
  }

  res <- with_progress(
    Map(load_one_concept_helper, x, names(x), aggregate,
        MoreArgs = c(list(...), list(progress = pba))),
    progress_bar = pba
  )

  if (isFALSE(merge_data)) {
    return(res)
  }

  if (length(res) > 1L) {

    ts <- lgl_ply(res, is_ts_tbl)
    id <- lgl_ply(res, is_id_tbl) & ! ts

    ind <- c(which(ts), which(id))
    res <- reduce(merge, res[ind], all = TRUE)
    res <- setcolorder(res, c(meta_vars(res), names(x)))

  } else if (length(res) == 1L) {

    res <- res[[1L]]
  }

  res
}

load_one_concept_helper <- function(x, name, aggregate, ..., progress) {

  progress_tick(name, progress, 0L)

  res <- load_concepts(x, aggregate, ..., progress = progress)

  assert_that(has_name(res, name), is_target(x, res))

  progress_tick(progress_bar = progress)

  res
}

rm_na_val_var <- function(x) {

  n_row <- nrow(x)
  x   <- rm_na(x, "val_var")
  n_rm  <- n_row - nrow(x)

  if (n_rm > 0L) {
    msg_progress(
      "removed {n_rm} ({prcnt(n_rm, n_row)}) of rows due to `NA` values")
  }

  x
}

#' @param progress Either `NULL`, or a progress bar object as created by
#' [progress::progress_bar]
#'
#' @rdname load_concepts
#' @export
load_concepts.cncpt <- function(x, aggregate = NULL, ..., progress = NULL) {

  res <- load_concepts(as_item(x), ..., progress = progress)
  res <- rm_na_val_var(res)

  res <- rm_cols(res, setdiff(data_vars(res), "val_var"), by_ref = TRUE)
  res <- rename_cols(res, x[["name"]], "val_var", by_ref = TRUE)

  stats::aggregate(x, res, aggregate)
}

filter_bounds <- function(x, col, min, max) {

  check_bound <- function(vc, val, op) {

    nna <- !is.na(vc)

    if (is.null(val)) {
      return(nna)
    }

    nna & op(vc, val)
  }

  keep  <- check_bound(x[[col]], min, `>=`) & check_bound(x[[col]], max, `<=`)
  n_row <- nrow(x)

  x <- x[keep, ]

  n_rm <- n_row - nrow(x)

  if (n_rm > 0L) {
    msg_progress("removed {n_rm} ({prcnt(n_rm, n_row)}) of rows due to out
                  of range (or `NA`) entries")
  }

  x
}

report_set_unit <- function(x, unit_var, val_var, unit) {

  if (has_name(x, unit_var)) {

    ct  <- table(x[[unit_var]], useNA = "ifany")
    nm  <- names(ct)
    pct <- prcnt(ct)

    if (has_length(unit)) {

      ok <- tolower(nm) %in% tolower(unit)

      if (!all(ok)) {
        msg_progress("not all units are in {concat('[', unit, ']')}:
                      {concat(nm[!ok])} ({pct[!ok]})")
      }

    } else if (length(nm) > 1L) {

      msg_progress("multiple units detected: {concat(nm, ' (', pct, ')')}")
    }
  }

  if (not_null(unit)) {
    setattr(x[[val_var]], "units", unit[1L])
  }

  x
}

#' @rdname load_concepts
#' @export
load_concepts.num_cncpt <- function(x, aggregate = NULL, ...,
                                    progress = NULL) {

  force_num <- force_type("double")

  res <- load_concepts(as_item(x), ..., progress = progress)
  res <- set(res, j = "val_var", value = force_num(res[["val_var"]]))

  res <- filter_bounds(res, "val_var", x[["min"]], x[["max"]])
  res <- report_set_unit(res, "unit_var", "val_var", x[["unit"]])

  res <- rm_cols(res, setdiff(data_vars(res), "val_var"), by_ref = TRUE)
  res <- rename_cols(res, x[["name"]], "val_var", by_ref = TRUE)

  stats::aggregate(x, res, aggregate)
}

#' @rdname load_concepts
#' @export
load_concepts.fct_cncpt <- function(x, aggregate = NULL, ...,
                                    progress = NULL) {

  lvl <- x[["levels"]]

  res <- load_concepts(as_item(x), ..., progress = progress)
  res <- rm_na_val_var(res)

  if (is.character(lvl)) {
    keep <- res[["val_var"]] %chin% lvl
  } else {
    keep <- res[["val_var"]] %in% lvl
  }

  if (!all(keep)) {

    n_row <- nrow(res)

    res <- res[keep, ]

    n_rm <- n_row - nrow(res)

    msg_progress(
      "removed {n_rm} ({prcnt(n_rm, n_row)}) of rows due to level mismatch"
    )
  }

  res <- rm_cols(res, setdiff(data_vars(res), "val_var"), by_ref = TRUE)
  res <- rename_cols(res, x[["name"]], "val_var", by_ref = TRUE)

  stats::aggregate(x, res, aggregate)
}

#' @rdname load_concepts
#' @export
load_concepts.lgl_cncpt <- function(x, aggregate = NULL, ...,
                                    progress = NULL) {

  force_lgl <- force_type("logical")

  res <- load_concepts(as_item(x), ..., progress = progress)
  res <- rm_na_val_var(res)

  res <- rm_cols(res, setdiff(data_vars(res), "val_var"), by_ref = TRUE)
  res <- set(res, j = "val_var", value = force_lgl(res[["val_var"]]))

  res <- stats::aggregate(x, res, aggregate)

  if (is.null(aggregate) || identical(aggregate, "any")) {
    # default aggregation corresponds to any()
    res <- set(res, j = "val_var", value = force_lgl(res[["val_var"]]))
  }

  res <- rename_cols(res, x[["name"]], "val_var", by_ref = TRUE)

  res
}

#' @rdname load_concepts
#' @export
load_concepts.rec_cncpt <- function(x, aggregate = NULL, patient_ids = NULL,
                                    id_type = "icustay", interval = hours(1L),
                                    ..., progress = NULL) {

  do_load_one <- function(x, nme, aggregate, extra, ..., progress) {
    do.call(load_one_concept_helper,
      c(list(x = x, name = nme, aggregate = aggregate), extra, list(...),
        list(progress = progress))
    )
  }

  ext <- list(patient_ids = patient_ids, id_type = id_type,
              interval = coalesce(x[["interval"]], interval),
              progress = progress)


  sub <- x[["items"]]
  agg <- x[["aggregate"]]

  agg <- Map(coalesce, rep_arg(aggregate, names(agg)), agg)
  agg <- agg[names(sub)]

  dat <- Map(do_load_one, sub, names(sub), agg, x[["extra"]], MoreArgs = ext)

  do_callback(x, dat, ..., interval = interval)
}

#' @param patient_ids Optional vector of patient ids to subset the fetched data
#' with
#' @param id_type String specifying the patient id type to return
#' @param interval The time interval used to discretize time stamps with,
#' specified as [base::difftime()] object
#'
#' @rdname load_concepts
#' @export
load_concepts.item <- function(x, patient_ids = NULL, id_type = "icustay",
                               interval = hours(1L), progress = NULL, ...) {

  load_one <- function(x, prog, ...) {

    progress_tick(progress_bar = prog)

    src <- src_name(x)
    res <- load_concepts(x, patient_ids[[src]], ...)

    if (mulit_src) {
      res <- add_src_col(res, src)
    }

    res
  }

  assert_that(has_length(x))

  srcs <- unique(src_name(x))
  mulit_src <- length(srcs) > 1L

  patient_ids <- split_patid(patient_ids, srcs)

  # slightly inefficient, as cols might get filled which were only relevant
  # during callback

  rbind_lst(
    lapply(x, load_one, progress, id_type, interval, ...),
    fill = TRUE
  )
}

#' @rdname load_concepts
#' @export
load_concepts.itm <- function(x, patient_ids = NULL, id_type = "icustay",
                              interval = hours(1L), ...) {

  warn_dots(..., ok_args = "keep_components")

  res <- do_itm_load(x, id_type, interval = interval)
  res <- merge_patid(res, patient_ids)

  do_callback(x, res)
}

#' @export
load_concepts.default <- function(x, ...) stop_generic(x, .Generic)

merge_patid <- function(x, patid) {

  if (is.null(patid)) {
    return(x)
  }

  id_col <- id_vars(x)

  if (!inherits(patid, "data.frame")) {
    assert_that(is.atomic(patid), length(patid) > 0L)
    patid <- setnames(setDT(list(unique(patid))), id_col)
  }

  if (is_id_tbl(patid)) {
    merge(x, patid, all = FALSE)
  } else {
    merge(x, patid, by = id_col, all = FALSE)
  }
}

add_src_col <- function(x, src) {
  x <- x[, c("source") := src]
  x <- set_id_vars(x, c("source", id_vars(x)))
  x
}

split_patid <- function(x, srcs) {

  if (is.null(x)) {

    x <- rep(list(NULL), length(srcs))
    names(x) <- srcs

  } else if (length(srcs) > 1L) {

    if (is_df(x)) {

      assert_that(has_col(x, "source"))

      if (is_dt(x)) {
        x <- split(x, by = "source")
      } else {
        x <- split(x, x[["source"]])
      }
    }

  } else {

    x <- list(x)
    names(x) <- srcs
  }

  assert_that(is.list(x), has_name(x, srcs))

  x
}
