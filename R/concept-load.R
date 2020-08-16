
#' Load concept data
#'
#' Data specified as `concept` objects is loaded, aggregated per combination
#' of id and time-step and merged into wide format.
#'
#' @param x Object specifying the data to be loaded
#' @param ... Passed to downstream methods
#'
#' @rdname load_concepts
#' @export
load_concepts <- function(x, ...) UseMethod("load_concepts", x)

#' @param src A character vector, used to subset the `concepts`; `NULL`
#' means no subsetting
#' @param concepts The concepts to be used or `NULL` in which case
#' [load_dictionary()] is called
#' @param dict_name,dict_file In case not concepts are passed as `concepts`,
#' these are forwarded to [load_dictionary()] as `name` and `file` arguments
#'
#' @rdname load_concepts
#' @export
load_concepts.character <- function(x, src = NULL, concepts = NULL, ...,
                                    dict_name = "concept-dict",
                                    dict_file = NULL) {

  get_src <- function(x) x[names(x) == src]

  if (is.null(concepts)) {

    assert_that(not_null(src))

    x <- load_dictionary(src, x, name = dict_name, file = dict_file)

  } else {

    assert_that(is_concept(concepts), length(x) > 0L)

    x <- concepts[x]

    if (not_null(src)) {

      assert_that(is.string(src))

      x <- new_concept(
        Map(`[[<-`, x, "items", lapply(lst_xtr(x, "items"), get_src))
      )
    }
  }

  load_concepts(x, ...)
}

#' @param aggregate Controls how data within concepts is aggregated
#' @param merge_data Logical flag, specifying whether to merge concepts into
#' wide format or return a list, each entry corresponding to a concept
#' @param verbose Logical flag for muting informational output
#'
#' @rdname load_concepts
#' @export
load_concepts.concept <- function(x, aggregate = NULL, merge_data = TRUE,
                                  verbose = TRUE, ...) {

  assert_that(is.flag(merge_data), is.flag(verbose))

  srcs <- unlst(src_name(x), recursive = TRUE)

  assert_that(
    all_fun(srcs, identical, srcs[1L]), msg = "
    Only concept data from a single data source can be loaded a the time.
    Please choose one of {unique(srcs)}"
  )

  if (verbose) {
    pba <- progress_init(n_tick(x), "Loading {length(x)} concept{?s}")
  } else {
    pba <- FALSE
  }

  res <- with_progress(
    Map(load_one_concept_helper, x, rep_arg(aggregate, names(x)),
        MoreArgs = c(list(...), list(progress = pba))),
    progress_bar = pba
  )

  assert_that(all_fun(Map(inherits, res, chr_ply(x, target_class)), isTRUE))

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

load_one_concept_helper <- function(x, aggregate, ..., progress) {

  progress_tick(x[["name"]], progress, 0L)

  res <- load_concepts(x, aggregate, ..., progress = progress)

  assert_that(has_name(res, x[["name"]]))

  progress_tick(progress_bar = progress)

  res
}

#' @param progress Either `NULL`, or a progress bar object as created by
#' [progress::progress_bar]
#'
#' @rdname load_concepts
#' @export
load_concepts.cncpt <- function(x, aggregate = NULL, ..., progress = NULL) {

  res <- load_concepts(x[["items"]], ..., progress = progress)
  res <- rm_na(res)

  res <- rename_cols(res, x[["name"]], data_vars(res), by_ref = TRUE)

  stats::aggregate(x, res, aggregate)
}

#' @rdname load_concepts
#' @export
load_concepts.num_cncpt <- function(x, aggregate = NULL, ...,
                                    progress = NULL) {

  check_bound <- function(x, val, op) {
    vc  <- x[["val_var"]]
    nna <- !is.na(vc)
    if (is.null(val)) nna else nna & op(vc, val)
  }

  report_unit <- function(x, unt) {

    ct  <- table(x[["unit_var"]], useNA = "ifany")
    nm  <- names(ct)
    pct <- prcnt(ct)

    if (has_length(unt)) {

      ok <- tolower(nm) %in% tolower(unt)

      if (all(ok)) {
        return(NULL)
      }

      progress_msg("not all units are in ", concat("[", unt, "]"), ": ",
                   concat(nm[!ok], " (", pct[!ok], ")"))

    } else if (length(nm) > 1L) {

      progress_msg("multiple units detected: ", concat(nm, " (", pct, ")"))
    }
  }

  res <- load_concepts(x[["items"]], ..., progress = progress)

  keep <- check_bound(res, x[["min"]], `>=`) &
          check_bound(res, x[["max"]], `<=`)

  if (!all(keep)) {

    n_row <- nrow(res)

    res <- res[keep, ]

    n_rm <- n_row - nrow(res)

    progress_msg("removed ", n_rm, " (", prcnt(n_rm, n_row), ") of rows ",
                 "due to out of range entries")
  }

  unit <- x[["unit"]]

  if (has_name(res, "unit_var")) {
    report_unit(res, unit)
  }

  res <- rm_cols(res, "unit_var", skip_absent = TRUE, by_ref = TRUE)

  if (not_null(unit)) {
    setattr(res[["val_var"]], "units", unit[1L])
  }

  res <- rename_cols(res, x[["name"]], "val_var", by_ref = TRUE)

  stats::aggregate(x, res, aggregate)
}

#' @rdname load_concepts
#' @export
load_concepts.fct_cncpt <- function(x, aggregate = NULL, ...,
                                    progress = NULL) {

  lvl <- x[["levels"]]

  res <- load_concepts(x[["items"]], ..., progress = progress)

  if (is.character(lvl)) {
    keep <- res[["val_var"]] %chin% lvl
  } else {
    keep <- res[["val_var"]] %in% lvl
  }

  if (!all(keep)) {

    n_row <- nrow(res)

    res <- res[keep, ]

    n_rm <- n_row - nrow(res)

    progress_msg("  removed ", n_rm, " (", prcnt(n_rm, n_row), ") of rows ",
                 "due to level mismatch")
  }

  res <- rename_cols(res, x[["name"]], "val_var", by_ref = TRUE)

  stats::aggregate(x, res, aggregate)
}

#' @rdname load_concepts
#' @export
load_concepts.rec_cncpt <- function(x, aggregate = NULL, patient_ids = NULL,
                                    id_type = "icustay", interval = hours(1L),
                                    ..., progress = NULL) {

  ext <- list(patient_ids = patient_ids, id_type = id_type,
              interval = coalesce(x[["interval"]], interval),
              progress = progress)

  agg <- x[["aggregate"]]
  agg <- Map(coalesce, rep_arg(aggregate, names(agg)), agg)
  agg <- agg[names(x[["items"]])]

  dat <- Map(load_one_concept_helper, x[["items"]], agg, MoreArgs = ext)
  dat <- do.call(x[["callback"]], c(dat, list(...), list(interval = interval)))

  assert_that(inherits(dat, target_class(x)))

  dat
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

    res <- load_concepts(x, ...)

    assert_that(inherits(res, target_class(x)))

    res
  }

  warn_dots(...)

  if (length(x) == 0L) {

    if (need_idx(x)) {
      res <- setNames(list(integer(), interval[-1L], numeric()),
                      c("id_var", "index_var", "val_var"))
      res <- as_ts_tbl(res, interval = interval, by_ref = TRUE)
    } else {
      res <- setNames(list(integer(), numeric()), c("id_var", "val_var"))
      res <- as_id_tbl(res, by_ref = TRUE)
    }

    return(res)
  }

  rbind_lst(
    lapply(x, load_one, progress, patient_ids, id_type, interval),
    fill = TRUE
  )
}

#' @rdname load_concepts
#' @export
load_concepts.col_itm <- function(x, patient_ids = NULL, id_type = "icustay",
                                  interval = hours(1L), ...) {

  itm <- x[["itm_vars"]]
  cbc <- x[["cb_vars"]]

  res <- do_itm_load(x, c(itm, cbc), patient_ids, id_type, interval)

  if (!"unit_var" %in% names(itm)) {

    unt <- x[["unit_val"]]

    if (not_null(unt)) {
      res[, c("unit") := unt]
      itm <- c(itm, unit_var = "unit")
    }
  }

  itm_cleanup(res, itm, cbc, as_src_env(x), x[["callback"]])
}

#' @rdname load_concepts
#' @export
load_concepts.sel_itm <- function(x, patient_ids = NULL, id_type = "icustay",
                                  interval = hours(1L), ...) {

  warn_dots(...)
  load_sub_itm(x, patient_ids, id_type, interval)
}

#' @rdname load_concepts
#' @export
load_concepts.rgx_itm <- function(x, patient_ids = NULL, id_type = "icustay",
                                  interval = hours(1L), ...) {

  warn_dots(...)
  load_sub_itm(x, patient_ids, id_type, interval)
}

#' @rdname load_concepts
#' @export
load_concepts.hrd_itm <- function(x, patient_ids = NULL, id_type = "icustay",
                                  interval = hours(1L), ...) {

  warn_dots(...)

  itm <- x[["itm_vars"]]
  cbc <- x[["cb_vars"]]
  sub <- x[["sub_var"]]

  adu <- !"unit_var" %in% names(itm)
  env <- as_src_env(x)

  if (adu) {

    res <- do_itm_load(x, c(itm, sub, cbc), patient_ids, id_type, interval)
    unt <- load_src("variables", env, cols = c("id", "unit"))
    res <- merge(res, unt[!is.na(get("unit")), ], by.x = sub, by.y = "id",
                 all.x = TRUE)

    if (!sub %in% cbc) {
      res <- rm_cols(res, sub, by_ref = TRUE)
    }

    itm <- c(itm, unit_var = "unit")

  } else {
    res <- do_itm_load(x, c(itm, cbc), patient_ids, id_type, interval)
  }

  itm_cleanup(res, itm, cbc, env, x[["callback"]])
}

itm_cleanup <- function(x, itm, cbc, env, cbf) {

  arg <- c(list(x), as.list(c(itm, cbc)), list(env = env))
  fun <- eval(parse(text = cbf))

  assert_that(is.function(fun))

  x <- do.call(fun, arg)

  x <- rm_cols(x, setdiff(data_vars(x), itm), by_ref = TRUE)
  x <- rename_cols(x, names(itm), itm, by_ref = TRUE)

  x
}

do_itm_load <- function(x, cols, patient_ids, id_type, interval) {

  tbl <- as_src_tbl(x)
  qry <- prepare_query(x)
  id  <- id_vars(as_id_cfg(tbl)[id_type])

  if (need_idx(x)) {
    res <- load_ts(tbl, !!qry, cols, id, x[["index_var"]], interval)
  } else {
    res <- load_id(tbl, !!qry, cols, id, interval)
  }

  merge_patid(res, patient_ids)
}

load_sub_itm <- function(x, patient_ids, id_type, interval) {

  itm <- x[["itm_vars"]]
  cbc <- x[["cb_vars"]]

  res <- do_itm_load(x, c(itm, cbc), patient_ids, id_type, interval)

  itm_cleanup(res, itm, cbc, as_src_env(x), x[["callback"]])
}

#' @rdname load_concepts
#' @export
load_concepts.fun_itm <- function(x, patient_ids = NULL, id_type = "icustay",
                                  interval = hours(1L), ...) {

  warn_dots(...)

  args <- list(x = x, patient_ids = patient_ids, id_type = id_type,
               interval = interval)

  do.call(x[["callback"]], args)
}

merge_patid <- function(x, patid) {

  if (is.null(patid)) {
    return(x)
  }

  id_col <- id_vars(x)

  if (!inherits(patid, "data.frame")) {
    assert_that(is.atomic(patid), length(patid) > 0L)
    patid <- setnames(setDT(list(unique(patid))), id_col)
  }

  merge(x, patid, by = id_col, all = FALSE)
}
