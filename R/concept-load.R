
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
#'
load_concepts <- function(x, ...) UseMethod("load_concepts", x)

#' @param src A character vector, used to subset the `concepts`; `NULL`
#' means no subsetting
#' @param concepts The concepts to be used or `NULL` in which case
#' [read_dictionary()] is called
#'
#' @rdname load_concepts
#' @export
#'
load_concepts.character <- function(x, src = NULL, concepts = NULL, ...) {

  get_src <- function(x) x[names(x) == src]

  if (is.null(concepts)) {

    assert_that(not_null(src))

    x <- read_dictionary(src, x)

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
#'
load_concepts.concept <- function(x, aggregate = NULL, merge_data = TRUE,
                                  verbose = TRUE, ...) {

  load_one <- function(x, agg, pb, ...) {

    if (verbose) {
      progr_iter(x[["name"]], pb)
    }

    dat <- load_cncpt(x, verbose, ...)

    if (isFALSE(agg)) {
      return(dat)
    }

    make_unique(dat, agg)
  }

  assert_that(is.flag(merge_data), same_src(x), is.flag(verbose))

  len <- length(x)

  if (verbose) {
    pba <- progr_init(len, paste("Loading", len, "concepts"))
  } else {
    pba <- NULL
  }

  res <- Map(load_one, x, rep_arg(aggregate, names(x)),
             MoreArgs = c(list(pba), list(...)))

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

#' @param patient_ids Optional vector of patient ids to subset the fetched data
#' with
#' @param id_type String specifying the patient id type to return
#' @param interval The time interval used to discretize time stamps with,
#' specified as [base::difftime()] object
#'
#' @rdname load_concepts
#' @export
load_concepts.item <- function(x, patient_ids = NULL, id_type = "icustay",
                               interval = hours(1L), ...) {

  warn_dots(...)

  if (length(x) == 0L) {
    res <- setNames(list(integer(), numeric()), c(id_type, "val_var"))
    return(as_id_tbl(res, id_vars = id_type, by_ref = TRUE))
  }

  res <- lapply(x, load_itm, patient_ids, id_type, interval)

  assert_that(all_fun(Map(inherits, res, chr_xtr(x, "targ")), isTRUE))

  rbind_lst(res)
}

#' @inheritParams load_concepts
#' @rdname item_utils
#' @keywords internal
#' @export
load_cncpt <- function(x, verbose, ...) UseMethod("load_cncpt", x)

#' @export
load_cncpt.cncpt <- function(x, verbose, ...) {

  res <- load_concepts(x[["items"]], ...)
  res <- rm_na(res)

  rename_cols(res, x[["name"]], data_vars(res), by_ref = TRUE)
}

#' @export
load_cncpt.num_cncpt <- function(x, verbose, ...) {

  check_bound <- function(x, val, op) {
    vc  <- x[["val_var"]]
    nna <- !is.na(vc)
    if (is.null(val)) nna else nna & op(vc, val)
  }

  report_unit <- function(x, unt) {

    ct  <- table(x[["unit_var"]], useNA = "ifany")
    nm  <- names(ct)
    rep <- paste0(nm, " (", prcnt(ct), ")", collapse = ", ")

    if (sum(!is.na(nm)) > 1L) {
      msg <- paste0("multiple units detected: ", rep)
    } else if (!identical(tolower(unt), tolower(nm[!is.na(nm)]))) {
      msg <- paste0("not all units are in [", unt, "]: ", rep)
    } else msg <- NULL

    if (not_null(msg)) {
      message(paste(strwrap(msg, indent = 4L, exdent = 6L), collapse = "\n"))
    }
  }

  res <- load_concepts(x[["items"]], ...)

  keep <- check_bound(res, x[["min"]], `>=`) &
          check_bound(res, x[["max"]], `<=`)

  if (!all(keep)) {

    if (verbose) n_row <- nrow(res)

    res <- res[keep, ]

    if (verbose) {
      n_rm <- n_row - nrow(res)
      message("    removed ", n_rm, " (", prcnt(n_rm, n_row), ") of rows ",
              "due to out of spec entries")
    }
  }

  unit <- x[["unit"]]

  if (verbose && has_name(res, "unit_var")) {
    report_unit(res, unit)
  }

  res <- rm_cols(res, "unit_var", skip_absent = TRUE, by_ref = TRUE)

  if (not_null(unit)) {
    setattr(res[["val_var"]], "units", unit)
  }

  rename_cols(res, x[["name"]], "val_var", by_ref = TRUE)
}

#' @export
load_cncpt.fct_cncpt <- function(x, verbose, ...) {

  lvl <- x[["levels"]]

  res <- load_concepts(x[["items"]], ...)

  if (is.character(lvl)) {
    keep <- res[["val_var"]] %chin% lvl
  } else {
    keep <- res[["val_var"]] %in% lvl
  }

  if (!all(keep)) {

    if (verbose) n_row <- nrow(res)

    res <- res[keep, ]

    if (verbose) {
      n_rm <- n_row - nrow(res)
      message("    removed ", n_rm, " (", prcnt(n_rm, n_row), ") of rows ",
              "due to out of spec entries")
    }
  }

  rename_cols(res, x[["name"]], "val_var", by_ref = TRUE)
}

#' @rdname item_utils
#' @keywords internal
#' @export
load_itm <- function(x, patient_ids, id_type, interval) {
  UseMethod("load_itm", x)
}

#' @export
load_itm.col_itm <- function(x, patient_ids, id_type, interval) {

  tbl <- as_src_tbl(x)
  id  <- id_vars(as_id_cfg(tbl)[id_type])

  itm <- x[["itm_vars"]]
  cbc <- x[["cb_vars"]]

  if (need_idx(x)) {
    res <- load_ts(tbl, cols = c(itm, cbc), id_var = id,
                   index_var = x[["index_var"]], interval = interval)
  } else {
    res <- load_id(tbl, cols = c(itm, cbc), id_var = id, interval = interval)
  }

  res <- merge_patid(res, patient_ids)
  res <- do.call(x[["callback"]], c(list(res), as.list(c(itm, cbc)),
                                    list(env = as_src_env(tbl))))

  res <- rm_cols(res, setdiff(data_vars(res), itm), by_ref = TRUE)
  res <- rename_cols(res, names(itm), itm, by_ref = TRUE)

  res
}

#' @export
load_itm.sel_itm <- function(x, patient_ids, id_type, interval) {
  load_sub_itm(x, patient_ids, id_type, interval)
}

#' @export
load_itm.rgx_itm <- function(x, patient_ids, id_type, interval) {
  load_sub_itm(x, patient_ids, id_type, interval)
}

load_sub_itm <- function(x, patient_ids, id_type, interval) {

  qry <- prepare_query(x)
  tbl <- as_src_tbl(x)
  id  <- id_vars(as_id_cfg(tbl)[id_type])

  itm <- x[["itm_vars"]]
  cbc <- x[["cb_vars"]]

  if (need_idx(x)) {
    res <- load_ts(tbl, !!qry, c(itm, cbc), id, x[["index_var"]], interval)
  } else {
    res <- load_id(tbl, !!qry, c(itm, cbc), id, interval)
  }

  res <- merge_patid(res, patient_ids)
  res <- do.call(x[["callback"]], c(list(res), as.list(c(itm, cbc)),
                                    list(env = as_src_env(tbl))))

  res <- rm_cols(res, setdiff(data_vars(res), itm), by_ref = TRUE)
  res <- rename_cols(res, names(itm), itm, by_ref = TRUE)

  res
}

#' @export
load_itm.los_itm <- function(x, patient_ids, id_type, interval) {

  win <- x[["win_type"]]
  cfg <- as_id_cfg(x)
  res <- stay_windows(cfg, id_type = id_type, win_type = win,
                      in_time = NULL, interval = mins(1L))

  if (!identical(win, id_type)) {
    res <- rm_cols(res, id_vars(cfg[id_type]), by_ref = TRUE)
  }

  res <- merge_patid(res, patient_ids)
  res <- rename_cols(res, "val_var", data_vars(res), by_ref = TRUE)

  set(res, j = "val_var", value = as.double(res[["val_var"]], units = "days"))
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

#' @param concepts Character vector (or NULL, meaning everything) of concept
#' names
#' @param source Corresponds to `src`
#' @param dictionary Corresponds to `concepts`
#'
#' @rdname load_concepts
#' @export
#'
load_dictionary <- function(source = NULL, concepts = NULL,
                            dictionary = read_dictionary(source), ...) {

  message("`load_dictionary()` is deprecated, please use `load_concepts()` ",
          "instead.")

  load_concepts(concepts, source, dictionary, ...)
}

rep_arg <- function(arg, names) {

  if (length(arg) <= 1L) {
    arg <- rep(list(arg), length(names))
  }

  if (is.null(names(arg))) {
    names(arg) <- names
  } else {
    arg <- arg[names]
  }

  assert_that(identical(names(arg), names))

  arg
}
