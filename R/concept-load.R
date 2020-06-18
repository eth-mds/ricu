
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

    out <- NULL
    con <- textConnection("out", "w", local = TRUE)
    sink(con, type = "message")

    on.exit({
      sink(type = "message")
      close(con)
    })

    dat <- withCallingHandlers(
      load_cncpt(x, ...),
      error = function(e) sink(type = "message")
    )

    sink(type = "message")
    close(con)
    on.exit()

    if (verbose && is.null(pb) && has_length(out)) {
      message(paste0(out, collapse = "\n"))
      out <- NULL
    }

    if (!isFALSE(agg)) {
      dat <- make_unique(dat, agg)
    }

    list(dat, out)
  }

  assert_that(is.flag(merge_data), same_src(x), is.flag(verbose))

  len <- length(x)

  if (verbose && interactive()) {
    pba <- progr_init(len, paste("Loading", len, "concepts"))
  } else {
    pba <- NULL
  }

  res <- Map(load_one, x, rep_arg(aggregate, names(x)),
             MoreArgs = c(list(pba), list(...)))

  if (verbose && not_null(pba)) {

    out <- lst_xtr(res, 2L)
    out <- out[lengths(out) > 0L]

    if (has_length(out)) {

      out <- lapply(out, paste0, collapse = "\n")

      message("Successfully loaded ", len, " concepts, but encountered some ",
        "issues:\n", paste0("  * ", names(out), ":\n", out, collapse = "\n"))
    }
  }

  res <- lst_xtr(res, 1L)

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

  rbind_lst(res, fill = TRUE)
}

#' @inheritParams load_concepts
#' @rdname item_utils
#' @keywords internal
#' @export
load_cncpt <- function(x, ...) UseMethod("load_cncpt", x)

#' @export
load_cncpt.cncpt <- function(x, ...) {

  res <- load_concepts(x[["items"]], ...)
  res <- rm_na(res)

  rename_cols(res, x[["name"]], data_vars(res), by_ref = TRUE)
}

#' @export
load_cncpt.num_cncpt <- function(x, ...) {

  print_msg <- function(...) {
    message(paste(strwrap(paste0(...), indent = 4L, exdent = 6L),
                  collapse = "\n"))
  }

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

      if (!all(ok)) {
        print_msg("not all units are in ", concat("[", unt, "]"), ": ",
                  concat(nm[!ok], " (", pct[!ok], ")"))
      }

    } else if (length(nm) > 1L) {

      print_msg("multiple units detected: ", concat(nm, " (", pct, ")"))
    }
  }

  res <- load_concepts(x[["items"]], ...)

  keep <- check_bound(res, x[["min"]], `>=`) &
          check_bound(res, x[["max"]], `<=`)

  if (!all(keep)) {

    n_row <- nrow(res)

    res <- res[keep, ]

    n_rm <- n_row - nrow(res)

    print_msg("removed ", n_rm, " (", prcnt(n_rm, n_row), ") of rows ",
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

  rename_cols(res, x[["name"]], "val_var", by_ref = TRUE)
}

#' @export
load_cncpt.fct_cncpt <- function(x, ...) {

  lvl <- x[["levels"]]

  res <- load_concepts(x[["items"]], ...)

  if (is.character(lvl)) {
    keep <- res[["val_var"]] %chin% lvl
  } else {
    keep <- res[["val_var"]] %in% lvl
  }

  if (!all(keep)) {

    n_row <- nrow(res)

    res <- res[keep, ]

    n_rm <- n_row - nrow(res)

    message("    removed ", n_rm, " (", prcnt(n_rm, n_row), ") of rows ",
            "due to out of spec entries")
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

#' @export
load_itm.sel_itm <- function(x, patient_ids, id_type, interval) {
  load_sub_itm(x, patient_ids, id_type, interval)
}

#' @export
load_itm.rgx_itm <- function(x, patient_ids, id_type, interval) {
  load_sub_itm(x, patient_ids, id_type, interval)
}

#' @export
load_itm.hrd_itm <- function(x, patient_ids, id_type, interval) {

  itm <- x[["itm_vars"]]
  cbc <- x[["cb_vars"]]
  sub <- x[["sub_var"]]

  adu <- !"unit_var" %in% names(itm)
  env <- as_src_env(x)

  if (adu) {

    res <- do_itm_load(x, c(itm, sub, cbc), patient_ids, id_type, interval)
    unt <- load_src("variables", env, cols = c("id", "unit"))
    res <- merge(res, unt, by.x = sub, by.y = "id", all.x = TRUE)

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
  x <- do.call(cbf, arg)

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

#' @export
load_itm.los_itm <- function(x, patient_ids, id_type, interval) {

  as_day <- function(x) as.double(x, units = "days")

  win <- x[["win_type"]]
  cfg <- as_id_cfg(x)

  if (identical(win, id_type)) {

    res <- id_map_min(cfg, id_vars(cfg[id_type]), id_vars(cfg[win]),
                      NULL, "end")

    res <- res[, c("val_var", "end") := list(as_day(get("end")), NULL)]

  } else {

    res <- id_map_min(cfg, id_vars(cfg[id_type]), id_vars(cfg[win]),
                      "start", "end")

    res <- res[, c("val_var", "start", "end") := list(
      as_day(get("end") - get("start")), NULL, NULL
    )]

    res <- rm_cols(res, id_vars(cfg[win]), by_ref = TRUE)

    if (cfg[win] > cfg[id_type]) {
      res <- unique(res)
    }
  }

  res
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
