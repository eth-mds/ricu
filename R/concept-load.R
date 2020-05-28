
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
#' @param concepts The concepts to be used
#'
#' @rdname load_concepts
#' @export
#'
load_concepts.character <- function(x, src = NULL,
                                    concepts = read_dictionary(src), ...) {

  get_src <- function(x) x[names(x) == src]

  assert_that(is_concept(concepts), length(x) > 0L)

  x <- concepts[x]

  if (not_null(src)) {
    assert_that(is.string(src))
    field(x, "items") <- lapply(field(x, "items"), get_src)
  }

  load_concepts(x, ...)
}

#' @param aggregate Controls how data within concepts is aggregated
#' @param na_rm Logical flag, indicating whether to remove rows `NA` data in
#' individual concepts
#' @param merge_data Logical flag, specifying whether to merge concepts into
#' wide format or return a list, each entry corresponding to a concept
#'
#' @rdname load_concepts
#' @export
#'
load_concepts.concept <- function(x, aggregate = NA_character_, na_rm = TRUE,
                                  merge_data = TRUE, verbose = TRUE,
                                  patient_ids = NULL, ...) {

  load_one <- function(x, pb, ...) {
    if (verbose) progr_iter(names(x), pb)
    load_concepts(as_item(x), target_class = field(x, "class"), ...)
  }

  do_one <- function(x, agg, progress_bar, narm, ...) {

    browser()

    progr_iter(x[["name"]], progress_bar)

    x <- as_concept(x)

    res <- rbind_lst(
      do.call(load_concepts, c(list(as_item(x)), concept_meta(x), list(...)))
    )

    if (narm) {
      res <- rm_na(res)
    }

    if (isFALSE(agg)) {
      res
    } else {
      make_unique(res, fun = agg)
    }
  }

  assert_that(is.flag(merge_data), is.flag(na_rm), same_src(x))

  len <- length(x)

  if (verbose) {
    pba <- progr_init(len, paste("Loading", len, "concepts"))
  } else {
    pba <- NULL
  }

  dat <- lapply(x, load_one, pba, ...)

  aggregate <- rep_arg(aggregate, names(x))

  len <- length(x)
  pba <- progr_init(len, paste("Loading ", len, " concepts"))

  res <- Map(do_one, x, aggregate[names(x)],
             MoreArgs = c(list(progress_bar = pba, narm = na_rm), list(...)))

  if (!merge_data) {
    return(res)
  }

  if (length(res) > 1L) {

    ind <- c(which(lgl_ply(res, is_ts_tbl)), which(lgl_ply(res, is_id_tbl)))
    res <- reduce(merge, res[ind], all = TRUE)
    res <- setcolorder(res, c(meta_cols(res), names(x)))

  } else if (length(res) == 1L) {

    res <- res[[1L]]
  }

  res
}

#' @rdname load_concepts
#' @export
load_concepts.item <- function(x, target_class = c("ts_tbl", "id_tbl"),
                               id_type = "icustay", interval = hours(1L),
                               ...) {

  warn_dots(...)

  ma <- list(target = match.arg(target_class), id_type = id_type,
             interval = interval)

  rbind_lst(
    Map(load_itm, field(x, "itm"), src = field(x, "src"), MoreArgs = ma)
  )
}

#' @export
load_itm <- function(x, src, target, id_type, interval) {

  assert_that(is.string(src), is.string(target),
              target %in% c("ts_tbl", "id_tbl"))

  UseMethod("load_itm", x)
}

#' @export
load_itm.sel_itm <- function(x, src, target, id_type, interval) {

  qry <- prepare_query(x)
  tbl <- as_src_tbl(x[["table"]], src)
  col <- col_args(x, tbl, id_type)

  xtr <- unlist(col[setdiff(names(col), c("id_col", "index_col"))])
  id  <- col[["id_col"]]
  ind <- col[["index_col"]]

  if (identical(target, "id_tbl")) {
    res <- load_id(tbl, !!qry, xtr, id, interval)
  } else {
    res <- load_ts(tbl, !!qry, xtr, id, ind, interval)
  }

  cb_fun <- x[["callback"]]

  if (not_null(cb_fun)) {
    assert_that(is_fun_name(cb_fun))
    res <- do.call(cb_fun, c(list(res), col, list(env = as_src_env(tbl))))
  }

  dtc <- col[c("val_col", "unit_col")]

  res <- rm_cols(res, setdiff(data_cols(res), dtc))
  res <- rename_cols(res, )

  browser()
}

col_args <- function(x, tbl, id_type) {

  id_col <- get_id_col(tbl, id_type)
  df_col <- get_default_cols(tbl, c("index", "val", "unit"))
  xt_col <- get_extra_cols(x)

  res <- c(list(id_col = id_col), xt_col, df_col)

  res[!duplicated(names(res))]
}

#' @param unit Character vector of units (one per data item)
#' @param min,max Rage of plausible values
#' @param id_type String specifying the patient id type to return
#' @param patient_ids Optional vector of patient ids to subset the fetched data
#' with
#' @param verbose Logical flag for turning off reporting of unit/out of range
#' messages
#' @param interval The time interval used to discretize time stamps with,
#' specified as [base::difftime()] object
#'
#' @rdname load_concepts
#' @export
#'
load_concepts_item <- function(x, unit = NULL, min = NULL, max = NULL,
                               id_type = "icustay", patient_ids = NULL,
                               interval = hours(1L), verbose = TRUE, ...) {

  do_one <- function(x, unt, mi, ma) {

    fun <- get(x[["load_fun"]], mode = "function")
    res <- fun(as_item(x), id_type, patient_ids, interval)

    assert_that(is_icu_tbl(res))

    uco <- x[["unit_col"]]
    tbl <- x[["table"]]

    if (is.null(uco) && not_null(tbl)) {
      uco <- default_col(as_src_tbl(tbl, x[["source"]]), "unit")
    }

    res <- range_check(res, setdiff(data_cols(res), uco), mi, ma, verbose)
    res <- unit_check(res, uco, unt, verbose)

    res <- rename_cols(res, x[["concept"]], data_cols(res))
    res <- add_unit(res, unt)

    res
  }

  assert_that(is.flag(verbose))

  warn_dots(...)

  if (not_null(patient_ids)) {

    src <- unique(chr_xtr(x, "source"))

    assert_that(length(src) == 1L, msg = paste0(
      "Cannot subset patient IDs for multiple data sources at the same time")
    )

    idc <- get_id_col(as_src_tbl(x[[1L]][["table"]], src), "id_type")

    if (inherits(patient_ids, "data.frame")) {
      assert_that(has_name(patient_ids, idc))
      patient_ids <- patient_ids[[idc]]
    }

    assert_that(is.atomic(patient_ids), length(patient_ids) > 0L)
    patient_ids <- setnames(setDT(list(unique(patient_ids))), idc)
  }

  names <- names(x)

  Map(do_one, x, rep_arg(unit, names), rep_arg(min, names),
      rep_arg(max, names))
}

#' @param concepts Character vector (or NULL, meaning everything) of concept
#' names
#'
#' @rdname load_concepts
#' @export
#'
load_dictionary <- function(source = NULL, concepts = NULL,
                            dictionary = read_dictionary(), ...) {

  message("`load_dictionary()` is deprecated, please use `load_concepts()` ",
          "instead.")

  load_concepts(concepts, source, dictionary, ...)
}

load_default <- function(item, id_type, patient_ids, interval) {

  item <- item[[1L]]
  tble <- item[["table"]]

  assert_that(is.string(tble))

  tbl <- as_src_tbl(tble, item[["source"]])

  id_col <- get_id_col(tbl, id_type)
  ex_col <- get_default_cols(tbl, c("index", "val", "unit"))

  item <- item[setdiff(names(item),
                       c("concept", "source", "table", "load_fun"))]

  load_args <- c(
    list(tbl = tbl), item[setdiff(names(item), "callback")],
    list(id_col = id_col), ex_col, list(interval = interval)
  )

  res <- do.call(do_load, load_args[!duplicated(names(load_args))])

  if (not_null(patient_ids)) {
    res <- merge(res, patient_ids, by = id_col, all = FALSE)
  }

  cb_args <- c(
    list(x = res), item[setdiff(names(item), "regex")],
    list(id_col = id_col), ex_col, list(env = as_src_env(tbl))
  )

  do.call(do_callback, cb_args[!duplicated(names(cb_args))])
}

do_load <- function(tbl, column, ids, regex, ..., id_col, index_col,
                    val_col, unit_col, interval) {

  extra_cols <- list(...)

  ids  <- unique(ids)
  cols <- c(column, unit_col, unique(unlist(extra_cols)))

  if (is.null(cols)) {
    cols <- character(0L)
  }

  if (length(ids) == 0L) {

    query <- NULL

  } else {

    cols <- c(val_col, cols)
    col  <- as.name(column)

    if (isTRUE(regex)) {
      query <- substitute(grepl(id, col, ignore.case = TRUE),
        list(col = col, id = paste(ids, collapse = "|"))
      )
    } else if (length(ids) == 1L) {
      query <- substitute(is_fun(col, id),
        list(col = col, id = ids, is_fun = is_val)
      )
    } else if (is.character(ids)) {
      query <- substitute(col %chin% id, list(col = col, id = ids))
    } else {
      query <- substitute(col %in% id, list(col = col, id = ids))
    }
  }

  load_auto(tbl, query, cols, id_col, index_col, interval)
}

load_auto <- function(tbl, query, cols, id, index, ival) {

  if (is.null(query)) {
    if (is.null(index)) {
      load_id(tbl, cols = cols, id_col = id, interval = ival)
    } else {
      load_ts(tbl, cols = cols, id_col = id, index_col = index,
              interval = ival)
    }
  } else {
    if (is.null(index)) {
      load_id(tbl, !!query, cols, id, ival)
    } else {
      load_ts(tbl, !!query, cols, id, index, ival)
    }
  }
}

do_callback <- function(x, column, ids, callback, ..., id_col,
                        index_col, val_col, unit_col, env) {

  extra_cols <- list(...)

  if (length(extra_cols) && is.null(callback)) {
    warning("`extra_cols` is only effective if `callback` is specified.")
  }

  if (length(ids) > 0L) {
    val <- val_col
  } else {
    val <- column
  }

  if (is.string(callback)) {
    callback <- get0(callback, mode = "function")
  }

  if (is.function(callback)) {

    args <- c(
      list(x), list(id_col = id_col, index_col = index_col, val_col = val,
                    item_col = column, unit_col = unit_col),
      extra_cols, list(env = env)
    )

    x <- do.call(callback, args)
  }

  if (is_id_tbl(x)) {
    assert_that(identical(id(x), id_col))
  } else if (is_ts_tbl(x)) {
    assert_that(identical(meta_cols(x), c(id_col, index_col)))
  } else {
    stop("expecting an `id_tbl` or `ts_tbl` object")
  }

  x <- rm_cols(x, setdiff(colnames(x), c(id_col, index_col, val, unit_col)))

  x
}

unit_check <- function(x, unit_col, unit, verb) {

  if (verb && !(is.null(unit_col) || is.na(unit_col))) {

    unt <- unique(x[[unit_col]])
    unt <- unt[!is.na(unt)]
    unt <- unique(tolower(unt))

    if (null_or_na(unit)) {
      if (length(unt) > 1L) {
        ct <- table(x[[unit_col]], useNA = "ifany")
        msg <- paste0(names(ct), " (", formatC(ct / sum(ct) * 100, digits = 3),
                      "%)", collapse = ", ")
        msg <- paste0("multiple units detected: ", msg)
        message(paste(strwrap(msg, indent = 4L, exdent = 6L), collapse = "\n"))
      }
    } else {
      if (!identical(tolower(unit), unt)) {
        ct <- table(x[[unit_col]], useNA = "ifany")
        msg <- paste0(names(ct), " (", formatC(ct / sum(ct) * 100, digits = 3),
                      "%)", collapse = ", ")
        msg <- paste0("not all units are in ", unit, ": ", msg)
        message(paste(strwrap(msg, indent = 4L, exdent = 6L), collapse = "\n"))
      }
    }
  }

  if (!(is.null(unit_col) || is.na(unit_col))) {
    x <- rm_cols(x, unit_col)
  }

  x
}

add_unit <- function(x, unit) {

  if (null_or_na(unit)) {
    return(x)
  }

  col <- data_cols(x)

  assert_that(is.string(unit), is.string(col))

  setattr(x[[col]], "units", unit)

  x
}

range_check <- function(x, val_col, min, max, verb) {

  if (is.null(min) && is.null(max)) {
    return(x)
  }

  old_nrow <- nrow(x)

  if (is.null(min)) {

    min_ind <- TRUE

  } else {

    assert_that(is.number(min))

    min_ind <- x[[val_col]] >= min
  }

  if (is.null(max)) {

    max_ind <- TRUE

  } else {

    assert_that(is.number(max))

    max_ind <- x[[val_col]] <= max
  }

  x <- x[min_ind & max_ind, ]

  new_nrow <- nrow(x)

  if (verb && new_nrow != old_nrow) {
    message("   removed ", old_nrow - new_nrow,
            " rows based on range specification")
  }

  x
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
