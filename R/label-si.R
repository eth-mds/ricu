
#' Suspicion of infection label
#'
#' Suspected infection is defined as co-occurrence of of antibiotic treatment
#' and body-fluid sampling.
#'
#' @param source String valued name of data source
#' @param abx_count_win Time span during which to apply the `abx_min_count`
#' criterion
#' @param abx_min_count Minimal number of antibiotic administrations
#' @param positive_cultures Logical flag indicating whether to require
#' cultures to be positive
#' @param dictionary `dictionary` object to use for concept lookup
#' @param ... Passed to [load_concepts()]
#'
#' @details Suspected infection can occur in one of the two following ways:
#' - administration of antibiotics followed by a culture sampling within
#'   `samp_win` hours
#'
#'    ```
#'           samp_win
#'       |---------------|
#'      ABX           sampling (last possible)
#'    ```
#'
#' - culture sampling followed by an antibiotic administration within
#'   `abx_win` hours
#'
#'    ```
#'                          abx_win
#'       |---------------------------------------------|
#'    sampling                                        ABX (last possible)
#'    ```
#'
#' The default values of `samp_win` and `abx_win` are 24 and 72 hours
#' respectively, as per [Singer et.al.
#' ](https://jamanetwork.com/journals/jama/fullarticle/2492881).
#'
#' The ealier of the two times (fluid sampling, antibiotic treatment) is taken
#' as the time of suspected infection (SI time). The suspected infection
#' window (SI window) is defined to start `si_lwr` hours before the SI time
#' and end `si_upr` hours after the SI time. The default values of 48 and 24
#' hours (respectively) are chosen as used by [Seymour et.al.
#' ](https://jamanetwork.com/journals/jama/fullarticle/2492875) (see
#' Supplemental Material).
#'
#' ```
#'                 48h                       24h
#'   |------------------------------(|)---------------|
#'                                 SI time
#' ```
#'
#' For some datasets, however, information on body fluid sampling is not
#' available for majority of the patients (eICU data). Therefore, an
#' alternative definition of suspected infection is required. For this, we use
#' administration of multiple antibiotics (argument `abx_min_count` determines
#' the required number) within `abx_count_win` hours. The first time of
#' antibiotic administration is taken as the SI time in this case.
#'
#' @references
#' Singer M, Deutschman CS, Seymour CW, et al. The Third International
#' Consensus Definitions for Sepsis and Septic Shock (Sepsis-3). JAMA.
#' 2016;315(8):801–810. doi:10.1001/jama.2016.0287
#'
#' Seymour CW, Liu VX, Iwashyna TJ, et al. Assessment of Clinical Criteria for
#' Sepsis: For the Third International Consensus Definitions for Sepsis and
#' Septic Shock (Sepsis-3). JAMA. 2016;315(8):762–774.
#' doi:10.1001/jama.2016.0288
#'
#' @rdname label_si
#' @export
#'
si_data <- function(source, abx_count_win = hours(24L), abx_min_count = 1L,
                    positive_cultures = FALSE,
                    dictionary = read_dictionary(source), ...) {

  assert_that(is.count(abx_min_count), is.flag(positive_cultures))

  if (positive_cultures) {
    samp_fun <- "sum"
  } else {
    samp_fun <- quote(list(fluid_sampling = .N))
  }

  funs <- c(antibiotics = "sum", fluid_sampling = samp_fun)
  dictionary <- dictionary[names(funs)]

  dat <- load_concepts(dictionary, aggregate = funs, merge_data = FALSE, ...)

  if (has_name(dat, "antibiotics")) {
    dat[["antibiotics"]] <- si_abx(dat[["antibiotics"]], abx_count_win,
                                   abx_min_count)
  }

  if (has_name(dat, "fluid_sampling")) {
    dat[["fluid_sampling"]] <- si_samp(dat[["fluid_sampling"]])
  }

  res <- reduce(merge, dat, all = TRUE)

  if ("antibiotics" %in% data_cols(res)) {
    res <- rename_cols(res, "abx", "antibiotics")
  } else {
    res <- res[, c("abx") := NA]
  }

  if ("fluid_sampling" %in% data_cols(res)) {
    res <- rename_cols(res, "samp", "fluid_sampling")
  } else {
    res <- res[, c("samp") := NA]
  }

  res
}

si_abx <- function(x, count_win, min_count) {

  if (min_count > 1L) {

    assert_that(is_time(count_win, allow_neg = FALSE))

    query <- quote(list(antibiotics = sum(antibiotics, na.rm = TRUE)))

    x <- slide_quo(x, query, before = hours(0L), after = count_win)
  }

  set(x, j = "antibiotics", value = x[["antibiotics"]] >= min_count)
}

si_samp <- function(x) {
  set(x, j = "fluid_sampling", value = x[["fluid_sampling"]] > 0L)
}

#' @param tbl `ts_tbl` object to use for label computations
#' @param si_mode Switch between `and`/`or` modes
#' @param abx_win Time-span within which sampling has to occur
#' @param samp_win Time-span within which antibiotic administration has to
#' occur
#' @param si_lwr,si_upr Lower/upper extent of SI windows
#'
#' @rdname label_si
#' @export
#'
si_windows <- function(tbl, si_mode = c("and", "or"), abx_win = hours(24L),
                       samp_win = hours(72L), si_lwr = hours(48L),
                       si_upr = hours(24L)) {

  span_win <- function(x, col, win) {
    x <- x[, c("win_end", "time_copy") := list(get(ind) + win, get(ind))]
    x[is_true(get(col)), ]
  }

  min_fun <- function(x) if (length(x) == 0L) x else min(x)

  si_mode <- match.arg(si_mode)

  win_args <- list(abx_win = abx_win, samp_win = samp_win, si_lwr = si_lwr,
                   si_upr = si_upr)

  assert_that(all(lgl_ply(win_args, is_time, allow_neg = FALSE)),
              has_name(tbl, c("abx", "samp")))

  win_args <- lapply(win_args, `units<-`, time_unit(tbl))
  list2env(win_args, environment())

  id  <- id(tbl)
  ind <- index(tbl)

  if (identical(si_mode, "and")) {

    dat <- Map(span_win, unmerge(tbl), c("abx", "samp"),
               win_args[c("abx_win", "samp_win")])

    join_clause <- c(id, paste(c("win_end >=", "time_copy <="), ind))

    abx_samp <- dat[[1L]][dat[[2L]], list(si_time = min_fun(get(ind))),
                          on = join_clause, by = .EACHI, nomatch = NULL]
    samp_abx <- dat[[2L]][dat[[1L]], list(si_time = min_fun(get(ind))),
                          on = join_clause, by = .EACHI, nomatch = NULL]

    res <- unique(rbind(abx_samp[, c(id, "si_time"), with = FALSE],
                        samp_abx[, c(id, "si_time"), with = FALSE]))

    res <- as_ts_tbl(res, id = id, index = "si_time", interval = interval(tbl))

  } else {

    res <- tbl[get("abx") | get("samp"), ]
    res <- rm_cols(res, data_cols(res))
    res <- rename_cols(res, "si_time", ind)
  }

  res <- res[, c("si_lwr", "si_upr") := list(
    get("si_time") - win_args[["si_lwr"]],
    get("si_time") + win_args[["si_upr"]]
  )]

  res
}

#' @rdname label_si
#' @export
#'
si <- function(source, ...) {

  args <- list(...)

  assert_that(!has_name(args, "tbl"))

  win_args <- names(args)[names(args) %in% names(formals(si_windows))]
  dat_args <- setdiff(names(args), win_args)

  dat <- do.call(si_data, c(list(source), args[dat_args]))
  dat <- do.call(si_windows, c(list(dat), args[win_args]))

  dat
}
