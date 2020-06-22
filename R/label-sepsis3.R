
#' Sepsis 3 label
#'
#' The sepsis 3 label consists of a suspected infection combined with an acute
#' increase in SOFA score.
#'
#' @param sofa_score `ts_tbl` with a column `sofa_score`
#' @param susp_inf `ts_tbl` with columns `si_lwr` and `si_upr` defining windows for
#' which a suspected infection is valid
#' @param si_window Switch that can be used to filter SI windows
#' @param delta_fun Function used to determine the SOFA increase during an SI
#' window
#' @param sofa_thresh Required SOFA increase to trigger Sepsis 3
#' @param interval Time series interval (only used for checking consistency
#' of input data)
#'
#' @details The Sepsis-3 Consensus ([Singer et. al. 2016.](https://jamanetwork.com/journals/jama/fullarticle/2492881)) defined sepsis as an acute increase in the SOFA score (see [sofa_score()]) of &gt; 2 points within the suspected infection (SI) window (see [si_windows()]):
#'
#' \figure{sep-3.png}
#'
#' A patient can potentially have multiple SI windows. The argument `si_window` is used to control which SI window we focus on (options are `"first", "last", "any"`).
#'
#' Further, although a 2 or more point increase in the SOFA score is defined, it is not perfectly clear to which value the increase refers. For this the `delta_fun` argument is used. If the increase is required to happen with respect to the minimal SOFA value (within the SI window) up to the current time, the `delta_cummin` function should be used. If, however, we are looking for an increase with respect to the start of the SI window, then the `delta_start` function should be used. Lastly, the increase might be defined with respect to values of the previous 24 hours, in which case the `delta_min` function is used.
#'
#' @seealso [Sepsis-3 Consensus, Singer et. al.](https://jamanetwork.com/journals/jama/fullarticle/2492881).
#'
#' @rdname sepsis_3
#' @export
#'
sepsis_3 <- function(sofa_score, susp_inf,
                     si_window = c("first", "last", "any"),
                     delta_fun = delta_cummin, sofa_thresh = 2L,
                     interval = ricu::interval(sofa_score)) {

  assert_that(has_interval(sofa_score, interval),
              has_interval(susp_inf, interval), is.count(sofa_thresh))

  si_window <- match.arg(si_window)

  id <- id_vars(sofa_score)
  ind <- index_var(sofa_score)

  sofa_score <- sofa_score[, c("join_time1", "join_time2") := list(
    get(ind), get(ind)
  )]

  on.exit(rm_cols(sofa_score, c("join_time1", "join_time2"), by_ref = TRUE))

  join_clause <- c(id, "join_time1 >= si_lwr", "join_time2 <= si_upr")

  if (si_window %in%  c("first", "last")) {
    susp_inf <- dt_gforce(susp_inf, si_window, id)
  }

  res <- sofa_score[susp_inf,
    c(list(delta_sofa = delta_fun(get("sofa_score"))), mget(ind)),
    on = join_clause, by = .EACHI, nomatch = NULL]

  res <- res[is_true(get("delta_sofa") >= sofa_thresh), ]

  res <- rm_cols(res, c("join_time1", "join_time2", "delta_sofa"),
                 by_ref = TRUE)
  res <- rename_cols(res, "sepsis_3", index_var(res), by_ref = TRUE)

  res
}

#' @param x Vector of SOFA scores
#'
#' @rdname sepsis_3
#' @export
#'
delta_cummin <- function(x) {
  x - cummin(ifelse(is.na(x), .Machine$integer.max, x))
}

#' @rdname sepsis_3
#' @export
#'
delta_start <- function(x) x - x[!is.na(x)][1L]

#' @param shifts Vector of time shifts (multiples of the current interval) over
#' which [base::pmin()] is evaluated
#'
#' @rdname sepsis_3
#' @export
#'
delta_min <- function(x, shifts = seq.int(0L, 23L)) {
  if (length(x) == 0L) x
  else {
    x - do.call(pmin.int, c(data.table::shift(x, shifts), list(na.rm = TRUE)))
  }
}

