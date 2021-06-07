
#' Sepsis 3 label
#'
#' The sepsis 3 label consists of a suspected infection combined with an acute
#' increase in SOFA score.
#'
#' @param ... Data objects
#' @param si_window Switch that can be used to filter SI windows
#' @param delta_fun Function used to determine the SOFA increase during an SI
#' window
#' @param sofa_thresh Required SOFA increase to trigger Sepsis 3
#' @param si_lwr,si_upr Lower/upper extent of SI windows
#' @param keep_components Logical flag indicating whether to return the
#' individual components alongside the aggregated score
#' @param interval Time series interval (only used for checking consistency
#' of input data)
#'
#' @details The Sepsis-3 Consensus (Singer et. al.) defines sepsis as an acute
#' increase in the SOFA score (see [sofa_score()]) of 2 points or more within
#' the suspected infection (SI) window (see [susp_inf()]):
#'
#' ```{tikz sofa-sep-3, echo = FALSE}
#' \begin{tikzpicture}
#'   \draw (-6,0) -- (3,0);
#'   \draw (-6,-0.25) -- (-6,0.25);
#'   \draw (3,-0.25) -- (3,0.25);
#'   \draw (0,-0.25) -- (0,0.25);
#'   \node[align = center] at (0,-0.75) {SI time};
#'   \node[align = center] at (3,-0.75) {SI window\\ end};
#'   \node[align = center] at (-6,-0.75) {SI window\\ start};
#'   \filldraw  (-6, 1) circle (2pt);
#'   \draw (-6,1) -- (-5.5, 1);
#'   \filldraw (-5.5, 1) circle (2pt);
#'   \draw (-5.5,1) -- (-5, 1);
#'   \node at (-4.25, 1) {$\dots$} ;
#'   \filldraw (-5, 1) circle (2pt);
#'   \filldraw (-3.5, 1) circle (2pt);
#'   \filldraw (-3, 1.5) circle (2pt);
#'   \filldraw (-2.5, 1.5) circle (2pt);
#'   \filldraw (-2, 2.5) circle (2pt);
#'   \draw (-3.5,1) -- (-3, 1.5);
#'   \draw (-3,1.5) -- (-2.5, 1.5);
#'   \draw (-2.5,1.5) -- (-2, 2.5);
#'   \node [black] at (-0.25, 1.75) {$\Delta$SOFA $\geq 2$};
#'   \draw (-6.5, 1) -- (-6.5, 2.5) ;
#'   \node at (-6.5, 3) {SOFA} ;
#'   \draw (-6.5,1)--(-6.6,1) node[left,font=\small]{$0$};
#'   \draw (-6.5,1.5)--(-6.6,1.5) node[left,font=\small]{$1$};
#'   \draw (-6.5,2)--(-6.6,2) node[left,font=\small]{$2$};
#'   \draw (-6.5,2.5)--(-6.6, 2.5) node[left,font=\small]{$3$};
#'   \draw[red] (-2,-0.25) -- (-2,0.25);
#'   \draw[dashed,red] (-2, 2.35) -- (-2, 0) ;
#'   \node[red] at (-2, -0.75) {Sepsis-3 time};
#' \end{tikzpicture}
#' ```
#'
#' A patient can potentially have multiple SI windows. The argument
#' `si_window` is used to control which SI window we focus on (options are
#' `"first", "last", "any"`).
#'
#' Further, although a 2 or more point increase in the SOFA score is defined,
#' it is not perfectly clear to which value the increase refers. For this the
#' `delta_fun` argument is used. If the increase is required to happen with
#' respect to the minimal SOFA value (within the SI window) up to the current
#' time, the `delta_cummin` function should be used. If, however, we are
#' looking for an increase with respect to the start of the SI window, then
#' the `delta_start` function should be used. Lastly, the increase might be
#' defined with respect to values of the previous 24 hours, in which case the
#' `delta_min` function is used.
#'
#' @references
#' Singer M, Deutschman CS, Seymour CW, et al. The Third International
#' Consensus Definitions for Sepsis and Septic Shock (Sepsis-3). JAMA.
#' 2016;315(8):801–810. doi:10.1001/jama.2016.0287
#'
#' @rdname label_sep3
#' @export
#'
sep3 <- function(..., si_window = c("first", "last", "any"),
                 delta_fun = delta_cummin, sofa_thresh = 2L,
                 si_lwr = hours(48L), si_upr = hours(24L),
                 keep_components = FALSE, interval = NULL) {

  cnc <- c("sofa", "susp_inf")
  res <- collect_dots(cnc, interval, ...)

  assert_that(is.count(sofa_thresh), is.function(delta_fun),
              is_interval(si_lwr), is_interval(si_upr),
              is.flag(keep_components))

  si_window <- match.arg(si_window)

  sofa <- res[["sofa"]]
  susp <- res[["susp_inf"]]

  id <- id_vars(sofa)
  ind <- index_var(sofa)

  sus_cols <- setdiff(data_vars(susp), "susp_inf")

  sofa <- sofa[, c("join_time1", "join_time2") := list(
    get(ind), get(ind)
  )]

  on.exit(rm_cols(sofa, c("join_time1", "join_time2"), by_ref = TRUE))

  susp <- susp[is_true(get("susp_inf")), ]
  susp <- susp[, c("susp_inf") := NULL]

  susp <- susp[, c("si_lwr", "si_upr") := list(
    get(index_var(susp)) - si_lwr,
    get(index_var(susp)) + si_upr
  )]

  if (si_window %in%  c("first", "last")) {
    susp <- dt_gforce(susp, si_window, id)
  }

  join_clause <- c(id, "join_time1 >= si_lwr", "join_time2 <= si_upr")

  res <- sofa[susp,
    c(list(delta_sofa = delta_fun(get("sofa"))), mget(c(ind, sus_cols))),
    on = join_clause, by = .EACHI, nomatch = NULL]

  res <- res[is_true(get("delta_sofa") >= sofa_thresh), ]

  cols_rm <- c("join_time1", "join_time2")

  if (!keep_components) {
    cols_rm <- c(cols_rm, "delta_sofa")
  }

  res <- rm_cols(res, cols_rm, by_ref = TRUE)
  res <- res[, head(.SD, n = 1L), by = c(id_vars(res))]
  res <- res[, c("sep3") := TRUE]

  res
}

#' @param x Vector of SOFA scores
#'
#' @rdname label_sep3
#' @export
#'
delta_cummin <- function(x) {
  x - cummin(ifelse(is.na(x), .Machine$integer.max, x))
}

#' @rdname label_sep3
#' @export
#'
delta_start <- function(x) x - x[!is.na(x)][1L]

#' @param shifts Vector of time shifts (multiples of the current interval) over
#' which [base::pmin()] is evaluated
#'
#' @rdname label_sep3
#' @export
#'
delta_min <- function(x, shifts = seq.int(0L, 23L)) {
  if (length(x) == 0L) x
  else {
    x - do.call(pmin.int, c(data.table::shift(x, shifts), list(na.rm = TRUE)))
  }
}

#' Suspicion of infection label
#'
#' Suspected infection is defined as co-occurrence of of antibiotic treatment
#' and body-fluid sampling.
#'
#' @param ... Data and further arguments are passed to `si_calc()`
#' @param abx_count_win Time span during which to apply the `abx_min_count`
#' criterion
#' @param abx_min_count Minimal number of antibiotic administrations
#' @param positive_cultures Logical flag indicating whether to require
#' cultures to be positive
#' @param si_mode Switch between `and`, `or`, `abx`, `samp` modes
#' @param abx_win Time-span within which sampling has to occur
#' @param samp_win Time-span within which antibiotic administration has to
#' occur
#' @param by_ref Logical flag indicating whether to process data by reference
#' @param keep_components Logical flag indicating whether to return the
#' individual components alongside the aggregated score
#' @param interval Time series interval (only used for checking consistency
#' of input data)
#'
#' @details Suspected infection can occur in one of the two following ways:
#' - administration of antibiotics followed by a culture sampling within
#'   `samp_win` hours
#'
#'    ```
#'           abx_win
#'       |---------------|
#'      ABX           sampling (last possible)
#'    ```
#'
#' - culture sampling followed by an antibiotic administration within
#'   `abx_win` hours
#'
#'    ```
#'                         samp_win
#'       |---------------------------------------------|
#'    sampling                                        ABX (last possible)
#'    ```
#'
#' The default values of `samp_win` and `abx_win` are 24 and 72 hours
#' respectively, as per [Singer et.al.
#' ](https://jamanetwork.com/journals/jama/fullarticle/2492881).
#'
#' The earlier of the two times (fluid sampling, antibiotic treatment) is taken
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
susp_inf <- function(..., abx_count_win = hours(24L), abx_min_count = 1L,
                     positive_cultures = FALSE,
                     si_mode = c("and", "or", "abx", "samp"),
                     abx_win = hours(24L), samp_win = hours(72L),
                     by_ref = TRUE, keep_components = FALSE, interval = NULL) {

  rename_si <- function(x) {
    rename_cols(x, "susp_inf", data_vars(x), by_ref = TRUE)
  }

  si_mode <- match.arg(si_mode)

  assert_that(is.count(abx_min_count), is.flag(positive_cultures),
              is_interval(abx_count_win), is_interval(abx_win),
              is_interval(samp_win), is.flag(by_ref),
              is.flag(keep_components))

  cnc <- c("abx", "samp")
  res <- collect_dots(cnc, interval, ...)

  if (positive_cultures) {
    samp_fun <- "sum"
  } else {
    samp_fun <- quote(list(samp = .N))
  }

  if (!isTRUE(by_ref)) {
    res <- lapply(res, copy)
  }

  cmbn_fun <- switch(si_mode,
    and = si_and,
    or = si_or,
    abx = function(abx, samp, ...) rename_si(abx),
    samp = function(abx, samp, ...) rename_si(samp)
  )

  cmbn_fun(
    si_abx(res[["abx"]], abx_count_win, abx_min_count),
    si_samp(aggregate(res[["samp"]], samp_fun)),
    abx_win, samp_win, keep_components
  )
}

si_abx <- function(x, count_win, min_count) {

  if (min_count > 1L) {

    x <- slide(x, list(abx = sum(get("abx"), na.rm = TRUE)),
               before = hours(0L), after = count_win)
  }

  set(x, j = "abx", value = x[["abx"]] >= min_count)
}

si_samp <- function(x) {
  set(x, j = "samp", value = x[["samp"]] > 0L)
}

si_and <- function(abx, samp, abx_win, samp_win, keep) {

  assert_that(has_rows(abx), has_rows(samp), msg = "
    calling `susp_inf()` with `si_mode = and` requires data from both `abx`
    and `samp` concepts"
  )

  do_roll <- function(x, y, win) {

    met_y <- meta_vars(y)

    if (keep) {

      y[x, c(met_y, "samp_time", "abx_time"), with = FALSE, roll = -win,
        nomatch = NULL, on = paste(met_y, meta_vars(x), sep = " == ")]

    } else {

      y[x, met_y, with = FALSE, roll = -win, nomatch = NULL,
        on = paste(met_y, meta_vars(x), sep = " == ")]
    }
  }

  if (keep) {

    samp_idx <- index_var(samp)
    abx_idx  <- index_var(abx)

    samp <- samp[, c("samp_time") := get(samp_idx)]
    abx <-  abx[,  c("abx_time")  := get(abx_idx)]
  }

  res <- rbind(do_roll(abx, samp, abx_win),
               do_roll(samp, abx, samp_win))

  if (keep) {

    rmv <- duplicated(res, by = meta_vars(res))

    if (any(rmv)) {
      msg_progress("removing {sum(rmv)} duplicate si events")
      res <- res[!rmv, ]
    }

  } else {

    res <- unique(res)
  }

  res <- res[, c("susp_inf") := TRUE]

  res
}

si_or <- function(abx, samp, abx_win, samp_win, keep) {

  if (keep) {

    samp_idx <- index_var(samp)
    abx_idx  <- index_var(abx)

    samp <- samp[, c("samp_time") := get(samp_idx)]
    abx <-  abx[,  c("abx_time")  := get(abx_idx)]
  }

  res <- merge(abx, samp, all = TRUE)
  res <- res[get("abx") | get("samp"), ]
  res <- rm_cols(res, c("abx", "samp"))
  res <- res[, c("susp_inf") := TRUE]

  res
}
