
#' @export
si_data <- function(source, abx_count_win = hours(24L), abx_min_count = 1L,
                    positive_cultures = FALSE, interval = hours(1L),
                    patient_ids = NULL, col_cfg = get_col_config(source),
                    dictionary = get_config("concept-dict")) {

  assert_that(is.count(abx_min_count), is.flag(positive_cultures))

  funs <- c(antibiotics = "sum",
            fluid_sampling = if (positive_cultures) "sum" else "nrow")
  dict <- get_concepts(source, names(funs), dictionary)

  dat <- load_concepts(source, dict, patient_ids, col_cfg, funs,
                       interval, merge_data = FALSE)

  dat[["antibiotics"]] <- si_abx(dat[["antibiotics"]], abx_count_win,
                                 abx_min_count)

  dat[["fluid_sampling"]] <- data.table::set(
    dat[["fluid_sampling"]],
    j = "fluid_sampling",
    value = dat[["fluid_sampling"]][["fluid_sampling"]] > 0L
  )

  merge(dat[["antibiotics"]], dat[["fluid_sampling"]], all = TRUE)
}

si_abx <- function(x, count_win, min_count) {

  if (min_count > 1L) {

    assert_that(is_time(count_win, allow_neg = FALSE))

    x <- slide_quo(x, quote(list(antibiotics = sum(antibiotics))),
                   before = count_win)
  }

  x <- data.table::set(x, j = "antibiotics",
                       value = x[["antibiotics"]] >= min_count)

  x
}
