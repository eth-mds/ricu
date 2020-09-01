
set_true <- function(x) rep(TRUE, length(x))

set_na <- function(x) rep(NA, length(x))

vent_flag <- function(x, val_var, ...) {
  x <- x[as.logical(get(val_var)), ]
  set(x, j = c(index_var(x), val_var),
      value = list(x[[val_var]], rep(TRUE, nrow(x))))
}

percent_as_numeric <- function(x) as.numeric(sub("%", "", x))

#' @importFrom methods as
force_type <- function(type) {
  assert_that(is.string(type))
  function(x) {
    res <- suppressWarnings(as(x, type))
    new_na <- sum(is.na(res) & !is.na(x))
    if (new_na > 0L) {
      msg_progress("  lost ", new_na, " (", prcnt(new_na, length(x)),
                   ") entries due to coercion to ", type, ".")
    }
    res
  }
}

eicu_body_weight <- function(x, val_var, weight_var, env, ...) {

  do_calc <- function(rate, w1, w2) rate / fifelse(is.na(w1), w2, w1)

  idc <- id_vars(x)

  weight <- load_id("patient", env, cols = "admissionweight", id_var = idc)

  x <- merge(x, weight, all.x = TRUE, by = idc)

  force_num <- force_type("numeric")

  x <- set(x, j = val_var, value = force_num(x[[val_var]]))
  x <- set(x, j = weight_var, value = force_num(x[[weight_var]]))

  x <- x[, c(val_var) := do_calc(get(val_var), get(weight_var),
                                 get("admissionweight"))]

  rm_cols(x, "admissionweight")
}

combine_date_time <- function(x, date_var, date_shift = hours(12L), ...) {
  idx <- index_var(x)
  x[, c(idx) := fifelse(is.na(get(idx)),
                        get(date_var) + date_shift, get(idx))]
}

shift_all_date <- function(x, shift = hours(12L), ...) {
  idx <- index_var(x)
  x[, c(idx) := get(idx) + shift]
}

mimic_abx_shift_flag <- function(x, val_var, ...) {
  transform_fun(set_true)(shift_all_date(x, hours(12L)), val_var)
}

mimic_sampling <- function(x, val_var, aux_time, ...) {
  x <- combine_date_time(x, aux_time, hours(12L))
  set(x, j = val_var, value = !is.na(x[[val_var]]))
}

#' Item callback utilities
#'
#' For concept loading, item callback functions are used in order to handle
#' item-specific post-processing steps, such as converting measurement units,
#' mapping a set of values to another or for more involved data
#' transformations, like turning absolute drug administration rates into rates
#' that are relative to body weight. Item callback functions are called by
#' [load_concepts()] with arguments `x` (the data), a variable number of name/
#' string pairs specifying roles of columns for the given item, followed by
#' `env`, the data source environment as [`src_env`][new_src_env()] object.
#' Item callback functions can be specified by their name or using function
#' factories such as `transform_fun()`, `apply_map()` or `convert_unit()`.
#'
#' @details
#' The most forward setting is where a function is simply referred to by its
#' name. For example in eICU, age is available as character vector due to
#' ages 90 and above being represented by the string "> 89". A function such
#' as the following turns this into a numeric vector, replacing occurrences of
#' "> 89" by the number 90.
#'
#' ```
#' eicu_age <- function(x, val_var, ...) {
#'   data.table::set(
#'     data.table::set(x, which(x[[val_var]] == "> 89"), j = val_var,
#'                     value = 90),
#'     j = val_var,
#'     value = as.numeric(x[[val_var]])
#'   )
#' }
#' ```
#'
#' This function then is specified as item callback function for items
#' corresponding to eICU data sources of the `age` concept as
#'
#' ```
#' item(src = "eicu_demo", table = "patient", val_var = "age",
#'      callback = "eicu_age", class = "col_itm")
#' ```
#'
#' The string passed as `callback` argument is evaluated, meaning that an
#' expression can be passed which evaluates to a function that in turn can be
#' used as callback. Several function factories are provided which return
#' functions suitable for use as item callbacks: `transform_fun()` creates a
#' function that transforms the `val_var` column using the function supplied
#' as `fun` argument, `apply_map()` can be used to map one set of values to
#' another (again using the `val_var` column) and `convert_unit()` is intended
#' for converting a subset of rows (identified by matching `rgx` against the
#' `unit_var` column) by applying `fun` to the `val_var` column and setting
#' `new` as the transformed unit name (arguments are not limited to scalar
#' values). As transformations require unary functions, two utility function,
#' `binary_op()` and `comp_na()` are provided which can be used to fix the
#' second argument of binary functions such as `*` or `==`. Taking all this
#' together, an item callback function for dividing the `val_var` column by 2
#' could be specified as `"transform_fun(binary_op(`/`, 2))"`. The supplied
#' function factories create functions that operate on the data using
#' [by-reference semantics][data.table::set()]. Furthermore, during concept
#' loading, progress is reported by a [progress::progress_bar]. In order to
#' signal a message without disrupting the current loading status, see
#' [msg_progress()].
#'
#' @param fun Function to be used for data transformation
#' @param ... Further arguments passed to downstream function
#'
#' @examples
#' dat <- ts_tbl(x = rep(1:2, each = 5), y = hours(rep(1:5, 2)), z = 1:10)
#'
#' subtract_3 <- transform_fun(binary_op(`-`, 3))
#' subtract_3(data.table::copy(dat), val_var = "z")
#'
#' gte_4 <- transform_fun(comp_na(`>=`, 4))
#' gte_4(data.table::copy(dat), val_var = "z")
#'
#' map_letters <- apply_map(setNames(letters[1:9], 1:9))
#' res <- map_letters(data.table::copy(dat), val_var = "z")
#' res
#'
#' not_b <- transform_fun(comp_na(`!=`, "b"))
#' not_b(res, val_var = "z")
#'
#' @rdname callback_utils
#' @export
transform_fun <- function(fun, ...) {

  assert_that(is.function(fun))

  dots <- list(...)

  function(x, val_var, ...) {
    set(x, j = val_var, value = do.call(fun, c(list(x[[val_var]]), dots)))
    x
  }
}

fahr_to_cels <- function(x) (x - 32) * 5 / 9

#' @param op Function taking two arguments, such as `+`
#' @param y Value passed as second argument to function `op`
#'
#' @rdname callback_utils
#' @export
binary_op <- function(op, y) function(x) op(x, y)

#' @rdname callback_utils
#' @export
comp_na <- function(op, y) function(x) !is.na(x) & op(x, y)

distribute_amount <- function(x, val_var, unit_var, end_var, ...) {

  hr  <- `units<-`(hours(1L), time_unit(x))
  idx <- index_var(x)

  x <- x[get(end_var) - get(idx) >= 0, ]
  x <- x[get(end_var) - get(idx) == 0, c(end_var) := get(idx) + hr]
  x <- x[, c(val_var) := get(val_var) / as.numeric(get(end_var) - get(idx)) *
                            as.numeric(hr)]

  res <- expand(x, idx, end_var, keep_vars = c(id_vars(x), val_var))
  res <- res[, c(unit_var) := "units/hr"]

  res
}

aggregate_fun <- function(fun, new_unit) {

  assert_that(is.string(fun), is.string(new_unit))

  function(x, val_var, unit_var, ...) {
    res <- dt_gforce(set(x, j = unit_var, value = NULL), fun)
    res <- res[, c(unit_var) := new_unit]
    res
  }
}

aggregate_amount <- function(x, val_var, unit_var, ...) {
  res <- dt_gforce(set(x, j = unit_var, value = NULL), "sum")
  res <- res[, c(unit_var) := "units"]
  res
}

mimic_age <- function(x, val_var, ...) {
  x <- set(x, j = val_var,
    value = as.double(`units<-`(x[[val_var]], "days") / -365))
  x <- set(x, i = which(x[[val_var]] > 90), j = val_var, value = 90)
  x
}

eicu_age <- function(x, val_var, ...) {
  x <- set(x, which(x[[val_var]] == "> 89"), j = val_var, value = 90)
  x <- set(x, j = val_var, value = as.numeric(x[[val_var]]))
  x
}

hirid_death <- function(x, val_var, sub_var, ...) {

  threshold <- function(x, col, thresh) {
    set(x, j = col, value = x[[col]] <= thresh)
  }

  score <- function(x, id, val) x[, data.table::last(get(val)), by = c(id)]

  idc <- id_vars(x)

  tmp <- split(x, by = sub_var, keep.by = FALSE)
  tmp <- lapply(tmp, threshold, val_var, 40)
  tmp <- lapply(tmp, score, idc, val_var)
  res <- rbind_lst(tmp)
  res <- res[, any(get("V1")), by = idc]

  rename_cols(res, val_var, "V1", by_ref = TRUE)
}

#' @param map Named atomic vector used for mapping a set of values (the names
#' of `map`) to a different set (the values of `map`)
#'
#' @rdname callback_utils
#' @export
apply_map <- function(map) {

  assert_that(is.atomic(map), !is.null(names(map)), has_length(map))

  function(x, val_var, ...) {
    set(x, j = val_var, value = map[x[[val_var]]])
    x
  }
}

#' @param rgx Regular expression(s) used for identifying observations based on
#' their current unit of measurement
#' @param fun Function(s) used for transforming matching values
#' @param new Name(s) of transformed units
#' @param ignore_case Forwarded to [base::grep()]
#'
#' @rdname callback_utils
#' @export
convert_unit <- function(rgx, fun, new, ignore_case = TRUE, ...) {

  if (is.function(fun)) {
    fun <- list(fun)
  }

  assert_that(all(lgl_ply(fun, is.function)))

  len <- vctrs::vec_size_common(rgx, fun, new)
  xtr <- c(list(ignore.case = ignore_case), list(...))

  function(x, val_var, unit_var, ...) {

    for (i in seq_len(len)) {
      set(x, i = do.call(grep, c(list(rgx[[i]], x[[unit_var]]), xtr)),
          j = c(val_var, unit_var),
          value = list(fun[[i]](x[[val_var]]), new[[i]]))
    }

    x
  }
}

eicu_adx <- function(x, val_var, ...) {

  path <- strsplit(x[[val_var]], split = "|", fixed = TRUE)
  keep <- chr_ply(path, `[[`, 2L) == "All Diagnosis"

  x <- x[keep, ]
  path <- path[keep]

  cats <- fifelse(
    chr_ply(path, `[[`, 5L) %chin% c("Genitourinary", "Transplant"),
    "other",
    fifelse(chr_ply(path, `[[`, 3L) == "Operative", "surg", "med")
  )

  set(x, j = val_var, value = cats)
}

hirid_vaso <- function(x, val_var, unit_var, env, ...) {

  ids <- id_vars(x)

  x <- x[get(val_var) > 0, ]
  x <- x[get(unit_var) == "mg",
    c(val_var, unit_var) := list(1000 * get(val_var), "\u00b5g")
  ]

  old_row <- nrow(x)
  x <- x[get(unit_var) == "\u00b5g", ]
  dif_row <- old_row - nrow(x)

  if (dif_row > 0) {
    msg_progress("    lost ", dif_row, " (", prcnt(dif_row, old_row),
                 ") entries due to unexpected units")
  }

  sex <- load_id("general", env, cols = "sex", id_var = ids)
  sex <- sex[,
    c("weight", "sex") := list(fifelse(get("sex") == "M", 85, 65), NULL)
  ]

  x <- merge(dt_gforce(x, "sum", vars = val_var), sex, by = ids)

  frac <- 1 / as.double(interval(x), units = "mins")

  x[, c(val_var, unit_var, "weight") := list(
    frac * get(val_var) / get("weight"), "mcg/kg/min", NULL)
  ]
}

hirid_urine <- function(x, val_var, unit_var, ...) {

  do_diff <- function(x) {
    res <- c(x[1L], diff(x))
    res[res < 0] <- 0
    res
  }

  idx <- id_vars(x)

  x[, c(val_var, unit_var) := list(do_diff(get(val_var)), "mL"), by = idx]
}

los_callback <- function(x, patient_ids, id_type, interval) {

  as_day <- function(x) as.double(x, units = "days")

  win <- x[["win_type"]]
  cfg <- as_id_cfg(x)

  if (identical(win, id_type)) {

    res <- id_map(x, id_vars(cfg[id_type]), id_vars(cfg[win]), NULL, "end")

    res <- res[, c("val_var", "end") := list(as_day(get("end")), NULL)]

  } else {

    res <- id_map(x, id_vars(cfg[id_type]), id_vars(cfg[win]), "start", "end")

    res <- res[, c("val_var", "start", "end") := list(
      as_day(get("end") - get("start")), NULL, NULL
    )]

    res <- rm_cols(res, id_vars(cfg[win]), by_ref = TRUE)

    if (cfg[win] > cfg[id_type]) {
      res <- unique(res)
    }
  }

  merge_patid(res, patient_ids)
}
