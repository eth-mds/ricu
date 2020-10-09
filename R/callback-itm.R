
set_true <- function(x) rep(TRUE, length(x))

set_na <- function(x) rep(NA, length(x))

vent_flag <- function(x, val_var, ...) {
  x <- x[as.logical(get(val_var)), ]
  set(x, j = c(index_var(x), val_var),
      value = list(x[[val_var]], rep(TRUE, nrow(x))))
}

percent_as_numeric <- function(x) as.numeric(sub("%", "", x))

silent_as <- function(x, type) suppressWarnings(`storage.mode<-`(x, type))

silent_as_num <- function(x) silent_as(x, "numeric")

force_type <- function(type) {

  assert_that(is.string(type))

  function(x) {

    if (identical(typeof(x), type)) {
      return(x)
    }

    res <- silent_as(x, type)
    new_na <- sum(is.na(res) & !is.na(x))

    if (new_na > 0L) {
      msg_progress("lost {new_na} ({prcnt(new_na, length(x))}
                    ) entries due to coercion to {type}.")
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
#' @rdname callback_itm
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
#' @rdname callback_itm
#' @export
binary_op <- function(op, y) function(x) op(x, y)

#' @rdname callback_itm
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

  idx <- index_var(x)
  idc <- id_vars(x)

  tmp <- dt_gforce(x, "last", by = c(idc, sub_var), vars = c(idx, val_var))
  tmp <- tmp[, list(max(get(idx)), any(get(val_var) <= 40)), by = c(idc)]
  tmp <- rename_cols(tmp, c(idc, idx, val_var), by_ref = TRUE)

  as_ts_tbl(tmp, index_var = idx, interval = interval(x), by_ref = TRUE)
}

#' @param map Named atomic vector used for mapping a set of values (the names
#' of `map`) to a different set (the values of `map`)
#'
#' @rdname callback_itm
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
#' @rdname callback_itm
#' @export
convert_unit <- function(rgx, fun, new, ignore_case = TRUE, ...) {

  chr_to_fun <- function(a) {

    if (is.function(a)) return(a)

    a <- as.character(a)

    function(x) a
  }

  if (is.function(fun)) {
    fun <- list(fun)
  }

  new <- lapply(new, chr_to_fun)

  assert_that(all_fun(fun, is.function))

  len <- vctrs::vec_size_common(rgx, fun, new)
  xtr <- c(list(ignore.case = ignore_case), list(...))

  function(x, val_var, unit_var, ...) {

    for (i in seq_len(len)) {
      rows <- do.call(grep, c(list(rgx[[i]], x[[unit_var]]), xtr))
      vals <- list(fun[[i]](x[[val_var]][rows]), new[[i]](x[[unit_var]][rows]))
      set(x, i = rows, j = c(val_var, unit_var), value = vals)
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

weight_env <- new.env()

add_weight <- function(x, env, weight_var = "weight") {

  assert_that(is_id_tbl(x), is_src_env(env), is.string(weight_var))

  cache  <- paste(src_name(env), id_vars(x), sep = "_")
  weight <- get0(cache, envir = weight_env, inherits = FALSE)

  if (is.null(weight)) {
    weight <- load_concepts("weight", src_name(env), verbose = FALSE,
                            id_type = id_name_to_type(env, id_var(x)))
    assign(cache, weight, envir = weight_env)
  }

  if (is_in(weight_var, colnames(x))) {
    tmp_var <- new_names(c(colnames(x), colnames(weight)))
  } else {
    tmp_var <- weight_var
  }

  weight <- rename_cols(weight, tmp_var, "weight")
  res    <- merge(x, weight, by = id_var(x), all.x = TRUE)

  if (is_in(weight_var, colnames(x))) {
    res <- res[, c(weight_var) := silent_as_num(get(weight_var))]
    res <- res[, c(weight_var, tmp_var) := list(
      fifelse(is.na(get(weight_var)), get(tmp_var), get(weight_var)), NULL
    )]
  }

  res
}

eicu_extract_unit <- function(x) {

  res <- sub("^.+\\(", "", sub("\\)$", "", x))
  res[!nzchar(res)] <- NA_character_

  res
}

sub_trans <- function(regex, repl) {
  assert_that(is.string(regex), is.string(repl))
  function(x) sub(regex, repl, x, ignore.case = TRUE)
}

eicu_vaso_rate <- function(ml_to_mcg) {

  assert_that(is.count(ml_to_mcg))

  function(x, sub_var, val_var, weight_var, env, ...) {

    fix_units <- convert_unit(
      c("/hr$", "^mg/", "^units/", "^ml/", "^nanograms/", "^Unknown$", "^ml$"),
      c(binary_op(`/`, 60), binary_op(`*`, 1000), set_na,
        binary_op(`*`, ml_to_mcg), binary_op(`/`, 1000), set_na, set_na),
      c(sub_trans("/hr$", "/min"), sub_trans("^mg/", "mcg/"),
        "mcg/kg/min", sub_trans("^ml/", "mcg/"),
        sub_trans("^nanograms/", "mcg/"), "mcg/kg/min", "mcg/kg/min")
    )

    add_kg <- sub_trans("/", "/kg/")

    x <- x[, c(val_var) := silent_as_num(get(val_var))]
    x <- x[get(val_var) > 0, ]

    x <- add_weight(x, env, weight_var)
    x <- x[, c("unit_var") := eicu_extract_unit(get(sub_var))]

    x <- x[!grepl("/kg/", get("unit_var"), ignore.case = TRUE),
      c(val_var, "unit_var") := list(
        get(val_var) / get(weight_var), add_kg(get("unit_var"))
      )
    ]

    fix_units(x, val_var, "unit_var")
  }
}

hirid_vaso_rate <- function(x, val_var, unit_var, env, ...) {

  x <- x[get(val_var) > 0, ]
  x <- x[get(unit_var) == "mg",
    c(val_var, unit_var) := list(1000 * get(val_var), "\u00b5g")
  ]

  old_row <- nrow(x)
  x <- x[get(unit_var) == "\u00b5g", ]
  dif_row <- old_row - nrow(x)

  if (dif_row > 0) {
    msg_progress("lost {dif_row} ({prcnt(dif_row, old_row)}) entries due to
                  unexpected units")
  }

  x <- dt_gforce(x, "sum", vars = val_var)
  x <- add_weight(x, env, "weight")

  frac <- 1 / as.double(interval(x), units = "mins")

  x[, c(val_var, unit_var, "weight") := list(
    frac * get(val_var) / get("weight"), "mcg/kg/min", NULL)
  ]
}

mimic_dur_incv <- function(x, val_var, grp_var, ...) {
  calc_dur(x, val_var, index_var(x), index_var(x), grp_var)
}

mimic_dur_inmv <- function(x, val_var, grp_var, stop_var, ...) {
  calc_dur(x, val_var, index_var(x), stop_var, grp_var)
}

eicu_duration <- function(gap_length) {

  assert_that(is_interval(gap_length), is_scalar(gap_length))

  function(x, val_var, ...) {

    time_diff <- function(x) x - data.table::shift(x)

    grp_calc  <- function(x) {
      tmp <- rle(is_true(x <= gap_length))
      val <- tmp[["values"]]
      tmp[["values"]][val] <- seq_len(sum(val))
      inverse.rle(tmp)
    }

    id_vars   <- id_vars(x)
    index_var <- index_var(x)

    if (interval(x) > gap_length) {
      warn_ricu("splitting durations by gaps of length {format(gap_length)}
                 using data with a time resolution of {format(interval(x))}")
    }

    x <- x[, c("time_diff") := time_diff(get(index_var)), by = c(id_vars)]
    x <- x[, c("grp_var") := grp_calc(get("time_diff"))]

    calc_dur(x, val_var, index_var, index_var, "grp_var")
  }
}

hirid_duration <- function(x, val_var, grp_var, ...) {
  calc_dur(x, val_var, index_var(x), index_var(x), grp_var)
}

calc_dur <- function(x, val_var, min_var, max_var, grp_var) {

  id_vars   <- id_vars(x)
  index_var <- index_var(x)

  res <- x[, setNames(list(min(get(min_var)), max(get(max_var))),
                      c(index_var, val_var)), by = c(id_vars, grp_var)]
  res <- res[, c(val_var) := get(val_var) - get(index_var)]

  res
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

los_callback <- function(x, id_type, interval) {

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

  res
}
