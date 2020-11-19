
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

combine_date_time <- function(x, time_var, date_shift = hours(12L), ...) {
  idx <- index_var(x)
  x[, c(idx) := fifelse(is.na(get(time_var)),
                        get(idx) + date_shift, get(time_var))]
}

shift_all_date <- function(x, shift = hours(12L), ...) {
  idx <- index_var(x)
  x[, c(idx) := get(idx) + shift]
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
#' @return Callback function factories such as `transform_fun()`, `apply_map()`
#' or `convert_unit()` return functions suitable as item callback functions,
#' while transform function generators such as `binary_op()`, `comp_na()`
#' return functions that apply a transformation to a vector.
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

#' @param val Value to replace every element of x with
#'
#' @rdname callback_itm
#' @export
set_val <- function(val) function(x) rep(val, length(x))

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

mimic_age <- function(x) {
  x <- as.double(x, units = "days") / -365
  ifelse(x > 90, 90, x)
}

eicu_age <- function(x) as.numeric(ifelse(x == "> 89", 90, x))

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

#' @param fun Function(s) used for transforming matching values
#' @param new Name(s) of transformed units
#' @param rgx Regular expression(s) used for identifying observations based on
#' their current unit of measurement, `NULL` means everything
#' @param ignore_case Forwarded to [base::grep()]
#'
#' @rdname callback_itm
#' @export
convert_unit <- function(fun, new, rgx = NULL, ignore_case = TRUE, ...) {

  chr_to_fun <- function(a) {

    if (is.function(a)) return(a)

    a <- as.character(a)

    function(x) a
  }

  if (is.function(fun)) {
    fun <- list(fun)
  }

  if (is.null(rgx)) {
    rgx <- rep(list(NULL), length(fun))
  }

  new <- lapply(new, chr_to_fun)

  assert_that(all_fun(fun, is.function))

  len <- vctrs::vec_size_common(rgx, fun, new)
  xtr <- c(list(ignore.case = ignore_case), list(...))

  function(x, val_var, unit_var, ...) {

    for (i in seq_len(len)) {

      if (is.null(rgx[[i]])) {

        vals <- list(fun[[i]](x[[val_var]]), new[[i]](x[[unit_var]]))
        set(x, j = c(val_var, unit_var), value = vals)

      } else {

        rows <- do.call(grep, c(list(rgx[[i]], x[[unit_var]]), xtr))
        vals <- list(fun[[i]](x[[val_var]][rows]),
                     new[[i]](x[[unit_var]][rows]))

        set(x, i = rows, j = c(val_var, unit_var), value = vals)
      }
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

add_concept_env <- new.env()

#' Internal item callback utilities
#'
#' The utility function `add_concept()` is exported for convenience when adding
#' external datasets and integrating concepts that require other concepts.
#' While this could be solves by defining a `rec_concpt`, in some scenarios
#' this might not be ideal, as it might be only required that `itm`
#' implementations for certain data sources require additional information.
#' Examples for this include vasopressor rates which might rely on patient
#' weight, and blood cell counts when expressed as ratio. For performance
#' reasons, the pulled in concept is internally cached, as this might be used
#' unchanged many times, when loading several concepts that need to pull in
#' the given concept. Persistence of cache is session-level and therefore this
#' utility is intended to be used somewhat sparingly.
#'
#' @param x Object in loading
#' @param env Data source environment as available as `env` in callback
#' functions
#' @param concept String valued concept name that will be loaded from the
#' default dictionary
#' @param var_name String valued variable name
#' @param aggregate Forwarded to [load_concepts()]
#'
#' @return A copy of `x` with the requested concept merged in.
#'
#' @rdname callback_int
#' @keywords internal
#' @export
add_concept <- function(x, env, concept, var_name = concept,
                        aggregate = NULL) {

  assert_that(is_id_tbl(x), is_src_env(env), is.string(concept),
              is.string(var_name), is_disjoint(var_name, colnames(x)))

  idtyp <- id_name_to_type(env, id_var(x))
  inval <- interval(x)
  cache <- digest(concept, src_name(env), idtyp, inval, aggregate)

  res <- get0(cache, envir = add_concept_env, inherits = FALSE)

  if (is.null(res)) {

    res <- load_concepts(concept, src_name(env), aggregate = aggregate,
                         verbose = FALSE, id_type = idtyp, interval = inval)

    assign(cache, res, envir = add_concept_env)
  }

  if (!identical(concept, var_name)) {
    res <- rename_cols(res, var_name, concept)
  }

  merge(x, res, all.x = TRUE)
}

#' @rdname callback_int
#' @keywords internal
#' @export
add_weight <- function(x, env, var_name = "weight") {

  var_exists <- is_in(var_name, colnames(x))

  if (var_exists) {
    tmp_var <- new_names(x)
  } else {
    tmp_var <- var_name
  }

  x <- add_concept(x, env, "weight", tmp_var)

  if (var_exists) {
    x <- x[, c(var_name) := silent_as_num(get(var_name))]
    x <- x[, c(var_name, tmp_var) := list(
      fifelse(is.na(get(var_name)), get(tmp_var), get(var_name)), NULL
    )]
  }

  x
}

blood_cell_ratio <- function(x, val_var, unit_var, env, ...) {

  x <- add_concept(x, env, "wbc")
  x <- x[, c(val_var, "wbc", unit_var) := list(
    100 * get(val_var) / get("wbc"), NULL, "%"
  )]

  x
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

eicu_rate_kg <- function(ml_to_mcg) {

  assert_that(is_number(ml_to_mcg))

  function(x, sub_var, val_var, weight_var, env, ...) {

    fix_units <- convert_unit(
      c(binary_op(`/`, 60), binary_op(`*`, 1000), set_val(NA),
        binary_op(`*`, ml_to_mcg), binary_op(`/`, 1000), set_val(NA), set_val(NA)),
      c(sub_trans("/hr$", "/min"), sub_trans("^mg/", "mcg/"),
        "mcg/kg/min", sub_trans("^ml/", "mcg/"),
        sub_trans("^nanograms/", "mcg/"), "mcg/kg/min", "mcg/kg/min"),
      c("/hr$", "^mg/", "^units/", "^ml/", "^nanograms/", "^Unknown$", "^ml$")
    )

    add_kg <- sub_trans("/", "/kg/")

    x <- x[, c(val_var) := silent_as_num(get(val_var))]

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

eicu_rate <- function(ml_to_mcg, mcg_to_units) {

  assert_that(is_number(ml_to_mcg), is_number(mcg_to_units))

  function(x, sub_var, val_var, env, ...) {

    fix_units <- convert_unit(
      c(binary_op(`/`, 60), binary_op(`*`, 1000), set_val(NA),
        binary_op(`*`, ml_to_mcg), binary_op(`*`, mcg_to_units)),
      c(sub_trans("/hr$", "/min"), sub_trans("^mg/", "mcg/"),
        "units/min", sub_trans("^ml/", "mcg/"), sub_trans("^mcg/", "units/")),
      c("/hr$", "^mg/", "/kg/", "^ml/", "^mcg/")
    )

    x <- x[, c(val_var) := silent_as_num(get(val_var))]
    x <- x[, c("unit_var") := eicu_extract_unit(get(sub_var))]

    fix_units(x, val_var, "unit_var")
  }
}

hirid_rate_kg <- function(x, val_var, unit_var, env, ...) {

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

hirid_rate <- function(x, val_var, unit_var, env, ...) {

  frac <- 1 / as.double(interval(x), units = "mins")

  x <- dt_gforce(x, "sum", vars = val_var)
  x <- x[, c(val_var, unit_var) := list(
    frac * get(val_var), paste(get(unit_var), "min", sep = "/")
  )]

  x
}

mimic_dur_incv <- function(x, val_var, grp_var, ...) {
  calc_dur(x, val_var, index_var(x), index_var(x), grp_var)
}

mimic_dur_inmv <- function(x, val_var, grp_var, stop_var, ...) {
  calc_dur(x, val_var, index_var(x), stop_var, grp_var)
}

aumc_rate_kg <- function(x, val_var, unit_var, rel_weight, rate_uom, env,
                         ...) {

  mg_to_mcg <- convert_unit(binary_op(`*`, 1000), "mcg", "mg")
  hr_to_min <- convert_unit(binary_op(`/`, 60),   "min", "uur")

  res <- rm_na(x, c(unit_var, rate_uom), "any")
  res <- mg_to_mcg(res, val_var, unit_var)
  res <- hr_to_min(res, val_var, rate_uom)

  res <- add_weight(res, env, "weight")

  res <- res[!get(rel_weight), c(val_var) := get(val_var) / get("weight")]
  res <- res[get(unit_var) == "\u00b5g", c(unit_var) := "mcg"]
  res <- res[, c(unit_var) := paste(
    get(unit_var), get(rate_uom), sep = "/kg/"
  )]

  res
}

aumc_rate <- function(mcg_to_units) {

  assert_that(is_number(mcg_to_units))

  function(x, val_var, unit_var, rate_uom, env, ...) {

    frac <- 1 / as.double(interval(x), units = "mins")

    to_units <- convert_unit(
      c(identity, binary_op(`*`, 1000), binary_op(`*`, mcg_to_units)),
      c("mcg", "mcg", "units"),
      c("\u00b5g", "mg", "mcg")
    )

    to_min <- convert_unit(
      c(binary_op(`/`, 24), binary_op(`/`, 60)),
      c("uur", "min"),
      c("dag", "uur")
    )

    x <- x[is.na(get(rate_uom)), c(val_var, rate_uom) := list(
      sum(get(val_var)) * frac, "min"), by = c(meta_vars(x))
    ]

    x <- to_units(to_min(x, val_var, rate_uom), val_var, unit_var)
    x <- x[, c(unit_var) := paste(get(unit_var), get(rate_uom), sep = "/")]

    x
  }
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

aumc_dur <- function(x, val_var, stop_var, grp_var, ...) {
  calc_dur(x, val_var, index_var(x), stop_var, grp_var)
}

#' Used for determining vasopressor durations, `calc_dur()` will calculate
#' durations by taking either per ID or per combination of ID and `grp_var`
#' the minimum for `min_var` and the maximum of `max_var` and returning the
#' time difference among the two.
#'
#' @param val_var String valued column name corresponding to the value variable
#' @param min_var,max_var Column names denoting start and end times
#' @param grp_var Optional grouping variable (for example linking infusions)
#'
#' @rdname callback_int
#' @keywords internal
#' @export
calc_dur <- function(x, val_var, min_var, max_var, grp_var = NULL) {

  id_vars   <- id_vars(x)
  index_var <- index_var(x)

  if (nrow(x) == 0L) {
    return(x[, c(val_var) := get(index_var)])
  }

  expr <- quote(list(min(get(min_var)), max(get(max_var))))
  names(expr) <- c("", index_var, val_var)

  res <- x[, eval(expr), by = c(id_vars, grp_var)]
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

mimic_abx_presc <- function(x, id_type, interval) {

  itm_cb <- function(x, val_var, ...) {

    idx <- index_var(x)

    x <- x[, c(idx, val_var) := list(get(idx) + mins(720L), TRUE)]
    x
  }

  itm <- set_callback(x, itm_cb)
  itm <- do.call(new_itm, c(itm, list(class = "rgx_itm")))
  res <- do_itm_load(itm, "hadm", mins(1L))

  res <- change_id(res, id_type, src_name(x), keep_old_id = FALSE,
                   id_type = TRUE)

  res <- do_callback(itm, res)
  res <- change_interval(res, interval, by_ref = TRUE)

  res
}

aumc_death <- function(x, val_var, ...) {

  idx <- index_var(x)

  x <- x[, c(val_var) := is_true(get(idx) - get(val_var) < hours(72L))]
  x
}

aumc_bxs <- function(x, val_var, dir_var, ...) {
  x <- x[get(dir_var) == "-", c(val_var) := -1L * get(val_var)]
  x
}

aumc_rass <- function(x) as.integer(substr(x, 1L, 2L))
