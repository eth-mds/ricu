
mcg_to_mg <- function(x, unit_var, val_var) {

  which_rows <- x[[unit_var]] == "mcg"

  if (sum(which_rows) > 0L) {
    message("Changing ", sum(which_rows), " units from mcg to mg.")
  }

  x[which_rows, c(val_var, unit_var) := list(val_var * 1000, "mg")]
}

allow_na <- function(x, unit_var, val_var) {

  max_unit <- names(which.max(table(x[[unit_var]])))
  which_rows <- is.na(x[[unit_var]])

  if (sum(which_rows) > 0L) {
    message("Changing ", sum(which_rows), " units from NA to ", max_unit, ".")
    x <- x[which_rows, c(unit_var) := max_unit]
  }

  x
}

na_to_percent <- function(x, unit_var, val_var) {

  which_rows <- is.na(x[[unit_var]])

  if (sum(which_rows) > 0L) {
    message("Changing ", sum(which_rows), " units from NA to %.")
    x <- x[which_rows, c(unit_var) := "%"]
  }

  x
}

fix_percent <- function(x, unit_var, val_var) {

  x <- na_to_percent(x, unit_var, val_var)

  assert_that(all(x[[unit_var]] == "%"))

  oor <- x[[val_var]] < 0 | x[[val_var]] > 100
  if (any(oor)) {
    message("Removing ", sum(oor), " out of range percentages.")
    x <- x[!oor, ]
  }

  x
}

fix_char_case <- function(x, unit_var, val_var) {

  units <- names(table(x[[unit_var]]))

  if (length(units) > 1L) {
    max_unit <- names(which.max(table(x[[unit_var]])))
    units <- units[grepl(max_unit, units, ignore.case = TRUE)]
    mod <- x[[unit_var]] %in% units & x[[unit_var]] != max_unit
    if (any(mod)) {
      message("Changing case of ", sum(mod), " units to ", max_unit, ".")
      x <- x[mod, c(unit_var) := max_unit]
    }
  }

  x
}

fix_case_allow_na <- function(x, unit_var, val_var) {
  x <- fix_char_case(x, unit_var, val_var)
  x <- allow_na(x, unit_var, val_var)
  x
}

fix_rate <- function(x, unit_var, val_var) {

  drop <- x[[unit_var]] %in% "mcgmin"

  if (any(drop)) {
    message("Removing ", sum(drop), " absolute (w.r.t. body weight) rates.")
    x <- x[!drop, ]
  }

  x[[unit_var]][x[[unit_var]] %in% "mcgkgmin"] <- "mcg/kg/min"

  x
}

handle_ml <- function(x, unit_var, val_var) {
  x <- x[x[[val_var]] > 0, ]
  x[[unit_var]][is_val(x[[unit_var]], "ml")] <- "mL"
  x
}
