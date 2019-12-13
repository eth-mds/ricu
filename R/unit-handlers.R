
mcg_to_mg <- function(x, unit_col, val_col) {

  which_rows <- x[[unit_col]] == "mcg"

  if (sum(which_rows) > 0L) {
    message("Changing ", sum(which_rows), " units from mcg to mg.")
  }

  x[which_rows, c(val_col, unit_col) := list(val_col * 1000, "mg")]
}

allow_na <- function(x, unit_col, val_col) {

  max_unit <- names(which.max(table(x[[unit_col]])))
  which_rows <- is.na(x[[unit_col]])

  if (sum(which_rows) > 0L) {
    message("Changing ", sum(which_rows), " units from NA to ", max_unit, ".")
    x <- x[which_rows, c(unit_col) := max_unit]
  }

  x
}

na_to_percent <- function(x, unit_col, val_col) {

  which_rows <- is.na(x[[unit_col]])

  if (sum(which_rows) > 0L) {
    message("Changing ", sum(which_rows), " units from NA to %.")
    x <- x[which_rows, c(unit_col) := "%"]
  }

  x
}

fix_percent <- function(x, unit_col, val_col) {

  x <- na_to_percent(x, unit_col, val_col)

  assert_that(all(x[[unit_col]] == "%"))

  oor <- x[[val_col]] < 0 | x[[val_col]] > 100
  if (any(oor)) {
    message("Removing ", sum(oor), " out of range percentages.")
    x <- x[!oor, ]
  }

  x
}

fix_char_case <- function(x, unit_col, val_col) {

  units <- names(table(x[[unit_col]]))

  if (length(units) > 1L) {
    max_unit <- names(which.max(table(x[[unit_col]])))
    units <- units[grepl(max_unit, units, ignore.case = TRUE)]
    mod <- x[[unit_col]] %in% units & x[[unit_col]] != max_unit
    if (any(mod)) {
      message("Changing case of ", sum(mod), " units to ", max_unit, ".")
      x <- x[mod, c(unit_col) := max_unit]
    }
  }

  x
}

fix_case_allow_na <- function(x, unit_col, val_col) {
  x <- fix_char_case(x, unit_col, val_col)
  x <- allow_na(x, unit_col, val_col)
  x
}

fix_rate <- function(x, unit_col, val_col) {

  drop <- x[[unit_col]] %in% "mcgmin"

  if (any(drop)) {
    message("Removing ", sum(drop), " absolute (w.r.t. body weight) rates.")
    x <- x[!drop, ]
  }

  x[[unit_col]][x[[unit_col]] %in% "mcgkgmin"] <- "mcg/kg/min"

  x
}

handle_ml <- function(x, unit_col, val_col) {
  x <- x[x[[val_col]] > 0, ]
  x[[unit_col]][is_val(x[[unit_col]], "ml")] <- "mL"
  x
}
