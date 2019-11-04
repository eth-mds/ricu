
eicu_sofa_pa <- function(lab_item = "paO2", agg_fun = min, ...) {

  message("fetching pao2")

  lab <- eicu_get_lab_items(c(pao2 = lab_item), ...)
  res <- aggregate_vals(lab, agg_fun, by_cols = c("hadm_id", "rel_time"),
                        val_col = "pao2")

  assert_that(attr(res[["pao2"]], "units") == "mm Hg")

  res
}
