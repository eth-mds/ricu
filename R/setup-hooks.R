
setup_eicu_aux_tables <- function(source) {

  delayedAssign("patient_weight",
                load_concepts(source, "weight"),
                assign.env = get_source(source, "aux"))
}
