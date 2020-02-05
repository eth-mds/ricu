
cfg <- list(
  mimic = list(
    admissions = list(),
    callout = list(),
    caregivers = list(),
    chartevents = list(
      id_col = "hadm_id",
      time_col = "charttime",
      val_col = "valuenum"
    ),
    cptevents = list(),
    d_cpt = list(),
    d_icd_diagnoses = list(),
    d_icd_procedures = list(),
    d_items = list(),
    d_labitems = list(),
    datetimeevents = list(),
    diagnoses_icd = list(),
    drgcodes = list(),
    icustays = list(),
    inputevents_cv = list(),
    inputevents_mv = list(),
    labevents = list(
      id_col = "hadm_id",
      time_col = "charttime",
      val_col = "valuenum"
    ),
    microbiologyevents = list(),
    noteevents = list(),
    outputevents = list(),
    patients = list(),
    prescriptions = list(),
    procedureevents_mv = list(),
    procedures_icd = list(),
    services = list(),
    transfers = list()
  ),
  eicu = list(
    admissiondrug = list(),
    admissiondx = list(),
    allergy = list(),
    apacheapsvar = list(),
    apachepatientresult = list(),
    apachepredvar = list(),
    careplancareprovider = list(),
    careplaneol = list(),
    careplangeneral = list(),
    careplangoal = list(),
    careplaninfectiousdisease = list(),
    customlab = list(),
    diagnosis = list(),
    hospital = list(),
    infusiondrug = list(),
    intakeoutput = list(),
    lab = list(),
    medication = list(),
    microlab = list(),
    note = list(),
    nurseassessment = list(),
    nursecare = list(),
    nursecharting = list(),
    pasthistory = list(),
    patient = list(),
    physicalexam = list(),
    respiratorycare = list(),
    respiratorycharting = list(),
    treatment = list(),
    vitalaperiodic = list(),
    vitalperiodic = list()
  ),
  hirid = list(
    general = list(),
    observations = list(),
    ordinal = list(),
    pharma = list()
  )
)

jsonlite::write_json(cfg, "default_cols.json", auto_unbox = TRUE,
                     pretty = TRUE)
