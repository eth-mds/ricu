
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
    inputevents_cv = list(
      id_col = "hadm_id",
      time_col = "charttime",
      val_col = "rate"
    ),
    inputevents_mv = list(
      id_col = "hadm_id",
      time_col = "starttime",
      val_col = "rate"
    ),
    labevents = list(
      id_col = "hadm_id",
      time_col = "charttime",
      val_col = "valuenum"
    ),
    microbiologyevents = list(),
    noteevents = list(),
    outputevents = list(
      id_col = "hadm_id",
      time_col = "charttime",
      val_col = "value"
    ),
    patients = list(),
    prescriptions = list(
      id_col = "hadm_id",
      time_col = "startdate",
      val_col = "dose_val_rx"
    ),
    procedureevents_mv = list(
      id_col = "hadm_id",
      time_col = "starttime",
      val_col = "value"
    ),
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
    infusiondrug = list(
      id_col = "patienthealthsystemstayid",
      time_col = "infusionoffset",
      val_col = "drugrate"
    ),
    intakeoutput = list(
      id_col = "patienthealthsystemstayid",
      time_col = "intakeoutputoffset",
      val_col = "outputtotal"
    ),
    lab = list(
      id_col = "patienthealthsystemstayid",
      time_col = "labresultoffset",
      val_col = "labresult"
    ),
    medication = list(),
    microlab = list(),
    note = list(),
    nurseassessment = list(),
    nursecare = list(),
    nursecharting = list(
      id_col = "patienthealthsystemstayid",
      time_col = "nursingchartoffset",
      val_col = "nursingchartvalue"
    ),
    pasthistory = list(),
    patient = list(),
    physicalexam = list(),
    respiratorycare = list(
      id_col = "patienthealthsystemstayid",
      time_col = "respcarestatusoffset",
      val_col = NULL
    ),
    respiratorycharting = list(
      id_col = "patienthealthsystemstayid",
      time_col = "respchartoffset",
      val_col = "respchartvalue"
    ),
    treatment = list(),
    vitalaperiodic = list(
      id_col = "patienthealthsystemstayid",
      time_col = "observationoffset",
      val_col = NULL
    ),
    vitalperiodic = list(
      id_col = "patienthealthsystemstayid",
      time_col = "observationoffset",
      val_col = NULL
    )
  ),
  hirid = list(
    general = list(),
    observations = list(
      id_col = "patientid",
      time_col = "datetime",
      val_col = "value"
    ),
    ordinal = list(),
    pharma = list(
      id_col = "patientid",
      time_col = "givenat",
      val_col = "givendose"
    )
  )
)

file <- file.path(rprojroot::find_root(rprojroot::is_r_package), "inst",
                  "extdata", "config", "default-cols.json")

jsonlite::write_json(cfg, file, null = "null", auto_unbox = TRUE,
                     pretty = TRUE)
