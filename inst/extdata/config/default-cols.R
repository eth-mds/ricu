
cfg <- list(
  mimic = list(
    data_fun = "mimic_tbl_quo",
    id_cols = list(
      icustay = "icustay_id",
      hadm = "hadm_id",
      patient = "subject_id"
    ),
    tables = list(
      admissions = list(),
      callout = list(),
      caregivers = list(),
      chartevents = list(
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
        time_col = "charttime",
        val_col = "rate"
      ),
      inputevents_mv = list(
        time_col = "starttime",
        val_col = "rate"
      ),
      labevents = list(
        time_col = "charttime",
        val_col = "valuenum"
      ),
      microbiologyevents = list(
        time_col = "chartdate",
        val_col = "isolate_num"
      ),
      noteevents = list(),
      outputevents = list(
        time_col = "charttime",
        val_col = "value"
      ),
      patients = list(),
      prescriptions = list(
        time_col = "startdate",
        val_col = "dose_val_rx"
      ),
      procedureevents_mv = list(
        time_col = "starttime",
        val_col = "value"
      ),
      procedures_icd = list(),
      services = list(),
      transfers = list()
    )
  ),
  eicu = list(
    data_fun = "eicu_tbl_quo",
    id_cols = list(
      icustay = "patientunitstayid",
      hadm = "patienthealthsystemstayid",
      patient = "uniquepid"
    ),
    tables = list(
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
        time_col = "infusionoffset",
        val_col = "drugrate"
      ),
      intakeoutput = list(
        time_col = "intakeoutputoffset",
        val_col = "outputtotal"
      ),
      lab = list(
        time_col = "labresultoffset",
        val_col = "labresult"
      ),
      medication = list(),
      microlab = list(),
      note = list(),
      nurseassessment = list(),
      nursecare = list(),
      nursecharting = list(
        time_col = "nursingchartoffset",
        val_col = "nursingchartvalue"
      ),
      pasthistory = list(),
      patient = list(),
      physicalexam = list(),
      respiratorycare = list(
        time_col = "respcarestatusoffset"
      ),
      respiratorycharting = list(
        time_col = "respchartoffset",
        val_col = "respchartvalue"
      ),
      treatment = list(),
      vitalaperiodic = list(
        time_col = "observationoffset"
      ),
      vitalperiodic = list(
        time_col = "observationoffset"
      )
    )
  ),
  hirid = list(
    data_fun = "hirid_tbl_quo",
    id_cols = list(
      icustay = "patientid"
    ),
    tables = list(
      general = list(),
      observations = list(
        time_col = "datetime",
        val_col = "value"
      ),
      ordinal = list(),
      pharma = list(
        time_col = "givenat",
        val_col = "givendose"
      )
    )
  )
)

cfg <- c(cfg, mimic_demo = list(cfg[["mimic"]]),
               eicu_demo = list(cfg[["eicu"]]))

file <- file.path(rprojroot::find_root(rprojroot::is_r_package), "inst",
                  "extdata", "config", "default-cols.json")

jsonlite::write_json(cfg, file, null = "null", auto_unbox = TRUE,
                     pretty = TRUE)
