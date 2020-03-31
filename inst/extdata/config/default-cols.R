
cfg <- list(
  mimic = list(
    data_fun = "mimic_tbl_quo",
    id_cols = list(
      icustay = "icustay_id",
      hadm = "hadm_id",
      patient = "subject_id"
    ),
    tables = list(
      admissions = list(
        time_col = "admittime",
        val_col = "admission_type"
      ),
      callout = list(
        time_col = "callout_outcome",
        val_col = "outcometime"
      ),
      caregivers = list(
        id_col = "cgid",
        val_col = "label"
      ),
      chartevents = list(
        time_col = "charttime",
        val_col = "valuenum",
        unit_col = "valueuom"
      ),
      cptevents = list(
        time_col = "chartdate",
        val_col = "cpt_cd"
      ),
      d_cpt = list(
        id_col = "subsectionrange",
        val_col = "subsectionheader"
      ),
      d_icd_diagnoses = list(
        id_col = "icd9_code",
        val_col = "short_title"
      ),
      d_icd_procedures = list(
        id_col = "icd9_code",
        val_col = "short_title"
      ),
      d_items = list(
        id_col = "itemid",
        val_col = "label"
      ),
      d_labitems = list(
        id_col = "itemid",
        val_col = "label"
      ),
      datetimeevents = list(
        time_col = "charttime",
        val_col = "itemid"
      ),
      diagnoses_icd = list(
        val_col = "icd9_code"
      ),
      drgcodes = list(
        val_col = "drg_code"
      ),
      icustays = list(
        time_col = "intime",
        val_col = "last_careunit"
      ),
      inputevents_cv = list(
        time_col = "charttime",
        val_col = "rate",
        unit_col = "rateuom"
      ),
      inputevents_mv = list(
        time_col = "starttime",
        val_col = "rate",
        unit_col = "rateuom"
      ),
      labevents = list(
        time_col = "charttime",
        val_col = "valuenum",
        unit_col = "valueuom"
      ),
      microbiologyevents = list(
        time_col = "chartdate",
        val_col = "isolate_num"
      ),
      noteevents = list(
        time_col = "chartdate",
        val_col = "text"
      ),
      outputevents = list(
        time_col = "charttime",
        val_col = "value",
        unit_col = "valueuom"
      ),
      patients = list(
        val_col = "expire_flag"
      ),
      prescriptions = list(
        time_col = "startdate",
        val_col = "dose_val_rx",
        unit_col = "dose_unit_rx"
      ),
      procedureevents_mv = list(
        time_col = "starttime",
        val_col = "value",
        unit_col = "valueuom"
      ),
      procedures_icd = list(
        val_col = "icd9_code"
      ),
      services = list(
        time_col = "transfertime",
        val_col = "curr_service"
      ),
      transfers = list(
        time_col = "intime",
        val_col = "curr_careunit"
      )
    )
  ),
  eicu = list(
    data_fun = "eicu_tbl_quo",
    id_cols = list(
      icustay = "patientunitstayid",
      hadm = "patienthealthsystemstayid"
    ),
    tables = list(
      admissiondrug = list(
        time_col = "drugoffset",
        val_col = "drugdosage",
        unit_col = "drugunit"
      ),
      admissiondx = list(
        time_col = "admitdxenteredoffset",
        val_col = "admitdxtext"
      ),
      allergy = list(
        time_col = "allergyoffset",
        val_col = "allergyname"
      ),
      apacheapsvar = list(),
      apachepatientresult = list(
        val_col = "apachescore"
      ),
      apachepredvar = list(),
      careplancareprovider = list(
        time_col = "careprovidersaveoffset",
        val_col = "specialty"
      ),
      careplaneol = list(
        time_col = "cpleoldiscussionoffset"
      ),
      careplangeneral = list(
        time_col = "cplitemoffset",
        val_col = "cplitemvalue"
      ),
      careplangoal = list(
        time_col = "cplgoaloffset",
        val_col = "cplgoalvalue"
      ),
      careplaninfectiousdisease = list(
        time_col = "cplinfectdiseaseoffset",
        val_col = "infectdiseasesite"
      ),
      customlab = list(
        time_col = "labotheroffset",
        val_col = "labotherresult"
      ),
      diagnosis = list(
        time_col = "diagnosisoffset",
        val_col = "icd9code"
      ),
      hospital = list(
        id_col = "hospitalid",
        val_col = "numbedscategory"
      ),
      infusiondrug = list(
        time_col = "infusionoffset",
        val_col = "drugrate"
      ),
      intakeoutput = list(
        time_col = "intakeoutputoffset",
        val_col = "cellvaluenumeric"
      ),
      lab = list(
        time_col = "labresultoffset",
        val_col = "labresult",
        unit_col = "labmeasurenamesystem"
      ),
      medication = list(
        time_col = "drugstartoffset",
        val_col = "dosage"
      ),
      microlab = list(
        time_col = "culturetakenoffset",
        val_col = "organism"
      ),
      note = list(
        time_col = "noteoffset",
        val_col = "notetext"
      ),
      nurseassessment = list(
        time_col = "nurseassessoffset",
        val_col = "cellattributevalue"
      ),
      nursecare = list(
        time_col = "nursecareoffset",
        val_col = "cellattributevalue"
      ),
      nursecharting = list(
        time_col = "nursingchartoffset",
        val_col = "cellattributevalue"
      ),
      pasthistory = list(
        time_col = "pasthistoryoffset",
        val_col = "pasthistoryvalue"
      ),
      patient = list(
        val_col = "unitdischargestatus"
      ),
      physicalexam = list(
        time_col = "physicalexamoffset",
        val_col = "physicalexamvalue"
      ),
      respiratorycare = list(
        time_col = "respcarestatusoffset"
      ),
      respiratorycharting = list(
        time_col = "respchartoffset",
        val_col = "respchartvalue"
      ),
      treatment = list(
        time_col = "treatmentoffset",
        val_col = "treatmentstring"
      ),
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
