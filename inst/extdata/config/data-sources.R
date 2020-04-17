
download_pysionet_schema <- function(url) {

  dat <- ricu:::download_pysionet_file(
    url, dest = NULL, username = NULL, password = NULL
  )

  xml2::as_list(xml2::read_xml(rawToChar(dat)))
}

get_table_info <- function(url) {

  res <- download_pysionet_schema(url)
  res <- res[["database"]][["tables"]]

  res <- lapply(res, function(x) {
    cols <- names(x) == "column"
    names <- vapply(x[cols], attr, character(1L), "name")
    types <- vapply(x[cols], attr, character(1L), "type")
    list(
      table_name = tolower(attr(x, "name")),
      num_rows = as.integer(attr(x, "numRows")),
      cols = as_col_spec(names, types)
    )
  })

  unname(res)
}

col_spec_map <- function(type) {
  switch(type,
    bool = "col_logical()",
    int2 = ,
    int4 = ,
    int8 = "col_integer()",
    numeric = ,
    float8 = "col_double()",
    bpchar = ,
    text = ,
    varchar = "col_character()",
    timestamp = "col_datetime(format = \"%Y-%m-%d %H:%M:%S\")",
    stop("unknown type")
  )
}

as_col_spec <- function(names, types) {
  Map(list, name = tolower(names), col = names,
      spec = vapply(types, col_spec_map, character(1L)))
}

as_tbl_spec <- function(files, defaults, tbl_info, partitioning) {

  do_as <- function(file, default, tbl, part) {

    all_cols <- vapply(tbl[["cols"]], `[[`, character(1L), "name")
    name <- tolower(tbl[["table_name"]])

    stopifnot(identical(sub("\\.csv(\\.gz)?$", "", tolower(file)), name),
              all(vapply(default, `%in%`, logical(1L), all_cols)))

    res <- list(file = file, name = name, defaults = default)

    if ("num_rows" %in% names(tbl)) {
      res[["num_rows"]] <- tbl[["num_rows"]]
    }

    res[["cols"]] <- unname(tbl[["cols"]])

    if (!is.null(part)) {
      stopifnot(isTRUE(names(part) %in% all_cols))
      res <- c(res, list(partitioning = part))
    }

    res
  }

  tbls <- vapply(tbl_info, `[[`, character(1L), "table_name")

  Map(do_as, files[tbls], defaults[tbls], tbl_info, partitioning[tbls],
      USE.NAMES = FALSE)
}

eicu_tbl_cfg <- function(is_demo = FALSE) {

  files <- c("admissionDrug.csv.gz",
             "admissionDx.csv.gz",
             "allergy.csv.gz",
             "apacheApsVar.csv.gz",
             "apachePatientResult.csv.gz",
             "apachePredVar.csv.gz",
             "carePlanCareProvider.csv.gz",
             "carePlanEOL.csv.gz",
             "carePlanGeneral.csv.gz",
             "carePlanGoal.csv.gz",
             "carePlanInfectiousDisease.csv.gz",
             "customLab.csv.gz",
             "diagnosis.csv.gz",
             "hospital.csv.gz",
             "infusionDrug.csv.gz",
             "intakeOutput.csv.gz",
             "lab.csv.gz",
             "medication.csv.gz",
             "microLab.csv.gz",
             "note.csv.gz",
             "nurseAssessment.csv.gz",
             "nurseCare.csv.gz",
             "nurseCharting.csv.gz",
             "pastHistory.csv.gz",
             "patient.csv.gz",
             "physicalExam.csv.gz",
             "respiratoryCare.csv.gz",
             "respiratoryCharting.csv.gz",
             "treatment.csv.gz",
             "vitalAperiodic.csv.gz",
             "vitalPeriodic.csv.gz")
  names(files) <- sub("\\.csv\\.gz", "", tolower(files))

  if (is_demo) {
    files <- sub("Drug\\.csv\\.gz", "drug.csv.gz", files)
  }

  defaults <- list(
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
      val_col = "nursingchartvalue"
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

  part <-  list(
    nursecharting = list(
      patientunitstayid = `if`(is_demo,
        c(0L, 1775421L, 999999999L),
        c(
          0L, 514528L, 1037072L, 1453997L, 1775421L, 2499831L, 2937948L,
          3213286L, 999999999L
        )
      )
    ),
    vitalperiodic = list(
      patientunitstayid = `if`(is_demo,
        c(0L, 1775421L, 999999999L),
        c(
          0L, 514528L, 1037072L, 1453997L, 1775421L, 2499831L, 2937948L,
          3213286L, 999999999L
        )
      )
    )
  )


  info <- get_table_info(
    "https://mit-lcp.github.io/eicu-schema-spy/eicu.eicu_crd.xml"
  )

  if (is_demo) {

    info <- lapply(info, `[[<-`, "num_rows", NULL)

  } else {

    tbl <- vapply(info, `[[`, character(1L), "table_name") == "respiratorycare"
    new <- info[[which(tbl)]][["cols"]]
    col <- vapply(new, `[[`, character(1L), "col") == "apneaparams"

    new[[which(col)]][["col"]] <- "apneaparms"
    info[[which(tbl)]][["cols"]] <- new
  }

  as_tbl_spec(files, defaults, info, part)
}

mimic_tbl_cfg <- function(is_demo = FALSE) {

  files <- c("ADMISSIONS.csv.gz",
             "CALLOUT.csv.gz",
             "CAREGIVERS.csv.gz",
             "CHARTEVENTS.csv.gz",
             "CPTEVENTS.csv.gz",
             "DATETIMEEVENTS.csv.gz",
             "DIAGNOSES_ICD.csv.gz",
             "DRGCODES.csv.gz",
             "D_CPT.csv.gz",
             "D_ICD_DIAGNOSES.csv.gz",
             "D_ICD_PROCEDURES.csv.gz",
             "D_ITEMS.csv.gz",
             "D_LABITEMS.csv.gz",
             "ICUSTAYS.csv.gz",
             "INPUTEVENTS_CV.csv.gz",
             "INPUTEVENTS_MV.csv.gz",
             "LABEVENTS.csv.gz",
             "MICROBIOLOGYEVENTS.csv.gz",
             "NOTEEVENTS.csv.gz",
             "OUTPUTEVENTS.csv.gz",
             "PATIENTS.csv.gz",
             "PRESCRIPTIONS.csv.gz",
             "PROCEDUREEVENTS_MV.csv.gz",
             "PROCEDURES_ICD.csv.gz",
             "SERVICES.csv.gz",
             "TRANSFERS.csv.gz")
  names(files) <- sub("\\.csv\\.gz", "", tolower(files))

  if (is_demo) {
    files <- sub("\\.gz$", "", tolower(files))
  }

  defaults <- list(
    admissions = list(
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

  part <- list(
    chartevents = list(
      itemid = `if`(is_demo,
        c(0L, 100000L, 999999999L),
        c(
          0L, 127L, 210L, 425L, 549L, 643L, 741L, 1483L, 3458L, 3695L, 8440L,
          8553L, 220274L, 223921L, 224085L, 224859L, 227629L, 999999999L
        )
      )
    )
  )

  info <- get_table_info(
    "https://mit-lcp.github.io/mimic-schema-spy/mimic.mimiciii.xml"
  )

  info <- info[
    !grepl("^chartevents_", vapply(info, `[[`, character(1L), "table_name"))
  ]

  if (is_demo) {

    info <- lapply(info, `[[<-`, "num_rows", NULL)

    info <- info[
      vapply(info, `[[`, character(1L), "table_name") != "noteevents"
    ]

  } else {

    info <- lapply(info, function(x) {
      x[["cols"]] <- Map(`[[<-`, x[["cols"]], "col",
        toupper(vapply(x[["cols"]], `[[`, character(1L), "col"))
      )
      x
    })
  }

  as_tbl_spec(files, defaults, info, part)
}

hirid_tbl_cfg <- function() {

  info <- list(
    general = list(
      patientid = "col_integer()",
      admissiontime = "col_datetime(format = \"%Y-%m-%d %H:%M:%S\")",
      sex = "col_character",
      age = "col_integer()"
    ),
    observations = list(
      patientid = "col_integer()",
      datetime = "col_datetime(format = \"%Y-%m-%d %H:%M:%S\")",
      entertime = "col_datetime(format = \"%Y-%m-%d %H:%M:%S\")",
      status = "col_integer()",
      stringvalue = "col_character",
      type = "col_character",
      value = "col_double",
      variableid = "col_integer()"
    ),
    ordinal = list(
      variableid = "col_integer()",
      code = "col_integer()",
      stringvalue = "col_character"
    ),
    pharma = list(
      patientid = "col_integer()",
      pharmaid = "col_integer()",
      givenat = "col_datetime(format = \"%Y-%m-%d %H:%M:%S\")",
      enteredentryat = "col_datetime(format = \"%Y-%m-%d %H:%M:%S\")",
      givendose = "col_double",
      cumulativedose = "col_double",
      fluidamount_calc = "col_double",
      cumulfluidamount_calc = "col_double",
      doseunit = "col_character",
      route = "col_character",
      infusionid = "col_integer()",
      typeid = "col_integer()",
      subtypeid = "col_double",
      recordstatus = "col_integer()"
    )
  )

  files <- paste0(names(info), ".csv.gz")
  names(files) <- names(info)

  defaults <- list(
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

  part <- list(observations = list(patientid = 1L:16L))

  info <- Map(function(x, name) {
    list(table_name = name,
         cols = Map(list, name = names(x), col = names(x), spec = x))
  }, info, names(info))

  as_tbl_spec(files, defaults, info, part)
}

pkg_dir <- rprojroot::find_root(rprojroot::is_r_package)
cfg_dir <- file.path(pkg_dir, "inst", "extdata", "config")

cfg <- list(
  list(
    name = "eicu",
    base_url = "https://physionet.org/files/eicu-crd",
    version = "2.0",
    setup_hook = "setup_eicu_aux_tables",
    data_fun = "eicu_tbl_quo",
    id_cols = list(
      icustay = "patientunitstayid",
      hadm = "patienthealthsystemstayid"
    ),
    tables = eicu_tbl_cfg(is_demo = FALSE)
  ),
  list(
    name = "eicu_demo",
    base_url = "https://physionet.org/files/eicu-crd-demo",
    version = "2.0",
    setup_hook = "setup_eicu_aux_tables",
    data_fun = "eicu_tbl_quo",
    id_cols = list(
      icustay = "patientunitstayid",
      hadm = "patienthealthsystemstayid"
    ),
    tables = eicu_tbl_cfg(is_demo = TRUE)
  ),
  list(
    name = "mimic",
    base_url = "https://physionet.org/files/mimiciii",
    version = "1.4",
    setup_hook = "setup_mimic_aux_tables",
    data_fun = "mimic_tbl_quo",
    id_cols = list(
      icustay = "icustay_id",
      hadm = "hadm_id",
      patient = "subject_id"
    ),
    tables = mimic_tbl_cfg(is_demo = FALSE)
  ),
  list(
    name = "mimic_demo",
    base_url = "https://physionet.org/files/mimiciii-demo",
    version = "1.4",
    setup_hook = "setup_mimic_aux_tables",
    data_fun = "mimic_tbl_quo",
    id_cols = list(
      icustay = "icustay_id",
      hadm = "hadm_id",
      patient = "subject_id"
    ),
    tables = mimic_tbl_cfg(is_demo = TRUE)
  ),
  list(
    name = "hirid",
    base_url = "https://physionet.org/files/hirid",
    version = "0.1",
    setup_hook = "setup_hirid_aux_tables",
    data_fun = "hirid_tbl_quo",
    id_cols = list(
      icustay = "patientid"
    ),
    tables = hirid_tbl_cfg()
  )
)

ricu::set_config(cfg, "data-sources", cfg_dir)
devtools::install(pkg_dir)
