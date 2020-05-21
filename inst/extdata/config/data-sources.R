
download_pysionet_schema <- function(url) {

  dat <- ricu:::download_pysionet_file(
    url, dest = NULL, user = NULL, pass = NULL
  )

  message("downloading schema at ", url)

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
    bool = list(spec = "col_logical"),
    int2 = ,
    int4 = ,
    int8 = list(spec = "col_integer"),
    numeric = ,
    float8 = list(spec = "col_double"),
    bpchar = ,
    text = ,
    varchar = list(spec = "col_character"),
    timestamp = list(spec = "col_datetime", format = "%Y-%m-%d %H:%M:%S"),
    stop("unknown type")
  )
}

as_col_spec <- function(names, types) {
  Map(c, Map(list, name = tolower(names), col = names),
         lapply(types, col_spec_map))
}

as_tbl_spec <- function(files, defaults, time_cols, tbl_info, partitioning) {

  do_as <- function(file, default, time, tbl, part) {

    all_cols <- vapply(tbl[["cols"]], `[[`, character(1L), "name")
    name <- tolower(tbl[["table_name"]])
    common <- unique(sub("(_[0-9]+)?\\.csv(\\.gz)?$", "", tolower(file)))

    stopifnot(identical(common, name),
              all(vapply(default, `%in%`, logical(1L), all_cols)))

    if (length(time)) {
      res <- list(files = file, name = name, defaults = default,
                  time_cols = unname(time))
    } else {
      res <- list(files = file, name = name, defaults = default)
    }

    if ("num_rows" %in% names(tbl)) {
      res[["num_rows"]] <- tbl[["num_rows"]]
    } else {
      res[["num_rows"]] <- 0
    }

    res[["cols"]] <- unname(tbl[["cols"]])

    if (!is.null(part)) {
      stopifnot(isTRUE(names(part) %in% all_cols))
      res <- c(res, list(partitioning = list(col = names(part),
                                             breaks = part[[1L]])))
    }

    res
  }

  tbls <- vapply(tbl_info, `[[`, character(1L), "table_name")

  Map(do_as, files[tbls], defaults[tbls], time_cols[tbls], tbl_info,
      partitioning[tbls], USE.NAMES = FALSE)
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
      index_col = "drugoffset",
      val_col = "drugdosage",
      unit_col = "drugunit"
    ),
    admissiondx = list(
      index_col = "admitdxenteredoffset",
      val_col = "admitdxtext"
    ),
    allergy = list(
      index_col = "allergyoffset",
      val_col = "allergyname"
    ),
    apacheapsvar = list(),
    apachepatientresult = list(
      val_col = "apachescore"
    ),
    apachepredvar = list(),
    careplancareprovider = list(
      index_col = "careprovidersaveoffset",
      val_col = "specialty"
    ),
    careplaneol = list(
      index_col = "cpleoldiscussionoffset"
    ),
    careplangeneral = list(
      index_col = "cplitemoffset",
      val_col = "cplitemvalue"
    ),
    careplangoal = list(
      index_col = "cplgoaloffset",
      val_col = "cplgoalvalue"
    ),
    careplaninfectiousdisease = list(
      index_col = "cplinfectdiseaseoffset",
      val_col = "infectdiseasesite"
    ),
    customlab = list(
      index_col = "labotheroffset",
      val_col = "labotherresult"
    ),
    diagnosis = list(
      index_col = "diagnosisoffset",
      val_col = "icd9code"
    ),
    hospital = list(
      id_col = "hospitalid",
      val_col = "numbedscategory"
    ),
    infusiondrug = list(
      index_col = "infusionoffset",
      val_col = "drugrate"
    ),
    intakeoutput = list(
      index_col = "intakeoutputoffset",
      val_col = "cellvaluenumeric"
    ),
    lab = list(
      index_col = "labresultoffset",
      val_col = "labresult",
      unit_col = "labmeasurenameinterface"
    ),
    medication = list(
      index_col = "drugstartoffset",
      val_col = "dosage"
    ),
    microlab = list(
      index_col = "culturetakenoffset",
      val_col = "organism"
    ),
    note = list(
      index_col = "noteoffset",
      val_col = "notetext"
    ),
    nurseassessment = list(
      index_col = "nurseassessoffset",
      val_col = "cellattributevalue"
    ),
    nursecare = list(
      index_col = "nursecareoffset",
      val_col = "cellattributevalue"
    ),
    nursecharting = list(
      index_col = "nursingchartoffset",
      val_col = "nursingchartvalue"
    ),
    pasthistory = list(
      index_col = "pasthistoryoffset",
      val_col = "pasthistoryvalue"
    ),
    patient = list(
      val_col = "unitdischargestatus"
    ),
    physicalexam = list(
      index_col = "physicalexamoffset",
      val_col = "physicalexamvalue"
    ),
    respiratorycare = list(
      index_col = "respcarestatusoffset"
    ),
    respiratorycharting = list(
      index_col = "respchartoffset",
      val_col = "respchartvalue"
    ),
    treatment = list(
      index_col = "treatmentoffset",
      val_col = "treatmentstring"
    ),
    vitalaperiodic = list(
      index_col = "observationoffset"
    ),
    vitalperiodic = list(
      index_col = "observationoffset"
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

  time_cols <- lapply(info, function(x) {
    nme <- vapply(x[["cols"]], `[[`, character(1L), "name")
    typ <- vapply(x[["cols"]], `[[`, character(1L), "spec")
    nme[typ == "col_integer" & grepl("offset$", nme)]
  })

  names(time_cols) <- vapply(info, `[[`, character(1L), "table_name")

  as_tbl_spec(files, defaults, time_cols, info, part)
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
    files <- sub("\\.gz$", "", files)
  }

  defaults <- list(
    admissions = list(
      val_col = "admission_type"
    ),
    callout = list(
      index_col = "outcometime",
      val_col = "callout_outcome"
    ),
    caregivers = list(
      id_col = "cgid",
      val_col = "label"
    ),
    chartevents = list(
      index_col = "charttime",
      val_col = "valuenum",
      unit_col = "valueuom"
    ),
    cptevents = list(
      index_col = "chartdate",
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
      index_col = "charttime",
      val_col = "itemid"
    ),
    diagnoses_icd = list(
      val_col = "icd9_code"
    ),
    drgcodes = list(
      val_col = "drg_code"
    ),
    icustays = list(
      index_col = "intime",
      val_col = "last_careunit"
    ),
    inputevents_cv = list(
      index_col = "charttime",
      val_col = "rate",
      unit_col = "rateuom"
    ),
    inputevents_mv = list(
      index_col = "starttime",
      val_col = "rate",
      unit_col = "rateuom"
    ),
    labevents = list(
      index_col = "charttime",
      val_col = "valuenum",
      unit_col = "valueuom"
    ),
    microbiologyevents = list(
      index_col = "chartdate",
      val_col = "isolate_num"
    ),
    noteevents = list(
      index_col = "chartdate",
      val_col = "text"
    ),
    outputevents = list(
      index_col = "charttime",
      val_col = "value",
      unit_col = "valueuom"
    ),
    patients = list(
      val_col = "expire_flag"
    ),
    prescriptions = list(
      index_col = "startdate",
      val_col = "dose_val_rx",
      unit_col = "dose_unit_rx"
    ),
    procedureevents_mv = list(
      index_col = "starttime",
      val_col = "value",
      unit_col = "valueuom"
    ),
    procedures_icd = list(
      val_col = "icd9_code"
    ),
    services = list(
      index_col = "transfertime",
      val_col = "curr_service"
    ),
    transfers = list(
      index_col = "intime",
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

  time_cols <- lapply(info, function(x) {
    nme <- vapply(x[["cols"]], `[[`, character(1L), "name")
    typ <- vapply(x[["cols"]], `[[`, character(1L), "spec")
    nme[typ == "col_datetime"]
  })

  names(time_cols) <- vapply(info, `[[`, character(1L), "table_name")

  as_tbl_spec(files, defaults, time_cols, info, part)
}

hirid_tbl_cfg <- function() {

  info <- list(
    general = list(
      patientid = list(spec = "col_integer"),
      admissiontime = list(spec = "col_datetime",
                           format = "%Y-%m-%d %H:%M:%S"),
      sex = list(spec = "col_character"),
      age = list(spec = "col_integer")
    ),
    observations = list(
      patientid = list(spec = "col_integer"),
      datetime = list(spec = "col_datetime", format = "%Y-%m-%d %H:%M:%S"),
      entertime = list(spec = "col_datetime", format = "%Y-%m-%d %H:%M:%S"),
      status = list(spec = "col_integer"),
      stringvalue = list(spec = "col_character"),
      type = list(spec = "col_character"),
      value = list(spec = "col_double"),
      variableid = list(spec = "col_integer")
    ),
    ordinal = list(
      variableid = list(spec = "col_integer"),
      code = list(spec = "col_integer"),
      stringvalue = list(spec = "col_character")
    ),
    pharma = list(
      patientid = list(spec = "col_integer"),
      pharmaid = list(spec = "col_integer"),
      givenat = list(spec = "col_datetime", format = "%Y-%m-%d %H:%M:%S"),
      enteredentryat = list(spec = "col_datetime",
                            format = "%Y-%m-%d %H:%M:%S"),
      givendose = list(spec = "col_double"),
      cumulativedose = list(spec = "col_double"),
      fluidamount_calc = list(spec = "col_double"),
      cumulfluidamount_calc = list(spec = "col_double"),
      doseunit = list(spec = "col_character"),
      route = list(spec = "col_character"),
      infusionid = list(spec = "col_integer"),
      typeid = list(spec = "col_integer"),
      subtypeid = list(spec = "col_double"),
      recordstatus = list(spec = "col_integer")
    )
  )

  files <- list(
    "general.csv.gz",
    paste0("observations_", 1:3, ".csv.gz"),
    "ordinal.csv.gz",
    "pharma.csv.gz"
  )
  names(files) <- names(info)

  defaults <- list(
    general = list(),
    observations = list(
      index_col = "datetime",
      val_col = "value"
    ),
    ordinal = list(),
    pharma = list(
      index_col = "givenat",
      val_col = "givendose"
    )
  )

  part <- list(observations = list(patientid = 1L:16L))

  info <- Map(function(x, name) {
    list(table_name = name,
         cols = Map(c, Map(list, name = names(x), col = names(x)), x))
  }, info, names(info))

  time_cols <- lapply(info, function(x) {
    nme <- vapply(x[["cols"]], `[[`, character(1L), "name")
    typ <- vapply(x[["cols"]], `[[`, character(1L), "spec")
    nme[typ == "col_datetime"]
  })

  names(time_cols) <- vapply(info, `[[`, character(1L), "table_name")

  as_tbl_spec(files, defaults, time_cols, info, part)
}

pkg_dir <- rprojroot::find_root(rprojroot::is_r_package)
cfg_dir <- file.path(pkg_dir, "inst", "extdata", "config")

cfg <- list(
  list(
    name = "eicu",
    url = "https://physionet.org/files/eicu-crd/2.0",
    id_cfg = list(
      hadm = list(id = "patienthealthsystemstayid", position = 1L,
                  start = "hospitaladmitoffset",
                  end = "hospitaldischargeoffset", table = "patient"),
      icustay = list(id = "patientunitstayid", position = 2L,
                     end = "unitdischargeoffset", table = "patient")
    ),
    tables = eicu_tbl_cfg(is_demo = FALSE)
  ),
  list(
    name = "eicu_demo",
    url = "https://physionet.org/files/eicu-crd-demo/2.0",
    id_cfg = list(
      hadm = list(id = "patienthealthsystemstayid", position = 1L,
                  start = "hospitaladmitoffset",
                  end = "hospitaldischargeoffset", table = "patient"),
      icustay = list(id = "patientunitstayid", end = "unitdischargeoffset",
                     table = "patient", position = 2L)
    ),
    tables = eicu_tbl_cfg(is_demo = TRUE)
  ),
  list(
    name = "mimic",
    url = "https://physionet.org/files/mimiciii/1.4",
    id_cfg = list(
      patient = list(id = "subject_id", position = 1L, start = "dob",
                     end = "dod", table = "patients"),
      hadm = list(id = "hadm_id", position = 2L, start = "admittime",
                  end = "dischtime", table = "admissions"),
      icustay = list(id = "icustay_id", position = 3L, start = "intime",
                     end = "outtime", table = "icustays")
    ),
    tables = mimic_tbl_cfg(is_demo = FALSE)
  ),
  list(
    name = "mimic_demo",
    url = "https://physionet.org/files/mimiciii-demo/1.4",
    id_cfg = list(
      patient = list(id = "subject_id", position = 1L, start = "dob",
                     end = "dod", table = "patients"),
      hadm = list(id = "hadm_id", position = 2L, start = "admittime",
                  end = "dischtime", table = "admissions"),
      icustay = list(id = "icustay_id", position = 3L, start = "intime",
                     end = "outtime", table = "icustays")
    ),
    tables = mimic_tbl_cfg(is_demo = TRUE)
  ),
  list(
    name = "hirid",
    url = "https://physionet.org/files/hirid/0.1",
    id_cfg = list(
      icustay = list(id = "patientid", position = 1L, start = "admissiontime",
                     table = "general")
    ),
    tables = hirid_tbl_cfg()
  )
)

ricu::set_config(cfg, "data-sources", cfg_dir)
devtools::install(pkg_dir)
