
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
    int4 = list(spec = "col_integer"),
    int8 = ,
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

as_tbl_spec <- function(files, defaults, time_vars, tbl_info, partitioning) {

  mod_col <- function(x) {
    x[["name"]] <- x[["col"]]
    x[["col"]] <- NULL
    x
  }

  do_as <- function(file, default, time, tbl, part) {

    all_cols <- vapply(tbl[["cols"]], `[[`, character(1L), "name")

    stopifnot(all(vapply(default, `%in%`, logical(1L), all_cols)))

    if (length(time)) {
      default <- c(default, list(time_vars = unname(time)))
    }

    tbl <- c(list(files = file, defaults = default), tbl)

    tbl[["table_name"]] <- NULL
    tbl[["cols"]] <- setNames(lapply(tbl[["cols"]], mod_col), all_cols)

    if (!is.null(part)) {
      stopifnot(isTRUE(names(part) %in% all_cols))
      tbl <- c(tbl, list(partitioning = list(col = names(part),
                                             breaks = part[[1L]])))
    }

    tbl
  }

  tbls <- vapply(tbl_info, `[[`, character(1L), "table_name")

  res <- Map(do_as, files[tbls], defaults[tbls], time_vars[tbls], tbl_info,
             partitioning[tbls])
  names(res) <- tolower(tbls)

  res
}

as_minimal_tbl_spec <- function(x) {
  x[setdiff(names(x), c("files", "num_rows", "cols"))]
}

eicu_tbl_cfg <- function(info, is_demo = FALSE) {

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
      index_var = "drugoffset",
      val_var = "drugdosage",
      unit_var = "drugunit"
    ),
    admissiondx = list(
      index_var = "admitdxenteredoffset",
      val_var = "admitdxtext"
    ),
    allergy = list(
      index_var = "allergyoffset",
      val_var = "allergyname"
    ),
    apacheapsvar = list(),
    apachepatientresult = list(
      val_var = "apachescore"
    ),
    apachepredvar = list(),
    careplancareprovider = list(
      index_var = "careprovidersaveoffset",
      val_var = "specialty"
    ),
    careplaneol = list(
      index_var = "cpleoldiscussionoffset"
    ),
    careplangeneral = list(
      index_var = "cplitemoffset",
      val_var = "cplitemvalue"
    ),
    careplangoal = list(
      index_var = "cplgoaloffset",
      val_var = "cplgoalvalue"
    ),
    careplaninfectiousdisease = list(
      index_var = "cplinfectdiseaseoffset",
      val_var = "infectdiseasesite"
    ),
    customlab = list(
      index_var = "labotheroffset",
      val_var = "labotherresult"
    ),
    diagnosis = list(
      index_var = "diagnosisoffset",
      val_var = "icd9code"
    ),
    hospital = list(
      id_var = "hospitalid",
      val_var = "numbedscategory"
    ),
    infusiondrug = list(
      index_var = "infusionoffset",
      val_var = "drugrate"
    ),
    intakeoutput = list(
      index_var = "intakeoutputoffset",
      val_var = "cellvaluenumeric"
    ),
    lab = list(
      index_var = "labresultoffset",
      val_var = "labresult",
      unit_var = "labmeasurenameinterface"
    ),
    medication = list(
      index_var = "drugstartoffset",
      val_var = "dosage"
    ),
    microlab = list(
      index_var = "culturetakenoffset",
      val_var = "organism"
    ),
    note = list(
      index_var = "noteoffset",
      val_var = "notetext"
    ),
    nurseassessment = list(
      index_var = "nurseassessoffset",
      val_var = "cellattributevalue"
    ),
    nursecare = list(
      index_var = "nursecareoffset",
      val_var = "cellattributevalue"
    ),
    nursecharting = list(
      index_var = "nursingchartoffset",
      val_var = "nursingchartvalue"
    ),
    pasthistory = list(
      index_var = "pasthistoryoffset",
      val_var = "pasthistoryvalue"
    ),
    patient = list(
      val_var = "unitdischargestatus"
    ),
    physicalexam = list(
      index_var = "physicalexamoffset",
      val_var = "physicalexamvalue"
    ),
    respiratorycare = list(
      index_var = "respcarestatusoffset"
    ),
    respiratorycharting = list(
      index_var = "respchartoffset",
      val_var = "respchartvalue"
    ),
    treatment = list(
      index_var = "treatmentoffset",
      val_var = "treatmentstring"
    ),
    vitalaperiodic = list(
      index_var = "observationoffset"
    ),
    vitalperiodic = list(
      index_var = "observationoffset"
    )
  )

  part <-  list(
    nursecharting = list(
      patientunitstayid = `if`(is_demo,
         1775421L,
        c(514528L, 1037072L, 1453997L, 1775421L, 2499831L, 2937948L, 3213286L)
      )
    ),
    vitalperiodic = list(
      patientunitstayid = `if`(is_demo,
         1775421L,
        c(514528L, 1037072L, 1453997L, 1775421L, 2499831L, 2937948L, 3213286L)
      )
    )
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

  time_vars <- lapply(info, function(x) {
    nme <- vapply(x[["cols"]], `[[`, character(1L), "name")
    typ <- vapply(x[["cols"]], `[[`, character(1L), "spec")
    nme[typ == "col_integer" & grepl("offset$", nme)]
  })

  names(time_vars) <- vapply(info, `[[`, character(1L), "table_name")

  as_tbl_spec(files, defaults, time_vars, info, part)
}

mimic_tbl_cfg <- function(info, is_demo = FALSE) {

  find_entry <- function(x, what, name) {
    which(vapply(x, `[[`, character(1L), what) == name)
  }

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
      val_var = "admission_type"
    ),
    callout = list(
      index_var = "outcometime",
      val_var = "callout_outcome"
    ),
    caregivers = list(
      id_var = "cgid",
      val_var = "label"
    ),
    chartevents = list(
      index_var = "charttime",
      val_var = "valuenum",
      unit_var = "valueuom"
    ),
    cptevents = list(
      index_var = "chartdate",
      val_var = "cpt_cd"
    ),
    d_cpt = list(
      id_var = "subsectionrange",
      val_var = "subsectionheader"
    ),
    d_icd_diagnoses = list(
      id_var = "icd9_code",
      val_var = "short_title"
    ),
    d_icd_procedures = list(
      id_var = "icd9_code",
      val_var = "short_title"
    ),
    d_items = list(
      id_var = "itemid",
      val_var = "label"
    ),
    d_labitems = list(
      id_var = "itemid",
      val_var = "label"
    ),
    datetimeevents = list(
      index_var = "charttime",
      val_var = "itemid"
    ),
    diagnoses_icd = list(
      val_var = "icd9_code"
    ),
    drgcodes = list(
      val_var = "drg_code"
    ),
    icustays = list(
      index_var = "intime",
      val_var = "last_careunit"
    ),
    inputevents_cv = list(
      index_var = "charttime",
      val_var = "rate",
      unit_var = "rateuom"
    ),
    inputevents_mv = list(
      index_var = "starttime",
      val_var = "rate",
      unit_var = "rateuom"
    ),
    labevents = list(
      index_var = "charttime",
      val_var = "valuenum",
      unit_var = "valueuom"
    ),
    microbiologyevents = list(
      index_var = "chartdate",
      val_var = "isolate_num"
    ),
    noteevents = list(
      index_var = "chartdate",
      val_var = "text"
    ),
    outputevents = list(
      index_var = "charttime",
      val_var = "value",
      unit_var = "valueuom"
    ),
    patients = list(
      val_var = "expire_flag"
    ),
    prescriptions = list(
      index_var = "startdate",
      val_var = "dose_val_rx",
      unit_var = "dose_unit_rx"
    ),
    procedureevents_mv = list(
      index_var = "starttime",
      val_var = "value",
      unit_var = "valueuom"
    ),
    procedures_icd = list(
      val_var = "icd9_code"
    ),
    services = list(
      index_var = "transfertime",
      val_var = "curr_service"
    ),
    transfers = list(
      index_var = "intime",
      val_var = "curr_careunit"
    )
  )

  part <- list(
    chartevents = list(
      itemid = `if`(is_demo,
            100000L,
        c(     127L,    210L,  425L,  549L,    643L,    741L,   1483L,
              3458L,   3695L, 8440L, 8553L, 220274L, 223921L, 224085L,
            224859L, 227629L
        )
      )
    )
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

    icd_diag <- find_entry(info, "table_name", "d_icd_diagnoses")
    info[[icd_diag]]$num_rows <- 14567L

    icd_proc <- find_entry(info, "table_name", "d_icd_procedures")
    info[[icd_proc]]$num_rows <- 3882L

    note <- find_entry(info, "table_name", "noteevents")
    date <- find_entry(info[[note]]$cols, "name", "chartdate")
    info[[note]]$cols[[date]]$format <- "%Y-%m-%d"
  }

  time_vars <- lapply(info, function(x) {
    nme <- vapply(x[["cols"]], `[[`, character(1L), "name")
    typ <- vapply(x[["cols"]], `[[`, character(1L), "spec")
    nme[typ == "col_datetime"]
  })

  names(time_vars) <- vapply(info, `[[`, character(1L), "table_name")

  as_tbl_spec(files, defaults, time_vars, info, part)
}

hirid_tbl_cfg <- function() {

  info <- list(
    general = list(
      patientid = list(spec = "col_integer"),
      admissiontime = list(spec = "col_datetime",
                           format = "%Y-%m-%d %H:%M:%S"),
      sex = list(spec = "col_character"),
      age = list(spec = "col_integer"),
      discharge_status = list(spec = "col_character")
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
    ),
    variables = list(
      `Source Table` = list(spec = "col_character"),
      ID = list(spec = "col_integer"),
      `Variable Name` = list(spec = "col_character"),
      Unit = list(spec = "col_character"),
      `Additional information` = list(spec = "col_character")
    )
  )

  files <- list(
    general = "general_table.csv",
    observations = file.path(
      "observation_tables", "csv", paste0("part-", 0L:249L, ".csv")
    ),
    ordinal = "ordinal_vars_ref.csv",
    pharma = file.path(
      "pharma_records", "csv", paste0("part-", 0L:249L, ".csv")
    ),
    variables = "hirid_variable_reference.csv"
  )

  defaults <- list(
    general = list(
      index_var = "admissiontime"
    ),
    observations = list(
      index_var = "datetime",
      val_var = "value"
    ),
    ordinal = list(
      id_var = "variableid"
    ),
    pharma = list(
      index_var = "givenat",
      val_var = "givendose",
      unit_var = "doseunit"
    ),
    variables = list(
      id_var = "id"
    )
  )

  n_row <- list(
    general = 33905L,
    variables = 712L,
    ordinal = 72L,
    pharma = 16270399L,
    observations = 776921131L
  )

  zip_files <- list(
    general = "reference_data.tar.gz",
    variables = "reference_data.tar.gz",
    ordinal = "reference_data.tar.gz",
    pharma = "raw_stage/pharma_records_csv.tar.gz",
    observations = "raw_stage/observation_tables_csv.tar.gz"
  )

  part <- list(
    observations = list(variableid = c(
       110L,  120L,  200L,  210L,  211L,      300L,      620L,
      2010L, 2610L, 3110L, 4000L, 5685L, 15001565L, 30005075L)
    ),
    pharma = list(pharmaid = 431L)
  )

  info <- lapply(info, function(x) {
    Map(c, Map(list, name = sub(" ", "_", tolower(names(x))),
               col = names(x)), x)
  })

  info <- Map(list, table_name = names(info), cols = info,
              num_rows = n_row[names(info)],
              zip_file = zip_files[names(info)])

  time_vars <- lapply(info, function(x) {
    nme <- vapply(x[["cols"]], `[[`, character(1L), "name")
    typ <- vapply(x[["cols"]], `[[`, character(1L), "spec")
    nme[typ == "col_datetime"]
  })

  names(time_vars) <- vapply(info, `[[`, character(1L), "table_name")

  as_tbl_spec(files, defaults, time_vars, info, part)
}

aumc_tbl_cfg <- function() {

  info <- list(
    admissions = list(
      patientid = list(spec = "col_integer"),
      admissionid = list(spec = "col_integer"),
      admissioncount = list(spec = "col_integer"),
      location = list(spec = "col_character"),
      urgency = list(spec = "col_logical"),
      origin = list(spec = "col_character"),
      admittedat = list(spec = "col_double"),
      admissionyeargroup = list(spec = "col_character"),
      dischargedat = list(spec = "col_double"),
      lengthofstay = list(spec = "col_integer"),
      destination = list(spec = "col_character"),
      gender = list(spec = "col_character"),
      agegroup = list(spec = "col_character"),
      dateofdeath = list(spec = "col_double"),
      weightgroup = list(spec = "col_character"),
      weightsource = list(spec = "col_character"),
      specialty = list(spec = "col_character")
    ),
    drugitems = list(
      admissionid = list(spec = "col_integer"),
      orderid = list(spec = "col_integer"),
      ordercategoryid = list(spec = "col_integer"),
      ordercategory = list(spec = "col_character"),
      itemid = list(spec = "col_integer"),
      item = list(spec = "col_character"),
      isadditive = list(spec = "col_logical"),
      isconditional = list(spec = "col_logical"),
      rate = list(spec = "col_double"),
      rateunit = list(spec = "col_character"),
      rateunitid = list(spec = "col_integer"),
      ratetimeunitid = list(spec = "col_integer"),
      doserateperkg = list(spec = "col_logical"),
      dose = list(spec = "col_double"),
      doseunit = list(spec = "col_character"),
      doserateunit = list(spec = "col_character"),
      doseunitid = list(spec = "col_integer"),
      doserateunitid = list(spec = "col_integer"),
      administered = list(spec = "col_double"),
      administeredunit = list(spec = "col_character"),
      administeredunitid = list(spec = "col_integer"),
      action = list(spec = "col_character"),
      start = list(spec = "col_double"),
      stop = list(spec = "col_double"),
      duration = list(spec = "col_integer"),
      solutionitemid = list(spec = "col_integer"),
      solutionitem = list(spec = "col_character"),
      solutionadministered = list(spec = "col_double"),
      solutionadministeredunit = list(spec = "col_character"),
      fluidin = list(spec = "col_double"),
      iscontinuous = list(spec = "col_logical")
    ),
    freetextitems = list(
      admissionid = list(spec = "col_integer"),
      itemid = list(spec = "col_integer"),
      item = list(spec = "col_character"),
      value = list(spec = "col_character"),
      comment = list(spec = "col_character"),
      measuredat = list(spec = "col_double"),
      registeredat = list(spec = "col_double"),
      registeredby = list(spec = "col_character"),
      updatedat = list(spec = "col_double"),
      updatedby = list(spec = "col_character"),
      islabresult = list(spec = "col_logical")
    ),
    listitems = list(
      admissionid = list(spec = "col_integer"),
      itemid = list(spec = "col_integer"),
      item = list(spec = "col_character"),
      valueid = list(spec = "col_integer"),
      value = list(spec = "col_character"),
      measuredat = list(spec = "col_double"),
      registeredat = list(spec = "col_double"),
      registeredby = list(spec = "col_character"),
      updatedat = list(spec = "col_double"),
      updatedby = list(spec = "col_character"),
      islabresult = list(spec = "col_logical")
    ),
    numericitems = list(
      admissionid = list(spec = "col_integer"),
      itemid = list(spec = "col_integer"),
      item = list(spec = "col_character"),
      tag = list(spec = "col_character"),
      value = list(spec = "col_double"),
      unitid = list(spec = "col_integer"),
      unit = list(spec = "col_character"),
      comment = list(spec = "col_character"),
      measuredat = list(spec = "col_double"),
      registeredat = list(spec = "col_double"),
      registeredby = list(spec = "col_character"),
      updatedat = list(spec = "col_double"),
      updatedby = list(spec = "col_character"),
      islabresult = list(spec = "col_logical"),
      fluidout = list(spec = "col_double")
    ),
    procedureorderitems = list(
      admissionid = list(spec = "col_integer"),
      orderid = list(spec = "col_integer"),
      ordercategoryid = list(spec = "col_integer"),
      ordercategoryname = list(spec = "col_character"),
      itemid = list(spec = "col_integer"),
      item = list(spec = "col_character"),
      registeredat = list(spec = "col_double"),
      registeredby = list(spec = "col_character")
    ),
    processitems = list(
      admissionid = list(spec = "col_integer"),
      itemid = list(spec = "col_integer"),
      item = list(spec = "col_character"),
      start = list(spec = "col_double"),
      stop = list(spec = "col_double"),
      duration = list(spec = "col_integer")
    )
  )

  tables <- names(info)

  cols <- lapply(info, function(tbl) {
    Map(function(name, spec) c(list(name = name), spec), names(tbl), tbl)
  })

  defaults <- list(
    admissions = list(
      index_var = "admittedat",
      time_vars = c("admittedat", "dischargedat", "dateofdeath")
    ),
    drugitems = list(
      index_var = "start",
      val_var = "dose",
      unit_var = "doseunit",
      time_vars = c("start", "stop")
    ),
    freetextitems = list(
      index_var = "measuredat",
      id_var = "value",
      time_vars = c("measuredat", "registeredat", "updatedat")
    ),
    listitems = list(
      index_var = "measuredat",
      val_var = "value",
      time_vars = c("measuredat", "registeredat", "updatedat")
    ),
    numericitems = list(
      index_var = "measuredat",
      val_var = "value",
      unit_var = "unit",
      time_vars = c("measuredat", "registeredat", "updatedat")
    ),
    procedureorderitems = list(
      index_var = "registeredat",
      val_var = "item",
      time_vars = "registeredat"
    ),
    processitems = list(
      index_var = "start",
      val_var = "item",
      time_vars = c("start", "stop")
    )
  )

  n_row <- list(
    admissions = 23106L,
    drugitems = 4907269L,
    freetextitems = 651248L,
    listitems = 30744065L,
    numericitems = 977625612L,
    procedureorderitems = 2188626L,
    processitems = 256715L
  )

  part <- list(
    listitems = list(col = "itemid", breaks = 12290L),
    numericitems = list(col = "itemid", breaks = c(
       6641L,  6642L,  6643L,  6664L,  6666L,  6667L,  6669L,  6672L,  6673L,
       6675L,  6707L,  6709L,  8874L, 12270L, 12275L, 12278L, 12281L, 12286L,
      12303L, 12561L, 12576L, 12804L, 14841L)
    )
  )

  tables <- Map(list, files = setNames(paste0(tables, ".csv"), tables),
                 defaults = defaults[tables], num_rows = n_row[tables],
                 cols = cols[tables])

  tables[names(part)] <- Map(`[[<-`, tables[names(part)], "partitioning", part)

  tables
}

miiv_tbl_cfg <- function() {

  info <- list(
    admissions = list(
      subject_id = list(spec = "col_integer"),
      hadm_id = list(spec = "col_integer"),
      admittime = list(spec = "col_datetime", format = "%Y-%m-%d %H:%M:%S"),
      dischtime = list(spec = "col_datetime", format = "%Y-%m-%d %H:%M:%S"),
      deathtime = list(spec = "col_datetime", format = "%Y-%m-%d %H:%M:%S"),
      admission_type = list(spec = "col_character"),
      admission_location = list(spec = "col_character"),
      discharge_location = list(spec = "col_character"),
      insurance = list(spec = "col_character"),
      language = list(spec = "col_character"),
      marital_status = list(spec = "col_character"),
      ethnicity = list(spec = "col_character"),
      edregtime = list(spec = "col_datetime", format = "%Y-%m-%d %H:%M:%S"),
      edouttime = list(spec = "col_datetime", format = "%Y-%m-%d %H:%M:%S"),
      hospital_expire_flag = list(spec = "col_integer"),
      admit_provider_id = list(spec = "col_character")
    ),
    patients = list(
      subject_id = list(spec = "col_integer"),
      gender = list(spec = "col_character"),
      anchor_age = list(spec = "col_integer"),
      anchor_year = list(spec = "col_integer"),
      anchor_year_group = list(spec = "col_character"),
      dod = list(spec = "col_datetime", format = "%Y-%m-%d")
    ),
    transfers = list(
      subject_id = list(spec = "col_integer"),
      hadm_id = list(spec = "col_integer"),
      transfer_id = list(spec = "col_integer"),
      eventtype = list(spec = "col_character"),
      careunit = list(spec = "col_character"),
      intime = list(spec = "col_datetime", format = "%Y-%m-%d %H:%M:%S"),
      outtime = list(spec = "col_datetime", format = "%Y-%m-%d %H:%M:%S")
    ),
    d_hcpcs = list(
      code = list(spec = "col_character"),
      category = list(spec = "col_integer"),
      long_description = list(spec = "col_character"),
      short_description = list(spec = "col_character")
    ),
    diagnoses_icd = list(
      subject_id = list(spec = "col_integer"),
      hadm_id = list(spec = "col_integer"),
      seq_num = list(spec = "col_integer"),
      icd_code = list(spec = "col_character"),
      icd_version = list(spec = "col_integer")
    ),
    d_icd_diagnoses = list(
      icd_code = list(spec = "col_character"),
      icd_version = list(spec = "col_integer"),
      long_title = list(spec = "col_character")
    ),
    d_icd_procedures = list(
      icd_code = list(spec = "col_character"),
      icd_version = list(spec = "col_integer"),
      long_title = list(spec = "col_character")
    ),
    d_labitems = list(
      itemid = list(spec = "col_integer"),
      label = list(spec = "col_character"),
      fluid = list(spec = "col_character"),
      category = list(spec = "col_character"),
      loinc_code = list(spec = "col_character")
    ),
    drgcodes = list(
      subject_id = list(spec = "col_integer"),
      hadm_id = list(spec = "col_integer"),
      drg_type = list(spec = "col_character"),
      drg_code = list(spec = "col_character"),
      description = list(spec = "col_character"),
      drg_severity = list(spec = "col_integer"),
      drg_mortality = list(spec = "col_integer")
    ),
    emar_detail = list(
      subject_id = list(spec = "col_integer"),
      emar_id = list(spec = "col_character"),
      emar_seq = list(spec = "col_integer"),
      parent_field_ordinal = list(spec = "col_double"),
      administration_type = list(spec = "col_character"),
      pharmacy_id = list(spec = "col_integer"),
      barcode_type = list(spec = "col_character"),
      reason_for_no_barcode = list(spec = "col_character"),
      complete_dose_not_given = list(spec = "col_character"),
      dose_due = list(spec = "col_character"),
      dose_due_unit = list(spec = "col_character"),
      dose_given = list(spec = "col_character"),
      dose_given_unit = list(spec = "col_character"),
      will_remainder_of_dose_be_given = list(spec = "col_character"),
      product_amount_given = list(spec = "col_character"),
      product_unit = list(spec = "col_character"),
      product_code = list(spec = "col_character"),
      product_description = list(spec = "col_character"),
      product_description_other = list(spec = "col_character"),
      prior_infusion_rate = list(spec = "col_character"),
      infusion_rate = list(spec = "col_character"),
      infusion_rate_adjustment = list(spec = "col_character"),
      infusion_rate_adjustment_amount = list(spec = "col_character"),
      infusion_rate_unit = list(spec = "col_character"),
      route = list(spec = "col_character"),
      infusion_complete = list(spec = "col_character"),
      completion_interval = list(spec = "col_character"),
      new_iv_bag_hung = list(spec = "col_character"),
      continued_infusion_in_other_location = list(spec = "col_character"),
      restart_interval = list(spec = "col_character"),
      side = list(spec = "col_character"),
      site = list(spec = "col_character"),
      non_formulary_visual_verification = list(spec = "col_character")
    ),
    emar = list(
      subject_id = list(spec = "col_integer"),
      hadm_id = list(spec = "col_integer"),
      emar_id = list(spec = "col_character"),
      emar_seq = list(spec = "col_integer"),
      poe_id = list(spec = "col_character"),
      pharmacy_id = list(spec = "col_integer"),
      charttime = list(spec = "col_datetime", format = "%Y-%m-%d %H:%M:%S"),
      medication = list(spec = "col_character"),
      event_txt = list(spec = "col_character"),
      scheduletime = list(spec = "col_datetime", format = "%Y-%m-%d %H:%M:%S"),
      storetime = list(spec = "col_datetime", format = "%Y-%m-%d %H:%M:%S"),
      enter_provider_id = list(spec = "col_character")
    ),
    hcpcsevents = list(
      subject_id = list(spec = "col_integer"),
      hadm_id = list(spec = "col_integer"),
      chartdate = list(spec = "col_datetime", format = "%Y-%m-%d"),
      hcpcs_cd = list(spec = "col_character"),
      seq_num = list(spec = "col_integer"),
      short_description = list(spec = "col_character")
    ),
    labevents = list(
      labevent_id = list(spec = "col_integer"),
      subject_id = list(spec = "col_integer"),
      hadm_id = list(spec = "col_integer"),
      specimen_id = list(spec = "col_integer"),
      itemid = list(spec = "col_integer"),
      charttime = list(spec = "col_datetime", format = "%Y-%m-%d %H:%M:%S"),
      storetime = list(spec = "col_datetime", format = "%Y-%m-%d %H:%M:%S"),
      value = list(spec = "col_character"),
      valuenum = list(spec = "col_double"),
      valueuom = list(spec = "col_character"),
      ref_range_lower = list(spec = "col_double"),
      ref_range_upper = list(spec = "col_double"),
      flag = list(spec = "col_character"),
      priority = list(spec = "col_character"),
      comments = list(spec = "col_character"),
      order_provider_id = list(spec = "col_character")
    ),
    microbiologyevents = list(
      microevent_id = list(spec = "col_integer"),
      subject_id = list(spec = "col_integer"),
      hadm_id = list(spec = "col_integer"),
      micro_specimen_id = list(spec = "col_integer"),
      chartdate = list(spec = "col_datetime", format = "%Y-%m-%d %H:%M:%S"),
      charttime = list(spec = "col_datetime", format = "%Y-%m-%d %H:%M:%S"),
      spec_itemid = list(spec = "col_integer"),
      spec_type_desc = list(spec = "col_character"),
      test_seq = list(spec = "col_integer"),
      storedate = list(spec = "col_datetime", format = "%Y-%m-%d %H:%M:%S"),
      storetime = list(spec = "col_datetime", format = "%Y-%m-%d %H:%M:%S"),
      test_itemid = list(spec = "col_integer"),
      test_name = list(spec = "col_character"),
      org_itemid = list(spec = "col_integer"),
      org_name = list(spec = "col_character"),
      isolate_num = list(spec = "col_integer"),
      quantity = list(spec = "col_character"),
      ab_itemid = list(spec = "col_integer"),
      ab_name = list(spec = "col_character"),
      dilution_text = list(spec = "col_character"),
      dilution_comparison = list(spec = "col_character"),
      dilution_value = list(spec = "col_double"),
      interpretation = list(spec = "col_character"),
      comments = list(spec = "col_character"),
      order_provider_id = list(spec = "col_character")
    ),
    pharmacy = list(
      subject_id = list(spec = "col_integer"),
      hadm_id = list(spec = "col_integer"),
      pharmacy_id = list(spec = "col_integer"),
      poe_id = list(spec = "col_character"),
      starttime = list(spec = "col_datetime", format = "%Y-%m-%d %H:%M:%S"),
      stoptime = list(spec = "col_datetime", format = "%Y-%m-%d %H:%M:%S"),
      medication = list(spec = "col_character"),
      proc_type = list(spec = "col_character"),
      status = list(spec = "col_character"),
      entertime = list(spec = "col_datetime", format = "%Y-%m-%d %H:%M:%S"),
      verifiedtime = list(spec = "col_datetime", format = "%Y-%m-%d %H:%M:%S"),
      route = list(spec = "col_character"),
      frequency = list(spec = "col_character"),
      disp_sched = list(spec = "col_character"),
      infusion_type = list(spec = "col_character"),
      sliding_scale = list(spec = "col_character"),
      lockout_interval = list(spec = "col_character"),
      basal_rate = list(spec = "col_double"),
      one_hr_max = list(spec = "col_character"),
      doses_per_24_hrs = list(spec = "col_double"),
      duration = list(spec = "col_double"),
      duration_interval = list(spec = "col_character"),
      expiration_value = list(spec = "col_integer"),
      expiration_unit = list(spec = "col_character"),
      expirationdate = list(spec = "col_datetime",
                            format = "%Y-%m-%d %H:%M:%S"),
      dispensation = list(spec = "col_character"),
      fill_quantity = list(spec = "col_character")
    ),
    poe_detail = list(
      poe_id = list(spec = "col_character"),
      poe_seq = list(spec = "col_integer"),
      subject_id = list(spec = "col_integer"),
      field_name = list(spec = "col_character"),
      field_value = list(spec = "col_character")
    ),
    poe = list(
      poe_id = list(spec = "col_character"),
      poe_seq = list(spec = "col_integer"),
      subject_id = list(spec = "col_integer"),
      hadm_id = list(spec = "col_integer"),
      ordertime = list(spec = "col_datetime", format = "%Y-%m-%d %H:%M:%S"),
      order_type = list(spec = "col_character"),
      order_subtype = list(spec = "col_character"),
      transaction_type = list(spec = "col_character"),
      discontinue_of_poe_id = list(spec = "col_character"),
      discontinued_by_poe_id = list(spec = "col_character"),
      order_status = list(spec = "col_character"),
      order_provider_id = list(spec = "col_character")
    ),
    prescriptions = list(
      subject_id = list(spec = "col_integer"),
      hadm_id = list(spec = "col_integer"),
      pharmacy_id = list(spec = "col_integer"),
      starttime = list(spec = "col_datetime", format = "%Y-%m-%d %H:%M:%S"),
      stoptime = list(spec = "col_datetime", format = "%Y-%m-%d %H:%M:%S"),
      drug_type = list(spec = "col_character"),
      drug = list(spec = "col_character"),
      gsn = list(spec = "col_character"),
      ndc = list(spec = "col_character"),
      prod_strength = list(spec = "col_character"),
      form_rx = list(spec = "col_character"),
      dose_val_rx = list(spec = "col_character"),
      dose_unit_rx = list(spec = "col_character"),
      form_val_disp = list(spec = "col_character"),
      form_unit_disp = list(spec = "col_character"),
      doses_per_24_hrs = list(spec = "col_double"),
      route = list(spec = "col_character"),
      order_provider_id = list(spec = "col_character"),
      formulary_drug_cd = list(spec = "col_character")
    ),
    procedures_icd = list(
      subject_id = list(spec = "col_integer"),
      hadm_id = list(spec = "col_integer"),
      seq_num = list(spec = "col_integer"),
      chartdate = list(spec = "col_datetime", format = "%Y-%m-%d"),
      icd_code = list(spec = "col_character"),
      icd_version = list(spec = "col_integer")
    ),
    services = list(
      subject_id = list(spec = "col_integer"),
      hadm_id = list(spec = "col_integer"),
      transfertime = list(spec = "col_datetime", format = "%Y-%m-%d %H:%M:%S"),
      prev_service = list(spec = "col_character"),
      curr_service = list(spec = "col_character")
    ),
    chartevents = list(
      subject_id = list(spec = "col_integer"),
      hadm_id = list(spec = "col_integer"),
      stay_id = list(spec = "col_integer"),
      charttime = list(spec = "col_datetime", format = "%Y-%m-%d %H:%M:%S"),
      storetime = list(spec = "col_datetime", format = "%Y-%m-%d %H:%M:%S"),
      itemid = list(spec = "col_integer"),
      value = list(spec = "col_character"),
      valuenum = list(spec = "col_double"),
      valueuom = list(spec = "col_character"),
      warning = list(spec = "col_integer"),
      caregiver_id = list(spec = "col_integer")
    ),
    datetimeevents = list(
      subject_id = list(spec = "col_integer"),
      hadm_id = list(spec = "col_integer"),
      stay_id = list(spec = "col_integer"),
      charttime = list(spec = "col_datetime", format = "%Y-%m-%d %H:%M:%S"),
      storetime = list(spec = "col_datetime", format = "%Y-%m-%d %H:%M:%S"),
      itemid = list(spec = "col_integer"),
      value = list(spec = "col_datetime", format = "%Y-%m-%d %H:%M:%S"),
      valueuom = list(spec = "col_character"),
      warning = list(spec = "col_integer"),
      caregiver_id = list(spec = "col_integer")
    ),
    d_items = list(
      itemid = list(spec = "col_integer"),
      label = list(spec = "col_character"),
      abbreviation = list(spec = "col_character"),
      linksto = list(spec = "col_character"),
      category = list(spec = "col_character"),
      unitname = list(spec = "col_character"),
      param_type = list(spec = "col_character"),
      lownormalvalue = list(spec = "col_double"),
      highnormalvalue = list(spec = "col_double")
    ),
    icustays = list(
      subject_id = list(spec = "col_integer"),
      hadm_id = list(spec = "col_integer"),
      stay_id = list(spec = "col_integer"),
      first_careunit = list(spec = "col_character"),
      last_careunit = list(spec = "col_character"),
      intime = list(spec = "col_datetime", format = "%Y-%m-%d %H:%M:%S"),
      outtime = list(spec = "col_datetime", format = "%Y-%m-%d %H:%M:%S"),
      los = list(spec = "col_double")
    ),
    inputevents = list(
      subject_id = list(spec = "col_integer"),
      hadm_id = list(spec = "col_integer"),
      stay_id = list(spec = "col_integer"),
      caregiver_id = list(spec = "col_integer"),
      starttime = list(spec = "col_datetime", format = "%Y-%m-%d %H:%M:%S"),
      endtime = list(spec = "col_datetime", format = "%Y-%m-%d %H:%M:%S"),
      storetime = list(spec = "col_datetime", format = "%Y-%m-%d %H:%M:%S"),
      itemid = list(spec = "col_integer"),
      amount = list(spec = "col_double"),
      amountuom = list(spec = "col_character"),
      rate = list(spec = "col_double"),
      rateuom = list(spec = "col_character"),
      orderid = list(spec = "col_integer"),
      linkorderid = list(spec = "col_integer"),
      ordercategoryname = list(spec = "col_character"),
      secondaryordercategoryname = list(spec = "col_character"),
      ordercomponenttypedescription = list(spec = "col_character"),
      ordercategorydescription = list(spec = "col_character"),
      patientweight = list(spec = "col_double"),
      totalamount = list(spec = "col_double"),
      totalamountuom = list(spec = "col_character"),
      isopenbag = list(spec = "col_integer"),
      continueinnextdept = list(spec = "col_integer"),
      cancelreason = list(spec = "col_integer"),
      statusdescription = list(spec = "col_character"),
      originalamount = list(spec = "col_double"),
      originalrate = list(spec = "col_double")
    ),
    outputevents = list(
      subject_id = list(spec = "col_integer"),
      hadm_id = list(spec = "col_integer"),
      stay_id = list(spec = "col_integer"),
      charttime = list(spec = "col_datetime", format = "%Y-%m-%d %H:%M:%S"),
      storetime = list(spec = "col_datetime", format = "%Y-%m-%d %H:%M:%S"),
      itemid = list(spec = "col_integer"),
      value = list(spec = "col_double"),
      valueuom = list(spec = "col_character"),
      caregiver_id = list(spec = "col_integer")
    ),
    procedureevents = list(
      subject_id = list(spec = "col_integer"),
      hadm_id = list(spec = "col_integer"),
      stay_id = list(spec = "col_integer"),
      starttime = list(spec = "col_datetime", format = "%Y-%m-%d %H:%M:%S"),
      endtime = list(spec = "col_datetime", format = "%Y-%m-%d %H:%M:%S"),
      storetime = list(spec = "col_datetime", format = "%Y-%m-%d %H:%M:%S"),
      itemid = list(spec = "col_integer"),
      value = list(spec = "col_double"),
      valueuom = list(spec = "col_character"),
      location = list(spec = "col_character"),
      locationcategory = list(spec = "col_character"),
      orderid = list(spec = "col_integer"),
      linkorderid = list(spec = "col_integer"),
      ordercategoryname = list(spec = "col_character"),
      secondaryordercategoryname = list(spec = "col_character"),
      ordercategorydescription = list(spec = "col_character"),
      patientweight = list(spec = "col_double"),
      totalamount = list(spec = "col_double"),
      totalamountuom = list(spec = "col_character"),
      isopenbag = list(spec = "col_integer"),
      continueinnextdept = list(spec = "col_integer"),
      cancelreason = list(spec = "col_integer"),
      statusdescription = list(spec = "col_character"),
      comments_date = list(spec = "col_datetime",
                           format = "%Y-%m-%d %H:%M:%S"),
      originalamount = list(spec = "col_double"),
      originalrate = list(spec = "col_double"),
      caregiver_id = list(spec = "col_integer")
    ),
    omr = list(
      subject_id = list(spec = "col_integer"),
      chartdate = list(spec = "col_datetime", format = "%Y-%m-%d"),
      seq_num = list(spec = "col_integer"),
      result_name = list(spec = "col_character"),
      result_value = list(spec = "col_character")
    ),
    caregiver = list(
      caregiver_id = list(spec = "col_integer")
    ),
    provider = list(
      provider_id = list(spec = "col_character")
    ),
    ingredientevents = list(
      subject_id = list(spec = "col_integer"),
      hadm_id = list(spec = "col_integer"),
      stay_id = list(spec = "col_integer"),
      caregiver_id = list(spec = "col_integer"),
      starttime = list(spec = "col_datetime", format = "%Y-%m-%d %H:%M:%S"),
      endtime = list(spec = "col_datetime", format = "%Y-%m-%d %H:%M:%S"),
      storetime = list(spec = "col_datetime", format = "%Y-%m-%d %H:%M:%S"),
      itemid = list(spec = "col_integer"),
      amount = list(spec = "col_double"),
      amountuom = list(spec = "col_character"),
      rate = list(spec = "col_double"),
      rateuom = list(spec = "col_character"),
      orderid = list(spec = "col_integer"),
      linkorderid = list(spec = "col_integer"),
      statusdescription = list(spec = "col_character"),
      originalamount = list(spec = "col_double"),
      originalrate = list(spec = "col_double")
    )
  )

  tables <- names(info)

  cols <- lapply(info, function(tbl) {
    Map(function(name, spec) c(list(name = name), spec), names(tbl), tbl)
  })

  defaults <- list(
    admissions = list(
      val_var = "admission_type"
    ),
    d_hcpcs = list(
      id_var = "code",
      val_var = "short_description"
    ),
    d_icd_diagnoses = list(
      id_var = "icd_code",
      val_var = "long_title"
    ),
    d_icd_procedures = list(
      id_var = "icd_code",
      val_var = "long_title"
    ),
    d_labitems = list(
      id_var = "itemid",
      val_var = "label"
    ),
    diagnoses_icd = list(
      val_var = "icd_code"
    ),
    drgcodes = list(
      val_var = "drg_code"
    ),
    emar_detail = list(
      id_var = "emar_id"
    ),
    emar = list(
      index_var = "charttime"
    ),
    hcpcsevents = list(
      index_var = "chartdate"
    ),
    labevents = list(
      index_var = "charttime",
      val_var = "valuenum",
      unit_var = "valueuom"
    ),
    microbiologyevents = list(
      index_var = "chartdate",
      val_var = "isolate_num"
    ),
    pharmacy = list(
      id_var = "pharmacy_id",
      index_var = "starttime",
      val_var = "duration",
      unit_var = "duration_interval"
    ),
    poe_detail = list(
      id_var = "poe_id"
    ),
    poe = list(
      index_var = "ordertime"
    ),
    prescriptions = list(
      index_var = "starttime",
      val_var = "dose_val_rx",
      unit_var = "dose_unit_rx"
    ),
    procedures_icd = list(
      index_var = "chartdate",
      val_var = "icd_code"
    ),
    services = list(
      index_var = "transfertime",
      val_var = "curr_service"
    ),
    chartevents = list(
      index_var = "charttime",
      val_var = "valuenum",
      unit_var = "valueuom"
    ),
    d_items = list(
      id_var = "itemid",
      val_var = "label"
    ),
    datetimeevents = list(
      index_var = "charttime",
      val_var = "itemid"
    ),
    icustays = list(
      index_var = "intime",
      val_var = "last_careunit"
    ),
    inputevents = list(
      index_var = "starttime",
      val_var = "rate",
      unit_var = "rateuom"
    ),
    outputevents = list(
      index_var = "charttime",
      val_var = "value",
      unit_var = "valueuom"
    ),
    procedureevents = list(
      index_var = "starttime",
      val_var = "value",
      unit_var = "valueuom"
    ),
    omr = list(
      index_var = "chartdate",
      val_var = "result_value"
    ),
    caregiver = list(),
    provider = list(),
    ingredientevents = list(
      index_var = "starttime",
      val_var = "rate",
      unit_var = "rateuom"
    )
  )

  defaults <- Map(function(cl, df) {
    nme <- vapply(cl, `[[`, character(1L), "name")
    typ <- vapply(cl, `[[`, character(1L), "spec")
    tim <- nme[typ == "col_datetime"]
    if (length(tim)) c(df, list(time_vars = tim)) else df
  }, cols[tables], defaults[tables])

  n_row <- c(
    admissions = 431231L,
    patients = 299712L,
    transfers = 1890972L,
    d_hcpcs = 89200L,
    d_icd_diagnoses = 109775L,
    d_icd_procedures = 85257L,
    d_labitems = 1622L,
    diagnoses_icd = 4756326L,
    drgcodes = 604377L,
    emar_detail = 54744789L,
    emar = 26850359L,
    hcpcsevents = 150771L,
    labevents = 118171367L,
    microbiologyevents = 3228713L,
    pharmacy = 13584514L,
    poe_detail = 3879418L,
    poe = 39366291L,
    prescriptions = 15416708L,
    procedures_icd = 669186L,
    services = 468029L,
    chartevents = 313645063L,
    d_items = 4014L,
    datetimeevents = 7112999L,
    icustays = 73181L,
    inputevents = 8978893L,
    outputevents = 4234967L,
    procedureevents = 696092L,
    omr = 6439169L,
    caregiver = 15468L,
    provider = 40508L,
    ingredientevents = 11627821L
  )

  files <- c(
    admissions = "hosp/admissions.csv.gz",
    patients = "hosp/patients.csv.gz",
    transfers = "hosp/transfers.csv.gz",
    d_hcpcs = "hosp/d_hcpcs.csv.gz",
    d_icd_diagnoses = "hosp/d_icd_diagnoses.csv.gz",
    d_icd_procedures = "hosp/d_icd_procedures.csv.gz",
    d_labitems = "hosp/d_labitems.csv.gz",
    diagnoses_icd = "hosp/diagnoses_icd.csv.gz",
    drgcodes = "hosp/drgcodes.csv.gz",
    emar_detail = "hosp/emar_detail.csv.gz",
    emar = "hosp/emar.csv.gz",
    hcpcsevents = "hosp/hcpcsevents.csv.gz",
    labevents = "hosp/labevents.csv.gz",
    microbiologyevents = "hosp/microbiologyevents.csv.gz",
    pharmacy = "hosp/pharmacy.csv.gz",
    poe_detail = "hosp/poe_detail.csv.gz",
    poe = "hosp/poe.csv.gz",
    prescriptions = "hosp/prescriptions.csv.gz",
    procedures_icd = "hosp/procedures_icd.csv.gz",
    services = "hosp/services.csv.gz",
    chartevents = "icu/chartevents.csv.gz",
    d_items = "icu/d_items.csv.gz",
    datetimeevents = "icu/datetimeevents.csv.gz",
    icustays = "icu/icustays.csv.gz",
    inputevents = "icu/inputevents.csv.gz",
    outputevents = "icu/outputevents.csv.gz",
    procedureevents = "icu/procedureevents.csv.gz",
    omr = "hosp/omr.csv.gz",
    caregiver = "icu/caregiver.csv.gz",
    provider = "hosp/provider.csv.gz",
    ingredientevents = "icu/ingredientevents.csv.gz"
  )

  part <- list(
    labevents = list(
      col = "itemid",
      breaks = c(
        50868L, 50902L, 50943L, 50983L, 51146L, 51248L, 51256L, 51279L,
        51491L
      )
    ),
    chartevents = list(
      col = "itemid",
      breaks = c(
        220048L, 220059L, 220181L, 220228L, 220615L, 223782L, 223835L,
        223905L, 223962L, 223990L, 224015L, 224055L, 224082L, 224093L,
        224328L, 224650L, 224701L, 224850L, 225072L, 226104L, 227240L,
        227467L, 227950L, 227960L, 228004L, 228397L, 228594L, 228924L,
        229124L
      )
    ),
    poe = list(
      col = "subject_id",
      breaks = c(12017899L, 13999829L, 15979442L, 17994364L)
    )
  )

  tables <- Map(list, files = files[tables],
                 defaults = defaults[tables], num_rows = n_row[tables],
                 cols = cols[tables])

  tables[names(part)] <- Map(`[[<-`, tables[names(part)], "partitioning", part)

  tables
}

sic_tbl_cfg <- function() {
  
  info <- list(
    cases = list(
      caseid = list(name = "CaseID", spec = "col_integer"),
      patientid = list(name = "PatientID", spec = "col_integer"),
      admissionyear = list(name = "AdmissionYear", spec = "col_integer"),
      timeofstay = list(name = "TimeOfStay", spec = "col_integer"),
      icuoffset = list(name = "ICUOffset", spec = "col_integer"),
      saps3 = list(name = "saps3", spec = "col_double"),
      hospitaldischargetype = list(name = "HospitalDischargeType",
                                   spec = "col_integer"),
      dischargestate = list(name = "DischargeState",
                            spec = "col_integer"),
      dischargeunit = list(name = "DischargeUnit",
                           spec = "col_integer"),
      offsetofdeath = list(name = "OffsetOfDeath",
                           spec = "col_integer"),
      estimatedsurvivalobservationtime = list(name = "EstimatedSurvivalObservationTime", 
                                              spec = "col_integer"),
      sex = list(name = "Sex", spec = "col_integer"),
      weightonadmission = list(name = "WeightOnAdmission", spec = "col_double"),
      heightonadmission = list(name = "HeightOnAdmission", spec = "col_double"),
      ageonadmission = list(name = "AgeOnAdmission", spec = "col_integer"),
      hospitalunit = list(name = "HospitalUnit", spec = "col_integer"),
      referringunit = list(name = "ReferringUnit", spec = "col_integer"),
      icd10main = list(name = "ICD10Main", spec = "col_character"),
      icd10maintext = list(name = "ICD10MainText", spec = "col_character"),
      diagnosist2 = list(name = "DiagnosisT2", spec = "col_character"),
      surgicalsite = list(name = "SurgicalSite", spec = "col_integer"),
      hoursofcrrt = list(name = "HoursOfCRRT", spec = "col_integer"),
      admissionformhassepsis = list(name = "AdmissionFormHasSepsis", spec = "col_integer"),
      orbisdataavailable = list(name = "OrbisDataAvailable", spec = "col_character"),
      heartsurgeryadditionaldata = list(name = "HeartSurgeryAdditionalData",
                                        spec = "col_integer"),
      heartsurgerycrossclamptime = list(name = "HeartSurgeryCrossClampTime", 
                                        spec = "col_integer"),
      heartsurgerybeginoffset = list(name = "HeartSurgeryBeginOffset",
                                     spec = "col_integer"),
      heartsurgeryendoffset = list(name = "HeartSurgeryEndOffset",
                                   spec = "col_integer"),
      offsetafterfirstadmission = list(name = "OffsetAfterFirstAdmission", 
                                       spec = "col_integer")
    ),
    d_references = list(
      referenceglobalid = list(name = "ReferenceGlobalID",
                               spec = "col_integer"),
      referencevalue = list(name = "ReferenceValue",
                            spec = "col_character"),
      referencename = list(name = "ReferenceName",
                           spec = "col_character"),
      referencedescription = list(name = "ReferenceDescription", spec = "col_character"),
      referenceunit = list(name = "ReferenceUnit", spec = "col_character"),
      referenceorder = list(name = "ReferenceOrder", spec = "col_integer"),
      referencetype = list(name = "ReferenceType", spec = "col_integer"),
      data = list(name = "Data", spec = "col_character")
    ),
    data_float_h = list(
      id = list(name = "id", spec = "col_integer"),
      caseid = list(name = "CaseID",
                    spec = "col_integer"),
      dataid = list(name = "DataID",
                    spec = "col_integer"),
      offset = list(name = "Offset",
                    spec = "col_integer"),
      val = list(name = "Val", spec = "col_double"),
      cnt = list(name = "cnt", spec = "col_integer"),
      rawdata = list(name = "rawdata", spec = "col_double")
    ),
    data_ref = list(
      id = list(name = "id", spec = "col_integer"),
      caseid = list(name = "CaseID", spec = "col_integer"),
      refid = list(name = "RefID", spec = "col_integer"),
      customfieldid = list(name = "CustomFieldID", spec = "col_integer")
    ),
    laboratory = list(
      id = list(name = "id", spec = "col_integer"),
      caseid = list(name = "CaseID", spec = "col_integer"),
      laboratoryid = list(name = "LaboratoryID", spec = "col_integer"),
      offset = list(name = "Offset", spec = "col_integer"),
      laboratoryvalue = list(name = "LaboratoryValue", spec = "col_double"),
      laboratorytype = list(name = "LaboratoryType", spec = "col_integer")
    ),
    medication = list(
      id = list(name = "id", spec = "col_integer"),
      caseid = list(name = "CaseID",
                    spec = "col_integer"),
      patientid = list(name = "PatientID",
                       spec = "col_integer"),
      drugid = list(name = "DrugID",
                    spec = "col_integer"),
      offset = list(name = "Offset",
                    spec = "col_integer"),
      offsetdrugend = list(name = "OffsetDrugEnd",
                           spec = "col_integer"),
      issingledose = list(name = "IsSingleDose",
                          spec = "col_logical"),
      amount = list(name = "Amount",
                    spec = "col_double"),
      amountperminute = list(name = "AmountPerMinute",
                             spec = "col_double"),
      givenstate = list(name = "GivenState",
                        spec = "col_integer")
    ),
    data_range = list(
      id = list(name = "id",
                spec = "col_integer"),
      caseid = list(name = "CaseID",
                    spec = "col_integer"),
      dataid = list(name = "DataID",
                    spec = "col_integer"),
      offset = list(name = "Offset",
                    spec = "col_integer"),
      offsetend = list(name = "OffsetEnd",
                       spec = "col_integer"),
      data = list(name = "Data",
                  spec = "col_character")
    ),
    unitlog = list(
      id = list(name = "id",
                spec = "col_integer"),
      caseid = list(name = "CaseID",
                    spec = "col_integer"),
      patientid = list(name = "PatientID",
                       spec = "col_integer"),
      logstate = list(name = "LogState",
                      spec = "col_integer"),
      offset = list(name = "Offset",
                    spec = "col_integer"),
      hospitalunit = list(name = "HospitalUnit",
                          spec = "col_integer")
    ),
    microbiology = list(
      caseid = list(name = "CaseID", spec = "col_integer"),
      offset = list(name = "Offset", spec = "col_integer"),
      timeofstay = list(name = "TimeOfStay", spec = "col_integer")
    ),
    gcs = list(
      caseid = list(name = "CaseID", spec = "col_integer"),
      admissionformgcs = list(name = "AdmissionFormGCS", spec = "col_integer"),
      offset = list(name = "Offset", spec = "col_integer")
    ),
    rass = list(
      caseid = list(name = "CaseID", spec = "col_integer"),
      dataid = list(name = "DataID", spec = "col_integer"),
      offset = list(name = "Offset", spec = "col_integer"),
      offseth = list(name = "OffsetH", spec = "col_integer"),
      val = list(name = "Val", spec = "col_double"),
      cnt = list(name = "cnt", spec = "col_integer"),
      rawdata = list(name = "rawdata", spec = "col_double")
    )
  )
  
  tables <- names(info)
  cols <- info
  
  defaults <- list(
    cases = list(
      index_var = "ICUOffset"
    ),
    d_references = list(),
    data_float_h = list(
      index_var = "Offset",
      val_var = "Val"
    ),
    data_ref = list(
      index_var = "OffsetAfterFirstAdmission"
    ),
    laboratory = list(
      index_var = "Offset",
      val_var = "LaboratoryValue"
    ),
    medication = list(
      index_var = "Offset",
      val_var = "Amount"
    ),
    data_range = list(
      index_var = "Offset"
    ),
    unitlog = list(
      index_var = "Offset"
    ),
    microbiology = list(
      index_var = "Offset"
    ),
    gcs = list(
      index_var = "Offset"
    ),
    rass = list(
      index_var = "Offset",
      val_var = "Val"
    )
  )
  
  defaults <- Map(function(cl, df) {
    nme <- vapply(cl, `[[`, character(1L), "name")
    typ <- vapply(cl, `[[`, character(1L), "spec")
    tim <- nme[grep("offset", names(nme), ignore.case = TRUE)]
    if (length(tim)) c(df, list(time_vars = tim)) else df
  }, cols[tables], defaults[tables])
  
  n_row <- c(
    cases = 27386L, 
    d_references = 1608L, 
    data_float_h = 36785241L, 
    data_range = 183339L, 
    data_ref = 354157L, 
    laboratory = 17572279L, 
    medication = 5141346L, 
    unitlog = 139968L, 
    microbiology = 28935L, 
    gcs = 17766L, 
    rass = 539398L
  )
  
  files <- c(
    cases = "cases.csv.gz", 
    d_references = "d_references.csv.gz", 
    data_float_h = "data_float_h.csv.gz", 
    data_range = "data_range.csv.gz", 
    data_ref = "data_ref.csv.gz", 
    laboratory = "laboratory.csv.gz", 
    medication = "medication.csv.gz", 
    unitlog = "unitlog.csv.gz", 
    microbiology = "microbiology.csv.gz", 
    gcs = "gcs.csv.gz", 
    rass = "rass.csv.gz"
  )
  
  part <- list(
    data_float_h = list(
      col = "dataid",
      breaks = c(
        1L, 2L, 3L, 4L, 7L, 28L, 29L, 702L, 703L, 705L, 708L, 709L, 710L, 715L, 
        717L, 719L, 724L, 725L, 731L, 773L, 2018L, 2274L, 2278L, 2280L, 2283L, 
        2290L, 3056L, 3059L, 3071L
      )
    )
  )
  
  tables <- Map(list, files = files[tables],
                defaults = defaults[tables], num_rows = n_row[tables],
                cols = cols[tables])
  
  tables[names(part)] <- Map(`[[<-`, tables[names(part)], "partitioning", part)
  
  tables
}

pkg_dir <- rprojroot::find_root(rprojroot::is_r_package)
cfg_dir <- file.path(pkg_dir, "inst", "extdata", "config")

eicu_id_cfg <- list(
  hadm = list(id = "patienthealthsystemstayid", position = 1L,
              start = "hospitaladmitoffset",
              end = "hospitaldischargeoffset", table = "patient"),
  icustay = list(id = "patientunitstayid", position = 2L,
                 start = "unitadmitoffset", end = "unitdischargeoffset",
                 table = "patient")
)

mimic_id_cfg <- list(
  patient = list(id = "subject_id", position = 1L, start = "dob",
                 end = "dod", table = "patients"),
  hadm = list(id = "hadm_id", position = 2L, start = "admittime",
              end = "dischtime", table = "admissions"),
  icustay = list(id = "icustay_id", position = 3L, start = "intime",
                 end = "outtime", table = "icustays")
)

eicu <- get_table_info(
  "https://mit-lcp.github.io/eicu-schema-spy/eicu.eicu_crd.xml"
)

mimic <- get_table_info(
  "https://mit-lcp.github.io/mimic-schema-spy/mimic.mimiciii.xml"
)

eicu_demo_tbls <- eicu_tbl_cfg(eicu, is_demo = TRUE)
mimic_demo_tbls <- mimic_tbl_cfg(mimic, is_demo = TRUE)

cfg <- list(
  list(
    name = "eicu",
    url = "https://physionet.org/files/eicu-crd/2.0",
    id_cfg = eicu_id_cfg,
    tables = eicu_tbl_cfg(eicu, is_demo = FALSE)
  ),
  list(
    name = "eicu_demo",
    class_prefix = c("eicu_demo", "eicu"),
    url = "https://physionet.org/files/eicu-crd-demo/2.0.1",
    id_cfg = eicu_id_cfg,
    tables = eicu_demo_tbls
  ),
  list(
    name = "mimic",
    url = "https://physionet.org/files/mimiciii/1.4",
    id_cfg = mimic_id_cfg,
    tables = mimic_tbl_cfg(mimic, is_demo = FALSE)
  ),
  list(
    name = "mimic_demo",
    class_prefix = c("mimic_demo", "mimic"),
    url = "https://physionet.org/files/mimiciii-demo/1.4",
    id_cfg = mimic_id_cfg,
    tables = mimic_demo_tbls
  ),
  list(
    name = "miiv",
    url = "https://physionet.org/files/mimiciv/2.2",
    id_cfg = list(
      patient = list(id = "subject_id", position = 1L, start = "anchor_year",
                     end = "dod", table = "patients"),
      hadm = list(id = "hadm_id", position = 2L, start = "admittime",
                  end = "dischtime", table = "admissions"),
      icustay = list(id = "stay_id", position = 3L, start = "intime",
                     end = "outtime", table = "icustays")
    ),
    tables = miiv_tbl_cfg()
  ),
  list(
    name = "hirid",
    url = "https://physionet.org/files/hirid/1.1.1",
    id_cfg = list(
      icustay = list(id = "patientid", position = 1L, start = "admissiontime",
                     table = "general")
    ),
    tables = hirid_tbl_cfg()
  ),
  list(
    name = "aumc",
    id_cfg = list(
      patient = list(id = "patientid", position = 1L,
                     start = "firstadmittedat", end = "dateofdeath",
                     table = "admissions"),
      icustay = list(id = "admissionid", position = 2L, start = "admittedat",
                     end = "dischargedat", table = "admissions")
    ),
    unit_mapping = list(
      list(symbol = "uur", def = "1 hour"),
      list(symbol = "dag", def = "1 day")
    ),
    tables = aumc_tbl_cfg()
  ),
  list(
    name = "sic",
    url = "https://physionet.org/files/sicdb/1.0.6",
    id_cfg = list(
      patient = list(id = "PatientID", position = 1L,
                     start = "ICUOffset", end = "OffsetOfDeath",
                     table = "cases"),
      icustay = list(id = "CaseID", position = 2L, start = "ICUOffset",
                     end = "TimeOfStay",
                     table = "cases")
    ),
    tables = sic_tbl_cfg()
  )
)

ricu::set_config(cfg, "data-sources", cfg_dir)

cfg <- list(
  list(
    name = "eicu_test",
    class_prefix = c("eicu_test", "eicu_demo", "eicu"),
    id_cfg = eicu_id_cfg,
    tables = lapply(eicu_demo_tbls, as_minimal_tbl_spec)
  ),
  list(
    name = "mimic_test",
    class_prefix = c("mimic_test", "mimic_demo", "mimic"),
    id_cfg = mimic_id_cfg,
    tables = lapply(mimic_demo_tbls, as_minimal_tbl_spec)
  )
)

ricu::set_config(cfg, "data-sources", file.path(pkg_dir, "inst", "testdata"))

devtools::install(pkg_dir)
