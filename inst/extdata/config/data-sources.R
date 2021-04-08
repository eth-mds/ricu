
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
    pharma= file.path(
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
    url = "https://physionet.org/files/eicu-crd-demo/2.0",
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
    name = "hirid",
    url = "https://physionet.org/files/hirid/1.1",
    unzip = c(
      "reference_data.tar.gz",
      "raw_stage/observation_tables_csv.tar.gz",
      "raw_stage/pharma_records_csv.tar.gz"
    ),
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
    tables = aumc_tbl_cfg()
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
