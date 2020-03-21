
mk_cfg <- function(is_demo = FALSE) {
  res <- list(
    ADMISSIONS = list(
      col_spec = list(
        ROW_ID = list(type = "col_integer"),
        SUBJECT_ID = list(type = "col_integer"),
        HADM_ID = list(type = "col_integer"),
        ADMITTIME = list(type = "col_datetime", format = "%Y-%m-%d %H:%M:%S"),
        DISCHTIME = list(type = "col_datetime", format = "%Y-%m-%d %H:%M:%S"),
        DEATHTIME = list(type = "col_datetime", format = "%Y-%m-%d %H:%M:%S"),
        ADMISSION_TYPE = list(type = "col_character"),
        ADMISSION_LOCATION = list(type = "col_character"),
        DISCHARGE_LOCATION = list(type = "col_character"),
        INSURANCE = list(type = "col_character"),
        LANGUAGE = list(type = "col_character"),
        RELIGION = list(type = "col_character"),
        MARITAL_STATUS = list(type = "col_character"),
        ETHNICITY = list(type = "col_character"),
        EDREGTIME = list(type = "col_datetime", format = "%Y-%m-%d %H:%M:%S"),
        EDOUTTIME = list(type = "col_datetime", format = "%Y-%m-%d %H:%M:%S"),
        DIAGNOSIS = list(type = "col_character"),
        HOSPITAL_EXPIRE_FLAG = list(type = "col_integer"),
        HAS_CHARTEVENTS_DATA = list(type = "col_integer")
      )
    ),
    CALLOUT = list(
      col_spec = list(
        ROW_ID = list(type = "col_integer"),
        SUBJECT_ID = list(type = "col_integer"),
        HADM_ID = list(type = "col_integer"),
        SUBMIT_WARDID = list(type = "col_integer"),
        SUBMIT_CAREUNIT = list(type = "col_character"),
        CURR_WARDID = list(type = "col_integer"),
        CURR_CAREUNIT = list(type = "col_character"),
        CALLOUT_WARDID = list(type = "col_integer"),
        CALLOUT_SERVICE = list(type = "col_character"),
        REQUEST_TELE = list(type = "col_integer"),
        REQUEST_RESP = list(type = "col_integer"),
        REQUEST_CDIFF = list(type = "col_integer"),
        REQUEST_MRSA = list(type = "col_integer"),
        REQUEST_VRE = list(type = "col_integer"),
        CALLOUT_STATUS = list(type = "col_character"),
        CALLOUT_OUTCOME = list(type = "col_character"),
        DISCHARGE_WARDID = list(type = "col_integer"),
        ACKNOWLEDGE_STATUS = list(type = "col_character"),
        CREATETIME = list(type = "col_datetime", format = "%Y-%m-%d %H:%M:%S"),
        UPDATETIME = list(type = "col_datetime", format = "%Y-%m-%d %H:%M:%S"),
        ACKNOWLEDGETIME = list(type = "col_datetime",
                               format = "%Y-%m-%d %H:%M:%S"),
        OUTCOMETIME = list(type = "col_datetime",
                           format = "%Y-%m-%d %H:%M:%S"),
        FIRSTRESERVATIONTIME = list(type = "col_datetime",
                                    format = "%Y-%m-%d %H:%M:%S"),
        CURRENTRESERVATIONTIME = list(type = "col_datetime",
                                      format = "%Y-%m-%d %H:%M:%S")
      )
    ),
    CAREGIVERS = list(
      col_spec = list(
        ROW_ID = list(type = "col_integer"),
        CGID = list(type = "col_integer"),
        LABEL = list(type = "col_character"),
        DESCRIPTION = list(type = "col_character")
      )
    ),
    CHARTEVENTS = list(
      col_spec = list(
        ROW_ID = list(type = "col_integer"),
        SUBJECT_ID = list(type = "col_integer"),
        HADM_ID = list(type = "col_integer"),
        ICUSTAY_ID = list(type = "col_integer"),
        ITEMID = list(type = "col_integer"),
        CHARTTIME = list(type = "col_datetime", format = "%Y-%m-%d %H:%M:%S"),
        STORETIME = list(type = "col_datetime", format = "%Y-%m-%d %H:%M:%S"),
        CGID = list(type = "col_integer"),
        VALUE = list(type = "col_character"),
        VALUENUM = list(type = "col_double"),
        VALUEUOM = list(type = "col_character"),
        WARNING = list(type = "col_integer"),
        ERROR = list(type = "col_integer"),
        RESULTSTATUS = list(type = "col_character"),
        STOPPED = list(type = "col_character")
      ),
      partitioning = list(
        ITEMID = `if`(is_demo,
          c(0L, 100000L, 999999999L),
          c(
            0L, 127L, 210L, 425L, 549L, 643L, 741L, 1483L, 3458L, 3695L, 8440L,
            8553L, 220274L, 223921L, 224085L, 224859L, 227629L, 999999999L
          )
        )
      )
    ),
    CPTEVENTS = list(
      col_spec = list(
        ROW_ID = list(type = "col_integer"),
        SUBJECT_ID = list(type = "col_integer"),
        HADM_ID = list(type = "col_integer"),
        COSTCENTER = list(type = "col_character"),
        CHARTDATE = list(type = "col_datetime", format = "%Y-%m-%d %H:%M:%S"),
        CPT_CD = list(type = "col_character"),
        CPT_NUMBER = list(type = "col_integer"),
        CPT_SUFFIX = list(type = "col_character"),
        TICKET_ID_SEQ = list(type = "col_integer"),
        SECTIONHEADER = list(type = "col_character"),
        SUBSECTIONHEADER = list(type = "col_character"),
        DESCRIPTION = list(type = "col_character")
      )
    ),
    DATETIMEEVENTS = list(
      col_spec = list(
        ROW_ID = list(type = "col_integer"),
        SUBJECT_ID = list(type = "col_integer"),
        HADM_ID = list(type = "col_integer"),
        ICUSTAY_ID = list(type = "col_integer"),
        ITEMID = list(type = "col_integer"),
        CHARTTIME = list(type = "col_datetime", format = "%Y-%m-%d %H:%M:%S"),
        STORETIME = list(type = "col_datetime", format = "%Y-%m-%d %H:%M:%S"),
        CGID = list(type = "col_integer"),
        VALUE = list(type = "col_datetime", format = "%Y-%m-%d %H:%M:%S"),
        VALUEUOM = list(type = "col_character"),
        WARNING = list(type = "col_integer"),
        ERROR = list(type = "col_integer"),
        RESULTSTATUS = list(type = "col_character"),
        STOPPED = list(type = "col_character")
      )
    ),
    DIAGNOSES_ICD = list(
      col_spec = list(
        ROW_ID = list(type = "col_integer"),
        SUBJECT_ID = list(type = "col_integer"),
        HADM_ID = list(type = "col_integer"),
        SEQ_NUM = list(type = "col_integer"),
        ICD9_CODE = list(type = "col_character")
      )
    ),
    DRGCODES = list(
      col_spec = list(
        ROW_ID = list(type = "col_integer"),
        SUBJECT_ID = list(type = "col_integer"),
        HADM_ID = list(type = "col_integer"),
        DRG_TYPE = list(type = "col_character"),
        DRG_CODE = list(type = "col_character"),
        DESCRIPTION = list(type = "col_character"),
        DRG_SEVERITY = list(type = "col_integer"),
        DRG_MORTALITY = list(type = "col_integer")
      )
    ),
    D_CPT = list(
      col_spec = list(
        ROW_ID = list(type = "col_integer"),
        CATEGORY = list(type = "col_integer"),
        SECTIONRANGE = list(type = "col_character"),
        SECTIONHEADER = list(type = "col_character"),
        SUBSECTIONRANGE = list(type = "col_character"),
        SUBSECTIONHEADER = list(type = "col_character"),
        CODESUFFIX = list(type = "col_character"),
        MINCODEINSUBSECTION = list(type = "col_integer"),
        MAXCODEINSUBSECTION = list(type = "col_integer")
      )
    ),
    D_ICD_DIAGNOSES = list(
      col_spec = list(
        ROW_ID = list(type = "col_integer"),
        ICD9_CODE = list(type = "col_character"),
        SHORT_TITLE = list(type = "col_character"),
        LONG_TITLE = list(type = "col_character")
      )
    ),
    D_ICD_PROCEDURES = list(
      col_spec = list(
        ROW_ID = list(type = "col_integer"),
        ICD9_CODE = list(type = "col_character"),
        SHORT_TITLE = list(type = "col_character"),
        LONG_TITLE = list(type = "col_character")
      )
    ),
    D_ITEMS = list(
      col_spec = list(
        ROW_ID = list(type = "col_integer"),
        ITEMID = list(type = "col_integer"),
        LABEL = list(type = "col_character"),
        ABBREVIATION = list(type = "col_character"),
        DBSOURCE = list(type = "col_character"),
        LINKSTO = list(type = "col_character"),
        CATEGORY = list(type = "col_character"),
        UNITNAME = list(type = "col_character"),
        PARAM_TYPE = list(type = "col_character"),
        CONCEPTID = list(type = "col_integer")
      )
    ),
    D_LABITEMS = list(
      col_spec = list(
        ROW_ID = list(type = "col_integer"),
        ITEMID = list(type = "col_integer"),
        LABEL = list(type = "col_character"),
        FLUID = list(type = "col_character"),
        CATEGORY = list(type = "col_character"),
        LOINC_CODE = list(type = "col_character")
      )
    ),
    ICUSTAYS = list(
      col_spec = list(
        ROW_ID = list(type = "col_integer"),
        SUBJECT_ID = list(type = "col_integer"),
        HADM_ID = list(type = "col_integer"),
        ICUSTAY_ID = list(type = "col_integer"),
        DBSOURCE = list(type = "col_character"),
        FIRST_CAREUNIT = list(type = "col_character"),
        LAST_CAREUNIT = list(type = "col_character"),
        FIRST_WARDID = list(type = "col_integer"),
        LAST_WARDID = list(type = "col_integer"),
        INTIME = list(type = "col_datetime", format = "%Y-%m-%d %H:%M:%S"),
        OUTTIME = list(type = "col_datetime", format = "%Y-%m-%d %H:%M:%S"),
        LOS = list(type = "col_double")
      )
    ),
    INPUTEVENTS_CV = list(
      col_spec = list(
        ROW_ID = list(type = "col_integer"),
        SUBJECT_ID = list(type = "col_integer"),
        HADM_ID = list(type = "col_integer"),
        ICUSTAY_ID = list(type = "col_integer"),
        CHARTTIME = list(type = "col_datetime", format = "%Y-%m-%d %H:%M:%S"),
        ITEMID = list(type = "col_integer"),
        AMOUNT = list(type = "col_double"),
        AMOUNTUOM = list(type = "col_character"),
        RATE = list(type = "col_double"),
        RATEUOM = list(type = "col_character"),
        STORETIME = list(type = "col_datetime", format = "%Y-%m-%d %H:%M:%S"),
        CGID = list(type = "col_integer"),
        ORDERID = list(type = "col_integer"),
        LINKORDERID = list(type = "col_integer"),
        STOPPED = list(type = "col_character"),
        NEWBOTTLE = list(type = "col_integer"),
        ORIGINALAMOUNT = list(type = "col_double"),
        ORIGINALAMOUNTUOM = list(type = "col_character"),
        ORIGINALROUTE = list(type = "col_character"),
        ORIGINALRATE = list(type = "col_double"),
        ORIGINALRATEUOM = list(type = "col_character"),
        ORIGINALSITE = list(type = "col_character")
      )
    ),
    INPUTEVENTS_MV = list(
      col_spec = list(
        ROW_ID = list(type = "col_integer"),
        SUBJECT_ID = list(type = "col_integer"),
        HADM_ID = list(type = "col_integer"),
        ICUSTAY_ID = list(type = "col_integer"),
        STARTTIME = list(type = "col_datetime", format = "%Y-%m-%d %H:%M:%S"),
        ENDTIME = list(type = "col_datetime", format = "%Y-%m-%d %H:%M:%S"),
        ITEMID = list(type = "col_integer"),
        AMOUNT = list(type = "col_double"),
        AMOUNTUOM = list(type = "col_character"),
        RATE = list(type = "col_double"),
        RATEUOM = list(type = "col_character"),
        STORETIME = list(type = "col_datetime", format = "%Y-%m-%d %H:%M:%S"),
        CGID = list(type = "col_integer"),
        ORDERID = list(type = "col_integer"),
        LINKORDERID = list(type = "col_integer"),
        ORDERCATEGORYNAME = list(type = "col_character"),
        SECONDARYORDERCATEGORYNAME = list(type = "col_character"),
        ORDERCOMPONENTTYPEDESCRIPTION = list(type = "col_character"),
        ORDERCATEGORYDESCRIPTION = list(type = "col_character"),
        PATIENTWEIGHT = list(type = "col_double"),
        TOTALAMOUNT = list(type = "col_double"),
        TOTALAMOUNTUOM = list(type = "col_character"),
        ISOPENBAG = list(type = "col_integer"),
        CONTINUEINNEXTDEPT = list(type = "col_integer"),
        CANCELREASON = list(type = "col_integer"),
        STATUSDESCRIPTION = list(type = "col_character"),
        COMMENTS_EDITEDBY = list(type = "col_character"),
        COMMENTS_CANCELEDBY = list(type = "col_character"),
        COMMENTS_DATE = list(type = "col_datetime",
                             format = "%Y-%m-%d %H:%M:%S"),
        ORIGINALAMOUNT = list(type = "col_double"),
        ORIGINALRATE = list(type = "col_double")
      )
    ),
    LABEVENTS = list(
      col_spec = list(
        ROW_ID = list(type = "col_integer"),
        SUBJECT_ID = list(type = "col_integer"),
        HADM_ID = list(type = "col_integer"),
        ITEMID = list(type = "col_integer"),
        CHARTTIME = list(type = "col_datetime", format = "%Y-%m-%d %H:%M:%S"),
        VALUE = list(type = "col_character"),
        VALUENUM = list(type = "col_double"),
        VALUEUOM = list(type = "col_character"),
        FLAG = list(type = "col_character")
      )
    ),
    MICROBIOLOGYEVENTS = list(
      col_spec = list(
        ROW_ID = list(type = "col_integer"),
        SUBJECT_ID = list(type = "col_integer"),
        HADM_ID = list(type = "col_integer"),
        CHARTDATE = list(type = "col_datetime", format = "%Y-%m-%d %H:%M:%S"),
        CHARTTIME = list(type = "col_datetime", format = "%Y-%m-%d %H:%M:%S"),
        SPEC_ITEMID = list(type = "col_integer"),
        SPEC_TYPE_DESC = list(type = "col_character"),
        ORG_ITEMID = list(type = "col_integer"),
        ORG_NAME = list(type = "col_character"),
        ISOLATE_NUM = list(type = "col_integer"),
        AB_ITEMID = list(type = "col_integer"),
        AB_NAME = list(type = "col_character"),
        DILUTION_TEXT = list(type = "col_character"),
        DILUTION_COMPARISON = list(type = "col_character"),
        DILUTION_VALUE = list(type = "col_double"),
        INTERPRETATION = list(type = "col_character")
      )
    ),
    NOTEEVENTS = list(
      col_spec = list(
        ROW_ID = list(type = "col_integer"),
        SUBJECT_ID = list(type = "col_integer"),
        HADM_ID = list(type = "col_integer"),
        CHARTDATE = list(type = "col_date", format = "%Y-%m-%d"),
        CHARTTIME = list(type = "col_datetime", format = "%Y-%m-%d %H:%M:%S"),
        STORETIME = list(type = "col_datetime", format = "%Y-%m-%d %H:%M:%S"),
        CATEGORY = list(type = "col_character"),
        DESCRIPTION = list(type = "col_character"),
        CGID = list(type = "col_integer"),
        ISERROR = list(type = "col_character"),
        TEXT = list(type = "col_character")
      )
    ),
    OUTPUTEVENTS = list(
      col_spec = list(
        ROW_ID = list(type = "col_integer"),
        SUBJECT_ID = list(type = "col_integer"),
        HADM_ID = list(type = "col_integer"),
        ICUSTAY_ID = list(type = "col_integer"),
        CHARTTIME = list(type = "col_datetime", format = "%Y-%m-%d %H:%M:%S"),
        ITEMID = list(type = "col_integer"),
        VALUE = list(type = "col_double"),
        VALUEUOM = list(type = "col_character"),
        STORETIME = list(type = "col_datetime", format = "%Y-%m-%d %H:%M:%S"),
        CGID = list(type = "col_integer"),
        STOPPED = list(type = "col_character"),
        NEWBOTTLE = list(type = "col_character"),
        ISERROR = list(type = "col_integer")
      )
    ),
    PATIENTS = list(
      col_spec = list(
        ROW_ID = list(type = "col_integer"),
        SUBJECT_ID = list(type = "col_integer"),
        GENDER = list(type = "col_character"),
        DOB = list(type = "col_datetime", format = "%Y-%m-%d %H:%M:%S"),
        DOD = list(type = "col_datetime", format = "%Y-%m-%d %H:%M:%S"),
        DOD_HOSP = list(type = "col_datetime", format = "%Y-%m-%d %H:%M:%S"),
        DOD_SSN = list(type = "col_datetime", format = "%Y-%m-%d %H:%M:%S"),
        EXPIRE_FLAG = list(type = "col_integer")
      )
    ),
    PRESCRIPTIONS = list(
      col_spec = list(
        ROW_ID = list(type = "col_integer"),
        SUBJECT_ID = list(type = "col_integer"),
        HADM_ID = list(type = "col_integer"),
        ICUSTAY_ID = list(type = "col_integer"),
        STARTDATE = list(type = "col_datetime", format = "%Y-%m-%d %H:%M:%S"),
        ENDDATE = list(type = "col_datetime", format = "%Y-%m-%d %H:%M:%S"),
        DRUG_TYPE = list(type = "col_character"),
        DRUG = list(type = "col_character"),
        DRUG_NAME_POE = list(type = "col_character"),
        DRUG_NAME_GENERIC = list(type = "col_character"),
        FORMULARY_DRUG_CD = list(type = "col_character"),
        GSN = list(type = "col_character"),
        NDC = list(type = "col_character"),
        PROD_STRENGTH = list(type = "col_character"),
        DOSE_VAL_RX = list(type = "col_character"),
        DOSE_UNIT_RX = list(type = "col_character"),
        FORM_VAL_DISP = list(type = "col_character"),
        FORM_UNIT_DISP = list(type = "col_character"),
        ROUTE = list(type = "col_character")
      )
    ),
    PROCEDUREEVENTS_MV = list(
      col_spec = list(
        ROW_ID = list(type = "col_integer"),
        SUBJECT_ID = list(type = "col_integer"),
        HADM_ID = list(type = "col_integer"),
        ICUSTAY_ID = list(type = "col_integer"),
        STARTTIME = list(type = "col_datetime", format = "%Y-%m-%d %H:%M:%S"),
        ENDTIME = list(type = "col_datetime", format = "%Y-%m-%d %H:%M:%S"),
        ITEMID = list(type = "col_integer"),
        VALUE = list(type = "col_double"),
        VALUEUOM = list(type = "col_character"),
        LOCATION = list(type = "col_character"),
        LOCATIONCATEGORY = list(type = "col_character"),
        STORETIME = list(type = "col_datetime", format = "%Y-%m-%d %H:%M:%S"),
        CGID = list(type = "col_integer"),
        ORDERID = list(type = "col_integer"),
        LINKORDERID = list(type = "col_integer"),
        ORDERCATEGORYNAME = list(type = "col_character"),
        SECONDARYORDERCATEGORYNAME = list(type = "col_character"),
        ORDERCATEGORYDESCRIPTION = list(type = "col_character"),
        ISOPENBAG = list(type = "col_integer"),
        CONTINUEINNEXTDEPT = list(type = "col_integer"),
        CANCELREASON = list(type = "col_integer"),
        STATUSDESCRIPTION = list(type = "col_character"),
        COMMENTS_EDITEDBY = list(type = "col_character"),
        COMMENTS_CANCELEDBY = list(type = "col_character"),
        COMMENTS_DATE = list(type = "col_datetime",
                             format = "%Y-%m-%d %H:%M:%S")
      )
    ),
    PROCEDURES_ICD = list(
      col_spec = list(
        ROW_ID = list(type = "col_integer"),
        SUBJECT_ID = list(type = "col_integer"),
        HADM_ID = list(type = "col_integer"),
        SEQ_NUM = list(type = "col_integer"),
        ICD9_CODE = list(type = "col_character")
      )
    ),
    SERVICES = list(
      col_spec = list(
        ROW_ID = list(type = "col_integer"),
        SUBJECT_ID = list(type = "col_integer"),
        HADM_ID = list(type = "col_integer"),
        TRANSFERTIME = list(type = "col_datetime",
                            format = "%Y-%m-%d %H:%M:%S"),
        PREV_SERVICE = list(type = "col_character"),
        CURR_SERVICE = list(type = "col_character")
      )
    ),
    TRANSFERS = list(
      col_spec = list(
        ROW_ID = list(type = "col_integer"),
        SUBJECT_ID = list(type = "col_integer"),
        HADM_ID = list(type = "col_integer"),
        ICUSTAY_ID = list(type = "col_integer"),
        DBSOURCE = list(type = "col_character"),
        EVENTTYPE = list(type = "col_character"),
        PREV_CAREUNIT = list(type = "col_character"),
        CURR_CAREUNIT = list(type = "col_character"),
        PREV_WARDID = list(type = "col_integer"),
        CURR_WARDID = list(type = "col_integer"),
        INTIME = list(type = "col_datetime", format = "%Y-%m-%d %H:%M:%S"),
        OUTTIME = list(type = "col_datetime", format = "%Y-%m-%d %H:%M:%S"),
        LOS = list(type = "col_double")
      )
    )
  )

  if (is_demo) {
    names(res) <- paste0(tolower(names(res)), ".csv")
    for (i in seq_along(res)) {
      names(res[[i]][["col_spec"]]) <- tolower(names(res[[i]][["col_spec"]]))
      if (!is.null(res[[i]][["partitioning"]])) {
        names(res[[i]][["partitioning"]]) <- tolower(
          names(res[[i]][["partitioning"]])
        )
      }
      names(res[[i]][["col_spec"]]) <- tolower(names(res[[i]][["col_spec"]]))
      res[[i]][["col_spec"]] <- c(res[[i]][["col_spec"]],
                                  list(mimic_id = list(type = "col_skip")))
    }
    res <- res[!grepl("noteevents.csv", names(res), fixed = TRUE)]
  } else {
    names(res) <- paste0(names(res), ".csv.gz")
  }

  res
}

path <- file.path(rprojroot::find_root(rprojroot::is_r_package), "inst",
                  "extdata", "config")

jsonlite::write_json(
  list(
    name = "mimic",
    base_url = "https://physionet.org/files/mimiciii",
    version = "1.4",
    setup_hook = "setup_mimic_aux_tables",
    tables = mk_cfg(is_demo = FALSE)
  ),
  file.path(path, "mimic-setup.json"),
  auto_unbox = TRUE,
  pretty = TRUE
)

jsonlite::write_json(
  list(
    name = "mimic_demo",
    base_url = "https://physionet.org/files/mimiciii-demo",
    version = "1.4",
    setup_hook = "setup_mimic_aux_tables",
    tables = mk_cfg(is_demo = TRUE)
  ),
  file.path(path, "mimic-demo.json"),
  auto_unbox = TRUE,
  pretty = TRUE
)
