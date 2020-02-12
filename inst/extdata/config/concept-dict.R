
cfg <- list(
  heart_rate = list(
    mimic = list(
      list(id = 220045L, table = "chartevents", column = "itemid"),
      list(id = 211L, table = "chartevents", column = "itemid")
    ),
    eicu = list(
      list(id = NULL, table = "vitalperiodic", column = "heartrate")
    ),
    hirid = list(
      list(id = 200L, table = "observations", column = "variableid")
    )
  ),
  systolic_bp = list(
    mimic = list(
      list(id = 220050L, table = "chartevents", column = "itemid"),
      list(id = 51L, table = "chartevents", column = "itemid"),
      list(id = 455L, table = "chartevents", column = "itemid"),
      list(id = 6701L, table = "chartevents", column = "itemid"),
      list(id = 220179L, table = "chartevents", column = "itemid")
    ),
    eicu = list(
      list(id = NULL, table = "vitalperiodic", column = "systemicsystolic")
    ),
    hirid = list(
      list(id = 100L, table = "observations", column = "variableid")
    )
  ),
  diastolic_bp = list(
    mimic = list(
      list(id = 220051L, table = "chartevents", column = "itemid"),
      list(id = 8368L, table = "chartevents", column = "itemid"),
      list(id = 8441L, table = "chartevents", column = "itemid"),
      list(id = 8555L, table = "chartevents", column = "itemid"),
      list(id = 220180L, table = "chartevents", column = "itemid")
    ),
    eicu = list(
      list(id = NULL, table = "vitalperiodic", column = "systemicdiastolic")
    ),
    hirid = list(
      list(id = 120L, table = "observations", column = "variableid")
    )
  ),
  mean_bp = list(
    mimic = list(
      list(id = 220052L, table = "chartevents", column = "itemid"),
      list(id = 6072L, table = "chartevents", column = "itemid"),
      list(id = 456L, table = "chartevents", column = "itemid"),
      list(id = 52L, table = "chartevents", column = "itemid"),
      list(id = 220181L, table = "chartevents", column = "itemid")
    ),
    eicu = list(
      list(id = NULL, table = "vitalperiodic", column = "systemicmean")
    ),
    hirid = list(
      list(id = 110L, table = "observations", column = "variableid")
    )
  ),
  respiratory_rate = list(
    mimic = list(
      list(id = 220210L, table = "chartevents", column = "itemid"),
      list(id = 224688L, table = "chartevents", column = "itemid"),
      list(id = 224689L, table = "chartevents", column = "itemid"),
      list(id = 224690L, table = "chartevents", column = "itemid"),
      list(id = 618L, table = "chartevents", column = "itemid"),
      list(id = 619L, table = "chartevents", column = "itemid")
    ),
    eicu = list(
      list(id = NULL, table = "vitalperiodic", column = "respiration")
    ),
    hirid = list(
      list(id = 300L, table = "observations", column = "variableid"),
      list(id = 310L, table = "observations", column = "variableid")
    )
  ),
  o2_saturation = list(
    mimic = list(
      list(id = 220277L, table = "chartevents", column = "itemid"),
      list(id = 226253L, table = "chartevents", column = "itemid"),
      list(id = 646L, table = "chartevents", column = "itemid"),
      list(id = 50817L, table = "labevents", column = "itemid")
    ),
    eicu = list(
      list(id = NULL, table = "vitalperiodic", column = "sao2"),
      list(id = "O2 Sat (%)", table = "lab", column = "labname")
    ),
    hirid = list(
      list(id = 4000L, table = "observations", column = "variableid"),
      list(id = 8280L, table = "observations", column = "variableid"),
      list(id = 20000800L, table = "observations", column = "variableid")
    )
  ),
  fi_o2 = list(
    mimic = list(
      list(id = 50816L, table = "labevents", column = "itemid"),
      list(id = 223835L, table = "chartevents", column = "itemid"),
      list(id = 3420L, table = "chartevents", column = "itemid")
    ),
    eicu = list(
      list(id = "FiO2", table = "respiratorycharting",
           column = "respchartvaluelabel"),
      list(id = "FiO2", table = "lab", column = "labname")
    ),
    hirid = list(
      list(id = 2010L, table = "observations", column = "variableid")
    )
  ),
  calculated_total_co2 = list(
    mimic = list(
      list(id = 50804L, table = "labevents", column = "itemid")
    ),
    eicu = list(
      list(id = "Total CO2", table = "lab", column = "labname")
    ),
    hirid = NULL
  ),
  alanine_aminotransferase = list(
    mimic = list(
      list(id = 50861L, table = "labevents", column = "itemid")
    ),
    eicu = list(
      list(id = "ALT (SGPT)", table = "lab", column = "labname")
    ),
    hirid = list(
      list(id = 20002600L, table = "observations", column = "variableid")
    )
  ),
  asparate_aminotransferase = list(
    mimic = list(
      list(id = 50878L, table = "labevents", column = "itemid")
    ),
    eicu = list(
      list(id = "AST (SGOT)", table = "lab", column = "labname")
    ),
    hirid = list(
      list(id = 24000330L, table = "observations", column = "variableid")
    )
  ),
  phosphate = list(
    mimic = list(
      list(id = 50970L, table = "labevents", column = "itemid")
    ),
    eicu = list(
      list(id = "phosphate", table = "lab", column = "labname")
    ),
    hirid = list(
      list(id = 20002500L, table = "observations", column = "variableid")
    )
  ),
  urea_nitrogen = list(
    mimic = list(
      list(id = 51006L, table = "labevents", column = "itemid")
    ),
    eicu = list(
      list(id = "BUN", table = "lab", column = "labname")
    ),
    hirid = list(
      list(id = 20004100L, table = "observations", column = "variableid")
    )
  ),
  ph = list(
    mimic = list(
      list(id = 50820L, table = "labevents", column = "itemid")
    ),
    eicu = list(
      list(id = "pH", table = "lab", column = "labname")
    ),
    hirid = list(
      list(id = 20000300L, table = "observations", column = "variableid")
    )
  ),
  bilirubin_total = list(
    mimic = list(
      list(id = 50885L, table = "labevents", column = "itemid")
    ),
    eicu = list(
      list(id = "total bilirubin", table = "lab", column = "labname")
    ),
    hirid = list(
      list(id = 20004300L, table = "observations", column = "variableid")
    )
  ),
  inr_pt = list(
    mimic = list(
      list(id = 51237L, table = "labevents", column = "itemid")
    ),
    eicu = list(
      list(id = "PT - INR", table = "lab", column = "labname")
    ),
    hirid = list(
      list(id = 24000567L, table = "observations", column = "variableid")
    )
  ),
  platelet_count = list(
    mimic = list(
      list(id = 51265L, table = "labevents", column = "itemid")
    ),
    eicu = list(
      list(id = "platelets x 1000", table = "lab", column = "labname")
    ),
    hirid = list(
      list(id = 20000110L, table = "observations", column = "variableid")
    )
  ),
  lactate = list(
    mimic = list(
      list(id = 50813L, table = "labevents", column = "itemid")
    ),
    eicu = list(
      list(id = "lactate", table = "lab", column = "labname")
    ),
    hirid = list(
      list(id = 24000524L, table = "observations", column = "variableid")
    )
  ),
  lymphocytes = list(
    mimic = list(
      list(id = 51244L, table = "labevents", column = "itemid")
    ),
    eicu = list(
      list(id = "-lymphs", table = "lab", column = "labname")
    ),
    hirid = list(
      list(id = 24000480L, table = "observations", column = "variableid")
    )
  ),
  bicarbonate = list(
    mimic = list(
      list(id = 50882L, table = "labevents", column = "itemid")
    ),
    eicu = list(
      list(id = "bicarbonate", table = "lab", column = "labname")
    ),
    hirid = list(
      list(id = 20004200L, table = "observations", column = "variableid")
    )
  ),
  creatinine = list(
    mimic = list(
      list(id = 50912L, table = "labevents", column = "itemid")
    ),
    eicu = list(
      list(id = "creatinine", table = "lab", column = "labname")
    ),
    hirid = list(
      list(id = 20000600L, table = "observations", column = "variableid")
    )
  ),
  prothrombine_time = list(
    mimic = list(
      list(id = 51274L, table = "labevents", column = "itemid")
    ),
    eicu = list(
      list(id = "PT", table = "lab", column = "labname")
    ),
    hirid = NULL
  ),
  rdw = list(
    mimic = list(
      list(id = 51277L, table = "labevents", column = "itemid")
    ),
      eicu = list(
      list(id = "RDW", table = "lab", column = "labname")
    ),
    hirid = NULL
  ),
  alkaline_phosphatase = list(
    mimic = list(
      list(id = 50863L, table = "labevents", column = "itemid")
    ),
    eicu = list(
      list(id = "alkaline phos.", table = "lab", column = "labname")
    ),
    hirid = list(
      list(id = 24000550L, table = "observations", column = "variableid")
    )
  ),
  white_blood_cells = list(
    mimic = list(
      list(id = 51301L, table = "labevents", column = "itemid")
    ),
    eicu = list(
      list(id = "WBC x 1000", table = "lab", column = "labname")
    ),
    hirid = list(
      list(id = 20000700L, table = "observations", column = "variableid")
    )
  ),
  hemoglobin = list(
    mimic = list(
      list(id = 51222L, table = "labevents", column = "itemid")
    ),
    eicu = list(
      list(id = "Hgb", table = "lab", column = "labname")
    ),
    hirid = list(
      list(id = 24000548L, table = "observations", column = "variableid"),
      list(id = 20000900L, table = "observations", column = "variableid"),
      list(id = 24000836L, table = "observations", column = "variableid")
    )
  ),
  hematocrit = list(
    mimic = list(
      list(id = 51221L, table = "labevents", column = "itemid")
    ),
    eicu = list(
      list(id = "Hct", table = "lab", column = "labname")
    ),
    hirid = NULL
  ),
  pa_co2 = list(
    mimic = list(
      list(id = 50818L, table = "labevents", column = "itemid")
    ),
    eicu = list(
      list(id = "paCO2", table = "lab", column = "labname")
    ),
    hirid = list(
      list(id = 20001200L, table = "observations", column = "variableid")
    )
  ),
  pa_o2 = list(
    mimic = list(
      list(id = 50821L, table = "labevents", column = "itemid",
           extra_cols = "valueuom", resolver = "check_mmhg")
    ),
    eicu = list(
      list(id = "paO2", table = "lab", column = "labname")
    ),
    hirid = list(
      list(id = 20000200L, table = "observations", column = "variableid")
    )
  ),
  mch = list(
    mimic = list(
      list(id = 51248L, table = "labevents", column = "itemid")
    ),
    eicu = list(
      list(id = "MCH", table = "lab", column = "labname")
    ),
    hirid = list(
      list(id = 24000160L, table = "observations", column = "variableid")
    )
  ),
  mchc = list(
    mimic = list(
      list(id = 51249L, table = "labevents", column = "itemid")
    ),
    eicu = list(
      list(id = "MCHC", table = "lab", column = "labname")
    ),
    hirid = list(
      list(id = 24000170L, table = "observations", column = "variableid")
    )
  ),
  mcv = list(
    mimic = list(
      list(id = 51250L, table = "labevents", column = "itemid")
    ),
    eicu = list(
      list(id = "MCV", table = "lab", column = "labname")
    ),
    hirid = list(
      list(id = 24000150L, table = "observations", column = "variableid")
    )
  ),
  ptt = list(
    mimic = list(
      list(id = 51275L, table = "labevents", column = "itemid")
    ),
    eicu = list(
      list(id = "PTT", table = "lab", column = "labname")
    ),
    hirid = list(
      list(id = 20004410L, table = "observations", column = "variableid")
    )
  ),
  calcium = list(
    mimic = list(
      list(id = 50893L, table = "labevents", column = "itemid")
    ),
    eicu = list(
      list(id = "calcium", table = "lab", column = "labname")
    ),
    hirid = list(
      list(id = 20005100L, table = "observations", column = "variableid")
    )
  ),
  chloride = list(
    mimic = list(
      list(id = 50902L, table = "labevents", column = "itemid")
    ),
    eicu = list(
      list(id = "chloride", table = "lab", column = "labname")
    ),
    hirid = list(
      list(id = 24000439L, table = "observations", column = "variableid"),
      list(id = 24000521L, table = "observations", column = "variableid")
    )
  ),
  magnesium = list(
    mimic = list(
      list(id = 50960L, table = "labevents", column = "itemid")
    ),
    eicu = list(
      list(id = "magnesium", table = "lab", column = "labname")
    ),
    hirid = list(
      list(id = 24000230L, table = "observations", column = "variableid")
    )
  ),
  potassium = list(
    mimic = list(
      list(id = 50971L, table = "labevents", column = "itemid")
    ),
    eicu = list(
      list(id = "potassium", table = "lab", column = "labname")
    ),
    hirid = list(
      list(id = 20000500L, table = "observations", column = "variableid"),
      list(id = 24000520L, table = "observations", column = "variableid"),
      list(id = 24000833L, table = "observations", column = "variableid"),
      list(id = 24000867L, table = "observations", column = "variableid")
    )
  ),
  sodium = list(
    mimic = list(
      list(id = 50983L, table = "labevents", column = "itemid")
    ),
    eicu = list(
      list(id = "sodium", table = "lab", column = "labname")
    ),
    hirid = list(
      list(id = 20000400L, table = "observations", column = "variableid"),
      list(id = 24000519L, table = "observations", column = "variableid"),
      list(id = 24000658L, table = "observations", column = "variableid"),
      list(id = 24000835L, table = "observations", column = "variableid"),
      list(id = 24000866L, table = "observations", column = "variableid")
    )
  ),
  basophils = list(
    mimic = list(
      list(id = 51146L, table = "labevents", column = "itemid")
    ),
    eicu = list(
      list(id = "-basos", table = "lab", column = "labname")
    ),
    hirid = NULL
  ),
  eosinophils = list(
    mimic = list(
      list(id = 51200L, table = "labevents", column = "itemid")
    ),
    eicu = list(
      list(id = "-eos", table = "lab", column = "labname")
    ),
    hirid = NULL
  ),
  neutrophils = list(
    mimic = list(
      list(id = 51256L, table = "labevents", column = "itemid")
    ),
    eicu = list(
      list(id = "-polys", table = "lab", column = "labname")
    ),
    hirid = list(
      list(id = 24000550L, table = "observations", column = "variableid")
    )
  ),
  glucose = list(
    mimic = list(
      list(id = 50809L, table = "labevents", column = "itemid"),
      list(id = 50931L, table = "labevents", column = "itemid")
    ),
    eicu = list(
      list(id = "glucose", table = "lab", column = "labname"),
      list(id = "bedside glucose", table = "lab", column = "labname")
    ),
    hirid = list(
      list(id = 20005110L, table = "observations", column = "variableid"),
      list(id = 24000523L, table = "observations", column = "variableid"),
      list(id = 24000585L, table = "observations", column = "variableid")
    )
  ),
  calcium_ionized = list(
    mimic = list(
      list(id = 50808L, table = "labevents", column = "itemid")
    ),
    eicu = list(
      list(id = "ionized calcium", table = "lab", column = "labname")
    ),
    hirid = list(
      list(id = 24000522L, table = "observations", column = "variableid")
    )
  ),
  c_reactive_protein = list(
    mimic = list(
      list(id = 50889L, table = "labevents", column = "itemid")
    ),
    eicu = list(
      list(id = "CRP", table = "lab", column = "labname")
    ),
    hirid = list(
      list(id = 20002200L, table = "observations", column = "variableid")
    )
  ),
  sedimentation_rate = list(
    mimic = list(
      list(id = 51288L, table = "labevents", column = "itemid")
    ),
    eicu = NULL,
    hirid = list(
      list(id = 24000668L, table = "observations", column = "variableid")
    )
  ),
  carboxyhemoglobin = list(
    mimic = list(
      list(id = 0L, table = "labevents", column = "itemid")
    ),
    eicu = list(
      list(id = "Carboxyhemoglobin", table = "lab", column = "labname")
    ),
    hirid = list(
      list(id = 24000526L, table = "observations", column = "variableid")
    )
  ),
  methemoglobin = list(
    mimic = list(
      list(id = 50814L, table = "labevents", column = "itemid")
    ),
    eicu = list(
      list(id = "Methemoglobin", table = "lab", column = "labname")
    ),
    hirid = list(
      list(id = 24000549L, table = "observations", column = "variableid")
    )
  ),
  troponin_t = list(
    mimic = list(
      list(id = 51003L, table = "labevents", column = "itemid")
    ),
    eicu = list(
      list(id = "troponin - T", table = "lab", column = "labname")
    ),
    hirid = list(
      list(id = 24000538L, table = "observations", column = "variableid"),
      list(id = 24000806L, table = "observations", column = "variableid")
    )
  ),
  albumin = list(
    mimic = list(
      list(id = 50862L, table = "labevents", column = "itemid")
    ),
    eicu = list(
      list(id = "albumin", table = "lab", column = "labname")
    ),
    hirid = list(
      list(id = 24000605L, table = "observations", column = "variableid")
    )
  ),
  fibrinogen = list(
    mimic = list(
      list(id = 51214L, table = "labevents", column = "itemid")
    ),
    eicu = list(
      list(id = "fibrinogen", table = "lab", column = "labname")
    ),
    hirid = list(
      list(id = 24000536L, table = "observations", column = "variableid")
    )
  ),
  base_excess = list(
    mimic = list(
      list(id = 50802L, table = "labevents", column = "itemid")
    ),
    eicu = list(
      list(id = "Base Excess", table = "lab", column = "labname")
    ),
    hirid = list(
      list(id = 20001300L, table = "observations", column = "variableid")
    )
  ),
  red_blood_cells = list(
    mimic = list(
      list(id = 51279L, table = "labevents", column = "itemid")
    ),
    eicu = list(
      list(id = "RBC", table = "lab", column = "labname")
    ),
    hirid = NULL
  ),
  creatine_kinase = list(
    mimic = list(
      list(id = 50910L, table = "labevents", column = "itemid")
    ),
    eicu = list(
      list(id = "CPK", table = "lab", column = "labname")
    ),
    hirid = list(
      list(id = 24000210L, table = "observations", column = "variableid")
    )
  ),
  creatine_kinase_mb = list(
    mimic = list(
      list(id = 50911L, table = "labevents", column = "itemid")
    ),
    eicu = list(
      list(id = "CPK-MB", table = "lab", column = "labname")
    ),
    hirid = list(
      list(id = 24000220L, table = "observations", column = "variableid")
    )
  ),
  gcs_eye = list(
    mimic = list(
      list(id = 223900L, table = "chartevents", column = "itemid"),
      list(id = 723L, table = "chartevents", column = "itemid")
    ),
    eicu = list(
      list(id = "Eyes", table = "nursecharting",
           column = "nursingchartcelltypevalname")
    ),
    hirid = list(
      list(id = 10000100L, table = "observations", column = "variableid")
    )
  ),
  gcs_verbal = list(
    mimic = list(
      list(id = 223901L, table = "chartevents", column = "itemid"),
      list(id = 454L, table = "chartevents", column = "itemid")
    ),
    eicu = list(
      list(id = "Verbal", table = "nursecharting",
           column = "nursingchartcelltypevalname")
    ),
    hirid = list(
        list(id = 10000200L, table = "observations", column = "variableid")
      )
  ),
  gcs_motor = list(
    mimic = list(
      list(id = 220739L, table = "chartevents", column = "itemid"),
      list(id = 184L, table = "chartevents", column = "itemid")
    ),
    eicu = list(
      list(id = "Motor", table = "nursecharting",
           column = "nursingchartcelltypevalname")
    ),
    hirid = list(
        list(id = 10000300L, table = "observations", column = "variableid")
      )
  ),
  gcs_total = list(
    mimic = NULL,
    eicu = list(
      list(id = "GCS Total", table = "nursecharting",
           column = "nursingchartcelltypevalname")
    ),
    hirid = NULL
  ),
  urine_events = list(
    mimic = list(
      list(id = 40055L, table = "outputevents", column = "itemid"),
      list(id = 40056L, table = "outputevents", column = "itemid"),
      list(id = 40057L, table = "outputevents", column = "itemid"),
      list(id = 40065L, table = "outputevents", column = "itemid"),
      list(id = 40069L, table = "outputevents", column = "itemid"),
      list(id = 40085L, table = "outputevents", column = "itemid"),
      list(id = 40086L, table = "outputevents", column = "itemid"),
      list(id = 40094L, table = "outputevents", column = "itemid"),
      list(id = 40096L, table = "outputevents", column = "itemid"),
      list(id = 40405L, table = "outputevents", column = "itemid"),
      list(id = 40428L, table = "outputevents", column = "itemid"),
      list(id = 40473L, table = "outputevents", column = "itemid"),
      list(id = 40715L, table = "outputevents", column = "itemid"),
      list(id = 43175L, table = "outputevents", column = "itemid"),
      list(id = 226557L, table = "outputevents", column = "itemid"),
      list(id = 226558L, table = "outputevents", column = "itemid"),
      list(id = 226559L, table = "outputevents", column = "itemid"),
      list(id = 226560L, table = "outputevents", column = "itemid"),
      list(id = 226561L, table = "outputevents", column = "itemid"),
      list(id = 226563L, table = "outputevents", column = "itemid"),
      list(id = 226564L, table = "outputevents", column = "itemid"),
      list(id = 226565L, table = "outputevents", column = "itemid"),
      list(id = 226566L, table = "outputevents", column = "itemid"),
      list(id = 226567L, table = "outputevents", column = "itemid"),
      list(id = 226584L, table = "outputevents", column = "itemid"),
      list(id = 227510L, table = "outputevents", column = "itemid")
    ),
    eicu = list(
      list(id = "Urine", table = "intakeoutput", column = "celllabel"),
      list(id = "URINE CATHETER", table = "intakeoutput",
           column = "celllabel"),
      list(id = "Urinary Catheter Output: Indwelling/Continuous Ure",
           table = "intakeoutput", column = "celllabel"),
      list(id = "Indwelling Catheter Output", table = "intakeoutput",
           column = "celllabel")
    ),
    hirid = NULL
  ),
  urine_hourly = list(
    mimic = NULL,
    eicu = NULL,
    hirid = list(
      list(id = 10020000L, table = "observations", column = "variableid")
    )
  ),
  urine_cumulative = list(
    mimic = NULL,
    eicu = NULL,
    hirid = list(
      list(id = 30005110L, table = "observations", column = "variableid")
    )
  ),
  dobutamine = list(
    mimic = list(
      list(id = 221653L, table = "inputevents_mv", column = "itemid"),
      list(id = 30042L, table = "inputevents_cv", column = "itemid"),
      list(id = 30306L, table = "inputevents_cv", column = "itemid")
    ),
    eicu = list(
      list(id = "Dobutamine (mcg/kg/min)", table = "infusiondrug",
           column = "drugname"),
      list(id = "Dobutamine (ml/hr)", table = "infusiondrug",
           column = "drugname"),
      list(id = "Dobutamine ()", table = "infusiondrug", column = "drugname")
    ),
    hirid = list(
      list(id = 426L, table = "pharma", column = "variableid")
    )
  ),
  dopamine = list(
    mimic = list(
      list(id = 221662L, table = "inputevents_mv", column = "itemid"),
      list(id = 30043L, table = "inputevents_cv", column = "itemid"),
      list(id = 30125L, table = "inputevents_cv", column = "itemid"),
      list(id = 30307L, table = "inputevents_cv", column = "itemid")
    ),
    eicu = list(
      list(id = "Dopamine (mcg/kg/min)", table = "infusiondrug",
           column = "drugname")
    ),
    hirid = NULL
  ),
  norepinephrine = list(
    mimic = list(
      list(id = 221906L, table = "inputevents_mv", column = "itemid"),
      list(id = 30047L, table = "inputevents_cv", column = "itemid"),
      list(id = 30120L, table = "inputevents_cv", column = "itemid")
    ),
    eicu = list(
      list(id = "Norepinephrine (mcg/kg/min)", table = "infusiondrug",
           column = "drugname"),
      list(id = "Norepinephrine (ml/hr)", table = "infusiondrug",
           column = "drugname"),
      list(id = "Norepinephrine (mcg/min)", table = "infusiondrug",
           column = "drugname")
    ),
    hirid = list(
      list(id = 1000462L, table = "pharma", column = "variableid"),
      list(id = 1000656L, table = "pharma", column = "variableid"),
      list(id = 1000657L, table = "pharma", column = "variableid"),
      list(id = 1000658L, table = "pharma", column = "variableid")
    )
  ),
  epinephrine = list(
    mimic = list(
      list(id = 221289L, table = "inputevents_mv", column = "itemid"),
      list(id = 30044L, table = "inputevents_cv", column = "itemid"),
      list(id = 30119L, table = "inputevents_cv", column = "itemid"),
      list(id = 30309L, table = "inputevents_cv", column = "itemid")
    ),
    eicu = list(
      list(id = "Epinephrine (mcg/kg/min)", table = "infusiondrug",
           column = "drugname"),
      list(id = "Epinephrine (ml/hr)", table = "infusiondrug",
           column = "drugname"),
      list(id = "Epinephrine (mcg/min)", table = "infusiondrug",
           column = "drugname")
    ),
    hirid = list(
      list(id = 71L, table = "pharma", column = "variableid"),
      list(id = 1000750L, table = "pharma", column = "variableid"),
      list(id = 1000649L, table = "pharma", column = "variableid"),
      list(id = 1000650L, table = "pharma", column = "variableid"),
      list(id = 1000655L, table = "pharma", column = "variableid")
    )
  ),
  vent_start = list(
    mimic = list(
      list(id = 1L, table = "chartevents", column = "itemid"),
      list(id = 60L, table = "chartevents", column = "itemid"),
      list(id = 218L, table = "chartevents", column = "itemid"),
      list(id = 221L, table = "chartevents", column = "itemid"),
      list(id = 223L, table = "chartevents", column = "itemid"),
      list(id = 436L, table = "chartevents", column = "itemid"),
      list(id = 437L, table = "chartevents", column = "itemid"),
      list(id = 444L, table = "chartevents", column = "itemid"),
      list(id = 445L, table = "chartevents", column = "itemid"),
      list(id = 448L, table = "chartevents", column = "itemid"),
      list(id = 449L, table = "chartevents", column = "itemid"),
      list(id = 450L, table = "chartevents", column = "itemid"),
      list(id = 459L, table = "chartevents", column = "itemid"),
      list(id = 501L, table = "chartevents", column = "itemid"),
      list(id = 502L, table = "chartevents", column = "itemid"),
      list(id = 503L, table = "chartevents", column = "itemid"),
      list(id = 505L, table = "chartevents", column = "itemid"),
      list(id = 506L, table = "chartevents", column = "itemid"),
      list(id = 535L, table = "chartevents", column = "itemid"),
      list(id = 543L, table = "chartevents", column = "itemid"),
      list(id = 639L, table = "chartevents", column = "itemid"),
      list(id = 654L, table = "chartevents", column = "itemid"),
      list(id = 667L, table = "chartevents", column = "itemid"),
      list(id = 668L, table = "chartevents", column = "itemid"),
      list(id = 669L, table = "chartevents", column = "itemid"),
      list(id = 670L, table = "chartevents", column = "itemid"),
      list(id = 671L, table = "chartevents", column = "itemid"),
      list(id = 672L, table = "chartevents", column = "itemid"),
      list(id = 681L, table = "chartevents", column = "itemid"),
      list(id = 682L, table = "chartevents", column = "itemid"),
      list(id = 683L, table = "chartevents", column = "itemid"),
      list(id = 684L, table = "chartevents", column = "itemid"),
      list(id = 686L, table = "chartevents", column = "itemid"),
      list(id = 1211L, table = "chartevents", column = "itemid"),
      list(id = 1340L, table = "chartevents", column = "itemid"),
      list(id = 1486L, table = "chartevents", column = "itemid"),
      list(id = 1600L, table = "chartevents", column = "itemid"),
      list(id = 1655L, table = "chartevents", column = "itemid"),
      list(id = 2000L, table = "chartevents", column = "itemid"),
      list(id = 3459L, table = "chartevents", column = "itemid"),
      list(id = 5865L, table = "chartevents", column = "itemid"),
      list(id = 5866L, table = "chartevents", column = "itemid"),
      list(id = 220339L, table = "chartevents", column = "itemid"),
      list(id = 223848L, table = "chartevents", column = "itemid"),
      list(id = 223849L, table = "chartevents", column = "itemid"),
      list(id = 224419L, table = "chartevents", column = "itemid"),
      list(id = 224684L, table = "chartevents", column = "itemid"),
      list(id = 224685L, table = "chartevents", column = "itemid"),
      list(id = 224686L, table = "chartevents", column = "itemid"),
      list(id = 224687L, table = "chartevents", column = "itemid"),
      list(id = 224695L, table = "chartevents", column = "itemid"),
      list(id = 224696L, table = "chartevents", column = "itemid"),
      list(id = 224697L, table = "chartevents", column = "itemid"),
      list(id = 224700L, table = "chartevents", column = "itemid"),
      list(id = 224701L, table = "chartevents", column = "itemid"),
      list(id = 224702L, table = "chartevents", column = "itemid"),
      list(id = 224703L, table = "chartevents", column = "itemid"),
      list(id = 224704L, table = "chartevents", column = "itemid"),
      list(id = 224705L, table = "chartevents", column = "itemid"),
      list(id = 224706L, table = "chartevents", column = "itemid"),
      list(id = 224707L, table = "chartevents", column = "itemid"),
      list(id = 224709L, table = "chartevents", column = "itemid"),
      list(id = 224738L, table = "chartevents", column = "itemid"),
      list(id = 224746L, table = "chartevents", column = "itemid"),
      list(id = 224747L, table = "chartevents", column = "itemid"),
      list(id = 224750L, table = "chartevents", column = "itemid"),
      list(id = 226873L, table = "chartevents", column = "itemid"),
      list(id = 227187L, table = "chartevents", column = "itemid")
    ),
    eicu = list(
      list(id = NULL, table = "respiratorycare", column = "ventstartoffset"),
      list(id = NULL, table = "respiratorycare",
           column = "priorventstartoffset"),
      list(id = "respFlowPtVentData", table = "respiratorycharting",
           column = "respcharttypecat")
    ),
    hirid = list(
      list(id = 15001552L, table = "observations", column = "variableid")
    )
  ),
  vent_end = list(
    mimic = list(
      list(id = 227194L, table = "procedureevents_mv", column = "itemid"),
      list(id = 225468L, table = "procedureevents_mv", column = "itemid"),
      list(id = 225477L, table = "procedureevents_mv", column = "itemid"),
      list(id = 467L, table = "chartevents", column = "itemid"),
      list(id = 469L, table = "chartevents", column = "itemid"),
      list(id = 226732L, table = "chartevents", column = "itemid")
    ),
    eicu = list(
      list(id = NULL, table = "respiratorycare", column = "ventendoffset"),
      list(id = NULL, table = "respiratorycare",
           column = "priorventendoffset"),
      list(id = "Off", table = "respiratorycharting",
           column = "respchartvalue"),
      list(id = "off", table = "respiratorycharting",
           column = "respchartvalue"),
      list(id = "Suspended", table = "respiratorycharting",
           column = "respchartvalue")
    ),
    hirid = list(
      list(id = 15001552L, table = "observations", column = "variableid")
    )
  ),
  tracheostomy = list(
    mimic = list(
      list(id = "No Response-ETT", table = "chartevents", column = "value"),
      list(id = "1.0 ET/Trach", table = "chartevents", column = "value")
    ),
    eicu = NULL,
    hirid = list(
      list(id = 15001552L, table = "observations", column = "variableid")
    )
  ),
  rass_scale = list(
    mimic = NULL,
    eicu = list(
      list(id = "Sedation Score", table = "nursecharting",
           column = "nursingchartcelltypevalname")
    ),
    hirid = list(
      list(id = 15001565L, table = "observations", column = "variableid")
    )
  )
)

jsonlite::write_json(cfg[order(names(cfg))], "concept-dict.json",
                     null = "null", auto_unbox = TRUE, pretty = TRUE)
