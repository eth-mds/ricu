
cfg <- list(
  heart_rate = list(
    unit = "bpm",
    sources = list(
      mimic = list(
        list(ids = c(211L, 220045L), table = "chartevents", column = "itemid")
      ),
      eicu = list(
        list(table = "vitalperiodic", column = "heartrate")
      ),
      hirid = list(
        list(ids = 200L, table = "observations", column = "variableid")
      )
    )
  ),
  systolic_bp = list(
    unit = "mmHg",
    sources = list(
      mimic = list(
        list(ids = c(51L, 455L, 6701L, 220050L, 220179L),
             table = "chartevents", column = "itemid")
      ),
      eicu = list(
        list(table = "vitalperiodic", column = "systemicsystolic")
      ),
      hirid = list(
        list(ids = 100L, table = "observations", column = "variableid")
      )
    )
  ),
  diastolic_bp = list(
    unit = "mmHg",
    sources = list(
      mimic = list(
        list(ids = c(8368L, 8441L, 8555L, 220051L, 220180L),
             table = "chartevents", column = "itemid")
      ),
      eicu = list(
        list(table = "vitalperiodic", column = "systemicdiastolic")
      ),
      hirid = list(
        list(ids = 120L, table = "observations", column = "variableid")
      )
    )
  ),
  mean_bp = list(
    unit = "mmHg",
    sources = list(
      mimic = list(
        list(ids = c(52L, 443L, 456L, 6072L, 220052L, 220181L, 225312L),
             table = "chartevents", column = "itemid")
      ),
      eicu = list(
        list(table = "vitalperiodic", column = "systemicmean")
      ),
      hirid = list(
        list(ids = 110L, table = "observations", column = "variableid")
      )
    )
  ),
  respiratory_rate = list(
    unit = "insp/min",
    sources = list(
      mimic = list(
        list(ids = c(618L, 619L, 220210L, 224688L, 224689L, 224690L),
             table = "chartevents", column = "itemid")
      ),
      eicu = list(
        list(table = "vitalperiodic", column = "respiration")
      ),
      hirid = list(
        list(ids = c(300L, 310L), table = "observations",
             column = "variableid")
      )
    )
  ),
  o2_saturation = list(
    unit = "%",
    sources = list(
      mimic = list(
        list(ids = c(646L, 220277L, 226253L, 50817L), table = "chartevents",
             column = "itemid")
      ),
      eicu = list(
        list(table = "vitalperiodic", column = "sao2"),
        list(ids = "O2 Sat (%)", table = "lab", column = "labname")
      ),
      hirid = list(
        list(ids = c(4000L, 8280L, 20000800L), table = "observations",
             column = "variableid")
      )
    )
  ),
  fi_o2 = list(
    unit = "%",
    sources = list(
      mimic = list(
        list(ids = c(3420L, 50816L, 223835L), table = "labevents",
             column = "itemid")
      ),
      eicu = list(
        list(ids = "FiO2", table = "respiratorycharting",
             column = "respchartvaluelabel", callback = "percent_as_numeric"),
        list(ids = "FiO2", table = "lab", column = "labname")
      ),
      hirid = list(
        list(ids = 2010L, table = "observations", column = "variableid")
      )
    )
  ),
  calculated_total_co2 = list(
    unit = "mEq/L",
    sources = list(
      mimic = list(
        list(ids = 50804L, table = "labevents", column = "itemid")
      ),
      eicu = list(
        list(ids = "Total CO2", table = "lab", column = "labname")
      )
  )
  ),
  alanine_aminotransferase = list(
    unit = "IU/L",
    sources = list(
      mimic = list(
        list(ids = 50861L, table = "labevents", column = "itemid")
      ),
      eicu = list(
        list(ids = "ALT (SGPT)", table = "lab", column = "labname")
      ),
      hirid = list(
        list(ids = 20002600L, table = "observations", column = "variableid")
      )
    )
  ),
  asparate_aminotransferase = list(
    unit = "IU/L",
    sources = list(
      mimic = list(
        list(ids = 50878L, table = "labevents", column = "itemid")
      ),
      eicu = list(
        list(ids = "AST (SGOT)", table = "lab", column = "labname")
      ),
      hirid = list(
        list(ids = 24000330L, table = "observations", column = "variableid")
      )
    )
  ),
  phosphate = list(
    unit = "mg/dL",
    sources = list(
      mimic = list(
        list(ids = 50970L, table = "labevents", column = "itemid")
      ),
      eicu = list(
        list(ids = "phosphate", table = "lab", column = "labname")
      ),
      hirid = list(
        list(ids = 20002500L, table = "observations", column = "variableid",
             callback = "multiply_hirid_phos")
      )
    )
  ),
  urea_nitrogen = list(
    unit = "mg/dL",
    sources = list(
      mimic = list(
        list(ids = 51006L, table = "labevents", column = "itemid")
      ),
      eicu = list(
        list(ids = "BUN", table = "lab", column = "labname")
      ),
      hirid = list(
        list(ids = 20004100L, table = "observations", column = "variableid",
             callback = "multiply_hirid_urea")
      )
    )
  ),
  ph = list(
    sources = list(
      mimic = list(
        list(ids = 50820L, table = "labevents", column = "itemid")
      ),
      eicu = list(
        list(ids = "pH", table = "lab", column = "labname")
      ),
      hirid = list(
        list(ids = 20000300L, table = "observations", column = "variableid")
      )
    )
  ),
  bilirubin_total = list(
    unit = "mg/dL",
    sources = list(
      mimic = list(
        list(ids = 50885L, table = "labevents", column = "itemid")
      ),
      eicu = list(
        list(ids = "total bilirubin", table = "lab", column = "labname")
      ),
      hirid = list(
        list(ids = 20004300L, table = "observations", column = "variableid",
             callback = "multiply_hirid_bili")
      )
    )
  ),
  inr_pt = list(
    sources = list(
      mimic = list(
        list(ids = 51237L, table = "labevents", column = "itemid")
      ),
      eicu = list(
        list(ids = "PT - INR", table = "lab", column = "labname")
      ),
      hirid = list(
        list(ids = 24000567L, table = "observations", column = "variableid")
      )
    )
  ),
  platelet_count = list(
    unit = "K/uL",
    sources = list(
      mimic = list(
        list(ids = 51265L, table = "labevents", column = "itemid")
      ),
      eicu = list(
        list(ids = "platelets x 1000", table = "lab", column = "labname")
      ),
      hirid = list(
        list(ids = 20000110L, table = "observations", column = "variableid")
      )
    )
  ),
  lactate = list(
    unit = "mmol/L",
    sources = list(
      mimic = list(
        list(ids = 50813L, table = "labevents", column = "itemid")
      ),
      eicu = list(
        list(ids = "lactate", table = "lab", column = "labname")
      ),
      hirid = list(
        list(ids = 24000524L, table = "observations", column = "variableid")
      )
    )
  ),
  lymphocytes = list(
    unit = "%",
    sources = list(
      mimic = list(
        list(ids = 51244L, table = "labevents", column = "itemid")
      ),
      eicu = list(
        list(ids = "-lymphs", table = "lab", column = "labname")
      ),
      hirid = list(
        list(ids = 24000480L, table = "observations", column = "variableid")
      )
    )
  ),
  bicarbonate = list(
    unit = "mEq/L",
    sources = list(
      mimic = list(
        list(ids = 50882L, table = "labevents", column = "itemid")
      ),
      eicu = list(
        list(ids = "bicarbonate", table = "lab", column = "labname")
      ),
      hirid = list(
        list(ids = 20004200L, table = "observations", column = "variableid")
      )
    )
  ),
  creatinine = list(
    unit = "mg/dL",
    sources = list(
      mimic = list(
        list(ids = 50912L, table = "labevents", column = "itemid")
      ),
      eicu = list(
        list(ids = "creatinine", table = "lab", column = "labname")
      ),
      hirid = list(
        list(ids = 20000600L, table = "observations", column = "variableid",
             callback = "multiply_hirid_crea")
      )
    )
  ),
  prothrombine_time = list(
    unit = "sec",
    sources = list(
      mimic = list(
        list(ids = 51274L, table = "labevents", column = "itemid")
      ),
      eicu = list(
        list(ids = "PT", table = "lab", column = "labname")
      )
  )
  ),
  rdw = list(
    unit = "%",
    sources = list(
      mimic = list(
        list(ids = 51277L, table = "labevents", column = "itemid")
      ),
        eicu = list(
        list(ids = "RDW", table = "lab", column = "labname")
      )
  )
  ),
  alkaline_phosphatase = list(
    unit = "IU/L",
    sources = list(
      mimic = list(
        list(ids = 50863L, table = "labevents", column = "itemid")
      ),
      eicu = list(
        list(ids = "alkaline phos.", table = "lab", column = "labname")
      ),
      hirid = list(
        list(ids = 20002700L, table = "observations", column = "variableid")
      )
    )
  ),
  white_blood_cells = list(
    unit = "K/uL",
    sources = list(
      mimic = list(
        list(ids = 51301L, table = "labevents", column = "itemid")
      ),
      eicu = list(
        list(ids = "WBC x 1000", table = "lab", column = "labname")
      ),
      hirid = list(
        list(ids = 20000700L, table = "observations", column = "variableid")
      )
    )
  ),
  hemoglobin = list(
    unit = "g/dL",
    sources = list(
      mimic = list(
        list(ids = 51222L, table = "labevents", column = "itemid")
      ),
      eicu = list(
        list(ids = "Hgb", table = "lab", column = "labname")
      ),
      hirid = list(
        list(ids = c(24000548L, 24000836L, 20000900L), table = "observations",
             column = "variableid", callback = "multiply_hirid_hemo")
      )
    )
  ),
  hematocrit = list(
    unit = "%",
    sources = list(
      mimic = list(
        list(ids = 51221L, table = "labevents", column = "itemid")
      ),
      eicu = list(
        list(ids = "Hct", table = "lab", column = "labname")
      )
    )
  ),
  pa_co2 = list(
    unit = "mmHg",
    sources = list(
      mimic = list(
        list(ids = 50818L, table = "labevents", column = "itemid")
      ),
      eicu = list(
        list(ids = "paCO2", table = "lab", column = "labname")
      ),
      hirid = list(
        list(ids = 20001200L, table = "observations", column = "variableid")
      )
    )
  ),
  pa_o2 = list(
    unit = "mmHg",
    sources = list(
      mimic = list(
        list(ids = 50821L, table = "labevents", column = "itemid")
      ),
      eicu = list(
        list(ids = "paO2", table = "lab", column = "labname")
      ),
      hirid = list(
        list(ids = 20000200L, table = "observations", column = "variableid")
      )
    )
  ),
  mch = list(
    unit = "pg",
    sources = list(
      mimic = list(
        list(ids = 51248L, table = "labevents", column = "itemid")
      ),
      eicu = list(
        list(ids = "MCH", table = "lab", column = "labname")
      ),
      hirid = list(
        list(ids = 24000160L, table = "observations", column = "variableid")
      )
    )
  ),
  mchc = list(
    unit = "%",
    sources = list(
      mimic = list(
        list(ids = 51249L, table = "labevents", column = "itemid")
      ),
      eicu = list(
        list(ids = "MCHC", table = "lab", column = "labname")
      ),
      hirid = list(
        list(ids = 24000170L, table = "observations", column = "variableid")
      )
    )
  ),
  mcv = list(
    unit = "fL",
    sources = list(
      mimic = list(
        list(ids = 51250L, table = "labevents", column = "itemid")
      ),
      eicu = list(
        list(ids = "MCV", table = "lab", column = "labname")
      ),
      hirid = list(
        list(ids = 24000150L, table = "observations", column = "variableid")
      )
    )
  ),
  ptt = list(
    unit = "sec",
    sources = list(
      mimic = list(
        list(ids = 51275L, table = "labevents", column = "itemid")
      ),
      eicu = list(
        list(ids = "PTT", table = "lab", column = "labname")
      ),
      hirid = list(
        list(ids = 20004410L, table = "observations", column = "variableid")
      )
    )
  ),
  calcium = list(
    unit = "mg/dL",
    sources = list(
      mimic = list(
        list(ids = 50893L, table = "labevents", column = "itemid")
      ),
      eicu = list(
        list(ids = "calcium", table = "lab", column = "labname")
      ),
      hirid = list(
        list(ids = 20005100L, table = "observations", column = "variableid",
             callback = "multiply_hirid_calc")
      )
    )
  ),
  chloride = list(
    unit = "mEq/L",
    sources = list(
      mimic = list(
        list(ids = 50902L, table = "labevents", column = "itemid")
      ),
      eicu = list(
        list(ids = "chloride", table = "lab", column = "labname")
      ),
      hirid = list(
        list(ids = c(24000439L, 24000521L), table = "observations",
             column = "variableid")
      )
    )
  ),
  magnesium = list(
    unit = "mg/dL",
    sources = list(
      mimic = list(
        list(ids = 50960L, table = "labevents", column = "itemid")
      ),
      eicu = list(
        list(ids = "magnesium", table = "lab", column = "labname")
      ),
      hirid = list(
        list(ids = 24000230L, table = "observations", column = "variableid",
             callback = "multiply_hirid_magn")
      )
    )
  ),
  potassium = list(
    unit = "mEq/L",
    sources = list(
      mimic = list(
        list(ids = 50971L, table = "labevents", column = "itemid")
      ),
      eicu = list(
        list(ids = "potassium", table = "lab", column = "labname")
      ),
      hirid = list(
        list(ids = c(20000500L, 24000520L, 24000833L, 24000867L),
             table = "observations", column = "variableid")
      )
    )
  ),
  sodium = list(
    unit = "mEq/L",
    sources = list(
      mimic = list(
        list(ids = 50983L, table = "labevents", column = "itemid")
      ),
      eicu = list(
        list(ids = "sodium", table = "lab", column = "labname")
      ),
      hirid = list(
        list(ids = c(20000400L, 24000519L, 24000658L, 24000835L, 24000866L),
             table = "observations", column = "variableid")
      )
    )
  ),
  basophils = list(
    unit = "%",
    sources = list(
      mimic = list(
        list(ids = 51146L, table = "labevents", column = "itemid")
      ),
      eicu = list(
        list(ids = "-basos", table = "lab", column = "labname")
      )
  )
  ),
  eosinophils = list(
    unit = "%",
    sources = list(
      mimic = list(
        list(ids = 51200L, table = "labevents", column = "itemid")
      ),
      eicu = list(
        list(ids = "-eos", table = "lab", column = "labname")
      )
  )
  ),
  neutrophils = list(
    unit = "%",
    sources = list(
      mimic = list(
        list(ids = 51256L, table = "labevents", column = "itemid")
      ),
      eicu = list(
        list(ids = "-polys", table = "lab", column = "labname")
      ),
      hirid = list(
        list(ids = 24000550L, table = "observations", column = "variableid")
      )
    )
  ),
  glucose = list(
    unit = "mg/dL",
    sources = list(
      mimic = list(
        list(ids = c(50809L, 50931L), table = "labevents", column = "itemid")
      ),
      eicu = list(
        list(ids = "glucose", table = "lab", column = "labname"),
        list(ids = "bedside glucose", table = "lab", column = "labname")
      ),
      hirid = list(
        list(ids = c(20005110L, 24000523L, 24000585L), table = "observations",
             column = "variableid", callback = "multiply_hirid_gluc")
      )
    )
  ),
  calcium_ionized = list(
    unit = "mmol/L",
    sources = list(
      mimic = list(
        list(ids = 50808L, table = "labevents", column = "itemid")
      ),
      eicu = list(
        list(ids = "ionized calcium", table = "lab", column = "labname")
      ),
      hirid = list(
        list(ids = 24000522L, table = "observations", column = "variableid")
      )
    )
  ),
  c_reactive_protein = list(
    unit = "mg/L",
    sources = list(
      mimic = list(
        list(ids = 50889L, table = "labevents", column = "itemid")
      ),
      eicu = list(
        list(ids = "CRP", table = "lab", column = "labname",
             callback = "multiply_eicu_cprot")
      ),
      hirid = list(
        list(ids = 20002200L, table = "observations", column = "variableid")
      )
    )
  ),
  sedimentation_rate = list(
    unit = "mm/hr",
    sources = list(
      mimic = list(
        list(ids = 51288L, table = "labevents", column = "itemid")
      ),
      hirid = list(
        list(ids = 24000668L, table = "observations", column = "variableid")
      )
    )
  ),
  carboxyhemoglobin = list(
    sources = list(
      mimic = list(
        list(ids = 0L, table = "labevents", column = "itemid")
      ),
      eicu = list(
        list(ids = "Carboxyhemoglobin", table = "lab", column = "labname")
      ),
      hirid = list(
        list(ids = 24000526L, table = "observations", column = "variableid")
      )
    )
  ),
  methemoglobin = list(
    unit = "%",
    sources = list(
      mimic = list(
        list(ids = 50814L, table = "labevents", column = "itemid")
      ),
      eicu = list(
        list(ids = "Methemoglobin", table = "lab", column = "labname")
      ),
      hirid = list(
        list(ids = 24000549L, table = "observations", column = "variableid")
      )
    )
  ),
  troponin_t = list(
    unit = "ng/mL",
    sources = list(
      mimic = list(
        list(ids = 51003L, table = "labevents", column = "itemid")
      ),
      eicu = list(
        list(ids = "troponin - T", table = "lab", column = "labname")
      ),
      hirid = list(
        list(ids = 24000806L, table = "observations", column = "variableid")
      )
    )
  ),
  albumin = list(
    unit = "g/dL",
    sources = list(
      mimic = list(
        list(ids = 50862L, table = "labevents", column = "itemid")
      ),
      eicu = list(
        list(ids = "albumin", table = "lab", column = "labname")
      ),
      hirid = list(
        list(ids = 24000605L, table = "observations", column = "variableid",
             callback = "multiply_hirid_albu")
      )
    )
  ),
  fibrinogen = list(
    unit = "mg/dL",
    sources = list(
      mimic = list(
        list(ids = 51214L, table = "labevents", column = "itemid")
      ),
      eicu = list(
        list(ids = "fibrinogen", table = "lab", column = "labname")
      ),
      hirid = list(
        list(ids = 24000536L, table = "observations", column = "variableid",
             callback = "multiply_hirid_fibr")
      )
    )
  ),
  base_excess = list(
    unit = "mEq/L",
    sources = list(
      mimic = list(
        list(ids = 50802L, table = "labevents", column = "itemid")
      ),
      eicu = list(
        list(ids = "Base Excess", table = "lab", column = "labname")
      ),
      hirid = list(
        list(ids = 20001300L, table = "observations", column = "variableid")
      )
    )
  ),
  red_blood_cells = list(
    unit = "m/uL",
    sources = list(
      mimic = list(
        list(ids = 51279L, table = "labevents", column = "itemid")
      ),
      eicu = list(
        list(ids = "RBC", table = "lab", column = "labname")
      )
  )
  ),
  creatine_kinase = list(
    unit = "IU/L",
    sources = list(
      mimic = list(
        list(ids = 50910L, table = "labevents", column = "itemid")
      ),
      eicu = list(
        list(ids = "CPK", table = "lab", column = "labname")
      ),
      hirid = list(
        list(ids = 24000210L, table = "observations", column = "variableid")
      )
    )
  ),
  creatine_kinase_mb = list(
    unit = "ng/mL",
    sources = list(
      mimic = list(
        list(ids = 50911L, table = "labevents", column = "itemid")
      ),
      eicu = list(
        list(ids = "CPK-MB", table = "lab", column = "labname")
      ),
      hirid = list(
        list(ids = 24000220L, table = "observations", column = "variableid")
      )
    )
  ),
  gcs_eye = list(
    sources = list(
      mimic = list(
        list(ids = c(184L, 220739L), table = "chartevents", column = "itemid")
      ),
      eicu = list(
        list(ids = "Eyes", table = "nursecharting",
             column = "nursingchartcelltypevalname",
             callback = "force_numeric_val_col")
      ),
      hirid = list(
        list(ids = 10000300L, table = "observations", column = "variableid")
      )
    )
  ),
  gcs_verbal = list(
    sources = list(
      mimic = list(
        list(ids = c(723L, 223900L), table = "chartevents", column = "itemid")
      ),
      eicu = list(
        list(ids = "Verbal", table = "nursecharting",
             column = "nursingchartcelltypevalname",
             callback = "force_numeric_val_col")
      ),
      hirid = list(
        list(ids = 10000100L, table = "observations", column = "variableid")
      )
    )
  ),
  gcs_motor = list(
    sources = list(
      mimic = list(
        list(ids = c(454L, 223901L), table = "chartevents", column = "itemid")
      ),
      eicu = list(
        list(ids = "Motor", table = "nursecharting",
             column = "nursingchartcelltypevalname",
             callback = "force_numeric_val_col")
      ),
      hirid = list(
        list(ids = 10000200L, table = "observations", column = "variableid")
      )
    )
  ),
  gcs_total = list(
    sources = list(
      mimic = list(
        list(ids = 198L, table = "chartevents", column = "itemid")
      ),
      eicu = list(
        list(ids = "GCS Total", table = "nursecharting",
             column = "nursingchartcelltypevalname",
             callback = "force_numeric_val_col")
      )
  )
  ),
  urine_events = list(
    unit = "mL",
    sources = list(
      mimic = list(
        list(ids = c(40055L,   40056L,  40057L,  40065L,  40069L,  40085L,
                     40086L,   40094L,  40096L,  40405L,  40428L,  40473L,
                     40715L,   43175L, 226557L, 226558L, 226559L, 226560L,
                    226561L,  226563L, 226564L, 226565L, 226566L, 226567L,
                    226584L, 227510L),
             table = "outputevents", column = "itemid")
      ),
      eicu = list(
        list(ids = c("Urine", "URINE CATHETER"), table = "intakeoutput",
             column = "celllabel"),
        list(ids = c("catheter.+output", "output.+catheter"),
             table = "intakeoutput", column = "celllabel", regex = TRUE)
      )
  )
  ),
  urine_hourly = list(
    unit = "mL",
    sources = list(
      hirid = list(
        list(ids = 10020000L, table = "observations", column = "variableid")
      )
    )
  ),
  urine_cumulative = list(
    unit = "mL",
    sources = list(
      hirid = list(
        list(ids = 30005110L, table = "observations", column = "variableid")
      )
    )
  ),
  dobutamine = list(
    unit = "mcg/kg/min",
    sources = list(
      mimic = list(
        list(ids = c(30042L, 30306L, 221653L), table = "inputevents_mv",
             column = "itemid")
      ),
      eicu = list(
        list(ids = "Dobutamine (mcg/kg/min)", table = "infusiondrug",
             column = "drugname", callback = "force_numeric_val_col"),
        list(ids = c("Dobutamine ()", "Dobutamine (ml/hr)"),
             table = "infusiondrug", column = "drugname",
             callback = "eicu_body_weight", weight_col = "patientweight")
      ),
      hirid = list(
        list(ids = 426L, table = "pharma", column = "pharmaid")
      )
    )
  ),
  dopamine = list(
    unit = "mcg/kg/min",
    sources = list(
      mimic = list(
        list(ids = c(30043L, 30125L, 30307L, 221662L),
             table = "inputevents_mv", column = "itemid")
      ),
      eicu = list(
        list(ids = "Dopamine (mcg/kg/min)", table = "infusiondrug",
             column = "drugname", callback = "force_numeric_val_col")
      )
  )
  ),
  norepinephrine = list(
    unit = "mcg/kg/min",
    sources = list(
      mimic = list(
        list(ids = c(30047L, 30120L, 221906L), table = "inputevents_mv",
             column = "itemid")
      ),
      eicu = list(
        list(ids = "Norepinephrine (mcg/kg/min)", table = "infusiondrug",
             column = "drugname", callback = "force_numeric_val_col"),
        list(ids = c("Norepinephrine (ml/hr)", "Norepinephrine (mcg/min)"),
             table = "infusiondrug", column = "drugname",
             callback = "eicu_body_weight", weight_col = "patientweight")
      ),
      hirid = list(
        list(ids = c(1000462L, 1000656L, 1000657L, 1000658L), table = "pharma",
             column = "pharmaid")
      )
    )
  ),
  epinephrine = list(
    unit = "mcg/kg/min",
    sources = list(
      mimic = list(
        list(ids = c(30044L, 30119L, 30309L, 221289L),
             table = "inputevents_mv", column = "itemid")
      ),
      eicu = list(
        list(ids = "Epinephrine (mcg/kg/min)", table = "infusiondrug",
             column = "drugname", callback = "force_numeric_val_col"),
        list(ids = c("Epinephrine (ml/hr)", "Epinephrine (mcg/min)"),
             table = "infusiondrug", column = "drugname",
             callback = "eicu_body_weight", weight_col = "patientweight")
      ),
      hirid = list(
        list(ids = c(71L, 1000649L, 1000650L, 1000655L, 1000750L),
             table = "pharma", column = "pharmaid")
      )
    )
  ),
  vent_start = list(
    sources = list(
      mimic = list(
        list(
        ids = c(1L,     60L,    218L,    221L,    223L,    436L,    437L,
              444L,    445L,    448L,    449L,    450L,    459L,    501L,
              502L,    503L,    505L,    506L,    535L,    543L,    639L,
              654L,    667L,    668L,    669L,    670L,    671L,    672L,
              681L,    682L,    683L,    684L,    686L,   1211L,   1340L,
             1486L,   1600L,   1655L,   2000L,   3459L,   5865L,   5866L,
           220339L, 223848L, 223849L, 224419L, 224684L, 224685L, 224686L,
           224687L, 224695L, 224696L, 224697L, 224700L, 224701L, 224702L,
           224703L, 224704L, 224705L, 224706L, 224707L, 224709L, 224738L,
           224746L, 224747L, 224750L, 226873L, 227187L),
           table = "chartevents", column = "itemid", callback = "all_flag"
        )
      ),
      eicu = list(
        list(table = "respiratorycare", column = "ventstartoffset",
             callback = "vent_flag"),
        list(table = "respiratorycare", column = "priorventstartoffset",
             callback = "vent_flag"),
        list(ids = c("Start", "Continued", "respFlowPtVentData"),
             table = "respiratorycharting", column = "respcharttypecat",
             callback = "all_flag")
      ),
      hirid = list(
        list(ids = 15001552L, table = "observations", column = "variableid",
             callback = "hirid_vent_start")
      )
    )
  ),
  vent_end = list(
    sources = list(
      mimic = list(
        list(ids = c(225468L, 225477L, 227194L), table = "procedureevents_mv",
             column = "itemid", callback = "all_flag"),
        list(ids = c(467L, 469L, 226732L), table = "chartevents",
             column = "itemid", callback = "all_flag")
      ),
      eicu = list(
        list(table = "respiratorycare", column = "ventendoffset",
             callback = "vent_flag"),
        list(table = "respiratorycare", column = "priorventendoffset",
             callback = "vent_flag"),
        list(ids = c("off", "Off", "Suspended"), table = "respiratorycharting",
             column = "respchartvalue", callback = "all_flag")
      ),
      hirid = list(
        list(ids = 15001552L, table = "observations", column = "variableid",
             callback = "hirid_vent_end")
      )
    )
  ),
  tracheostomy = list(
    sources = list(
      mimic = list(
        list(ids = c("1.0 ET/Trach", "No Response-ETT"), table = "chartevents",
             column = "value", callback = "all_flag")
      ),
      hirid = list(
        list(ids = 15001552L, table = "observations", column = "variableid",
             callback = "hirid_trach")
      )
    )
  ),
  rass_scale = list(
    sources = list(
      eicu = list(
        list(ids = "Sedation Score", table = "nursecharting",
             column = "nursingchartcelltypevalname",
             callback = "force_numeric_val_col")
      ),
      hirid = list(
        list(ids = 15001565L, table = "observations", column = "variableid")
      )
    )
  ),
  antibiotics = list(
    sources = list(
      mimic = list(
        list(ids = c(
          "aztreonam", "bactrim", "cephalexin", "chloramphenicol", "cipro",
          "flagyl", "metronidazole", "nitrofurantoin", "tazobactam",
          "rifampin", "sulfadiazine", "timentin", "trimethoprim",
          "(amika|gentami|vanco)cin",
          "(amoxi|ampi|dicloxa|naf|oxa|peni|pipera)cillin",
          "(azithro|clarithro|erythro|clinda|strepto|tobra|vanco)mycin",
          "cef(azolin|tazidime|adroxil|epime|otetan|otaxime|podoxime|uroxime)",
          "(doxy|mino|tetra)cycline",
          "(levofl|moxifl|ofl)oxacin|macro(bid|dantin)",
          "(una|zo)syn"), table = "prescriptions", column = "drug",
          regex = TRUE, callback = "mimic_abx_shift_flag"
        ),
        list(ids = c(
          225798L, 225837L, 225838L, 225840L, 225842L, 225843L, 225844L,
          225845L, 225847L, 225848L, 225850L, 225851L, 225853L, 225855L,
          225857L, 225859L, 225860L, 225862L, 225863L, 225865L, 225866L,
          225868L, 225869L, 225871L, 225873L, 225875L, 225876L, 225877L,
          225879L, 225881L, 225882L, 225883L, 225884L, 225885L, 225886L,
          225888L, 225889L, 225890L, 225892L, 225893L, 225895L, 225896L,
          225897L, 225898L, 225899L, 225900L, 225902L, 225903L, 225905L,
          227691L, 228003L), table = "inputevents_mv", column = "itemid",
          callback = "all_flag"
        )
      ),
      eicu = list(
        list(ids = c(
          "bactrim", "cipro", "flagyl", "metronidazole", "zithromax", "zosyn",
          "(((amika|cleo|ofloxa)|(azithro|clinda|tobra|vanco)my)c",
          "(ampi|oxa|peni|pipera)cill|cefazol|levaqu|rifamp)in"),
          table = "infusiondrug", column = "drugname", regex = TRUE,
          callback = "all_flag"
        ),
        list(ids = c(
          "cipro", "flagyl", "maxipime", "metronidazole", "tazobactam",
          "zosyn", "cef(azolin|epime)", "(((azithro|clinda|vanco)my|ofloxa",
          "vanco)c|levaqu|piperacill|roceph)in"), table = "medication",
          column = "drugname", regex = TRUE, callback = "all_flag"
        )
      ),
      hirid = list(
        list(ids = c(
              163L,     176L,     181L,     186L,     189L,     300L,     326L,
              331L,     351L,     405L, 1000234L, 1000272L, 1000273L, 1000274L,
          1000284L, 1000299L, 1000300L, 1000302L, 1000304L, 1000305L, 1000306L,
          1000315L, 1000317L, 1000318L, 1000320L, 1000321L, 1000322L, 1000335L,
          1000348L, 1000352L, 1000363L, 1000365L, 1000390L, 1000407L, 1000408L,
          1000424L, 1000425L, 1000426L, 1000437L, 1000483L, 1000507L, 1000508L,
          1000518L, 1000519L, 1000549L, 1000601L, 1000648L, 1000666L, 1000670L,
          1000671L, 1000760L, 1000781L, 1000791L, 1000797L, 1000812L, 1000825L,
          1000829L, 1000830L, 1000837L, 1000838L, 1000854L, 1000855L, 1000893L,
          1000894L, 1001005L, 1001068L, 1001075L, 1001079L, 1001084L, 1001086L,
          1001095L, 1001096L, 1001097L, 1001098L, 1001168L, 1001169L, 1001170L,
          1001171L, 1001173L, 1001193L, 1001198L), table = "pharma",
          column = "pharmaid", callback = "all_flag")
      )
    )
  ),
  fluid_sampling = list(
    sources = list(
      mimic = list(
        list(table = "microbiologyevents", column = "org_itemid",
             callback = "mimic_sampling", aux_time = "charttime")
      ),
      eicu = list(
        list(table = "microlab", column = "organism",
             callback = "eicu_sampling")
      )
    )
  ),
  troponin_i = list(
    unit = "ng/mL",
    sources = list(
      mimic = list(
        list(ids = 51002L, table = "labevents", column = "itemid")
      ),
      eicu = list(
        list(ids = "troponin - I", table = "lab", column = "labname")
      )
    )
  ),
  bilirubin_direct = list(
    unit = "mg/dL",
    sources = list(
      mimic = list(
        list(ids = 50883L, table = "labevents", column = "itemid")
      ),
      eicu = list(
        list(ids = "Bilirubin, Direct", table = "lab", column = "labname")
      ),
      hirid = list(
        list(ids = 24000560L, table = "observations", column = "variableid",
             callback = "multiply_hirid_bili")
      )
    )
  ),
  temperature = list(
    unit = "C",
    sources = list(
      mimic = list(
        list(ids = c(676L, 677L, 223762L), table = "chartevents",
             column = "itemid"),
        list(ids = c(678L, 679L, 223761L, 224027L), table = "chartevents",
             column = "itemid", callback = "fahrenheit_to_celsius")
      ),
      eicu = list(
        list(table = "vitalperiodic", column = "temperature")
      ),
      hirid = list(
        list(ids = c(410L, 400L), table = "observations",
             column = "variableid")
      )
    )
  ),
  et_co2 = list(
    unit = "mmHg",
    sources = list(
      mimic = list(
        list(ids = c(1817L, 228640L), table = "chartevents", column = "itemid")
      ),
      hirid = list(
        list(ids = c(2200L, 8290L, 30010009L), table = "observations",
             column = "variableid")
      )
    )
  ),
  insulin = list(
    sources = list(
      mimic = list(
        list(ids = c(30045L, 30100L), table = "inputevents_cv",
             column = "itemid"),
        list(ids = c(223258L, 223260L), table = "inputevents_mv",
             column = "itemid", amount_col = "amount", end_col = "endtime",
             callback = "distribute_amount")
      ),
      eicu = list(
        list(ids = "^insulin (250.+)?\\(((ml|units)/hr)?\\)$",
             table = "infusiondrug", column = "drugname", regex = TRUE)
      ),
      hirid = list(
        list(ids = c(15L, 1000724L), table = "pharma", column = "pharmaid")
      )
    )
  ),
  sex = list(
    sources = list(
      mimic = list(
        list(table = "patients", column = "gender")
      ),
      eicu = list(
        list(table = "patient", column = "gender")
      ),
      hirid = list(
        list(table = "general", column = "sex")
      )
    )
  ),
  age = list(
    unit = "years",
    sources = list(
      mimic = list(
        list(table = "patients", time_col = "dob", callback = "mimic_age")
      ),
      eicu = list(
        list(table = "patient", column = "age", callback = "eicu_age")
      ),
      hirid = list(
        list(table = "general", column = "age")
      )
    )
  ),
  weight = list(
    unit = "kg",
    sources = list(
      eicu = list(
        list(table = "patient", column = "admissionweight")
      )
    )
  ),
  vasopressin = list(
    unit = "mcg/kg/min",
    sources = list(
      mimic = list(
        list(ids = c(30051L, 222315L), table = "inputevents_mv",
             column = "itemid")
      ),
      eicu = list(
        list(ids = c("Vasopressin (ml/hr)", "Vasopressin (units/min)",
                     "Vasopressin ()"),
             table = "infusiondrug", column = "drugname",
             callback = "eicu_body_weight", weight_col = "patientweight")
      ),
      hirid = list(
        list(ids = c(112L, 113L), table = "pharma", column = "pharmaid")
      )
    )
  ),
  death = list(
    sources = list(
      mimic = list(
        list(table = "admissions", column = "hospital_expire_flag",
             callback = "mimic_death")
      ),
      eicu = list(
        list(table = "patient", column = "hospitaldischargestatus",
             callback = "eicu_death")
      ),
      hirid = list(
        list(ids = c(110L, 200L), table = "observations",
             column = "variableid", callback = "hirid_death")
      )
    )
  )
)

cfg <- lapply(cfg, function(x) {

  if ("mimic" %in% names(x[["sources"]]))
    x[["sources"]] <- c(x[["sources"]],
                        mimic_demo = list(x[["sources"]][["mimic"]]))

  if ("eicu" %in% names(x[["sources"]]))
    x[["sources"]] <- c(x[["sources"]],
                        eicu_demo = list(x[["sources"]][["eicu"]]))

  x[["sources"]] <- x[["sources"]][order(names(x[["sources"]]))]

  x
})

pkg_dir <- rprojroot::find_root(rprojroot::is_r_package)
cfg_dir <- file.path(pkg_dir, "inst", "extdata", "config")

ricu::set_config(cfg[order(names(cfg))], "concept-dict", cfg_dir)

devtools::install(pkg_dir)
