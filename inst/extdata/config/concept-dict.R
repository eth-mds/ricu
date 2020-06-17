
cfg <- list(
  heart_rate = list(
    unit = c("bpm", "/min"),
    min = 0,
    max = 300,
    sources = list(
      mimic = list(
        list(ids = c(211L, 220045L), table = "chartevents", sub_var = "itemid")
      ),
      eicu = list(
        list(table = "vitalperiodic", itm_vars = "heartrate",
             class = "col_itm")
      ),
      hirid = list(
        list(ids = 200L, table = "observations", sub_var = "variableid",
             class = c("hrd_itm", "sel_itm"))
      )
    )
  ),
  systolic_bp = list(
    unit = "mmHg",
    min = 0,
    max = 300,
    sources = list(
      mimic = list(
        list(ids = c(51L, 455L, 6701L, 220050L, 220179L),
             table = "chartevents", sub_var = "itemid")
      ),
      eicu = list(
        list(table = "vitalperiodic", itm_vars = "systemicsystolic",
             class = "col_itm")
      ),
      hirid = list(
        list(ids = 100L, table = "observations", sub_var = "variableid",
             class = c("hrd_itm", "sel_itm"))
      )
    )
  ),
  diastolic_bp = list(
    unit = "mmHg",
    min = 0,
    max = 200,
    sources = list(
      mimic = list(
        list(ids = c(8368L, 8441L, 8555L, 220051L, 220180L),
             table = "chartevents", sub_var = "itemid")
      ),
      eicu = list(
        list(table = "vitalperiodic", itm_vars = "systemicdiastolic",
             class = "col_itm")
      ),
      hirid = list(
        list(ids = 120L, table = "observations", sub_var = "variableid",
             class = c("hrd_itm", "sel_itm"))
      )
    )
  ),
  mean_bp = list(
    unit = "mmHg",
    min = 0,
    max = 250,
    sources = list(
      mimic = list(
        list(ids = c(52L, 443L, 456L, 6072L, 220052L, 220181L, 225312L),
             table = "chartevents", sub_var = "itemid")
      ),
      eicu = list(
        list(table = "vitalperiodic", itm_vars = "systemicmean",
             class = "col_itm"),
        list(table = "vitalaperiodic", itm_vars = "noninvasivemean",
             class = "col_itm")
      ),
      hirid = list(
        list(ids = 110L, table = "observations", sub_var = "variableid",
             class = c("hrd_itm", "sel_itm"))
      )
    )
  ),
  respiratory_rate = list(
    unit = "insp/min",
    min = 0,
    max = 120,
    sources = list(
      mimic = list(
        list(ids = c(618L, 619L, 220210L, 224688L, 224689L, 224690L),
             table = "chartevents", sub_var = "itemid")
      ),
      eicu = list(
        list(table = "vitalperiodic", itm_vars = "respiration",
             class = "col_itm")
      ),
      hirid = list(
        list(ids = c(300L, 310L), table = "observations",
             sub_var = "variableid", class = c("hrd_itm", "sel_itm"))
      )
    )
  ),
  o2_saturation = list(
    unit = c("%", "% Sat."),
    min = 70,
    max = 100,
    sources = list(
      mimic = list(
        list(ids = c(646L, 220277L, 226253L, 50817L), table = "chartevents",
             sub_var = "itemid")
      ),
      eicu = list(
        list(table = "vitalperiodic", itm_vars = "sao2", unit_val = "%",
             class = "col_itm"),
        list(ids = "O2 Sat (%)", table = "lab", sub_var = "labname")
      ),
      hirid = list(
        list(ids = c(4000L, 8280L, 20000800L), table = "observations",
             sub_var = "variableid", class = c("hrd_itm", "sel_itm"))
      )
    )
  ),
  fi_o2 = list(
    unit = "%",
    min = 21,
    max = 100,
    sources = list(
      mimic = list(
        list(ids = c(3420L, 50816L, 223835L), table = "labevents",
             sub_var = "itemid")
      ),
      eicu = list(
        list(ids = "FiO2", table = "respiratorycharting",
             sub_var = "respchartvaluelabel", callback = "percent_as_numeric"),
        list(ids = "FiO2", table = "lab", sub_var = "labname",
             callback = "eicu_fio2")
      ),
      hirid = list(
        list(ids = 2010L, table = "observations", sub_var = "variableid",
             class = c("hrd_itm", "sel_itm"))
      )
    )
  ),
  calculated_total_co2 = list(
    unit = "mEq/L",
    min = 5,
    max = 60,
    sources = list(
      mimic = list(
        list(ids = 50804L, table = "labevents", sub_var = "itemid")
      ),
      eicu = list(
        list(ids = "Total CO2", table = "lab", sub_var = "labname",
             callback = "eicu_total_co2")
      )
  )
  ),
  alanine_aminotransferase = list(
    unit = "IU/L",
    min = 0,
    sources = list(
      mimic = list(
        list(ids = 50861L, table = "labevents", sub_var = "itemid")
      ),
      eicu = list(
        list(ids = "ALT (SGPT)", table = "lab", sub_var = "labname")
      ),
      hirid = list(
        list(ids = 20002600L, table = "observations", sub_var = "variableid",
             class = c("hrd_itm", "sel_itm"))
      )
    )
  ),
  asparate_aminotransferase = list(
    unit = "IU/L",
    min = 0,
    sources = list(
      mimic = list(
        list(ids = 50878L, table = "labevents", sub_var = "itemid")
      ),
      eicu = list(
        list(ids = "AST (SGOT)", table = "lab", sub_var = "labname")
      ),
      hirid = list(
        list(ids = 24000330L, table = "observations", sub_var = "variableid",
             class = c("hrd_itm", "sel_itm"))
      )
    )
  ),
  phosphate = list(
    unit = "mg/dL",
    min = 0,
    max = 40,
    sources = list(
      mimic = list(
        list(ids = 50970L, table = "labevents", sub_var = "itemid")
      ),
      eicu = list(
        list(ids = "phosphate", table = "lab", sub_var = "labname")
      ),
      hirid = list(
        list(ids = 20002500L, table = "observations", sub_var = "variableid",
             callback = "multiply_hirid_phos", class = c("hrd_itm", "sel_itm"))
      )
    )
  ),
  urea_nitrogen = list(
    unit = "mg/dL",
    min = 0,
    max = 200,
    sources = list(
      mimic = list(
        list(ids = 51006L, table = "labevents", sub_var = "itemid")
      ),
      eicu = list(
        list(ids = "BUN", table = "lab", sub_var = "labname")
      ),
      hirid = list(
        list(ids = 20004100L, table = "observations", sub_var = "variableid",
             callback = "multiply_hirid_urea", class = c("hrd_itm", "sel_itm"))
      )
    )
  ),
  ph = list(
    min = 6.8,
    max = 8,
    sources = list(
      mimic = list(
        list(ids = 50820L, table = "labevents", sub_var = "itemid")
      ),
      eicu = list(
        list(ids = "pH", table = "lab", sub_var = "labname")
      ),
      hirid = list(
        list(ids = 20000300L, table = "observations", sub_var = "variableid",
             class = c("hrd_itm", "sel_itm"))
      )
    )
  ),
  bilirubin_total = list(
    unit = "mg/dL",
    min = 0,
    max = 100,
    sources = list(
      mimic = list(
        list(ids = 50885L, table = "labevents", sub_var = "itemid")
      ),
      eicu = list(
        list(ids = "total bilirubin", table = "lab", sub_var = "labname")
      ),
      hirid = list(
        list(ids = 20004300L, table = "observations", sub_var = "variableid",
             callback = "multiply_hirid_bili", class = c("hrd_itm", "sel_itm"))
      )
    )
  ),
  inr_pt = list(
    sources = list(
      mimic = list(
        list(ids = 51237L, table = "labevents", sub_var = "itemid")
      ),
      eicu = list(
        list(ids = "PT - INR", table = "lab", sub_var = "labname")
      ),
      hirid = list(
        list(ids = 24000567L, table = "observations", sub_var = "variableid",
             class = c("hrd_itm", "sel_itm"))
      )
    )
  ),
  platelet_count = list(
    unit = "K/uL",
    min = 5,
    max = 1200,
    sources = list(
      mimic = list(
        list(ids = 51265L, table = "labevents", sub_var = "itemid")
      ),
      eicu = list(
        list(ids = "platelets x 1000", table = "lab", sub_var = "labname")
      ),
      hirid = list(
        list(ids = 20000110L, table = "observations", sub_var = "variableid",
             class = c("hrd_itm", "sel_itm"))
      )
    )
  ),
  lactate = list(
    unit = "mmol/L",
    min = 0,
    max = 50,
    sources = list(
      mimic = list(
        list(ids = 50813L, table = "labevents", sub_var = "itemid")
      ),
      eicu = list(
        list(ids = "lactate", table = "lab", sub_var = "labname")
      ),
      hirid = list(
        list(ids = 24000524L, table = "observations", sub_var = "variableid",
             class = c("hrd_itm", "sel_itm"))
      )
    )
  ),
  lymphocytes = list(
    unit = "%",
    min = 0,
    max = 100,
    sources = list(
      mimic = list(
        list(ids = 51244L, table = "labevents", sub_var = "itemid")
      ),
      eicu = list(
        list(ids = "-lymphs", table = "lab", sub_var = "labname")
      ),
      hirid = list(
        list(ids = 24000480L, table = "observations", sub_var = "variableid",
             class = c("hrd_itm", "sel_itm"))
      )
    )
  ),
  bicarbonate = list(
    unit = "mEq/L",
    min = 5,
    max = 50,
    sources = list(
      mimic = list(
        list(ids = 50882L, table = "labevents", sub_var = "itemid")
      ),
      eicu = list(
        list(ids = "bicarbonate", table = "lab", sub_var = "labname")
      ),
      hirid = list(
        list(ids = 20004200L, table = "observations", sub_var = "variableid",
             class = c("hrd_itm", "sel_itm"))
      )
    )
  ),
  creatinine = list(
    unit = "mg/dL",
    min = 0,
    max = 15,
    sources = list(
      mimic = list(
        list(ids = 50912L, table = "labevents", sub_var = "itemid")
      ),
      eicu = list(
        list(ids = "creatinine", table = "lab", sub_var = "labname")
      ),
      hirid = list(
        list(ids = 20000600L, table = "observations", sub_var = "variableid",
             callback = "multiply_hirid_crea", class = c("hrd_itm", "sel_itm"))
      )
    )
  ),
  prothrombine_time = list(
    unit = "sec",
    min = 0,
    sources = list(
      mimic = list(
        list(ids = 51274L, table = "labevents", sub_var = "itemid")
      ),
      eicu = list(
        list(ids = "PT", table = "lab", sub_var = "labname")
      )
  )
  ),
  rdw = list(
    unit = "%",
    min = 0,
    max = 100,
    sources = list(
      mimic = list(
        list(ids = 51277L, table = "labevents", sub_var = "itemid")
      ),
        eicu = list(
        list(ids = "RDW", table = "lab", sub_var = "labname")
      )
  )
  ),
  alkaline_phosphatase = list(
    unit = "IU/L",
    min = 0,
    sources = list(
      mimic = list(
        list(ids = 50863L, table = "labevents", sub_var = "itemid")
      ),
      eicu = list(
        list(ids = "alkaline phos.", table = "lab", sub_var = "labname")
      ),
      hirid = list(
        list(ids = 20002700L, table = "observations", sub_var = "variableid",
             class = c("hrd_itm", "sel_itm"))
      )
    )
  ),
  white_blood_cells = list(
    unit = "K/uL",
    min = 0,
    sources = list(
      mimic = list(
        list(ids = 51301L, table = "labevents", sub_var = "itemid")
      ),
      eicu = list(
        list(ids = "WBC x 1000", table = "lab", sub_var = "labname")
      ),
      hirid = list(
        list(ids = 20000700L, table = "observations", sub_var = "variableid",
             class = c("hrd_itm", "sel_itm"))
      )
    )
  ),
  hemoglobin = list(
    unit = "g/dL",
    min = 4,
    max = 18,
    sources = list(
      mimic = list(
        list(ids = 51222L, table = "labevents", sub_var = "itemid")
      ),
      eicu = list(
        list(ids = "Hgb", table = "lab", sub_var = "labname")
      ),
      hirid = list(
        list(ids = c(24000548L, 24000836L, 20000900L), table = "observations",
             sub_var = "variableid", callback = "multiply_hirid_hemo",
             class = c("hrd_itm", "sel_itm"))
      )
    )
  ),
  hematocrit = list(
    unit = "%",
    min = 15,
    max = 60,
    sources = list(
      mimic = list(
        list(ids = 51221L, table = "labevents", sub_var = "itemid")
      ),
      eicu = list(
        list(ids = "Hct", table = "lab", sub_var = "labname")
      )
    )
  ),
  pa_co2 = list(
    unit = "mmHg",
    min = 10,
    max = 150,
    sources = list(
      mimic = list(
        list(ids = 50818L, table = "labevents", sub_var = "itemid")
      ),
      eicu = list(
        list(ids = "paCO2", table = "lab", sub_var = "labname")
      ),
      hirid = list(
        list(ids = 20001200L, table = "observations", sub_var = "variableid",
             class = c("hrd_itm", "sel_itm"))
      )
    )
  ),
  pa_o2 = list(
    unit = "mmHg",
    min = 40,
    max = 600,
    sources = list(
      mimic = list(
        list(ids = 50821L, table = "labevents", sub_var = "itemid")
      ),
      eicu = list(
        list(ids = "paO2", table = "lab", sub_var = "labname")
      ),
      hirid = list(
        list(ids = 20000200L, table = "observations", sub_var = "variableid",
             class = c("hrd_itm", "sel_itm"))
      )
    )
  ),
  mch = list(
    unit = "pg",
    min = 0,
    sources = list(
      mimic = list(
        list(ids = 51248L, table = "labevents", sub_var = "itemid")
      ),
      eicu = list(
        list(ids = "MCH", table = "lab", sub_var = "labname")
      ),
      hirid = list(
        list(ids = 24000160L, table = "observations", sub_var = "variableid",
             class = c("hrd_itm", "sel_itm"))
      )
    )
  ),
  mchc = list(
    unit = "%",
    min = 20,
    max = 50,
    sources = list(
      mimic = list(
        list(ids = 51249L, table = "labevents", sub_var = "itemid")
      ),
      eicu = list(
        list(ids = "MCHC", table = "lab", sub_var = "labname")
      ),
      hirid = list(
        list(ids = 24000170L, table = "observations", sub_var = "variableid",
             class = c("hrd_itm", "sel_itm"))
      )
    )
  ),
  mcv = list(
    unit = "fL",
    min = 50,
    max = 150,
    sources = list(
      mimic = list(
        list(ids = 51250L, table = "labevents", sub_var = "itemid")
      ),
      eicu = list(
        list(ids = "MCV", table = "lab", sub_var = "labname")
      ),
      hirid = list(
        list(ids = 24000150L, table = "observations", sub_var = "variableid",
             class = c("hrd_itm", "sel_itm"))
      )
    )
  ),
  ptt = list(
    unit = "sec",
    min = 0,
    sources = list(
      mimic = list(
        list(ids = 51275L, table = "labevents", sub_var = "itemid")
      ),
      eicu = list(
        list(ids = "PTT", table = "lab", sub_var = "labname")
      ),
      hirid = list(
        list(ids = 20004410L, table = "observations", sub_var = "variableid",
             class = c("hrd_itm", "sel_itm"))
      )
    )
  ),
  calcium = list(
    unit = "mg/dL",
    min = 4,
    max = 20,
    sources = list(
      mimic = list(
        list(ids = 50893L, table = "labevents", sub_var = "itemid")
      ),
      eicu = list(
        list(ids = "calcium", table = "lab", sub_var = "labname",
             callback = "eicu_calcium")
      ),
      hirid = list(
        list(ids = 20005100L, table = "observations", sub_var = "variableid",
             callback = "multiply_hirid_calc", class = c("hrd_itm", "sel_itm"))
      )
    )
  ),
  chloride = list(
    unit = "mEq/L",
    min = 80,
    max = 130,
    sources = list(
      mimic = list(
        list(ids = 50902L, table = "labevents", sub_var = "itemid")
      ),
      eicu = list(
        list(ids = "chloride", table = "lab", sub_var = "labname")
      ),
      hirid = list(
        list(ids = c(24000439L, 24000521L), table = "observations",
             sub_var = "variableid", class = c("hrd_itm", "sel_itm"))
      )
    )
  ),
  magnesium = list(
    unit = "mg/dL",
    min = 0.5,
    max = 5,
    sources = list(
      mimic = list(
        list(ids = 50960L, table = "labevents", sub_var = "itemid")
      ),
      eicu = list(
        list(ids = "magnesium", table = "lab", sub_var = "labname",
             callback = "eicu_magnesium")
      ),
      hirid = list(
        list(ids = 24000230L, table = "observations", sub_var = "variableid",
             callback = "multiply_hirid_magn", class = c("hrd_itm", "sel_itm"))
      )
    )
  ),
  potassium = list(
    unit = "mEq/L",
    min = 0,
    max = 10,
    sources = list(
      mimic = list(
        list(ids = 50971L, table = "labevents", sub_var = "itemid")
      ),
      eicu = list(
        list(ids = "potassium", table = "lab", sub_var = "labname")
      ),
      hirid = list(
        list(ids = c(20000500L, 24000520L, 24000833L, 24000867L),
             table = "observations", sub_var = "variableid",
             class = c("hrd_itm", "sel_itm"))
      )
    )
  ),
  sodium = list(
    unit = "mEq/L",
    min = 110,
    max = 165,
    sources = list(
      mimic = list(
        list(ids = 50983L, table = "labevents", sub_var = "itemid")
      ),
      eicu = list(
        list(ids = "sodium", table = "lab", sub_var = "labname")
      ),
      hirid = list(
        list(ids = c(20000400L, 24000519L, 24000658L, 24000835L, 24000866L),
             table = "observations", sub_var = "variableid",
             class = c("hrd_itm", "sel_itm"))
      )
    )
  ),
  basophils = list(
    unit = "%",
    min = 0,
    max = 50,
    sources = list(
      mimic = list(
        list(ids = 51146L, table = "labevents", sub_var = "itemid")
      ),
      eicu = list(
        list(ids = "-basos", table = "lab", sub_var = "labname")
      )
  )
  ),
  eosinophils = list(
    unit = "%",
    min = 0,
    max = 50,
    sources = list(
      mimic = list(
        list(ids = 51200L, table = "labevents", sub_var = "itemid")
      ),
      eicu = list(
        list(ids = "-eos", table = "lab", sub_var = "labname")
      )
  )
  ),
  neutrophils = list(
    unit = "%",
    min = 0,
    max = 100,
    sources = list(
      mimic = list(
        list(ids = 51256L, table = "labevents", sub_var = "itemid")
      ),
      eicu = list(
        list(ids = "-polys", table = "lab", sub_var = "labname")
      ),
      hirid = list(
        list(ids = 24000550L, table = "observations", sub_var = "variableid",
             class = c("hrd_itm", "sel_itm"))
      )
    )
  ),
  glucose = list(
    unit = "mg/dL",
    min = 0,
    max = 1000,
    sources = list(
      mimic = list(
        list(ids = c(50809L, 50931L), table = "labevents", sub_var = "itemid")
      ),
      eicu = list(
        list(ids = "glucose", table = "lab", sub_var = "labname"),
        list(ids = "bedside glucose", table = "lab", sub_var = "labname")
      ),
      hirid = list(
        list(ids = c(20005110L, 24000523L, 24000585L), table = "observations",
             sub_var = "variableid", callback = "multiply_hirid_gluc",
             class = c("hrd_itm", "sel_itm"))
      )
    )
  ),
  calcium_ionized = list(
    unit = "mmol/L",
    min = 0.5,
    max = 2,
    sources = list(
      mimic = list(
        list(ids = 50808L, table = "labevents", sub_var = "itemid")
      ),
      eicu = list(
        list(ids = "ionized calcium", table = "lab", sub_var = "labname")
      ),
      hirid = list(
        list(ids = 24000522L, table = "observations", sub_var = "variableid",
             class = c("hrd_itm", "sel_itm"))
      )
    )
  ),
  c_reactive_protein = list(
    unit = "mg/L",
    min = 0,
    sources = list(
      mimic = list(
        list(ids = 50889L, table = "labevents", sub_var = "itemid",
             callback = "crp_dl_to_l")
      ),
      eicu = list(
        list(ids = "CRP", table = "lab", sub_var = "labname",
             callback = "crp_dl_to_l")
      ),
      hirid = list(
        list(ids = 20002200L, table = "observations", sub_var = "variableid",
             class = c("hrd_itm", "sel_itm"))
      )
    )
  ),
  sedimentation_rate = list(
    unit = "mm/hr",
    min = 0,
    max = 200,
    sources = list(
      mimic = list(
        list(ids = 51288L, table = "labevents", sub_var = "itemid")
      ),
      hirid = list(
        list(ids = 24000668L, table = "observations", sub_var = "variableid",
             class = c("hrd_itm", "sel_itm"))
      )
    )
  ),
  carboxyhemoglobin = list(
    sources = list(
      eicu = list(
        list(ids = "Carboxyhemoglobin", table = "lab", sub_var = "labname")
      ),
      hirid = list(
        list(ids = 24000526L, table = "observations", sub_var = "variableid",
             class = c("hrd_itm", "sel_itm"))
      )
    )
  ),
  methemoglobin = list(
    unit = "%",
    min = 0,
    max = 100,
    sources = list(
      mimic = list(
        list(ids = 50814L, table = "labevents", sub_var = "itemid")
      ),
      eicu = list(
        list(ids = "Methemoglobin", table = "lab", sub_var = "labname")
      ),
      hirid = list(
        list(ids = 24000549L, table = "observations", sub_var = "variableid",
             class = c("hrd_itm", "sel_itm"))
      )
    )
  ),
  troponin_t = list(
    unit = "ng/mL",
    min = 0,
    sources = list(
      mimic = list(
        list(ids = 51003L, table = "labevents", sub_var = "itemid")
      ),
      eicu = list(
        list(ids = "troponin - T", table = "lab", sub_var = "labname")
      ),
      hirid = list(
        list(ids = 24000806L, table = "observations", sub_var = "variableid",
             class = c("hrd_itm", "sel_itm"))
      )
    )
  ),
  albumin = list(
    unit = "g/dL",
    min = 0,
    max = 6,
    sources = list(
      mimic = list(
        list(ids = 50862L, table = "labevents", sub_var = "itemid")
      ),
      eicu = list(
        list(ids = "albumin", table = "lab", sub_var = "labname")
      ),
      hirid = list(
        list(ids = 24000605L, table = "observations", sub_var = "variableid",
             callback = "multiply_hirid_albu", class = c("hrd_itm", "sel_itm"))
      )
    )
  ),
  fibrinogen = list(
    unit = "mg/dL",
    min = 0,
    max = 15,
    sources = list(
      mimic = list(
        list(ids = 51214L, table = "labevents", sub_var = "itemid")
      ),
      eicu = list(
        list(ids = "fibrinogen", table = "lab", sub_var = "labname")
      ),
      hirid = list(
        list(ids = 24000536L, table = "observations", sub_var = "variableid",
             callback = "multiply_hirid_fibr", class = c("hrd_itm", "sel_itm"))
      )
    )
  ),
  base_excess = list(
    unit = "mEq/L",
    min = -25,
    max = 25,
    sources = list(
      mimic = list(
        list(ids = 50802L, table = "labevents", sub_var = "itemid")
      ),
      eicu = list(
        list(ids = "Base Excess", table = "lab", sub_var = "labname")
      ),
      hirid = list(
        list(ids = 20001300L, table = "observations", sub_var = "variableid",
             class = c("hrd_itm", "sel_itm"))
      )
    )
  ),
  red_blood_cells = list(
    unit = "m/uL",
    min = 0,
    max = 20,
    sources = list(
      mimic = list(
        list(ids = 51279L, table = "labevents", sub_var = "itemid")
      ),
      eicu = list(
        list(ids = "RBC", table = "lab", sub_var = "labname")
      )
  )
  ),
  creatine_kinase = list(
    unit = "IU/L",
    min = 0,
    sources = list(
      mimic = list(
        list(ids = 50910L, table = "labevents", sub_var = "itemid")
      ),
      eicu = list(
        list(ids = "CPK", table = "lab", sub_var = "labname")
      ),
      hirid = list(
        list(ids = 24000210L, table = "observations", sub_var = "variableid",
             class = c("hrd_itm", "sel_itm"))
      )
    )
  ),
  creatine_kinase_mb = list(
    unit = "ng/mL",
    min = 0,
    sources = list(
      mimic = list(
        list(ids = 50911L, table = "labevents", sub_var = "itemid")
      ),
      eicu = list(
        list(ids = "CPK-MB", table = "lab", sub_var = "labname")
      ),
      hirid = list(
        list(ids = 24000220L, table = "observations", sub_var = "variableid",
             class = c("hrd_itm", "sel_itm"))
      )
    )
  ),
  gcs_eye = list(
    sources = list(
      mimic = list(
        list(ids = c(184L, 220739L), table = "chartevents", sub_var = "itemid")
      ),
      eicu = list(
        list(ids = "Eyes", table = "nursecharting",
             sub_var = "nursingchartcelltypevalname",
             callback = "force_numeric_val_var")
      ),
      hirid = list(
        list(ids = 10000300L, table = "observations", sub_var = "variableid",
             class = c("hrd_itm", "sel_itm"))
      )
    )
  ),
  gcs_verbal = list(
    sources = list(
      mimic = list(
        list(ids = c(723L, 223900L), table = "chartevents", sub_var = "itemid")
      ),
      eicu = list(
        list(ids = "Verbal", table = "nursecharting",
             sub_var = "nursingchartcelltypevalname",
             callback = "force_numeric_val_var")
      ),
      hirid = list(
        list(ids = 10000100L, table = "observations", sub_var = "variableid",
             class = c("hrd_itm", "sel_itm"))
      )
    )
  ),
  gcs_motor = list(
    sources = list(
      mimic = list(
        list(ids = c(454L, 223901L), table = "chartevents", sub_var = "itemid")
      ),
      eicu = list(
        list(ids = "Motor", table = "nursecharting",
             sub_var = "nursingchartcelltypevalname",
             callback = "force_numeric_val_var")
      ),
      hirid = list(
        list(ids = 10000200L, table = "observations", sub_var = "variableid",
             class = c("hrd_itm", "sel_itm"))
      )
    )
  ),
  gcs_total = list(
    sources = list(
      mimic = list(
        list(ids = 198L, table = "chartevents", sub_var = "itemid")
      ),
      eicu = list(
        list(ids = "GCS Total", table = "nursecharting",
             sub_var = "nursingchartcelltypevalname",
             callback = "force_numeric_val_var")
      )
  )
  ),
  urine_events = list(
    unit = "mL",
    min = 0,
    max = 2000,
    sources = list(
      mimic = list(
        list(ids = c(40055L,   40056L,  40057L,  40065L,  40069L,  40085L,
                     40086L,   40094L,  40096L,  40405L,  40428L,  40473L,
                     40715L,   43175L, 226557L, 226558L, 226559L, 226560L,
                    226561L,  226563L, 226564L, 226565L, 226566L, 226567L,
                    226584L, 227510L),
             table = "outputevents", sub_var = "itemid")
      ),
      eicu = list(
        list(ids = c("Urine", "URINE CATHETER"), table = "intakeoutput",
             sub_var = "celllabel"),
        list(regex = c("catheter.+output|output.+catheter"),
             table = "intakeoutput", sub_var = "celllabel", class = "rgx_itm")
      )
  )
  ),
  urine_hourly = list(
    unit = "mL",
    sources = list(
      hirid = list(
        list(ids = 10020000L, table = "observations", sub_var = "variableid",
             class = c("hrd_itm", "sel_itm"))
      )
    )
  ),
  urine_cumulative = list(
    unit = "mL",
    sources = list(
      hirid = list(
        list(ids = 30005110L, table = "observations", sub_var = "variableid",
             class = c("hrd_itm", "sel_itm"))
      )
    )
  ),
  dobutamine = list(
    unit = "mcg/kg/min",
    min = 0,
    max = 50,
    sources = list(
      mimic = list(
        list(ids = c(30042L, 30306L, 221653L), table = "inputevents_mv",
             sub_var = "itemid")
      ),
      eicu = list(
        list(ids = "Dobutamine (mcg/kg/min)", table = "infusiondrug",
             sub_var = "drugname", callback = "force_numeric_val_var"),
        list(ids = c("Dobutamine ()", "Dobutamine (ml/hr)"),
             table = "infusiondrug", sub_var = "drugname",
             callback = "eicu_body_weight", weight_var = "patientweight")
      ),
      hirid = list(
        list(ids = 426L, table = "pharma", sub_var = "pharmaid",
             callback = "hirid_vaso")
      )
    )
  ),
  dopamine = list(
    unit = "mcg/kg/min",
    min = 0,
    max = 50,
    sources = list(
      mimic = list(
        list(ids = c(30043L, 30125L, 30307L, 221662L),
             table = "inputevents_mv", sub_var = "itemid")
      ),
      eicu = list(
        list(ids = "Dopamine (mcg/kg/min)", table = "infusiondrug",
             sub_var = "drugname", callback = "force_numeric_val_var")
      )
  )
  ),
  norepinephrine = list(
    unit = "mcg/kg/min",
    min = 0,
    max = 3,
    sources = list(
      mimic = list(
        list(ids = c(30047L, 30120L, 221906L), table = "inputevents_mv",
             sub_var = "itemid")
      ),
      eicu = list(
        list(ids = "Norepinephrine (mcg/kg/min)", table = "infusiondrug",
             sub_var = "drugname", callback = "force_numeric_val_var"),
        list(ids = c("Norepinephrine (ml/hr)", "Norepinephrine (mcg/min)"),
             table = "infusiondrug", sub_var = "drugname",
             callback = "eicu_body_weight", weight_var = "patientweight")
      ),
      hirid = list(
        list(ids = c(1000462L, 1000656L, 1000657L, 1000658L), table = "pharma",
             sub_var = "pharmaid", callback = "hirid_vaso")
      )
    )
  ),
  epinephrine = list(
    unit = "mcg/kg/min",
    min = 0,
    max = 1.5,
    sources = list(
      mimic = list(
        list(ids = c(30044L, 30119L, 30309L, 221289L),
             table = "inputevents_mv", sub_var = "itemid")
      ),
      eicu = list(
        list(ids = "Epinephrine (mcg/kg/min)", table = "infusiondrug",
             sub_var = "drugname", callback = "force_numeric_val_var"),
        list(ids = c("Epinephrine (ml/hr)", "Epinephrine (mcg/min)"),
             table = "infusiondrug", sub_var = "drugname",
             callback = "eicu_body_weight", weight_var = "patientweight")
      ),
      hirid = list(
        list(ids = c(71L, 1000649L, 1000650L, 1000655L, 1000750L),
             table = "pharma", sub_var = "pharmaid", callback = "hirid_vaso")
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
           table = "chartevents", sub_var = "itemid", callback = "all_flag"
        )
      ),
      eicu = list(
        list(table = "respiratorycare", itm_vars = "ventstartoffset",
             callback = "vent_flag", class = "col_itm"),
        list(table = "respiratorycare", itm_vars = "priorventstartoffset",
             callback = "vent_flag", class = "col_itm"),
        list(ids = c("Start", "Continued", "respFlowPtVentData"),
             table = "respiratorycharting", sub_var = "respcharttypecat",
             callback = "all_flag")
      ),
      hirid = list(
        list(ids = 15001552L, table = "observations", sub_var = "variableid",
             callback = "hirid_vent_start", class = c("hrd_itm", "sel_itm"))
      )
    )
  ),
  vent_end = list(
    sources = list(
      mimic = list(
        list(ids = c(225468L, 225477L, 227194L), table = "procedureevents_mv",
             sub_var = "itemid", callback = "all_flag"),
        list(ids = c(467L, 469L, 226732L), table = "chartevents",
             sub_var = "itemid", callback = "all_flag")
      ),
      eicu = list(
        list(table = "respiratorycare", itm_vars = "ventendoffset",
             callback = "vent_flag", class = "col_itm"),
        list(table = "respiratorycare", itm_vars = "priorventendoffset",
             callback = "vent_flag", class = "col_itm"),
        list(ids = c("off", "Off", "Suspended"), table = "respiratorycharting",
             sub_var = "respchartvalue", callback = "all_flag")
      ),
      hirid = list(
        list(ids = 15001552L, table = "observations", sub_var = "variableid",
             callback = "hirid_vent_end", class = c("hrd_itm", "sel_itm"))
      )
    )
  ),
  tracheostomy = list(
    sources = list(
      mimic = list(
        list(ids = c("1.0 ET/Trach", "No Response-ETT"), table = "chartevents",
             sub_var = "value", callback = "all_flag")
      ),
      hirid = list(
        list(ids = 15001552L, table = "observations", sub_var = "variableid",
             callback = "hirid_trach", class = c("hrd_itm", "sel_itm"))
      )
    )
  ),
  rass_scale = list(
    sources = list(
      eicu = list(
        list(ids = "Sedation Score", table = "nursecharting",
             sub_var = "nursingchartcelltypevalname",
             callback = "force_numeric_val_var")
      ),
      hirid = list(
        list(ids = 15001565L, table = "observations", sub_var = "variableid",
             class = c("hrd_itm", "sel_itm"))
      )
    )
  ),
  antibiotics = list(
    sources = list(
      mimic = list(
        list(regex = paste(
          "aztreonam", "bactrim", "cephalexin", "chloramphenicol", "cipro",
          "flagyl", "metronidazole", "nitrofurantoin", "tazobactam",
          "rifampin", "sulfadiazine", "timentin", "trimethoprim", "(amika",
          "gentami", "vanco)cin", "(amoxi", "ampi", "dicloxa", "naf", "oxa",
          "peni", "pipera)cillin", "(azithro", "clarithro", "erythro",
          "clinda", "strepto", "tobra", "vanco)mycin", "cef(azolin",
          "tazidime", "adroxil", "epime", "otetan", "otaxime", "podoxime",
          "uroxime)", "(doxy", "mino", "tetra)cycline", "(levofl", "moxifl",
          "ofl)oxacin", "macro(bid", "dantin)", "(una", "zo)syn", sep = "|"),
          table = "prescriptions", sub_var = "drug",
          callback = "mimic_abx_shift_flag", class = "rgx_itm"
        ),
        list(ids = c(
          225798L, 225837L, 225838L, 225840L, 225842L, 225843L, 225844L,
          225845L, 225847L, 225848L, 225850L, 225851L, 225853L, 225855L,
          225857L, 225859L, 225860L, 225862L, 225863L, 225865L, 225866L,
          225868L, 225869L, 225871L, 225873L, 225875L, 225876L, 225877L,
          225879L, 225881L, 225882L, 225883L, 225884L, 225885L, 225886L,
          225888L, 225889L, 225890L, 225892L, 225893L, 225895L, 225896L,
          225897L, 225898L, 225899L, 225900L, 225902L, 225903L, 225905L,
          227691L, 228003L), table = "inputevents_mv", sub_var = "itemid",
          callback = "all_flag"
        )
      ),
      eicu = list(
        list(regex = paste(
          "bactrim", "cipro", "flagyl", "metronidazole", "zithromax", "zosyn",
          "(((amika", "cleo", "ofloxa)", "(azithro", "clinda", "tobra",
          "vanco)my)c", "(ampi", "oxa", "peni", "pipera)cill", "cefazol",
          "levaqu", "rifamp)in", sep = "|"), table = "infusiondrug",
          sub_var = "drugname", callback = "all_flag", class = "rgx_itm"
        ),
        list(regex = paste(
          "cipro", "flagyl", "maxipime", "metronidazole", "tazobactam",
          "zosyn", "cef(azolin", "epime)", "(((azithro", "clinda", "vanco)my",
          "ofloxa", "vanco)c", "levaqu", "piperacill", "roceph)in", sep = "|"),
          table = "medication", sub_var = "drugname", callback = "all_flag",
          class = "rgx_itm"
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
          sub_var = "pharmaid", callback = "all_flag")
      )
    )
  ),
  fluid_sampling = list(
    sources = list(
      mimic = list(
        list(table = "microbiologyevents", itm_vars = "org_itemid",
             callback = "mimic_sampling", aux_time = "charttime",
             class = "col_itm")
      ),
      eicu = list(
        list(table = "microlab", itm_vars = "organism",
             callback = "eicu_sampling", class = "col_itm")
      )
    )
  ),
  troponin_i = list(
    unit = "ng/mL",
    min = 0,
    sources = list(
      mimic = list(
        list(ids = 51002L, table = "labevents", sub_var = "itemid")
      ),
      eicu = list(
        list(ids = "troponin - I", table = "lab", sub_var = "labname")
      )
    )
  ),
  bilirubin_direct = list(
    unit = "mg/dL",
    min = 0,
    max = 50,
    sources = list(
      mimic = list(
        list(ids = 50883L, table = "labevents", sub_var = "itemid")
      ),
      eicu = list(
        list(ids = "direct bilirubin", table = "lab", sub_var = "labname")
      ),
      hirid = list(
        list(ids = 24000560L, table = "observations", sub_var = "variableid",
             callback = "multiply_hirid_bili", class = c("hrd_itm", "sel_itm"))
      )
    )
  ),
  temperature = list(
    unit = "C",
    min = 32,
    max = 42,
    sources = list(
      mimic = list(
        list(ids = c(676L, 677L, 223762L), table = "chartevents",
             sub_var = "itemid"),
        list(ids = c(678L, 679L, 223761L, 224027L), table = "chartevents",
             sub_var = "itemid", callback = "fahrenheit_to_celsius")
      ),
      eicu = list(
        list(table = "vitalperiodic", itm_vars = "temperature",
             class = "col_itm")
      ),
      hirid = list(
        list(ids = c(410L, 400L, 7100L), table = "observations",
             sub_var = "variableid", class = c("hrd_itm", "sel_itm"))
      )
    )
  ),
  et_co2 = list(
    unit = "mmHg",
    min = 10,
    max = 60,
    sources = list(
      mimic = list(
        list(ids = c(1817L, 228640L), table = "chartevents",
             sub_var = "itemid")
      ),
      hirid = list(
        list(ids = c(2200L, 8290L, 30010009L), table = "observations",
             sub_var = "variableid", class = c("hrd_itm", "sel_itm"))
      )
    )
  ),
  insulin = list(
    sources = list(
      mimic = list(
        list(ids = c(30045L, 30100L), table = "inputevents_cv",
             sub_var = "itemid"),
        list(ids = c(223258L, 223260L), table = "inputevents_mv",
             sub_var = "itemid", amount_var = "amount", end_var = "endtime",
             callback = "distribute_amount")
      ),
      eicu = list(
        list(regex = "^insulin (250.+)?\\(((ml|units)/hr)?\\)$",
             table = "infusiondrug", sub_var = "drugname",
             callback = "force_numeric_val_var", class = "rgx_itm")
      ),
      hirid = list(
        list(ids = c(15L, 1000724L), table = "pharma", sub_var = "pharmaid",
             callback = "hirid_insulin")
      )
    )
  ),
  sex = list(
    target = "id_tbl",
    levels = c("Female", "Male"),
    class = "fct_cncpt",
    sources = list(
      mimic = list(
        list(table = "patients", itm_vars = "gender", callback = "mf_sex",
             class = "col_itm")
      ),
      eicu = list(
        list(table = "patient", itm_vars = "gender", class = "col_itm")
      ),
      hirid = list(
        list(table = "general", itm_vars = "sex", callback = "mf_sex",
             class = "col_itm")
      )
    )
  ),
  age = list(
    unit = "years",
    min = 0,
    max = 100,
    target = "id_tbl",
    sources = list(
      mimic = list(
        list(table = "patients", itm_vars = "dob", callback = "mimic_age",
             class = "col_itm")
      ),
      eicu = list(
        list(table = "patient", itm_vars = "age", callback = "eicu_age",
             class = "col_itm")
      ),
      hirid = list(
        list(table = "general", itm_vars = "age", class = "col_itm")
      )
    )
  ),
  weight = list(
    unit = "kg",
    min = 0,
    max = 500,
    target = "id_tbl",
    sources = list(
      eicu = list(
        list(table = "patient", itm_vars = "admissionweight",
             class = "col_itm")
      )
    )
  ),
  vasopressin = list(
    unit = "mcg/kg/min",
    sources = list(
      mimic = list(
        list(ids = c(30051L, 222315L), table = "inputevents_mv",
             sub_var = "itemid")
      ),
      eicu = list(
        list(ids = c("Vasopressin (ml/hr)", "Vasopressin (units/min)",
                     "Vasopressin ()"),
             table = "infusiondrug", sub_var = "drugname",
             callback = "eicu_body_weight", weight_var = "patientweight")
      ),
      hirid = list(
        list(ids = c(112L, 113L), table = "pharma", sub_var = "pharmaid")
      )
    )
  ),
  death = list(
    target = "id_tbl",
    sources = list(
      mimic = list(
        list(table = "admissions", itm_vars = "hospital_expire_flag",
             callback = "mimic_death", class = "col_itm")
      ),
      eicu = list(
        list(table = "patient", itm_vars = "hospitaldischargestatus",
             callback = "eicu_death", class = "col_itm")
      ),
      hirid = list(
        list(ids = c(110L, 200L), table = "observations",
             sub_var = "variableid", item_var = "variableid",
             callback = "hirid_death", class = c("hrd_itm", "sel_itm"))
      )
    )
  ),
  admission_type = list(
    target = "id_tbl",
    levels = c("med", "surg", "other"),
    class = "fct_cncpt",
    sources = list(
      mimic = list(
        list(table = "services", itm_vars = "curr_service",
             callback = "mimic_adx", class = "col_itm")
      ),
      eicu = list(
        list(table = "admissiondx", itm_vars = "admitdxpath",
             callback = "eicu_adx", class = "col_itm")
      )
    )
  ),
  los_icu = list(
    unit = "days",
    min = 0,
    target = "id_tbl",
    sources = list(
      mimic = list(
        list(win_type = "icustay", class = "los_itm")
      ),
      eicu = list(
        list(win_type = "icustay", class = "los_itm")
      ),
      hirid = list(
        list(win_type = "icustay", class = "los_itm")
      )
    )
  ),
  los_hosp = list(
    unit = "days",
    min = 0,
    target = "id_tbl",
    sources = list(
      mimic = list(
        list(win_type = "hadm", class = "los_itm")
      ),
      eicu = list(
        list(win_type = "hadm", class = "los_itm")
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
