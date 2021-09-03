
strip_ws <- function(x) gsub("\\s+", " ", x)

cfg <- list(
  hr = list(
    unit = c("bpm", "/min"),
    min = 0,
    max = 300,
    description = "heart rate",
    category = "vitals",
    sources = list(
      mimic = list(
        list(ids = c(211L, 220045L), table = "chartevents", sub_var = "itemid")
      ),
      eicu = list(
        list(table = "vitalperiodic", val_var = "heartrate",
             class = "col_itm")
      ),
      hirid = list(
        list(ids = 200L, table = "observations", sub_var = "variableid",
             class = "hrd_itm")
      ),
      aumc = list(
        list(ids = 6640L, table = "numericitems", sub_var = "itemid")
      ),
      miiv = list(
        list(ids = 220045L, table = "chartevents", sub_var = "itemid")
      )
    )
  ),
  sbp = list(
    unit = c("mmHg", "mm Hg"),
    min = 0,
    max = 300,
    description = "systolic blood pressure",
    category = "vitals",
    sources = list(
      mimic = list(
        list(ids = c(51L, 455L, 6701L, 220050L, 220179L),
             table = "chartevents", sub_var = "itemid")
      ),
      eicu = list(
        list(table = "vitalperiodic", val_var = "systemicsystolic",
             class = "col_itm")
      ),
      hirid = list(
        list(ids = 100L, table = "observations", sub_var = "variableid",
             class = "hrd_itm")
      ),
      aumc = list(
        list(ids = 6641L, table = "numericitems", sub_var = "itemid")
      ),
      miiv = list(
        list(ids = c(220050L, 220179L), table = "chartevents",
             sub_var = "itemid")
      )
    )
  ),
  dbp = list(
    unit = c("mmHg", "mm Hg"),
    min = 0,
    max = 200,
    description = "diastolic blood pressure",
    category = "vitals",
    sources = list(
      mimic = list(
        list(ids = c(8368L, 8441L, 8555L, 220051L, 220180L),
             table = "chartevents", sub_var = "itemid")
      ),
      eicu = list(
        list(table = "vitalperiodic", val_var = "systemicdiastolic",
             class = "col_itm")
      ),
      hirid = list(
        list(ids = 120L, table = "observations", sub_var = "variableid",
             class = "hrd_itm")
      ),
      aumc = list(
        list(ids = 6643L, table = "numericitems", sub_var = "itemid")
      ),
      miiv = list(
        list(ids = c(220051L, 220180L), table = "chartevents",
             sub_var = "itemid")
      )
    )
  ),
  map = list(
    unit = c("mmHg", "mm Hg"),
    min = 0,
    max = 250,
    description = "mean arterial pressure",
    category = "vitals",
    sources = list(
      mimic = list(
        list(ids = c(52L, 443L, 456L, 6072L, 220052L, 220181L, 225312L),
             table = "chartevents", sub_var = "itemid")
      ),
      eicu = list(
        list(table = "vitalperiodic", val_var = "systemicmean",
             class = "col_itm"),
        list(table = "vitalaperiodic", val_var = "noninvasivemean",
             class = "col_itm")
      ),
      hirid = list(
        list(ids = c(110L, 610L), table = "observations",
             sub_var = "variableid", class = "hrd_itm")
      ),
      aumc = list(
        list(ids = 6642L, table = "numericitems", sub_var = "itemid")
      ),
      miiv = list(
        list(ids = c(220052L, 220181L, 225312L), table = "chartevents",
             sub_var = "itemid")
      )
    )
  ),
  resp = list(
    unit = c("insp/min", "/min"),
    min = 0,
    max = 120,
    description = "respiratory rate",
    category = "respiratory",
    sources = list(
      mimic = list(
        list(ids = c(618L, 619L, 220210L, 224688L, 224689L, 224690L),
             table = "chartevents", sub_var = "itemid")
      ),
      eicu = list(
        list(table = "vitalperiodic", val_var = "respiration",
             class = "col_itm")
      ),
      hirid = list(
        list(ids = c(300L, 310L), table = "observations",
             sub_var = "variableid", class = "hrd_itm")
      ),
      aumc = list(
        list(ids = c(8874L, 12266L), table = "numericitems",
             sub_var = "itemid")
      ),
      miiv = list(
        list(ids = c(220210L, 224688L, 224689L, 224690L),
             table = "chartevents", sub_var = "itemid")
      )
    )
  ),
  o2sat = list(
    unit = c("%", "% Sat."),
    min = 50,
    max = 100,
    description = "oxygen saturation",
    category = "respiratory",
    sources = list(
      mimic = list(
        list(ids = c(646L, 220277L, 226253L, 50817L), table = "chartevents",
             sub_var = "itemid")
      ),
      eicu = list(
        list(table = "vitalperiodic", val_var = "sao2", unit_val = "%",
             class = "col_itm"),
        list(ids = "O2 Sat (%)", table = "lab", sub_var = "labname")
      ),
      hirid = list(
        list(ids = c(4000L, 8280L, 20000800L), table = "observations",
             sub_var = "variableid", class = "hrd_itm")
      ),
      aumc = list(
        list(ids = c(6709L, 8903L), table = "numericitems",
             sub_var = "itemid"),
        list(ids = 12311L, table = "numericitems", sub_var = "itemid",
             callback = "transform_fun(binary_op(`*`, 100))")
      ),
      miiv = list(
        list(ids = c(220277L, 226253L, 50817L), table = "chartevents",
             sub_var = "itemid")
      )
    )
  ),
  fio2 = list(
    unit = "%",
    min = 21,
    max = 100,
    description = "fraction of inspired oxygen",
    category = "blood gas",
    sources = list(
      mimic = list(
        list(ids = c(3420L, 223835L), table = "chartevents",
             sub_var = "itemid"),
        list(ids = 50816L, table = "labevents", sub_var = "itemid")
      ),
      eicu = list(
        list(ids = "FiO2", table = "respiratorycharting",
             sub_var = "respchartvaluelabel",
             callback = "transform_fun(percent_as_numeric)"),
        list(ids = "FiO2", table = "lab", sub_var = "labname",
             callback = "convert_unit(set_val(NA), '%', 'mm\\\\(hg\\\\)')")
      ),
      hirid = list(
        list(ids = 2010L, table = "observations", sub_var = "variableid",
             class = "hrd_itm")
      ),
      aumc = list(
        list(ids = 12279L, table = "numericitems", sub_var = "itemid")
      ),
      miiv = list(
        list(ids = 223835L, table = "chartevents", sub_var = "itemid"),
        list(ids = 50816L, table = "labevents", sub_var = "itemid")
      )
    )
  ),
  tco2 = list(
    unit = "mEq/L",
    min = 5,
    max = 60,
    description = "totcal CO2",
    category = "blood gas",
    sources = list(
      mimic = list(
        list(ids = 50804L, table = "labevents", sub_var = "itemid")
      ),
      eicu = list(
        list(ids = "Total CO2", table = "lab", sub_var = "labname",
             callback = "convert_unit(set_val(NA), 'mEq/L', 'lpm')")
      ),
      miiv = list(
        list(ids = 50804L, table = "labevents", sub_var = "itemid")
      )
    )
  ),
  alt = list(
    unit = c("IU/L", "U/l"),
    min = 0,
    description = "alanine aminotransferase",
    category = "chemistry",
    sources = list(
      mimic = list(
        list(ids = 50861L, table = "labevents", sub_var = "itemid")
      ),
      eicu = list(
        list(ids = "ALT (SGPT)", table = "lab", sub_var = "labname")
      ),
      hirid = list(
        list(ids = 20002600L, table = "observations", sub_var = "variableid",
             class = "hrd_itm")
      ),
      aumc = list(
        list(ids = c(6800L, 11978L), table = "numericitems",
             sub_var = "itemid")
      ),
      miiv = list(
        list(ids = 50861L, table = "labevents", sub_var = "itemid")
      )
    )
  ),
  ast = list(
    unit = c("IU/L", "U/l"),
    min = 0,
    description = "aspartate aminotransferase",
    category = "chemistry",
    sources = list(
      mimic = list(
        list(ids = 50878L, table = "labevents", sub_var = "itemid")
      ),
      eicu = list(
        list(ids = "AST (SGOT)", table = "lab", sub_var = "labname")
      ),
      hirid = list(
        list(ids = 24000330L, table = "observations", sub_var = "variableid",
             class = "hrd_itm")
      ),
      aumc = list(
        list(ids = c(6806L, 11990L), table = "numericitems",
             sub_var = "itemid")
      ),
      miiv = list(
        list(ids = 50878L, table = "labevents", sub_var = "itemid")
      )
    )
  ),
  phos = list(
    unit = "mg/dL",
    min = 0,
    max = 40,
    description = "phosphate",
    category = "chemistry",
    sources = list(
      mimic = list(
        list(ids = 50970L, table = "labevents", sub_var = "itemid")
      ),
      eicu = list(
        list(ids = "phosphate", table = "lab", sub_var = "labname")
      ),
      hirid = list(
        list(ids = 20002500L, table = "observations", sub_var = "variableid",
             callback = "convert_unit(binary_op(`*`, 3.097521), 'mg/dL')",
             class = "hrd_itm")
      ),
      aumc = list(
        list(ids = c(6828L, 9935L), table = "numericitems",
             sub_var = "itemid",
             callback = "convert_unit(binary_op(`*`, 3.097521), 'mg/dL')")
      ),
      miiv = list(
        list(ids = 50970L, table = "labevents", sub_var = "itemid")
      )
    )
  ),
  bun = list(
    unit = "mg/dL",
    min = 0,
    max = 200,
    description = "blood urea nitrogen",
    category = "chemistry",
    sources = list(
      mimic = list(
        list(ids = 51006L, table = "labevents", sub_var = "itemid")
      ),
      eicu = list(
        list(ids = "BUN", table = "lab", sub_var = "labname")
      ),
      hirid = list(
        list(ids = 20004100L, table = "observations", sub_var = "variableid",
             callback = "convert_unit(binary_op(`*`, 2.8), 'mg/dL')",
             class = "hrd_itm")
      ),
      aumc = list(
        list(ids = c(6850L, 9943L), table = "numericitems",
             sub_var = "itemid",
             callback = "convert_unit(binary_op(`*`, 2.8), 'mg/dL')")
      ),
      miiv = list(
        list(ids = 51006L, table = "labevents", sub_var = "itemid")
      )
    )
  ),
  ph = list(
    min = 6.8,
    max = 8,
    description = "pH of blood",
    category = "blood gas",
    sources = list(
      mimic = list(
        list(ids = 50820L, table = "labevents", sub_var = "itemid")
      ),
      eicu = list(
        list(ids = "pH", table = "lab", sub_var = "labname")
      ),
      hirid = list(
        list(ids = 20000300L, table = "observations", sub_var = "variableid",
             class = "hrd_itm")
      ),
      aumc = list(
        list(ids = c(6848L, 12310L), table = "numericitems",
             sub_var = "itemid")
      ),
      miiv = list(
        list(ids = 50820L, table = "labevents", sub_var = "itemid")
      )
    )
  ),
  bili = list(
    unit = "mg/dL",
    min = 0,
    max = 100,
    description = "total bilirubin",
    category = "chemistry",
    sources = list(
      mimic = list(
        list(ids = 50885L, table = "labevents", sub_var = "itemid")
      ),
      eicu = list(
        list(ids = "total bilirubin", table = "lab", sub_var = "labname")
      ),
      hirid = list(
        list(ids = 20004300L, table = "observations", sub_var = "variableid",
             callback = "convert_unit(binary_op(`*`, 0.058467), 'mg/dL')",
             class = "hrd_itm")
      ),
      aumc = list(
        list(ids = c(6813L, 9945L), table = "numericitems",
             sub_var = "itemid",
             callback = "convert_unit(binary_op(`*`, 0.058467), 'mg/dL')")
      ),
      miiv = list(
        list(ids = 50885L, table = "labevents", sub_var = "itemid")
      )
    )
  ),
  inr_pt = list(
    description = "prothrombin time/international normalized ratio",
    category = "hematology",
    sources = list(
      mimic = list(
        list(ids = 51237L, table = "labevents", sub_var = "itemid")
      ),
      eicu = list(
        list(ids = "PT - INR", table = "lab", sub_var = "labname")
      ),
      hirid = list(
        list(ids = 24000567L, table = "observations", sub_var = "variableid",
             class = "hrd_itm")
      ),
      aumc = list(
        list(ids = 11893L, table = "numericitems", sub_var = "itemid")
      ),
      miiv = list(
        list(ids = 51237L, table = "labevents", sub_var = "itemid")
      )
    )
  ),
  plt = list(
    unit = c("K/uL", "G/l"),
    min = 5,
    max = 1200,
    description = "platelet count",
    category = "hematology",
    sources = list(
      mimic = list(
        list(ids = 51265L, table = "labevents", sub_var = "itemid")
      ),
      eicu = list(
        list(ids = "platelets x 1000", table = "lab", sub_var = "labname")
      ),
      hirid = list(
        list(ids = 20000110L, table = "observations", sub_var = "variableid",
             class = "hrd_itm")
      ),
      aumc = list(
        list(ids = c(6797L, 9964L, 10409L, 14252L), table = "numericitems",
             sub_var = "itemid")
      ),
      miiv = list(
        list(ids = 51265L, table = "labevents", sub_var = "itemid")
      )
    )
  ),
  lact = list(
    unit = "mmol/L",
    min = 0,
    max = 50,
    description = "lactate",
    category = "blood gas",
    sources = list(
      mimic = list(
        list(ids = 50813L, table = "labevents", sub_var = "itemid")
      ),
      eicu = list(
        list(ids = "lactate", table = "lab", sub_var = "labname")
      ),
      hirid = list(
        list(ids = 24000524L, table = "observations", sub_var = "variableid",
             class = "hrd_itm")
      ),
      aumc = list(
        list(ids = c(6837L, 9580L, 10053L), table = "numericitems",
             sub_var = "itemid")
      ),
      miiv = list(
        list(ids = 50813L, table = "labevents", sub_var = "itemid")
      )
    )
  ),
  lymph = list(
    unit = "%",
    min = 0,
    max = 100,
    description = "lymphocytes",
    category = "hematology",
    sources = list(
      mimic = list(
        list(ids = 51244L, table = "labevents", sub_var = "itemid")
      ),
      eicu = list(
        list(ids = "-lymphs", table = "lab", sub_var = "labname")
      ),
      hirid = list(
        list(ids = 24000480L, table = "observations", sub_var = "variableid",
             callback = "blood_cell_ratio", class = "hrd_itm")
      ),
      aumc = list(
        list(ids = 11846L, table = "numericitems", sub_var = "itemid"),
        list(ids = 14258L, table = "numericitems", sub_var = "itemid",
             callback = "blood_cell_ratio")
      ),
      miiv = list(
        list(ids = 51244L, table = "labevents", sub_var = "itemid")
      )
    )
  ),
  bicar = list(
    unit = c("mEq/L", "mmol/l"),
    min = 5,
    max = 50,
    description = "bicarbonate",
    category = "chemistry",
    sources = list(
      mimic = list(
        list(ids = 50882L, table = "labevents", sub_var = "itemid")
      ),
      eicu = list(
        list(ids = "bicarbonate", table = "lab", sub_var = "labname")
      ),
      hirid = list(
        list(ids = 20004200L, table = "observations", sub_var = "variableid",
             class = "hrd_itm")
      ),
      aumc = list(
        list(ids = c(6810L, 9992L), table = "numericitems",
             sub_var = "itemid")
      ),
      miiv = list(
        list(ids = 50882L, table = "labevents", sub_var = "itemid")
      )
    )
  ),
  crea = list(
    unit = "mg/dL",
    min = 0,
    max = 15,
    description = "creatinine",
    category = "chemistry",
    sources = list(
      mimic = list(
        list(ids = 50912L, table = "labevents", sub_var = "itemid")
      ),
      eicu = list(
        list(ids = "creatinine", table = "lab", sub_var = "labname")
      ),
      hirid = list(
        list(ids = 20000600L, table = "observations", sub_var = "variableid",
             callback = "convert_unit(binary_op(`*`, 0.011312), 'mg/dL')",
             class = "hrd_itm")
      ),
      aumc = list(
        list(ids = c(6836L, 9941L, 14216L), table = "numericitems",
             sub_var = "itemid",
             callback = "convert_unit(binary_op(`*`, 0.011309), 'mg/dL')")
      ),
      miiv = list(
        list(ids = 50912L, table = "labevents", sub_var = "itemid")
      )
    )
  ),
  pt = list(
    unit = "sec",
    min = 0,
    description = "prothrombine time",
    category = "hematology",
    sources = list(
      mimic = list(
        list(ids = 51274L, table = "labevents", sub_var = "itemid")
      ),
      eicu = list(
        list(ids = "PT", table = "lab", sub_var = "labname")
      ),
      aumc = list(
        list(ids = 6789L, table = "numericitems", sub_var = "itemid")
      ),
      miiv = list(
        list(ids = 51274L, table = "labevents", sub_var = "itemid")
      )
    )
  ),
  rdw = list(
    unit = "%",
    min = 0,
    max = 100,
    description = "erythrocyte distribution width",
    category = "hematology",
    sources = list(
      mimic = list(
        list(ids = 51277L, table = "labevents", sub_var = "itemid")
      ),
        eicu = list(
        list(ids = "RDW", table = "lab", sub_var = "labname")
      ),
      aumc = list(
        list(ids = 18952L, table = "numericitems", sub_var = "itemid")
      ),
      miiv = list(
        list(ids = 51277L, table = "labevents", sub_var = "itemid")
      )
    )
  ),
  alp = list(
    unit = c("IU/L", "U/l"),
    min = 0,
    description = "alkaline phosphatase",
    category = "chemistry",
    sources = list(
      mimic = list(
        list(ids = 50863L, table = "labevents", sub_var = "itemid")
      ),
      eicu = list(
        list(ids = "alkaline phos.", table = "lab", sub_var = "labname")
      ),
      hirid = list(
        list(ids = 20002700L, table = "observations", sub_var = "variableid",
             class = "hrd_itm")
      ),
      aumc = list(
        list(ids = c(6803L, 11984L), table = "numericitems",
             sub_var = "itemid")
      ),
      miiv = list(
        list(ids = 50863L, table = "labevents", sub_var = "itemid")
      )
    )
  ),
  wbc = list(
    unit = c("K/uL", "G/l"),
    min = 0,
    description = "white blood cell count",
    category = "hematology",
    sources = list(
      mimic = list(
        list(ids = 51301L, table = "labevents", sub_var = "itemid")
      ),
      eicu = list(
        list(ids = "WBC x 1000", table = "lab", sub_var = "labname")
      ),
      hirid = list(
        list(ids = 20000700L, table = "observations", sub_var = "variableid",
             class = "hrd_itm")
      ),
      aumc = list(
        list(ids = c(6779L, 9965L), table = "numericitems",
             sub_var = "itemid")
      ),
      miiv = list(
        list(ids = 51301L, table = "labevents", sub_var = "itemid")
      )
    )
  ),
  hgb = list(
    unit = "g/dL",
    min = 4,
    max = 18,
    description = "hemoglobin",
    category = "hematology",
    sources = list(
      mimic = list(
        list(ids = 51222L, table = "labevents", sub_var = "itemid")
      ),
      eicu = list(
        list(ids = "Hgb", table = "lab", sub_var = "labname")
      ),
      hirid = list(
        list(ids = c(24000548L, 24000836L, 20000900L), table = "observations",
             sub_var = "variableid",
             callback = "convert_unit(binary_op(`*`, 0.1), 'g/dL', 'g/l')",
             class = "hrd_itm")
      ),
      aumc = list(
        list(ids = c(6778L, 9553L, 9960L, 10286L, 19703L),
             table = "numericitems", sub_var = "itemid",
             callback = "convert_unit(binary_op(`*`, 1.611344), 'g/dL')")
      ),
      miiv = list(
        list(ids = 51222L, table = "labevents", sub_var = "itemid")
      )
    )
  ),
  hct = list(
    unit = "%",
    min = 15,
    max = 60,
    description = "hematocrit",
    category = "hematology",
    sources = list(
      mimic = list(
        list(ids = 51221L, table = "labevents", sub_var = "itemid")
      ),
      eicu = list(
        list(ids = "Hct", table = "lab", sub_var = "labname")
      ),
      aumc = list(
        list(ids = c(6777L, 11423L, 11545L), table = "numericitems",
             sub_var = "itemid",
             callback = "transform_fun(binary_op(`*`, 100))")
      ),
      miiv = list(
        list(ids = 51221L, table = "labevents", sub_var = "itemid")
      )
    )
  ),
  pco2 = list(
    unit = c("mmHg", "mm Hg"),
    min = 10,
    max = 150,
    description = "CO2 partial pressure",
    category = "blood gas",
    sources = list(
      mimic = list(
        list(ids = 50818L, table = "labevents", sub_var = "itemid")
      ),
      eicu = list(
        list(ids = "paCO2", table = "lab", sub_var = "labname")
      ),
      hirid = list(
        list(ids = 20001200L, table = "observations", sub_var = "variableid",
             class = "hrd_itm")
      ),
      aumc = list(
        list(ids = c(6846L, 9990L, 21213L), table = "numericitems",
             sub_var = "itemid")
      ),
      miiv = list(
        list(ids = 50818L, table = "labevents", sub_var = "itemid")
      )
    )
  ),
  po2 = list(
    unit = c("mmHg", "mm Hg"),
    min = 40,
    max = 600,
    description = "O2 partial pressure",
    category = "blood gas",
    sources = list(
      mimic = list(
        list(ids = 50821L, table = "labevents", sub_var = "itemid")
      ),
      eicu = list(
        list(ids = "paO2", table = "lab", sub_var = "labname")
      ),
      hirid = list(
        list(ids = 20000200L, table = "observations", sub_var = "variableid",
             class = "hrd_itm")
      ),
      aumc = list(
        list(ids = c(7433L, 9996L, 21214L), table = "numericitems",
             sub_var = "itemid")
      ),
      miiv = list(
        list(ids = 50821L, table = "labevents", sub_var = "itemid")
      )
    )
  ),
  mch = list(
    unit = "pg",
    min = 0,
    description = "mean cell hemoglobin",
    category = "hematology",
    sources = list(
      mimic = list(
        list(ids = 51248L, table = "labevents", sub_var = "itemid")
      ),
      eicu = list(
        list(ids = "MCH", table = "lab", sub_var = "labname")
      ),
      hirid = list(
        list(ids = 24000160L, table = "observations", sub_var = "variableid",
             class = "hrd_itm")
      ),
      aumc = list(
        list(ids = 11679L, table = "numericitems", sub_var = "itemid",
             callback = "convert_unit(binary_op(`*`, 0.016113), 'pg')")
      ),
      miiv = list(
        list(ids = 51248L, table = "labevents", sub_var = "itemid")
      )
    )
  ),
  mchc = list(
    unit = "%",
    min = 20,
    max = 50,
    description = "mean corpuscular hemoglobin concentration",
    category = "hematology",
    sources = list(
      mimic = list(
        list(ids = 51249L, table = "labevents", sub_var = "itemid")
      ),
      eicu = list(
        list(ids = "MCHC", table = "lab", sub_var = "labname")
      ),
      hirid = list(
        list(ids = 24000170L, table = "observations", sub_var = "variableid",
             callback = "convert_unit(binary_op(`*`, 0.16114), '%')",
             class = "hrd_itm")
      ),
      aumc = list(
        list(ids = 18666L, table = "numericitems", sub_var = "itemid",
             callback = "convert_unit(binary_op(`*`, 1.611), '%')")
      ),
      miiv = list(
        list(ids = 51249L, table = "labevents", sub_var = "itemid")
      )
    )
  ),
  mcv = list(
    unit = "fL",
    min = 50,
    max = 150,
    description = "mean corpuscular volume",
    category = "hematology",
    sources = list(
      mimic = list(
        list(ids = 51250L, table = "labevents", sub_var = "itemid")
      ),
      eicu = list(
        list(ids = "MCV", table = "lab", sub_var = "labname")
      ),
      hirid = list(
        list(ids = 24000150L, table = "observations", sub_var = "variableid",
             class = "hrd_itm")
      ),
      aumc = list(
        list(ids = 9968L, table = "numericitems", sub_var = "itemid")
      ),
      miiv = list(
        list(ids = 51250L, table = "labevents", sub_var = "itemid")
      )
    )
  ),
  ptt = list(
    unit = "sec",
    min = 0,
    description = "partial thromboplastin time",
    category = "hematology",
    sources = list(
      mimic = list(
        list(ids = 51275L, table = "labevents", sub_var = "itemid")
      ),
      eicu = list(
        list(ids = "PTT", table = "lab", sub_var = "labname")
      ),
      hirid = list(
        list(ids = 20004410L, table = "observations", sub_var = "variableid",
             class = "hrd_itm")
      ),
      aumc = list(
        list(ids = 17982L, table = "numericitems", sub_var = "itemid")
      ),
      miiv = list(
        list(ids = 51275L, table = "labevents", sub_var = "itemid")
      )
    )
  ),
  ca = list(
    unit = "mg/dL",
    min = 4,
    max = 20,
    description = "calcium",
    category = "chemistry",
    sources = list(
      mimic = list(
        list(ids = 50893L, table = "labevents", sub_var = "itemid")
      ),
      eicu = list(
        list(ids = "calcium", table = "lab", sub_var = "labname",
             callback = "convert_unit(binary_op(`*`, 4), 'mg/dL', 'mmol/l')")
      ),
      hirid = list(
        list(ids = 20005100L, table = "observations", sub_var = "variableid",
             callback = "convert_unit(binary_op(`*`, 4.008), 'mg/dL')",
             class = "hrd_itm")
      ),
      aumc = list(
        list(ids = c(6817L, 9933L), table = "numericitems", sub_var = "itemid",
             callback = "convert_unit(binary_op(`*`, 4.008), 'mg/dL')")
      ),
      miiv = list(
        list(ids = 50893L, table = "labevents", sub_var = "itemid")
      )
    )
  ),
  cl = list(
    unit = c("mEq/L", "mmol/l"),
    min = 80,
    max = 130,
    description = "chloride",
    category = "chemistry",
    sources = list(
      mimic = list(
        list(ids = 50902L, table = "labevents", sub_var = "itemid")
      ),
      eicu = list(
        list(ids = "chloride", table = "lab", sub_var = "labname")
      ),
      hirid = list(
        list(ids = c(24000439L, 24000521L), table = "observations",
             sub_var = "variableid", class = "hrd_itm")
      ),
      aumc = list(
        list(ids = 9930L, table = "numericitems", sub_var = "itemid")
      ),
      miiv = list(
        list(ids = 50902L, table = "labevents", sub_var = "itemid")
      )
    )
  ),
  mg = list(
    unit = "mg/dL",
    min = 0.5,
    max = 5,
    description = "magnesium",
    category = "chemistry",
    sources = list(
      mimic = list(
        list(ids = 50960L, table = "labevents", sub_var = "itemid")
      ),
      eicu = list(
        list(ids = "magnesium", table = "lab", sub_var = "labname",
          callback = "convert_unit(binary_op(`/`, 1.215), 'mg/dL', 'mEq/L')"
        )
      ),
      hirid = list(
        list(ids = 24000230L, table = "observations", sub_var = "variableid",
             callback = "convert_unit(binary_op(`*`, 2.431), 'mg/dL')",
             class = "hrd_itm")
      ),
      aumc = list(
        list(ids = c(6839L, 9952L), table = "numericitems", sub_var = "itemid",
             callback = "convert_unit(binary_op(`*`, 2.431), 'mg/dL')")
      ),
      miiv = list(
        list(ids = 50960L, table = "labevents", sub_var = "itemid")
      )
    )
  ),
  k = list(
    unit = c("mEq/L", "mmol/l"),
    min = 0,
    max = 10,
    description = "potassium",
    category = "chemistry",
    sources = list(
      mimic = list(
        list(ids = 50971L, table = "labevents", sub_var = "itemid")
      ),
      eicu = list(
        list(ids = "potassium", table = "lab", sub_var = "labname")
      ),
      hirid = list(
        list(ids = c(20000500L, 24000520L, 24000833L, 24000867L),
             table = "observations", sub_var = "variableid", class = "hrd_itm")
      ),
      aumc = list(
        list(ids = c(6835, 9556, 9927, 10285), table = "numericitems",
             sub_var = "itemid")
      ),
      miiv = list(
        list(ids = 50971L, table = "labevents", sub_var = "itemid")
      )
    )
  ),
  na = list(
    unit = c("mEq/L", "mmol/l"),
    min = 110,
    max = 165,
    description = "sodium",
    category = "chemistry",
    sources = list(
      mimic = list(
        list(ids = 50983L, table = "labevents", sub_var = "itemid")
      ),
      eicu = list(
        list(ids = "sodium", table = "lab", sub_var = "labname")
      ),
      hirid = list(
        list(ids = c(20000400L, 24000519L, 24000658L, 24000835L, 24000866L),
             table = "observations", sub_var = "variableid", class = "hrd_itm")
      ),
      aumc = list(
        list(ids = c(6840, 9555, 9924, 10284), table = "numericitems",
             sub_var = "itemid")
      ),
      miiv = list(
        list(ids = 50983L, table = "labevents", sub_var = "itemid")
      )
    )
  ),
  basos = list(
    unit = "%",
    min = 0,
    max = 50,
    description = "basophils",
    category = "hematology",
    sources = list(
      mimic = list(
        list(ids = 51146L, table = "labevents", sub_var = "itemid")
      ),
      eicu = list(
        list(ids = "-basos", table = "lab", sub_var = "labname")
      ),
      aumc = list(
        list(ids = 11710L, table = "numericitems", sub_var = "itemid"),
        list(ids = 14256L, table = "numericitems", sub_var = "itemid",
             callback = "blood_cell_ratio")
      ),
      miiv = list(
        list(ids = 51146L, table = "labevents", sub_var = "itemid")
      )
    )
  ),
  eos = list(
    unit = "%",
    min = 0,
    max = 50,
    description = "eosinophils",
    category = "hematology",
    sources = list(
      mimic = list(
        list(ids = 51200L, table = "labevents", sub_var = "itemid")
      ),
      eicu = list(
        list(ids = "-eos", table = "lab", sub_var = "labname")
      ),
      aumc = list(
        list(ids = 6773L, table = "numericitems", sub_var = "itemid"),
        list(ids = 9967L, table = "numericitems", sub_var = "itemid",
             callback = "blood_cell_ratio")
      ),
      miiv = list(
        list(ids = 51200L, table = "labevents", sub_var = "itemid")
      )
    )
  ),
  neut = list(
    unit = "%",
    min = 0,
    max = 100,
    description = "neutrophils",
    category = "hematology",
    sources = list(
      mimic = list(
        list(ids = 51256L, table = "labevents", sub_var = "itemid")
      ),
      eicu = list(
        list(ids = "-polys", table = "lab", sub_var = "labname")
      ),
      hirid = list(
        list(ids = 24000550L, table = "observations", sub_var = "variableid",
             class = "hrd_itm")
      ),
      aumc = list(
        list(ids = c(6786L, 11856L), table = "numericitems",
             sub_var = "itemid"),
        list(ids = 14254L, table = "numericitems", sub_var = "itemid",
             callback = "blood_cell_ratio")
      ),
      miiv = list(
        list(ids = 51256L, table = "labevents", sub_var = "itemid")
      )
    )
  ),
  glu = list(
    unit = "mg/dL",
    min = 0,
    max = 1000,
    description = "glucose",
    category = "chemistry",
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
             sub_var = "variableid",
             callback = "convert_unit(binary_op(`*`, 18.016), 'mg/dL')",
             class = "hrd_itm")
      ),
      aumc = list(
        list(ids = c(6833L, 9557L, 9947L), table = "numericitems",
             sub_var = "itemid",
             callback = "convert_unit(binary_op(`*`, 18.016), 'mg/dL')")
      ),
      miiv = list(
        list(ids = c(50809L, 50931L), table = "labevents", sub_var = "itemid")
      )
    )
  ),
  cai = list(
    unit = "mmol/L",
    min = 0.5,
    max = 2,
    description = "calcium ionized",
    category = "blood gas",
    sources = list(
      mimic = list(
        list(ids = 50808L, table = "labevents", sub_var = "itemid")
      ),
      eicu = list(
        list(ids = "ionized calcium", table = "lab", sub_var = "labname")
      ),
      hirid = list(
        list(ids = 24000522L, table = "observations", sub_var = "variableid",
             class = "hrd_itm")
      ),
      aumc = list(
        list(ids = c(6815L, 8915L, 9560L, 9561L, 10267L),
             table = "numericitems", sub_var = "itemid")
      ),
      miiv = list(
        list(ids = 50808L, table = "labevents", sub_var = "itemid")
      )
    )
  ),
  crp = list(
    unit = "mg/L",
    min = 0,
    description = "C-reactive protein",
    category = "chemistry",
    sources = list(
      mimic = list(
        list(ids = 50889L, table = "labevents", sub_var = "itemid",
             callback = "convert_unit(binary_op(`*`, 10), 'mg/L', 'mg/dl')")
      ),
      eicu = list(
        list(ids = "CRP", table = "lab", sub_var = "labname",
             callback = "convert_unit(binary_op(`*`, 10), 'mg/L', 'mg/dl')")
      ),
      hirid = list(
        list(ids = 20002200L, table = "observations", sub_var = "variableid",
             class = "hrd_itm")
      ),
      aumc = list(
        list(ids = c(6825L, 10079L), table = "numericitems",
             sub_var = "itemid")
      ),
      miiv = list(
        list(ids = 50889L, table = "labevents", sub_var = "itemid",
             callback = "convert_unit(binary_op(`*`, 10), 'mg/L', 'mg/dl')")
      )
    )
  ),
  esr = list(
    unit = c("mm/hr", "mm/1h"),
    min = 0,
    max = 200,
    description = "erythrocyte sedimentation rate",
    category = "hematology",
    sources = list(
      mimic = list(
        list(ids = 51288L, table = "labevents", sub_var = "itemid")
      ),
      hirid = list(
        list(ids = 24000668L, table = "observations", sub_var = "variableid",
             class = "hrd_itm")
      ),
      aumc = list(
        list(ids = c(6808L, 11902L), table = "numericitems",
             sub_var = "itemid")
      ),
      miiv = list(
        list(ids = 51288L, table = "labevents", sub_var = "itemid")
      )
    )
  ),
  hbco = list(
    description = "carboxyhemoglobin",
    category = "blood gas",
    sources = list(
      eicu = list(
        list(ids = "Carboxyhemoglobin", table = "lab", sub_var = "labname")
      ),
      hirid = list(
        list(ids = 24000526L, table = "observations", sub_var = "variableid",
             class = "hrd_itm")
      ),
      aumc = list(
        list(ids = 11690L, table = "numericitems", sub_var = "itemid")
      )
    )
  ),
  methb = list(
    unit = "%",
    min = 0,
    max = 100,
    description = "methemoglobin",
    category = "blood gas",
    sources = list(
      mimic = list(
        list(ids = 50814L, table = "labevents", sub_var = "itemid")
      ),
      eicu = list(
        list(ids = "Methemoglobin", table = "lab", sub_var = "labname")
      ),
      hirid = list(
        list(ids = 24000549L, table = "observations", sub_var = "variableid",
             class = "hrd_itm")
      ),
      aumc = list(
        list(ids = 11692L, table = "numericitems", sub_var = "itemid")
      ),
      miiv = list(
        list(ids = 50814L, table = "labevents", sub_var = "itemid")
      )
    )
  ),
  tnt = list(
    unit = "ng/mL",
    min = 0,
    description = "troponin t",
    category = "chemistry",
    sources = list(
      mimic = list(
        list(ids = 51003L, table = "labevents", sub_var = "itemid")
      ),
      eicu = list(
        list(ids = "troponin - T", table = "lab", sub_var = "labname")
      ),
      hirid = list(
        list(ids = 24000806L, table = "observations", sub_var = "variableid",
             callback = "convert_unit(binary_op(`/`, 1000), 'ng/mL')",
             class = "hrd_itm")
      ),
      aumc = list(
        list(ids = 10407L, table = "numericitems", sub_var = "itemid")
      ),
      miiv = list(
        list(ids = 51003L, table = "labevents", sub_var = "itemid")
      )
    )
  ),
  alb = list(
    unit = "g/dL",
    min = 0,
    max = 6,
    description = "albumin",
    category = "chemistry",
    sources = list(
      mimic = list(
        list(ids = 50862L, table = "labevents", sub_var = "itemid")
      ),
      eicu = list(
        list(ids = "albumin", table = "lab", sub_var = "labname")
      ),
      hirid = list(
        list(ids = 24000605L, table = "observations", sub_var = "variableid",
             callback = "convert_unit(binary_op(`*`, 0.1), 'g/dL')",
             class = "hrd_itm")
      ),
      aumc = list(
        list(ids = c(6801L, 9937L), table = "numericitems",
             sub_var = "itemid",
             callback = "convert_unit(binary_op(`*`, 0.1), 'g/dL')")
      ),
      miiv = list(
        list(ids = 50862L, table = "labevents", sub_var = "itemid")
      )
    )
  ),
  fgn = list(
    unit = "mg/dL",
    min = 0,
    max = 1500,
    description = "fibrinogen",
    category = "hematology",
    sources = list(
      mimic = list(
        list(ids = 51214L, table = "labevents", sub_var = "itemid")
      ),
      eicu = list(
        list(ids = "fibrinogen", table = "lab", sub_var = "labname")
      ),
      hirid = list(
        list(ids = 24000536L, table = "observations", sub_var = "variableid",
             callback = "convert_unit(binary_op(`*`, 100), 'mg/dL')",
             class = "hrd_itm")
      ),
      aumc = list(
        list(ids = c(6776L, 9989L, 10175L), table = "numericitems",
             sub_var = "itemid",
             callback = "convert_unit(binary_op(`*`, 0.058467), 'mg/dL')")
      ),
      miiv = list(
        list(ids = 51214L, table = "labevents", sub_var = "itemid")
      )
    )
  ),
  be = list(
    unit = c("mEq/L", "mmol/l"),
    min = -25,
    max = 25,
    description = "base excess",
    category = "blood gas",
    sources = list(
      mimic = list(
        list(ids = 50802L, table = "labevents", sub_var = "itemid")
      ),
      eicu = list(
        list(ids = "Base Excess", table = "lab", sub_var = "labname")
      ),
      hirid = list(
        list(ids = 20001300L, table = "observations", sub_var = "variableid",
             class = "hrd_itm")
      ),
      aumc = list(
        list(ids = c(6807L, 9994L), table = "numericitems",
             sub_var = "itemid", dir_var = "tag", callback = "aumc_bxs")
      ),
      miiv = list(
        list(ids = 50802L, table = "labevents", sub_var = "itemid")
      )
    )
  ),
  rbc = list(
    unit = "m/uL",
    min = 0,
    max = 20,
    description = "red blood cell count",
    category = "hematology",
    sources = list(
      mimic = list(
        list(ids = 51279L, table = "labevents", sub_var = "itemid")
      ),
      eicu = list(
        list(ids = "RBC", table = "lab", sub_var = "labname")
      ),
      aumc = list(
        list(ids = c(6774L, 9962L), table = "numericitems", sub_var = "itemid")
      ),
      miiv = list(
        list(ids = 51279L, table = "labevents", sub_var = "itemid")
      )
    )
  ),
  ck = list(
    unit = c("IU/L", "U/l"),
    min = 0,
    description = "creatine kinase",
    category = "chemistry",
    sources = list(
      mimic = list(
        list(ids = 50910L, table = "labevents", sub_var = "itemid")
      ),
      eicu = list(
        list(ids = "CPK", table = "lab", sub_var = "labname")
      ),
      hirid = list(
        list(ids = 24000210L, table = "observations", sub_var = "variableid",
             class = "hrd_itm")
      ),
      aumc = list(
        list(ids = c(6822L, 11998L), table = "numericitems",
             sub_var = "itemid")
      ),
      miiv = list(
        list(ids = 50910L, table = "labevents", sub_var = "itemid")
      )
    )
  ),
  ckmb = list(
    unit = "ng/mL",
    min = 0,
    description = "creatine kinase MB",
    category = "chemistry",
    sources = list(
      mimic = list(
        list(ids = 50911L, table = "labevents", sub_var = "itemid")
      ),
      eicu = list(
        list(ids = "CPK-MB", table = "lab", sub_var = "labname")
      ),
      hirid = list(
        list(ids = 24000220L, table = "observations", sub_var = "variableid",
             class = "hrd_itm")
      ),
      aumc = list(
        list(ids = 6824L, table = "numericitems", sub_var = "itemid")
      ),
      miiv = list(
        list(ids = 50911L, table = "labevents", sub_var = "itemid")
      )
    )
  ),
  egcs = list(
    min = 1,
    max = 4,
    description = "GCS eye",
    category = "neurological",
    sources = list(
      mimic = list(
        list(ids = c(184L, 220739L), table = "chartevents", sub_var = "itemid")
      ),
      eicu = list(
        list(ids = "Eyes", table = "nursecharting",
             sub_var = "nursingchartcelltypevalname")
      ),
      hirid = list(
        list(ids = 10000300L, table = "observations", sub_var = "variableid",
             class = "hrd_itm")
      ),
      aumc = list(
        list(ids = 6732L, table = "listitems", sub_var = "itemid",
             callback = strip_ws("apply_map(
              c(`Geen reactie`               = 1,
                `Reactie op pijnprikkel`     = 2,
                `Reactie op verbale prikkel` = 3,
                `Spontane reactie`           = 4)
              )")),
        list(ids = 13077L, table = "listitems", sub_var = "itemid",
             callback = strip_ws("apply_map(
              c(`Niet`                       = 1,
                `Op pijn`                    = 2,
                `Op aanspreken`              = 3,
                `Spontaan`                   = 4)
            )"))
      ),
      miiv = list(
        list(ids = 220739L, table = "chartevents", sub_var = "itemid")
      )
    )
  ),
  vgcs = list(
    min = 1,
    max = 5,
    description = "GCS verbal",
    category = "neurological",
    sources = list(
      mimic = list(
        list(ids = c(723L, 223900L), table = "chartevents", sub_var = "itemid")
      ),
      eicu = list(
        list(ids = "Verbal", table = "nursecharting",
             sub_var = "nursingchartcelltypevalname")
      ),
      hirid = list(
        list(ids = 10000100L, table = "observations", sub_var = "variableid",
             class = "hrd_itm")
      ),
      aumc = list(
        list(ids = 6735L, table = "listitems", sub_var = "itemid",
             callback = strip_ws("apply_map(
              c(`Geen reactie (geen zichtbare poging tot praten)` = 1,
                `Onbegrijpelijke geluiden`                        = 2,
                `Onduidelijke woorden (pogingen tot communicatie,
                 maar onduidelijk)`                               = 3,
                `Verwarde conversatie`                            = 4,
                `Helder en adequaat (communicatie mogelijk)`      = 5)
              )")),
        list(ids = 13066L, table = "listitems", sub_var = "itemid",
             callback = strip_ws("apply_map(
              c(`Geen geluid`            = 1,
                `Onverstaanbare woorden` = 2,
                `Onjuiste woorden`       = 3,
                `Verwarde taal`          = 4,
                `Georiënteerd`           = 5)
            )"))
      ),
      miiv = list(
        list(ids = 223900L, table = "chartevents", sub_var = "itemid")
      )
    )
  ),
  mgcs = list(
    min = 1,
    max = 6,
    description = "GCS motor",
    category = "neurological",
    sources = list(
      mimic = list(
        list(ids = c(454L, 223901L), table = "chartevents", sub_var = "itemid")
      ),
      eicu = list(
        list(ids = "Motor", table = "nursecharting",
             sub_var = "nursingchartcelltypevalname")
      ),
      hirid = list(
        list(ids = 10000200L, table = "observations", sub_var = "variableid",
             class = "hrd_itm")
      ),
      aumc = list(
        list(ids = 6734L, table = "listitems", sub_var = "itemid",
             callback = strip_ws("apply_map(
              c(`Geen reactie`                         = 1,
                `Strekken`                             = 2,
                `Decortatie reflex (abnormaal buigen)` = 3,
                `Spastische reactie (terugtrekken)`    = 4,
                `Localiseert pijn`                     = 5,
                `Volgt verbale commando's op`          = 6)
              )")),
        list(ids = 13072L, table = "listitems", sub_var = "itemid",
             callback = strip_ws("apply_map(
              c(`Geen reactie`              = 1,
                `Strekken op pijn`          = 2,
                `Abnormaal buigen bij pijn` = 3,
                `Terugtrekken bij pijn`     = 4,
                `Localiseren pijn`          = 5,
                `Voert opdrachten uit`      = 6)
            )"))
      ),
      miiv = list(
        list(ids = 223901L, table = "chartevents", sub_var = "itemid")
      )
    )
  ),
  tgcs = list(
    min = 3,
    max = 15,
    description = "GCS total",
    category = "neurological",
    sources = list(
      mimic = list(
        list(ids = 198L, table = "chartevents", sub_var = "itemid")
      ),
      eicu = list(
        list(ids = "GCS Total", table = "nursecharting",
             sub_var = "nursingchartcelltypevalname")
      )
    )
  ),
  urine = list(
    unit = "mL",
    min = 0,
    max = 2000,
    aggregate = "sum",
    description = "urine output",
    category = "output",
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
      ),
      hirid = list(
        list(ids = 30005110L, table = "observations", sub_var = "variableid",
             callback = "hirid_urine", class = "hrd_itm")
      ),
      aumc = list(
        list(ids = 8794L, table = "numericitems", sub_var = "itemid")
      ),
      miiv = list(
        list(ids = c(226557L, 226558L, 226559L, 226560L, 226561L,  226563L,
                     226564L, 226565L, 226566L, 226567L, 226584L, 227510L),
             table = "outputevents", sub_var = "itemid")
      )
    )
  ),
  dobu_rate = list(
    unit = c("mcg/kg/min", "mcgkgmin"),
    min = 0,
    max = 50,
    description = "dobutamine rate",
    category = "medications",
    sources = list(
      mimic = list(
        list(ids = c(30042L, 30306L), table = "inputevents_cv",
             sub_var = "itemid", grp_var = "linkorderid",
             callback = "mimic_rate_cv"),
        list(ids = 221653L, table = "inputevents_mv", sub_var = "itemid",
             stop_var = "endtime", callback = "mimic_rate_mv")
      ),
      eicu = list(
        list(regex = "^dobu.*\\(.+\\)$", table = "infusiondrug",
             sub_var = "drugname", weight_var = "patientweight",
             callback = "eicu_rate_kg(ml_to_mcg = 2000)", class = "rgx_itm")
      ),
      hirid = list(
        list(ids = 426L, table = "pharma", sub_var = "pharmaid",
             grp_var = "infusionid", callback = "hirid_rate_kg")
      ),
      aumc = list(
        list(ids = 7178L, table = "drugitems", sub_var = "itemid",
             rel_weight = "doserateperkg", rate_uom = "doserateunit",
             stop_var = "stop", callback = "aumc_rate_kg")
      ),
      miiv = list(
        list(ids = 221653L, table = "inputevents", sub_var = "itemid",
             stop_var = "endtime", callback = "mimic_rate_mv")
      )
    )
  ),
  dobu_dur = list(
    description = "dobutamine duration",
    category = "medications",
    aggregate = "max",
    sources = list(
      mimic = list(
        list(ids = c(30042L, 30306L), table = "inputevents_cv",
             sub_var = "itemid", grp_var = "linkorderid",
             callback = "mimic_dur_incv"),
        list(ids = 221653L, table = "inputevents_mv", sub_var = "itemid",
             stop_var = "endtime", grp_var = "linkorderid",
             callback = "mimic_dur_inmv")
      ),
      eicu = list(
        list(regex = "^dobu", table = "infusiondrug", sub_var = "drugname",
             callback = "eicu_duration(gap_length = hours(5L))",
             class = "rgx_itm")
      ),
      hirid = list(
        list(ids = 426L, table = "pharma", sub_var = "pharmaid",
             grp_var = "infusionid", callback = "hirid_duration")
      ),
      aumc = list(
        list(ids = 7178L, table = "drugitems", sub_var = "itemid",
             stop_var = "stop", grp_var = "orderid", callback = "aumc_dur")
      ),
      miiv = list(
        list(ids = 221653L, table = "inputevents", sub_var = "itemid",
             stop_var = "endtime", grp_var = "linkorderid",
             callback = "mimic_dur_inmv")
      )
    )
  ),
  dobu60 = list(
    concepts = c("dobu_rate", "dobu_dur"),
    description = "dobutamine administration for min 1h",
    category = "medications",
    interval = "00:01:00",
    callback = "vaso60",
    class = "rec_cncpt"
  ),
  dopa_rate = list(
    unit = c("mcg/kg/min", "mcgkgmin"),
    min = 0,
    max = 50,
    description = "dopamine rate",
    category = "medications",
    sources = list(
      mimic = list(
        list(ids = c(30043L, 30125L, 30307L), table = "inputevents_cv",
             sub_var = "itemid", grp_var = "linkorderid",
             callback = "mimic_rate_cv"),
        list(ids = 221662L, table = "inputevents_mv", sub_var = "itemid",
             stop_var = "endtime", callback = "mimic_rate_mv")
      ),
      eicu = list(
        list(regex = "^dopa.*\\(.+\\)$", table = "infusiondrug",
             sub_var = "drugname", weight_var = "patientweight",
             callback = "eicu_rate_kg(ml_to_mcg = 1600)", class = "rgx_itm")
      ),
      aumc = list(
        list(ids = 7179L, table = "drugitems", sub_var = "itemid",
             rel_weight = "doserateperkg", rate_uom = "doserateunit",
             stop_var = "stop", callback = "aumc_rate_kg")
      ),
      miiv = list(
        list(ids = 221662L, table = "inputevents", sub_var = "itemid",
             stop_var = "endtime", callback = "mimic_rate_mv")
      )
    )
  ),
  dopa_dur = list(
    description = "dopamine duration",
    category = "medications",
    aggregate = "max",
    sources = list(
      mimic = list(
        list(ids = c(30043L, 30125L, 30307L), table = "inputevents_cv",
             sub_var = "itemid", grp_var = "linkorderid",
             callback = "mimic_dur_incv"),
        list(ids = 221662L, table = "inputevents_mv", sub_var = "itemid",
             stop_var = "endtime", grp_var = "linkorderid",
             callback = "mimic_dur_inmv")
      ),
      eicu = list(
        list(regex = "^dopa", table = "infusiondrug", sub_var = "drugname",
             callback = "eicu_duration(gap_length = hours(5L))",
             class = "rgx_itm")
      ),
      aumc = list(
        list(ids = 7179L, table = "drugitems", sub_var = "itemid",
             stop_var = "stop", grp_var = "orderid", callback = "aumc_dur")
      ),
      miiv = list(
        list(ids = 221662L, table = "inputevents", sub_var = "itemid",
             stop_var = "endtime", grp_var = "linkorderid",
             callback = "mimic_dur_inmv")
      )
    )
  ),
  dopa60 = list(
    concepts = c("dopa_rate", "dopa_dur"),
    description = "dopamine administration for min 1h",
    category = "outcome",
    interval = "00:01:00",
    callback = "vaso60",
    class = "rec_cncpt"
  ),
  norepi_rate = list(
    unit = c("mcg/kg/min", "mcgkgmin"),
    min = 0,
    max = 3,
    description = "norepinephrine rate",
    category = "medications",
    sources = list(
      mimic = list(
        list(ids = c(30047L, 30120L), table = "inputevents_cv",
             sub_var = "itemid", grp_var = "linkorderid",
             callback = "mimic_rate_cv"),
        list(ids = 221906L, table = "inputevents_mv", sub_var = "itemid",
             stop_var = "endtime", callback = "mimic_rate_mv")
      ),
      eicu = list(
        list(regex = "^norepi.*\\(.+\\)$", table = "infusiondrug",
             sub_var = "drugname", weight_var = "patientweight",
             callback = "eicu_rate_kg(ml_to_mcg = 32)", class = "rgx_itm")
      ),
      hirid = list(
        list(ids = c(1000462L, 1000656L, 1000657L, 1000658L), table = "pharma",
             sub_var = "pharmaid", grp_var = "infusionid",
             callback = "hirid_rate_kg")
      ),
      aumc = list(
        list(ids = 7229L, table = "drugitems", sub_var = "itemid",
             rel_weight = "doserateperkg", rate_uom = "doserateunit",
             stop_var = "stop", callback = "aumc_rate_kg")
      ),
      miiv = list(
        list(ids = 221906L, table = "inputevents", sub_var = "itemid",
             stop_var = "endtime", callback = "mimic_rate_mv")
      )
    )
  ),
  norepi_dur = list(
    description = "norepinephrine duration",
    category = "medications",
    aggregate = "max",
    sources = list(
      mimic = list(
        list(ids = c(30047L, 30120L), table = "inputevents_cv",
             sub_var = "itemid", grp_var = "linkorderid",
             callback = "mimic_dur_incv"),
        list(ids = 221906L, table = "inputevents_mv", sub_var = "itemid",
             stop_var = "endtime", grp_var = "linkorderid",
             callback = "mimic_dur_inmv")
      ),
      eicu = list(
        list(regex = "^norepi", table = "infusiondrug", sub_var = "drugname",
             callback = "eicu_duration(gap_length = hours(5L))",
             class = "rgx_itm")
      ),
      hirid = list(
        list(ids = c(1000462L, 1000656L, 1000657L, 1000658L), table = "pharma",
             sub_var = "pharmaid", grp_var = "infusionid",
             callback = "hirid_duration")
      ),
      aumc = list(
        list(ids = 7229L, table = "drugitems", sub_var = "itemid",
             stop_var = "stop", grp_var = "orderid", callback = "aumc_dur")
      ),
      miiv = list(
        list(ids = 221906L, table = "inputevents", sub_var = "itemid",
             stop_var = "endtime", grp_var = "linkorderid",
             callback = "mimic_dur_inmv")
      )
    )
  ),
  norepi60 = list(
    concepts = c("norepi_rate", "norepi_dur"),
    description = "norepinephrine administration for min 1h",
    category = "outcome",
    interval = "00:01:00",
    callback = "vaso60",
    class = "rec_cncpt"
  ),
  epi_rate = list(
    unit = c("mcg/kg/min", "mcgkgmin"),
    min = 0,
    max = 1.5,
    description = "epinephrine rate",
    category = "medications",
    sources = list(
      mimic = list(
        list(ids = c(30044L, 30119L, 30309L), table = "inputevents_cv",
             sub_var = "itemid", grp_var = "linkorderid",
             callback = "mimic_rate_cv"),
        list(ids = 221289L, table = "inputevents_mv", sub_var = "itemid",
             stop_var = "endtime", callback = "mimic_rate_mv")
      ),
      eicu = list(
        list(regex = "^epi( |n).*\\(.+\\)$", table = "infusiondrug",
             sub_var = "drugname", weight_var = "patientweight",
             callback = "eicu_rate_kg(ml_to_mcg = 40)", class = "rgx_itm")
      ),
      hirid = list(
        list(ids = c(71L, 1000649L, 1000650L, 1000655L, 1000750L),
             table = "pharma", sub_var = "pharmaid",
             grp_var = "infusionid", callback = "hirid_rate_kg")
      ),
      aumc = list(
        list(ids = 6818L, table = "drugitems", sub_var = "itemid",
             rel_weight = "doserateperkg", rate_uom = "doserateunit",
             stop_var = "stop", callback = "aumc_rate_kg")
      ),
      miiv = list(
        list(ids = 221289L, table = "inputevents", sub_var = "itemid",
             stop_var = "endtime", callback = "mimic_rate_mv")
      )
    )
  ),
  epi_dur = list(
    description = "epinephrine duration",
    category = "medications",
    aggregate = "max",
    sources = list(
      mimic = list(
        list(ids = c(30044L, 30119L, 30309L), table = "inputevents_cv",
             sub_var = "itemid", grp_var = "linkorderid",
             callback = "mimic_dur_incv"),
        list(ids = 221289L, table = "inputevents_mv", sub_var = "itemid",
             stop_var = "endtime", grp_var = "linkorderid",
             callback = "mimic_dur_inmv")
      ),
      eicu = list(
        list(regex = "^epi( |n)", table = "infusiondrug", sub_var = "drugname",
             callback = "eicu_duration(gap_length = hours(5L))",
             class = "rgx_itm")
      ),
      hirid = list(
        list(ids = c(71L, 1000649L, 1000650L, 1000655L, 1000750L),
             table = "pharma", sub_var = "pharmaid", grp_var = "infusionid",
             callback = "hirid_duration")
      ),
      aumc = list(
        list(ids = 6818L, table = "drugitems", sub_var = "itemid",
             stop_var = "stop", grp_var = "orderid", callback = "aumc_dur")
      ),
      miiv = list(
        list(ids = 221289L, table = "inputevents", sub_var = "itemid",
             stop_var = "endtime", grp_var = "linkorderid",
             callback = "mimic_dur_inmv")
      )
    )
  ),
  epi60 = list(
    concepts = c("epi_rate", "epi_dur"),
    description = "epinephrine administration for min 1h",
    category = "outcome",
    interval = "00:01:00",
    callback = "vaso60",
    class = "rec_cncpt"
  ),
  vent_start = list(
    class = "lgl_cncpt",
    description = "ventilation start",
    category = "respiratory",
    sources = list(
      mimic = list(
        list(
          ids = c(
                1L,     60L,    218L,    221L,    223L,    436L,    437L,
              444L,    445L,    448L,    449L,    450L,    459L,    501L,
              502L,    503L,    505L,    506L,    535L,    543L,    639L,
              654L,    667L,    668L,    669L,    670L,    671L,    672L,
              681L,    682L,    683L,    684L,    686L,   1211L,   1340L,
             1486L,   1600L,   1655L,   2000L,   3459L,   5865L,   5866L,
            220339L, 223848L, 223849L, 224419L, 224684L, 224685L, 224686L,
            224687L, 224695L, 224696L, 224697L, 224700L, 224701L, 224702L,
            224703L, 224704L, 224705L, 224706L, 224707L, 224709L, 224738L,
            224746L, 224747L, 224750L, 226873L, 227187L),
          table = "chartevents", sub_var = "itemid",
          callback = "transform_fun(set_val(TRUE))"
        )
      ),
      eicu = list(
        list(table = "respiratorycare", val_var = "ventstartoffset",
             callback = "vent_flag", class = "col_itm"),
        list(table = "respiratorycare", val_var = "priorventstartoffset",
             callback = "vent_flag", class = "col_itm"),
        list(ids = c("Start", "Continued", "respFlowPtVentData"),
             table = "respiratorycharting", sub_var = "respcharttypecat",
             callback = "transform_fun(set_val(TRUE))")
      )
    )
  ),
  vent_end = list(
    class = "lgl_cncpt",
    description = "ventilation end",
    category = "respiratory",
    sources = list(
      mimic = list(
        list(ids = c(225468L, 225477L, 227194L), table = "procedureevents_mv",
             sub_var = "itemid", callback = "transform_fun(set_val(TRUE))"),
        list(ids = c(467L, 469L, 226732L), table = "chartevents",
             sub_var = "itemid", callback = "transform_fun(set_val(TRUE))")
      ),
      eicu = list(
        list(table = "respiratorycare", val_var = "ventendoffset",
             callback = "vent_flag", class = "col_itm"),
        list(table = "respiratorycare", val_var = "priorventendoffset",
             callback = "vent_flag", class = "col_itm"),
        list(ids = c("off", "Off", "Suspended"), table = "respiratorycharting",
             sub_var = "respchartvalue", val_var = "respchartvaluelabel",
             callback = "transform_fun(set_val(TRUE))")
      )
    )
  ),
  mech_vent = list(
    class = "fct_cncpt",
    target = "win_tbl",
    levels = c("invasive", "noninvasive"),
    description = "mechanical ventilation windows",
    category = "respiratory",
    sources = list(
      aumc = list(
        list(ids = c(9328L, 10740L, 12635L), table = "processitems",
             sub_var = "itemid", dur_var = "stop",
             callback = "apply_map(c(`Beademen`              = 'invasive',
                                     `Beademen non-invasief` = 'noninvasive',
                                     `Tracheostoma`          = 'invasive'))"
        )
      ),
      miiv = list(
        list(ids = c(225792L, 225794L), table = "procedureevents",
             sub_var = "itemid", dur_var = "endtime",
             callback = "apply_map(c(`225792` = 'invasive',
                                     `225794` = 'noninvasive'),
                                   var = 'sub_var')")
      ),
      hirid = list(
        list(ids = 15001552L, table = "observations", sub_var = "variableid",
             target = "ts_tbl", interval = "00:01:00", class = "hrd_itm",
             callback = "combine_callbacks(hirid_vent,
                                           apply_map(c(`1` = 'invasive',
                                                       `2` = 'invasive',
                                                       `3` = 'noninvasive',
                                                       `4` = 'noninvasive',
                                                       `5` = 'noninvasive',
                                                       `6` = 'noninvasive')))")
      )
    )
  ),
  ett_gcs = list(
    class = "lgl_cncpt",
    description = "tracheostomy",
    category = "respiratory",
    sources = list(
      mimic = list(
        list(ids = c("1.0 ET/Trach", "No Response-ETT"), table = "chartevents",
             sub_var = "value", callback = "transform_fun(set_val(TRUE))")
      ),
      hirid = list(
        list(ids = 15001552L, table = "observations", sub_var = "variableid",
             callback = "transform_fun(comp_na(`==`, 2))", class = "hrd_itm")
      ),
      aumc = list(
        list(ids = 6735L, table = "listitems", sub_var = "itemid",
             callback = "transform_fun(comp_na(`==`, 'Geïntubeerd'))")
      ),
      miiv = list(
        list(ids = "No Response-ETT", table = "chartevents", sub_var = "value",
             callback = "transform_fun(set_val(TRUE))")
      )
    )
  ),
  rass = list(
    min = -5,
    max = 4,
    description = "Richmond agitation sedation scale",
    category = "neurological",
    sources = list(
      mimic = list(
        list(ids = 228096L, table = "chartevents", sub_var = "itemid")
      ),
      eicu = list(
        list(ids = "Sedation Score", table = "nursecharting",
             sub_var = "nursingchartcelltypevalname")
      ),
      hirid = list(
        list(ids = 15001565L, table = "observations", sub_var = "variableid",
             callback = "transform_fun(floor)", class = "hrd_itm")
      ),
      aumc = list(
        list(ids = 14444L, sub_var = "itemid", table = "listitems",
             callback = "transform_fun(aumc_rass)")
      ),
      miiv = list(
        list(ids = 228096L, table = "chartevents", sub_var = "itemid")
      )
    )
  ),
  abx = list(
    class = "lgl_cncpt",
    description = "antibiotics",
    category = "medications",
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
          table = "prescriptions", sub_var = "drug", interval = "00:01:00",
          callback = "mimic_abx_presc", class = "rgx_itm"
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
          callback = "transform_fun(set_val(TRUE))"
        )
      ),
      eicu = list(
        list(regex = paste(
          "bactrim", "cipro", "flagyl", "metronidazole", "zithromax", "zosyn",
          "(((amika", "cleo", "ofloxa)", "(azithro", "clinda", "tobra",
          "vanco)my)c", "(ampi", "oxa", "peni", "pipera)cill", "cefazol",
          "levaqu", "rifamp)in", sep = "|"), table = "infusiondrug",
          sub_var = "drugname", callback = "transform_fun(set_val(TRUE))",
          class = "rgx_itm"
        ),
        list(regex = paste(
          "cipro", "flagyl", "maxipime", "metronidazole", "tazobactam",
          "zosyn", "cef(azolin", "epime)", "(((azithro", "clinda", "vanco)my",
          "ofloxa", "vanco)c", "levaqu", "piperacill", "roceph)in", sep = "|"),
          table = "medication", sub_var = "drugname",
          callback = "transform_fun(set_val(TRUE))", class = "rgx_itm"
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
          sub_var = "pharmaid", callback = "transform_fun(set_val(TRUE))")
      ),
      aumc = list(
        list(ids = c(
              2L,    13L,    19L,    24L,    28L,    29L,    57L,    59L,
             82L,   103L,   240L,   247L,   333L,  1133L,  1199L,  1300L,
           1371L,  1795L,  2284L,  2834L,  3237L,  3741L,  5576L,  6834L,
           6847L,  6871L,  6919L,  6948L,  6953L,  6958L,  7044L,  7064L,
           7185L,  7187L,  7208L,  7227L,  7235L,  8064L,  8394L,  8942L,
           9029L,  9030L,  9052L,  9070L,  9117L,  9128L,  9133L,  9142L,
           9151L,  9152L, 12262L, 12389L, 12398L, 12956L, 12997L, 13057L,
          13094L, 13102L, 15591L, 18860L, 19137L, 19773L, 20563L, 23166L,
          24241L, 25776L, 27617L, 29321L), table = "drugitems",
        sub_var = "itemid", callback = "transform_fun(set_val(TRUE))")
      ),
      miiv = list(
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
          callback = "transform_fun(set_val(TRUE))", class = "rgx_itm"
        ),
        list(ids = c(
          225798L, 225837L, 225838L, 225840L, 225842L, 225843L, 225844L,
          225845L, 225847L, 225848L, 225850L, 225851L, 225853L, 225855L,
          225857L, 225859L, 225860L, 225862L, 225863L, 225865L, 225866L,
          225868L, 225869L, 225871L, 225873L, 225875L, 225876L, 225877L,
          225879L, 225881L, 225882L, 225883L, 225884L, 225885L, 225886L,
          225888L, 225889L, 225890L, 225892L, 225893L, 225895L, 225896L,
          225897L, 225898L, 225899L, 225900L, 225902L, 225903L, 225905L,
          227691L, 228003L), table = "inputevents", sub_var = "itemid",
          callback = "transform_fun(set_val(TRUE))"
        )
      )
    )
  ),
  samp = list(
    class = "lgl_cncpt",
    category = "microbiology",
    description = "fluid sampling",
    sources = list(
      mimic = list(
        list(table = "microbiologyevents", val_var = "org_itemid",
             callback = "mimic_sampling", aux_time = "charttime",
             class = "col_itm")
      ),
      eicu = list(
        list(table = "microlab", val_var = "organism",
             callback = "transform_fun(comp_na(`!=`, 'no growth'))",
             class = "col_itm")
      ),
      aumc = list(
        list(ids = c(
           8097L,  8418L, 8588L, 9189L, 9190L, 9191L, 9192L, 9193L,  9194L,
           9195L,  9197L, 9198L, 9200L, 9202L, 9203L, 9204L, 9205L, 13024L,
          19663L, 19664L), table = "procedureorderitems", sub_var = "itemid",
        callback = "transform_fun(set_val(TRUE))")
      ),
      miiv = list(
        list(table = "microbiologyevents", val_var = "org_itemid",
             callback = "mimic_sampling", aux_time = "charttime",
             class = "col_itm")
      )
    )
  ),
  tri = list(
    unit = "ng/mL",
    min = 0,
    description = "troponin I",
    category = "chemistry",
    sources = list(
      mimic = list(
        list(ids = 51002L, table = "labevents", sub_var = "itemid")
      ),
      eicu = list(
        list(ids = "troponin - I", table = "lab", sub_var = "labname")
      ),
      miiv = list(
        list(ids = 51002L, table = "labevents", sub_var = "itemid")
      )
    )
  ),
  bili_dir = list(
    unit = "mg/dL",
    min = 0,
    max = 50,
    description = "bilirubin direct",
    category = "chemistry",
    sources = list(
      mimic = list(
        list(ids = 50883L, table = "labevents", sub_var = "itemid")
      ),
      eicu = list(
        list(ids = "direct bilirubin", table = "lab", sub_var = "labname")
      ),
      hirid = list(
        list(ids = 24000560L, table = "observations", sub_var = "variableid",
             callback = "convert_unit(binary_op(`*`, 0.058467), 'mg/dL')",
             class = "hrd_itm")
      ),
      aumc = list(
        list(ids = 6812L, table = "numericitems", sub_var = "itemid",
             callback = "convert_unit(binary_op(`*`, 0.058467), 'mg/dL')")
      ),
      miiv = list(
        list(ids = 50883L, table = "labevents", sub_var = "itemid")
      )
    )
  ),
  temp = list(
    unit = c("C", "°C"),
    min = 32,
    max = 42,
    description = "temperature",
    category = "vitals",
    sources = list(
      mimic = list(
        list(ids = c(676L, 677L, 223762L), table = "chartevents",
             sub_var = "itemid"),
        list(ids = c(678L, 679L, 223761L, 224027L), table = "chartevents",
             sub_var = "itemid",
             callback = "convert_unit(fahr_to_cels, 'C', 'f')")
      ),
      eicu = list(
        list(table = "vitalperiodic", val_var = "temperature",
             class = "col_itm")
      ),
      hirid = list(
        list(ids = c(410L, 400L, 7100L), table = "observations",
             sub_var = "variableid", class = "hrd_itm")
      ),
      aumc = list(
        list(ids = c(8658L, 13952L, 16110L), table = "numericitems",
             sub_var = "itemid")
      ),
      miiv = list(
        list(ids = 223762L, table = "chartevents", sub_var = "itemid"),
        list(ids = c(223761L, 224027L), table = "chartevents",
             sub_var = "itemid",
             callback = "convert_unit(fahr_to_cels, 'C', 'f')")
      )
    )
  ),
  etco2 = list(
    unit = c("mmHg", "mm Hg"),
    min = 10,
    max = 60,
    description = "endtidal CO2",
    category = "vitals",
    sources = list(
      mimic = list(
        list(ids = c(1817L, 228640L), table = "chartevents",
             sub_var = "itemid")
      ),
      hirid = list(
        list(ids = c(2200L, 8290L, 30010009L), table = "observations",
             sub_var = "variableid", class = "hrd_itm")
      ),
      aumc = list(
        list(
          ids = c(6707L, 8884L, 8885L, 9658L, 12805L, 12356L),
          table = "numericitems", sub_var = "itemid",
          callback = "convert_unit(binary_op(`*`, 7.6), 'mmHg', 'None|Geen')"
        )
      ),
      miiv = list(
        list(ids = 228640L, table = "chartevents", sub_var = "itemid")
      )
    )
  ),
  ins = list(
    unit = "units/hr",
    min = 0,
    description = "insulin",
    category = "medications",
    sources = list(
      mimic = list(
        list(ids = c(30045L, 30100L), table = "inputevents_cv",
             sub_var = "itemid"),
        list(ids = c(223258L, 223260L), table = "inputevents_mv",
             sub_var = "itemid", val_var = "amount", end_var = "endtime",
             callback = "distribute_amount")
      ),
      eicu = list(
        list(regex = "^insulin (250.+)?\\(((ml|units)/hr)?\\)$",
             table = "infusiondrug", sub_var = "drugname", class = "rgx_itm")
      ),
      hirid = list(
        list(ids = c(15L, 1000724L), table = "pharma", sub_var = "pharmaid",
             callback = "aggregate_fun('sum', 'units')")
      ),
      aumc = list(
        list(ids = c(7624L, 9014L, 19129L), sub_var = "itemid",
             table = "drugitems")
      ),
      miiv = list(
        list(ids = c(223258L, 223260L), table = "inputevents",
             sub_var = "itemid", val_var = "amount", end_var = "endtime",
             callback = "distribute_amount")
      )
    )
  ),
  sex = list(
    target = "id_tbl",
    levels = c("Female", "Male"),
    class = "fct_cncpt",
    description = "patient sex",
    category = "demographics",
    sources = list(
      mimic = list(
        list(table = "patients", val_var = "gender",
             callback = "apply_map(c(M = 'Male', F = 'Female'))",
             class = "col_itm")
      ),
      eicu = list(
        list(table = "patient", val_var = "gender", class = "col_itm")
      ),
      hirid = list(
        list(table = "general", val_var = "sex",
             callback = "apply_map(c(M = 'Male', F = 'Female'))",
             class = "col_itm")
      ),
      aumc = list(
        list(val_var = "gender", table = "admissions",
             callback = strip_ws("apply_map(c(Vrouw = 'Female',
                                              Man   = 'Male'))"),
             class = "col_itm")
      ),
      miiv = list(
        list(table = "patients", val_var = "gender",
             callback = "apply_map(c(M = 'Male', F = 'Female'))",
             class = "col_itm")
      )
    )
  ),
  age = list(
    unit = "years",
    min = 0,
    max = 100,
    target = "id_tbl",
    description = "patient age",
    category = "demographics",
    sources = list(
      mimic = list(
        list(table = "patients", val_var = "dob",
             callback = "transform_fun(mimic_age)", class = "col_itm")
      ),
      eicu = list(
        list(table = "patient", val_var = "age",
             callback = "transform_fun(eicu_age)", class = "col_itm")
      ),
      hirid = list(
        list(table = "general", val_var = "age", class = "col_itm")
      ),
      aumc = list(
        list(val_var = "agegroup", table = "admissions",
             callback = strip_ws("apply_map(c(`18-39` = 30,
                                              `40-49` = 45,
                                              `50-59` = 55,
                                              `60-69` = 65,
                                              `70-79` = 75,
                                              `80+`   = 90))"),
             class = "col_itm")
      ),
      miiv = list(
        list(table = "patients", val_var = "anchor_age", class = "col_itm")
      )
    )
  ),
  weight = list(
    unit = "kg",
    min = 1,
    max = 500,
    target = "id_tbl",
    description = "patient weight",
    category = "demographics",
    sources = list(
      mimic = list(
        list(ids = c(762L, 4183L, 226512L), table = "chartevents",
             sub_var = "itemid")
      ),
      eicu = list(
        list(table = "patient", val_var = "admissionweight",
             class = "col_itm")
      ),
      hirid = list(
        list(ids = 10000400L, table = "observations",
             sub_var = "variableid", class = "hrd_itm")
      ),
      aumc = list(
        list(val_var = "weightgroup", table = "admissions",
             callback = strip_ws("apply_map(c(`59-`     = 50,
                                              `60-69`   = 65,
                                              `70-79`   = 75,
                                              `80-89`   = 85,
                                              `90-99`   = 95,
                                              `100-109` = 105,
                                              `110+`    = 120))"),
             class = "col_itm")
      ),
      miiv = list(
        list(ids = 226512L, table = "chartevents", sub_var = "itemid")
      )
    )
  ),
  height = list(
    unit = "cm",
    min = 10,
    max = 230,
    target = "id_tbl",
    description = "patient height",
    category = "demographics",
    sources = list(
      mimic = list(
        list(ids = c(920L, 1394L, 3485L, 4187L, 4188L, 3486L, 226707L),
             table = "chartevents", sub_var = "itemid",
             callback = "convert_unit(binary_op(`*`, 2.54), 'cm', '^in')")
      ),
      eicu = list(
        list(table = "patient", val_var = "admissionheight",
             class = "col_itm")
      ),
      hirid = list(
        list(ids = 10000450L, table = "observations",
             sub_var = "variableid", class = "hrd_itm")
      ),
      aumc = list(
        list(val_var = "heightgroup", table = "admissions",
             callback = strip_ws("apply_map(c(`159-`    = 150,
                                              `160-169` = 165,
                                              `170-179` = 175,
                                              `180-189` = 185,
                                              `190+`    = 200))"),
             class = "col_itm")
      ),
      miiv = list(
        list(ids = 226707L, table = "chartevents", sub_var = "itemid",
             callback = "convert_unit(binary_op(`*`, 2.54), 'cm', '^in')")
      )
    )
  ),
  death = list(
    class = "lgl_cncpt",
    description = "in hospital mortality",
    category = "outcome",
    sources = list(
      mimic = list(
        list(table = "admissions", index_var = "deathtime",
             val_var = "hospital_expire_flag",
             callback = "transform_fun(comp_na(`==`, 1L))", class = "col_itm")
      ),
      eicu = list(
        list(table = "patient", index_var = "unitdischargeoffset",
             val_var = "hospitaldischargestatus",
             callback = "transform_fun(comp_na(`==`, 'Expired'))",
             class = "col_itm")
      ),
      hirid = list(
        list(ids = c(110L, 200L), table = "observations",
             sub_var = "variableid", callback = "hirid_death",
             class = "hrd_itm")
      ),
      aumc = list(
        list(table = "admissions", index_var = "dateofdeath",
             val_var = "dischargedat", callback = "aumc_death",
             class = "col_itm")
      ),
      miiv = list(
        list(table = "admissions", index_var = "deathtime",
             val_var = "hospital_expire_flag",
             callback = "transform_fun(comp_na(`==`, 1L))", class = "col_itm")
      )
    )
  ),
  adm = list(
    target = "id_tbl",
    levels = c("med", "surg", "other"),
    class = "fct_cncpt",
    description = "patient admission type",
    category = "demographics",
    sources = list(
      mimic = list(
        list(table = "services", val_var = "curr_service",
          callback = "apply_map(
            c(MED   = 'med',   SURG  = 'surg', CMED = 'med',  CSURG  = 'surg',
              VSURG = 'surg',  NSURG = 'surg', NB   = 'other', NMED  = 'med',
              ORTHO = 'surg',  TRAUM = 'surg', OMED = 'med',   GU    = 'other',
              NBB   = 'other', TSURG = 'surg', GYN  = 'other', PSURG = 'surg',
              OBS   = 'other', ENT   = 'surg', DENT = 'surg',  PSYCH = 'other')
          )",
          class = "col_itm"
        )
      ),
      eicu = list(
        list(table = "admissiondx", val_var = "admitdxpath",
             callback = "eicu_adx", class = "col_itm")
      ),
      aumc = list(
        list(val_var = "specialty", table = "admissions",
             callback = strip_ws("apply_map(
              c(Cardiochirurgie                = 'surg',
                Cardiologie                    = 'med',
                ders                           = 'other',
                Gynaecologie                   = 'other',
                `Heelkunde Gastro-enterologie` = 'surg',
                `Heelkunde Longen/Oncologie`   = 'surg',
                `Heelkunde Oncologie`          = 'surg',
                Hematologie                    = 'med',
                `Intensive Care Volwassenen`   = 'other',
                Inwendig                       = 'med',
                `Keel, Neus & Oorarts`         = 'surg',
                Longziekte                     = 'med',
                `Maag-,Darm-,Leverziekten`     = 'med',
                Mondheelkunde                  = 'surg',
                Nefrologie                     = 'med',
                Neurochirurgie                 = 'surg',
                Neurologie                     = 'med',
                Obstetrie                      = 'other',
                `Oncologie Inwendig`           = 'med',
                Oogheelkunde                   = 'surg',
                Orthopedie                     = 'surg',
                `Plastische chirurgie`         = 'surg',
                Reumatologie                   = 'med',
                Traumatologie                  = 'surg',
                Urologie                       = 'surg',
                Vaatchirurgie                  = 'surg',
                Verloskunde                    = 'other'))"),
              class = "col_itm")
      ),
      miiv = list(
        list(table = "services", val_var = "curr_service",
          callback = "apply_map(
            c(MED   = 'med',   SURG  = 'surg', CMED = 'med',  CSURG  = 'surg',
              VSURG = 'surg',  NSURG = 'surg', NB   = 'other', NMED  = 'med',
              ORTHO = 'surg',  TRAUM = 'surg', OMED = 'med',   GU    = 'other',
              NBB   = 'other', TSURG = 'surg', GYN  = 'other', PSURG = 'surg',
              OBS   = 'other', ENT   = 'surg', DENT = 'surg',  PSYCH = 'other')
          )",
          class = "col_itm"
        )
      )
    )
  ),
  los_icu = list(
    unit = "days",
    min = 0,
    target = "id_tbl",
    description = "ICU length of stay",
    category = "outcome",
    sources = list(
      mimic = list(
        list(callback = "los_callback", win_type = "icustay",
             class = "fun_itm")
      ),
      eicu = list(
        list(callback = "los_callback", win_type = "icustay",
             class = "fun_itm")
      ),
      hirid = list(
        list(callback = "los_callback", win_type = "icustay",
             class = "fun_itm")
      ),
      aumc = list(
        list(callback = "los_callback", win_type = "icustay",
             class = "fun_itm")
      ),
      miiv = list(
        list(callback = "los_callback", win_type = "icustay",
             class = "fun_itm")
      )
    )
  ),
  los_hosp = list(
    unit = "days",
    min = 0,
    target = "id_tbl",
    description = "hospital length of stay",
    category = "outcome",
    sources = list(
      mimic = list(
        list(callback = "los_callback", win_type = "hadm", class = "fun_itm")
      ),
      eicu = list(
        list(callback = "los_callback", win_type = "hadm", class = "fun_itm")
      ),
      miiv = list(
        list(callback = "los_callback", win_type = "hadm", class = "fun_itm")
      )
    )
  ),
  pafi = list(
    concepts = c("po2", "fio2"),
    description = "Horowitz index",
    category = "respiratory",
    aggregate = c("min", "max"),
    callback = "pafi",
    class = "rec_cncpt"
  ),
  safi = list(
    concepts = c("o2sat", "fio2"),
    description = "SaO2/FiO2",
    category = "respiratory",
    aggregate = c("min", "max"),
    callback = "safi",
    class = "rec_cncpt"
  ),
  vent_ind = list(
    concepts = c("vent_start", "vent_end", "mech_vent"),
    description = "ventilation durations",
    category = "respiratory",
    interval = "00:01:00",
    callback = "vent_ind",
    target = "win_tbl",
    class = "rec_cncpt"
  ),
  vaso_ind = list(
    concepts = c("dopa_dur", "norepi_dur", "dobu_dur", "epi_dur"),
    description = "vasopressor indicator",
    category = "medications",
    callback = "vaso_ind",
    class = "rec_cncpt"
  ),
  sed_gcs = list(
    concepts = c("ett_gcs", "rass"),
    description = "sedation status",
    category = "neurological",
    aggregate = c(NA, "min"),
    callback = "sed_gcs",
    class = "rec_cncpt"
  ),
  gcs = list(
    concepts = c("egcs", "mgcs", "vgcs", "tgcs", "sed_gcs"),
    description = "Glasgow coma scale (non-sedated)",
    category = "neurological",
    aggregate = c("min", "min", "min", "min", NA),
    callback = "gcs",
    class = "rec_cncpt"
  ),
  urine24 = list(
    concepts = "urine",
    description = "urine output per 24h",
    category = "output",
    callback = "urine24",
    class = "rec_cncpt"
  ),
  sofa_resp = list(
    concepts = c("pafi", "vent_ind"),
    description = "SOFA respiratory component",
    category = "outcome",
    callback = "sofa_resp",
    class = "rec_cncpt"
  ),
  sofa_coag = list(
    concepts = "plt",
    description = "SOFA coagulation component",
    category = "outcome",
    aggregate = "min",
    callback = "sofa_coag",
    class = "rec_cncpt"
  ),
  sofa_liver = list(
    concepts = "bili",
    description = "SOFA liver component",
    category = "outcome",
    aggregate = "max",
    callback = "sofa_liver",
    class = "rec_cncpt"
  ),
  sofa_cardio = list(
    concepts = c("map", "dopa60", "norepi60", "dobu60", "epi60"),
    description = "SOFA cardiovascular component",
    category = "outcome",
    aggregate = c("min", "max", "max", "max", "max"),
    callback = "sofa_cardio",
    class = "rec_cncpt"
  ),
  sofa_cns = list(
    concepts = "gcs",
    description = "SOFA central nervous system component",
    category = "outcome",
    callback = "sofa_cns",
    class = "rec_cncpt"
  ),
  sofa_renal = list(
    concepts = c("crea", "urine24"),
    description = "SOFA renal component",
    category = "outcome",
    aggregate = c("max", NA),
    callback = "sofa_renal",
    class = "rec_cncpt"
  ),
  sofa = list(
    concepts = c("sofa_resp", "sofa_coag", "sofa_liver", "sofa_cardio",
                 "sofa_cns", "sofa_renal"),
    description = "sequential organ failure assessment score",
    category = "outcome",
    callback = "sofa_score",
    class = "rec_cncpt"
  ),
  qsofa = list(
    concepts = c("gcs", "sbp", "resp"),
    description = "quick SOFA score",
    category = "outcome",
    callback = "qsofa_score",
    class = "rec_cncpt"
  ),
  susp_inf = list(
    concepts = c("abx", "samp"),
    description = "suspected infection",
    category = "outcome",
    aggregate = lapply(list("sum", FALSE), list),
    callback = "susp_inf",
    class = "rec_cncpt"
  ),
  sep3 = list(
    concepts = c("sofa", "susp_inf"),
    description = "sepsis-3 criterion",
    category = "outcome",
    callback = "sep3",
    class = "rec_cncpt"
  ),
  bnd = list(
    unit = "%",
    description = "band form neutrophils",
    category = "hematology",
    sources = list(
      mimic = list(
        list(ids = 51144L, table = "labevents", sub_var = "itemid")
      ),
      eicu = list(
        list(ids = "-bands", table = "lab", sub_var = "labname")
      ),
      hirid = list(
        list(ids = 24000557L, table = "observations", sub_var = "variableid",
             class = "hrd_itm")
      ),
      aumc = list(
        list(ids = c(6796L, 11586L), table = "numericitems",
             sub_var = "itemid")
      ),
      miiv = list(
        list(ids = 51144L, table = "labevents", sub_var = "itemid")
      )
    )
  ),
  sirs = list(
    concepts = c("temp", "hr", "resp", "pco2", "wbc", "bnd"),
    description = "systemic inflammatory response syndrome score",
    category = "outcome",
    callback = "sirs_score",
    class = "rec_cncpt"
  ),
  supp_o2 = list(
    concepts = c("vent_ind", "fio2"),
    description = "supplemental oxygen",
    category = "respiratory",
    callback = "supp_o2",
    class = "rec_cncpt"
  ),
  avpu = list(
    concepts = "gcs",
    description = "AVPU scale",
    category = "neurological",
    callback = "avpu",
    class = "rec_cncpt"
  ),
  news = list(
    concepts = c("resp", "o2sat", "supp_o2", "temp", "sbp", "hr", "avpu"),
    description = "national early warning score",
    category = "outcome",
    callback = "news_score",
    class = "rec_cncpt"
  ),
  mews = list(
    concepts = c("sbp", "hr", "resp", "temp","avpu"),
    description = "modified early warning score",
    category = "outcome",
    callback = "mews_score",
    class = "rec_cncpt"
  ),
  hba1c = list(
    unit = "%",
    description = "Hemoglobin A1C",
    category = "hematology",
    sources = list(
      mimic = list(
        list(ids = 50852L, table = "labevents", sub_var = "itemid")
      ),
      aumc = list(
        list(ids = c(11812L, 16166L), table = "numericitems",
             sub_var = "itemid")
      ),
      miiv = list(
        list(ids = 50852L, table = "labevents", sub_var = "itemid")
      )
    )
  ),
  bmi = list(
    concepts = c("weight", "height"),
    description = "patient body mass index",
    category = "demographics",
    callback = "bmi",
    target = "id_tbl",
    class = "rec_cncpt"
  ),
  adh_rate = list(
    unit = c("units/min", "U/min"),
    description = "vasopressin rate",
    category = "medications",
    sources = list(
      mimic = list(
        list(ids = 30051L, table = "inputevents_cv", sub_var = "itemid",
             grp_var = "linkorderid", callback = "combine_callbacks(
               convert_unit(binary_op(`/`, 60), 'units/min', 'Uhr'),
               mimic_rate_cv
             )"),
        list(ids = 222315L, table = "inputevents_mv", sub_var = "itemid",
             stop_var = "endtime", callback = "combine_callbacks(
               convert_unit(binary_op(`/`, 60), 'units/min', 'units/hour'),
               mimic_rate_mv
             )")
      ),
      eicu = list(
        list(regex = "^vasopressin.*\\(.+/.+\\)$",
             table = "infusiondrug", sub_var = "drugname",
             callback = "eicu_rate_units(2.65, 0.53)",
             class = "rgx_itm")
      ),
      hirid = list(
        list(ids = c(112L, 113L), table = "pharma", sub_var = "pharmaid",
             grp_var = "infusionid", callback = "hirid_rate")
      ),
      aumc = list(
        list(ids = 12467L, table = "drugitems", sub_var = "itemid",
             rate_uom = "doserateunit", stop_var = "stop",
             callback = "aumc_rate_units(0.53)")
      ),
      miiv = list(
        list(ids = 222315L, table = "inputevents", sub_var = "itemid",
             stop_var = "endtime", callback = "combine_callbacks(
               convert_unit(binary_op(`/`, 60), 'units/min', 'units/hour'),
               mimic_rate_mv
             )")
      )
    )
  ),
  phn_rate = list(
    unit = "mcg/kg/min",
    description = "phenylephrine rate",
    category = "medications",
    sources = list(
      mimic = list(
        list(ids = 30127L, table = "inputevents_cv", sub_var = "itemid",
             grp_var = "linkorderid", callback = "combine_callbacks(
               mimic_kg_rate, mimic_rate_cv)
             "),
        list(ids = 30128L, table = "inputevents_cv", sub_var = "itemid",
             grp_var = "linkorderid", callback = "mimic_rate_cv"),
        list(ids = 221749L, table = "inputevents_mv", sub_var = "itemid",
             stop_var = "endtime", callback = "mimic_rate_mv")
      ),
      eicu = list(
        list(regex = "^phenylephrine.*\\(.+\\)$", table = "infusiondrug",
             sub_var = "drugname", weight_var = "patientweight",
             callback = "eicu_rate_kg(ml_to_mcg = 200)", class = "rgx_itm")
      ),
      miiv = list(
        list(ids = 221749L, table = "inputevents", sub_var = "itemid",
             stop_var = "endtime", callback = "mimic_rate_mv")
      )
    )
  ),
  norepi_equiv = list(
    description = "norepinephrine equivalents",
    category = "medications",
    concepts = c("epi_rate", "norepi_rate", "dopa_rate", "adh_rate",
                 "phn_rate"),
    callback = "norepi_equiv",
    class = "rec_cncpt"
  ),
  cort = list(
    class = "lgl_cncpt",
    description = "corticosteroids",
    category = "medications",
    sources = list(
      hirid = list(
        list(ids = c(    146L,     151L, 1000325L, 1000383L, 1000431L,
                     1000432L, 1000433L, 1000434L, 1000435L, 1000486L,
                     1000487L, 1000488L, 1000769L, 1000770L, 1000929L),
             table = "pharma", sub_var = "pharmaid",
             callback = "transform_fun(set_val(TRUE))")
      ),
      aumc = list(
        list(ids = c(6922L, 6995L, 7106L, 8132L, 9042L, 9130L, 10628L),
             table = "drugitems", sub_var = "itemid",
             callback = "transform_fun(set_val(TRUE))")
      )
    )
  ),
  dex = list(
    min = 0,
    unit = "ml/hr",
    description = "dextrose (as D10)",
    category = "medications",
    target = "win_tbl",
    sources = list(
      mimic = list(
        list(ids = c(220950L, 228140L, 220952L), table = "inputevents_mv",
             sub_var = "itemid", dur_var = "endtime", amount_var = "amount",
             auom_var = "amountuom", callback = "combine_callbacks(
               mimv_rate,
               dex_to_10(c(228140L, 220952L), c(2, 5))
             )"),
        list(ids = c(30016L, 30017L), table = "inputevents_cv",
             sub_var = "itemid", grp_var = "linkorderid", val_var = "amount",
             unit_var = "amountuom", target = "ts_tbl", interval = "00:01:00",
             callback = "combine_callbacks(
               grp_mount_to_rate(mins(1L), hours(1L)),
               dex_to_10(30017L, 2)
             )")
      ),
      eicu = list(
        list(regex = "(d50|dextrose.+50 ?%)", table = "medication",
             sub_var = "drugname", dur_var = "drugstopoffset",
             callback = "eicu_dex_med", class = "rgx_itm"),
        list(regex = "(d10|dextrose.+10 ?%).+ml/hr", table = "infusiondrug",
             sub_var = "drugname", callback = "eicu_dex_inf", target = "ts_tbl",
             class = "rgx_itm")
      ),
      hirid = list(
        list(ids = c(1000022L, 1000690L, 1000689L, 1000544L, 1000746L,
                     1000835L, 1000060L, 1000545L, 1000567L),
             table = "pharma", sub_var = "pharmaid", grp_var = "infusionid",
             target = "ts_tbl", interval = "00:01:00",
             callback = "combine_callbacks(
               grp_mount_to_rate(mins(1L), hours(1L)),
               dex_to_10(list(c(1000689L, 1000544L, 1000746L, 1000835L),
                                1000060L, 1000545L, 1000567L), c(2, 3, 4, 5))
             )")
      ),
      aumc = list(
        list(ids = c(7254L, 7255L, 7256L, 8940L, 9571L), table = "drugitems",
             sub_var = "itemid", dur_var = "stop", rate_var = "doserateunit",
             callback = "combine_callbacks(
                           aumc_rate,
                           dex_to_10(list(7255L, 7256L, c(8940L, 9571L)),
                                     c(2, 3, 4))
                         )"
        )
      ),
      miiv = list(
        list(ids = c(220950L, 228140L, 220952L), table = "inputevents",
             sub_var = "itemid", dur_var = "endtime", amount_var = "amount",
             auom_var = "amountuom", callback = "combine_callbacks(
               mimv_rate,
               dex_to_10(c(228140L, 220952L), c(2, 5))
             )")
      )
    ),
    class = c("unt_cncpt", "num_cncpt")
  )
)

cfg <- lapply(cfg, function(x) {

  if ("sources" %in% names(x)) {

    if ("mimic" %in% names(x[["sources"]]))
      x[["sources"]] <- c(x[["sources"]],
                          mimic_demo = list(x[["sources"]][["mimic"]]))

    if ("eicu" %in% names(x[["sources"]]))
      x[["sources"]] <- c(x[["sources"]],
                          eicu_demo = list(x[["sources"]][["eicu"]]))

    x[["sources"]] <- x[["sources"]][order(names(x[["sources"]]))]
  }

  x
})

pkg_dir <- rprojroot::find_root(rprojroot::is_r_package)
cfg_dir <- file.path(pkg_dir, "inst", "extdata", "config")

ricu::set_config(cfg[order(names(cfg))], "concept-dict", cfg_dir)

devtools::install(pkg_dir)
