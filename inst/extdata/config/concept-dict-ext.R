
# additional concepts
add <- list(
  adm_episode = list(
    target = "id_tbl",
    sources = list(
      aumc = list(
        list(
          table = "admissions",
          val_var = "admissioncount",
          class = "col_itm"
        )
      ),
      mimic = list(
        list(
          table = "icustays",
          val_var = "subject_id",
          class = "col_itm",
          callback = "mimic_adm_epi_cb"
        )
      ),
      mimic_demo = list(
        list(
          table = "icustays",
          val_var = "subject_id",
          class = "col_itm",
          callback = "mimic_adm_epi_cb"
        )
      ),
      miiv = list(
        list(
          table = "icustays",
          val_var = "subject_id",
          class = "col_itm",
          callback = "miiv_adm_epi_cb"
        )
      ),
      sic = list(
        list(
          table = "cases",
          val_var = "AdmissionYear",
          grp_var = "PatientID",
          off_var = "OffsetAfterFirstAdmission",
          class = "col_itm",
          callback = "sic_adm_epi_cb"
        )
      ),
      anzics = list(
        list(
          table = "main",
          val_var = "AdmEpisode",
          class = "col_itm"
        )
      )
    )
  ),
  saps3 = list(
    target = "id_tbl",
    sources = list(
      sic = list(
        list(
          table = "cases",
          val_var = "saps3",
          class = "col_itm"
        )
      )
    )
  ),
  samp_raw = list(
    sources = list(
      sic = list(
        list(
          table = "microbiology",
          class = "col_itm",
          val_var = "TimeOfStay"
        )
      )
    )
  ),
  min_vol = list(
    sources = list(
      sic = list(
        list(
          table = "data_float_h",
          ids = c(2019),
          sub_var = "DataID"
        )
      )
    )
  ),
  antimycotic = list(
    sources = list(
      sic = list(
        list(
          table = "medication",
          ids = c(1464, 1466, 1856),
          sub_var = "DrugID",
          callback = "transform_fun(set_val(TRUE))"
        )
      )
    )
  ),
  map_beta200 = list(
    concepts = c("map", "norepi_equiv"),
    description = "MAP - 200 x NeEq",
    callback = "map_beta_200",
    class = "rec_cncpt"
  ),
  map_beta100 = list(
    concepts = c("map", "norepi_equiv"),
    description = "MAP - 100 x NeEq",
    callback = "map_beta_100",
    class = "rec_cncpt"
  ),
  map_beta50 = list(
    concepts = c("map", "norepi_equiv"),
    description = "MAP - 50 x NeEq",
    callback = "map_beta_50",
    class = "rec_cncpt"
  ),
  aptt_inr = list(
    concepts = c("ptt", "inr_pt"),
    description = "Bellomo index (APTT x INR)",
    category = "respiratory",
    aggregate = c("min", "max"),
    callback = "aptt_inr_cb",
    class = "rec_cncpt"
  ),
  neut_div_lymph = list(
    concepts = c("neut", "lymph"),
    description = "Neutrophils / Lymphocytes",
    category = "bone_marrow",
    callback = "neut_lymph_cb",
    class = "rec_cncpt"
  ),
  plt_div_inr = list(
    concepts = c("plt", "inr_pt"),
    description = "Platelets / Inter.Norm.Rat. Prothrobin Time",
    category = "coag",
    callback = "plt_inr_cb",
    class = "rec_cncpt"
  ),
  bun_div_crea = list(
    concepts = c("bun", "crea"),
    description = "Urea / Creatinine",
    category = "coag",
    callback = "bun_crea_cb",
    class = "rec_cncpt"
  ),
  sed_med = list(
    target = "win_tbl",
    description = "Sedation",
    class = "lgl_cncpt",
    sources = list(
      aumc = list(
        list(
          table = "drugitems",
          sub_var = "itemid",
          ids = c(7194, 7219, 7480, 6883, 12940, 7165, 6962, 9620, 12750, 7170,
                  19163, 12402, 21242, 7014, 9146, 9048),
          dur_var = "stop"
        )
      ),
      hirid = list(
        list(
          table = "pharma",
          sub_var = "pharmaid",
          ids = c(442, 1001215, 1000977, 1000978, 245, 246, 1000988, 1000239,
                  1000418, 1000700, 1001051, 1001054, 1000902, 251, 252, 1000976,
                  1000991, 1001049, 1000847, 1000475, 1000607, 202, 1000491, 1001050,
                  1001052, 1001053, 208, 1000691, 1000699),
          dur_var = "enteredentryat",
          group_var = "infusionid",
          target = "ts_tbl",
          callback = "hirid_pharma_win12"
        )
      ),
      miiv = list(
        list(
          table = "inputevents",
          sub_var = "itemid",
          ids = c(222168, 221744, 221668, 225942, 225154, 221833, 221385, 221623,
                  227520, 225972),
          dur_var = "endtime"
        )
      ),
      mimic = list(
        list(
          table = "inputevents_mv",
          sub_var = "itemid",
          ids = c(222168, 221744, 221668, 225942, 225154, 221833, 221385, 221623,
                  227520, 225972),
          dur_var = "endtime"
        ),
        list(
          table = "inputevents_cv",
          sub_var = "itemid",
          ids = c(30131, 30118, 30124, 30126, 30308, 30149, 30150, 44306, 45520,
                  46725, 45764, 30139, 30153, 41733, 43387),
          dur_var = "storetime",
          target = "ts_tbl",
          callback = "ts_to_win_12hours"
        )
      ),
      sic = list(
        list(
          ids = c(1400, 1430, 1445, 1480, 1495, 1499, 1501, 1549, 1696, 1723,
                  1747, 1914, 2142),
          table = "medication",
          sub_var = "DrugID",
          val_var = "AmountPerMinute",
          dur_var = "OffsetDrugEnd"
        )
      )
    )
  ),
  sed_rass = list(
    concepts = "rass",
    description = "RASS as 6h windows",
    category = "neurological",
    aggregate = "min",
    callback = "sed_rass",
    class = "rec_cncpt"
  ),
  gcs_raw = list(
    concepts = c("egcs", "mgcs", "vgcs", "tgcs"),
    description = "GCS (w/o sedation)",
    category = "neurological",
    aggregate = c("min", "min", "min", "min"),
    callback = "gcs_cb_generator('gcs_raw', NA_character_)",
    class = "rec_cncpt"
  ),
  gcs_med = list(
    concepts = c("egcs", "mgcs", "vgcs", "tgcs", "sed_med"),
    description = "GCS (with sedation)",
    category = "neurological",
    aggregate = c("min", "min", "min", "min", "any"),
    callback = "gcs_cb_generator('gcs_med', 'sed_med')",
    class = "rec_cncpt"
  ),
  gcs_rass = list(
    concepts = c("egcs", "mgcs", "vgcs", "tgcs", "sed_rass"),
    description = "GCS (with rass)",
    category = "neurological",
    aggregate = list("min", "min", "min", "min", NULL),
    callback = "gcs_cb_generator('gcs_rass', 'sed_rass')",
    class = "rec_cncpt"
  ),
  indig = list(
    target = "id_tbl",
    description = "Indigenous population indicator",
    sources = list(
      anzics = list(
        list(table = "main", val_var = "INDIGENOUS", class = "col_itm",
             callback = "anzics_binary")
      )
    )
  ),
  apache_iii_diag = list(
    target = "id_tbl",
    sources = list(
      anzics = list(
        list(table = "main", val_var = "AP3DIAG", class = "col_itm")
      )
    )
  ),
  apache_iii = list(
    target = "id_tbl",
    sources = list(
      anzics = list(
        list(table = "main", val_var = "Apache3Score", class = "col_itm")
      )
    )
  ),
  apache_iii_rod = list(
    target = "id_tbl",
    sources = list(
      anzics = list(
        list(table = "main", val_var = "Apache3RiskOfDeath", class = "col_itm")
      )
    )
  ),
  country = list(
    target = "id_tbl",
    sources = list(
      anzics = list(
        list(table = "main", val_var = "CountryCode", class = "col_itm")
      )
    )
  ),
  elective = list(
    target = "id_tbl",
    sources = list(
      anzics = list(
        list(table = "main", val_var = "ELECT", class = "col_itm", 
             callback = "anzics_binary")
      )
    )
  ),
  is_invasive2 = list(
    target = "id_tbl",
    sources = list(
      anzics = list(
        list(table = "main", val_var = "VENTILATED", class = "col_itm", 
             callback = "anzics_binary")
      )
    )
  ),
  is_noninvasive = list(
    target = "id_tbl",
    sources = list(
      anzics = list(
        list(table = "main", val_var = "NIV_IND", class = "col_itm", 
             callback = "anzics_binary")
      )
    )
  ),
  is_invasive = list(
    target = "id_tbl",
    sources = list(
      anzics = list(
        list(table = "main", val_var = "INV_IND", class = "col_itm", callback = "anzics_binary")
      )
    )
  ),
  is_vent2 = list(
    concepts = list("is_invasive", "is_invasive2", "is_noninvasive"),
    callback = "anzics_is_vent_cb",
    class = "rec_cncpt",
    target = "id_tbl"
  ),
  is_vent = list(
    concepts = c("vent_ind"),
    description = "Ventilation during ICU stay",
    category = "respiratory",
    target = "id_tbl",
    callback = "is_vent_callback",
    class = "rec_cncpt"
  ),
  is_vaso = list(
    concepts = c("norepi_equiv"),
    description = "Vasopressor administration during ICU stay",
    category = "cardio",
    target = "id_tbl",
    callback = "is_vaso_callback",
    class = "rec_cncpt"
  ),
  is_inotrop = list(
    target = "id_tbl",
    sources = list(
      anzics = list(
        list(table = "main", val_var = "INOTROP_IND", class = "col_itm", 
             callback = "anzics_binary")
      )
    )
  ),
  is_rrt = list(
    target = "id_tbl",
    sources = list(
      anzics = list(
        list(table = "main", val_var = "RENAL_IND", class = "col_itm", 
             callback = "anzics_binary")
      )
    )
  ),
  is_trache = list(
    target = "id_tbl",
    sources = list(
      anzics = list(
        list(table = "main", val_var = "TRACHE_IND", class = "col_itm", 
             callback = "anzics_binary")
      )
    )
  ),
  is_ecmo = list(
    target = "id_tbl",
    sources = list(
      anzics = list(
        list(table = "main", val_var = "ECMO_IND", class = "col_itm", 
             callback = "anzics_binary")
      )
    )
  ),
  adm_diag = list(
    target = "id_tbl",
    levels = c("CMED", "CSURG", "DENT", "ENT", "GU", "GYN", "MED", "NB", "NBB", 
               "NMED", "NSURG", "OBS", "ORTHO", "OMED", "PSURG", "PSYCH", 
               "SURG", "TRAUM", "TSURG", "VSURG"),
    class = "fct_cncpt",
    description = "patient admission type",
    category = "demographics",
    sources = list(
      aumc = list(
        list(
          val_var = "specialty",
          table = "admissions",
          callback = "apply_map(c(Cardiochirurgie = 'CSURG', Cardiologie = 'CMED', 
          ders = 'MED', Gynaecologie = 'GYN', 
          `Heelkunde Gastro-enterologie` = 'SURG', 
          `Heelkunde Longen/Oncologie` = 'TSURG', `Heelkunde Oncologie` = 'SURG', 
          Hematologie = 'MED', `Intensive Care Volwassenen` = 'MED', 
          Inwendig = 'MED', `Keel, Neus & Oorarts` = 'ENT', 
          Longziekte = 'MED', `Maag-,Darm-,Leverziekten` = 'MED', 
          Mondheelkunde = 'DENT', Nefrologie = 'GU', 
          Neurochirurgie = 'NSURG', Neurologie = 'NMED', 
          Obstetrie = 'OBS', `Oncologie Inwendig` = 'MED', 
          Oogheelkunde = 'MED', Orthopedie = 'ORTHO', 
          `Plastische chirurgie` = 'PSURG', 
          Reumatologie = 'OMED', Traumatologie = 'TRAUM', Urologie = 'GU', 
          Vaatchirurgie = 'VSURG', Verloskunde = 'OBS'))",
          class = "col_itm"
        )
      ),
      miiv = list(
        list(
          table = "services",
          val_var = "curr_service",
          class = "col_itm"
        )
      ),
      mimic = list(
        list(
          table = "services",
          val_var = "curr_service",
          callback = "mimic_adm_diag",
          class = "col_itm"
        )
      ),
      mimic_demo = list(
        list(
          table = "services",
          val_var = "curr_service",
          callback = "mimic_adm_diag",
          class = "col_itm"
        )
      ),
      anzics = list(
        list(
          table = "main",
          val_var = "AP3DIAG",
          callback = "anzics_adm_diag",
          class = "col_itm"
        )
      )
    )
  ),
  charlson = list(
    target = "id_tbl",
    description = "Charlson Comorbidity Index",
    sources = list(
      mimic_demo = list(
        list(table = "diagnoses_icd", val_var = "icd9_code",
             callback = "mimic_charlson_dir", class = "col_itm")
      ),
      mimic = list(
        list(table = "diagnoses_icd", val_var = "icd9_code",
             callback = "mimic_charlson_dir", class = "col_itm")
      ),
      miiv = list(
        list(ids = c(9, 10), table = "diagnoses_icd", val_var = "icd_code",
             sub_var = "icd_version", callback = "miiv_charlson_dir")
      )
    )
  ),
  acu_24 = list(
    concepts = c("sofa"),
    description = "SOFA at 24 hours",
    callback = "acute_dayone",
    class = "rec_cncpt",
    target = "id_tbl"
  ),
  race = list(
    description = "Race",
    category = "Misc.",
    levels = c("Caucasian", "Asian", "African American", "Hispanic", "Other"),
    class = "fct_cncpt",
    target = "id_tbl",
    sources = list(
      mimic_demo = list(
        list(
          table = "admissions",
          val_var = "ethnicity",
          class = "col_itm",
          callback = "race_mimic_cb"
        )
      ),
      mimic = list(
        list(
          table = "admissions",
          val_var = "ethnicity",
          class = "col_itm",
          callback = "race_mimic_cb"
        )
      ),
      miiv = list(
        list(
          table = "admissions",
          val_var = "race",
          class = "col_itm",
          callback = "race_miiv_cb"
        )
      ),
      eicu_demo = list(
        list(
          table = "patient",
          val_var = "ethnicity",
          class = "col_itm",
          callback = "race_eicu_cb"
        )
      ),
      eicu = list(
        list(
          table = "patient",
          val_var = "ethnicity",
          class = "col_itm",
          callback = "race_eicu_cb"
        )
      )
    )
  ),
  adm_year = list(
    target = "id_tbl",
    sources = list(
      anzics = list(
        list(
          table = "main",
          val_var = "IcuAdmitYYYY",
          class = "col_itm"
        )
      )
    )
  ),
  apache_iii_subcode = list(
    target = "id_tbl",
    sources = list(
      anzics = list(
        list(
          table = "main",
          val_var = "AP3_SUBCODE",
          class = "col_itm"
        )
      )
    )
  ),
  site = list(
    target = "id_tbl",
    sources = list(
      anzics = list(
        list(
          table = "main",
          val_var = "SiteID",
          class = "col_itm"
        )
      )
    )
  ),
  spfi = list(
    concepts = c("spo2", "fio2"),
    description = "SpO2/FiO2",
    category = "respiratory",
    aggregate = c("min", "max"),
    callback = "spfi",
    class = "rec_cncpt"
  ),
  frailty = list(
    target = "id_tbl",
    sources = list(
      anzics = list(
        list(
          table = "main",
          val_var = "FRAILTY",
          class = "col_itm"
        )
      )
    )
  ),
  notes = list(
    sources = list(
      mimic = list(
        list(
          table = "noteevents",
          val_var = "text",
          index2_var = "charttime",
          grp1_var = "category",
          grp2_var = "row_id",
          callback = "mimic_notes_cb",
          class = "col_itm"
        )
      )
    )
  ),
  surg_site = list(
    target = "id_tbl",
    sources = list(
      sic = list(
        list(
          table = "cases",
          val_var = "SurgicalSite",
          class = "col_itm",
          callback = "sic_surg_site_cb"
        )
      )
    )
  )
)

# extended existing concepts
ext <- list(
  weight = list(
    anzics = list(
      list(
        table = "main",
        val_var = "WEIGHT",
        add1_var = "HEIGHT",
        add2_var = "AGE",
        callback = "weight_anzics_cb",
        class = "col_itm"
      )
    )
  ),
  height = list(
    anzics = list(
      list(
        table = "main",
        val_var = "HEIGHT",
        add1_var = "WEIGHT",
        add2_var = "AGE",
        class = "col_itm",
        callback = "height_anzics_cb"
      )
    )
  ),
  death = list(
    sic = list(
      list(table = "cases", index_var = "OffsetAfterFirstAdmission",
           val_var = "HospitalDischargeType", callback = "sic_death_cb",
           add_var = "OffsetOfDeath", add_var2 = "TimeOfStay",
           class = "col_itm")
    )
  ),
  adm = list(
    sic = list(
      list(table = "cases", class = "col_itm", val_var = "SurgicalSite",
           callback = "sic_adm_cb")
    ),
    anzics = list(
      list(table = "main", class = "col_itm", val_var = "AP3DIAG",
           callback = "anzics_adm")
    )
  ),
  los_icu = list(
    sic = list(
      list(table = "cases", class = "col_itm", val_var = "TimeOfStay",
           callback = "sic_los_icu_cb")
    )
  ),
  tgcs = list(
    sic = list(
      list(table = "gcs", class = "col_itm", val_var = "AdmissionFormGCS",
           callback = "sic_gcs_cb")
    )
  ),
  samp = list(
    sic = list(
      list(table = "microbiology", class = "col_itm", val_var = "TimeOfStay",
           callback = "sic_samp_cb")
    )
  ),
  mech_vent = list(
    sic = list(
      list(ids = c(2019), table = "data_float_h", sub_var = "DataID", 
           dur_var = "rawdata", add_var = "cnt", callback = "sic_mv_cb")
    )
  ),
  rass = list(
    sic = list(
      list(table = "rass", sub_var = "DataID", ids = c(3123),
           val_var = "Val")
    )
  ),
  inr_pt = list(
    sic = list(
      list(table = "laboratory", sub_var = "LaboratoryID", ids = c(237),
           callback = "sic_inr_pt_cb")
    )
  ),
  sex = list(
    anzics = list(
      list(
        table = "main", val_var = "SEX", callback = "anzics_sex", 
        class = "col_itm"
      )
    )
  ),
  age = list(
    anzics = list(
      list(
        table = "main", val_var = "AGE", class = "col_itm"
      )
    )
  ),
  death = list(
    anzics = list(
      list(
        table = "main", val_var = "DIED", callback = "anzics_binary", 
        index_var = "HOSP_DS_DTM", class = "col_itm"
      )
    )
  ),
  map = list(
    anzics = list(
      list(
        table = "main", val_var = "MAP_ANZ", class = "col_itm",
        index_var = "ICU_DS_DTM"
      )
    )
  ),
  po2 = list(
    anzics = list(
      list(
        table = "main", val_var = "PAO2_ANZ", class = "col_itm",
        index_var = "ICU_DS_DTM"
      )
    )
  ),
  pco2 = list(
    anzics = list(
      list(
        table = "main", val_var = "PACO2_ANZ", class = "col_itm",
        index_var = "ICU_DS_DTM"
      )
    )
  ),
  resp = list(
    anzics = list(
      list(
        table = "main", val_var = "RR_ANZ", class = "col_itm",
        index_var = "ICU_DS_DTM"
      )
    )
  )
)

# adding the concepts
for (i in seq_along(add)) {
  
  nm <- names(add)[i]
  cfg[[nm]] <- add[[i]]
}

# extending the existing concepts
for (i in seq_along(ext)) {
  
  nm <- names(ext)[i]
  for (j in seq_along(ext[[i]])) {
    
    src_nm <- names(ext[[i]])[j]
    cfg[[nm]][["sources"]][[src_nm]] <- ext[[i]][[j]]
  }
}