
# additionally concepts
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
  )
)

# extended existing concepts
ext <- list(
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