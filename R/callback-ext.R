
is_vent_callback <- function(vent_ind, interval, ...) {
  
  vent_ind <- expand(vent_ind)
  vent_ind[, c(index_var(vent_ind)) := NULL]
  vent_ind <- unique(vent_ind)
  
  vent_ind[, is_vent := vent_ind]
  vent_ind[, vent_ind := NULL]
  
  vent_ind
  
}

is_vaso_callback <- function(..., interval) {
  
  x <- list(...)[["norepi_equiv"]]
  x[, c(index_var(x)) := NULL]
  
  x[, is_vaso := (max(norepi_equiv) > 0), by = get(id_var(x))]
  x[, norepi_equiv := NULL]
  
  unique(x)
  
}

sic_surg_site_cb <- function(x, ...) {
  
  category_list <- list(
    Abdomen = c(
      2211, # Abdomen: Unterer GI-Trakt
      2219, # Abdomen: Oberer GI-Trakt (inkl. Jejunum)
      2225, # Abdomen: Leber-Chriurgie
      2242, # Abdomen: Pankreas
      2253, # Abdomen: Biliärtrakt
      2259  # Abdomen: endokrine Chirurgie
    ),
    Vascular = c(
      2214, # Gefäß: Aortenchirurgie
      2234, # Gefäß: Karotischirurgie
      2243, # Gefäß: große Gefäße (Thorax und Bauch)
      2258, # Gefäß: Andere
      2229  # Gefäß: periphere Gefäße
    ),
    Heart = c(
      2227, # Herz: CABG
      2237, # Herz: Klappe mit CABG
      2240, # Herz: Klappe
      2241  # Herz: Andere
    ),
    Trauma_Orthopedics = c(
      2231, # Trauma: SHT
      2246, # Trauma: Extremitäten
      2269, # Trauma: Abdomen (Assuming Abdominal trauma)
      2254, # Trauma: Thorax (Assuming Thoracic trauma)
      2239, # Trauma: Polytrauma
      2207  # Extremitäten-Chirurgie
    ),
    Neuro = c(
      2245, # Neurochirurgie: zerebrovaskulär
      2250, # Neurochirurgie: Wirbelsäule
      2264, # Neurochirurgie: Andere
      2273  # Neurochirurgie: intrakranieller Tumor
    ),
    Transplant = c(
      2268, # Transplantation: Leber
      2270, # Transplantation: Herz/Lunge
      2271, # Transplantation: Andere
      2272  # Transplantation: Herz
    ),
    Gynecological = c(2260), # Gynäkologischer Eingriff
    Birth = c(2261),         # Geburtshilfe
    Other = c(
      2205, # Unknown
      2221, # .
      2217, # HNO
      2210, # Kieferchirurgie (Oral / Maxilofacial)
      2232  # Andere Eingriffe
    ),
    Thorax = c(
      2262, # Thorax: Lobektomie
      2263, # Thorax: Pneumonektomie
      2267, # Thorax: Pleura
      2226  # Thorax: Andere
    )
  )
  
  site_map <- do.call(
    rbind,
    Map(
      function(site, nums) {
        data.table(site = site, SurgicalSite = nums)
      }, names(category_list), category_list
    )
  )
  
  x <- merge(x, site_map, by = "SurgicalSite")
  x[, SurgicalSite := NULL]
  rename_cols(x, "SurgicalSite", "site")
}

spfi <- function(..., match_win = hours(2L),
                 mode = c("match_vals", "extreme_vals", "fill_gaps"),
                 fix_na_fio2 = TRUE, interval = NULL) {
  
  mode <- match.arg(mode)
  
  assert_that(is.flag(fix_na_fio2))
  
  cnc <- c("spo2", "fio2")
  res <- collect_dots(cnc, interval, ...)
  res <- match_fio2(res, match_win, mode, if (fix_na_fio2) cnc[2L] else NULL)
  
  res <- res[!is.na(get(cnc[1L])) & !is.na(get(cnc[2L])) & get(cnc[2L]) != 0, ]
  res <- res[, c("spfi") := 100 * get(cnc[1L]) / get(cnc[2L])]
  res <- rm_cols(res, cnc)
  
  res
}

race_mimic_cb <- function(x, val_var, env) {
  
  groups <- list(
    Caucasian = c("WHITE", "WHITE - BRAZILIAN", "WHITE - EASTERN EUROPEAN",
                  "WHITE - OTHER EUROPEAN", "WHITE - RUSSIAN"),
    Asian = c("ASIAN", "ASIAN - ASIAN INDIAN", "ASIAN - CAMBODIAN",
              "ASIAN - CHINESE", "ASIAN - FILIPINO", "ASIAN - JAPANESE",
              "ASIAN - KOREAN", "ASIAN - OTHER", "ASIAN - THAI",
              "ASIAN - VIETNAMESE"),
    Hispanic = c("HISPANIC/LATINO - CENTRAL AMERICAN (OTHER)",
                 "HISPANIC/LATINO - COLOMBIAN",
                 "HISPANIC/LATINO - CUBAN", "HISPANIC/LATINO - DOMINICAN",
                 "HISPANIC/LATINO - GUATEMALAN",
                 "HISPANIC/LATINO - HONDURAN", "HISPANIC/LATINO - MEXICAN",
                 "HISPANIC/LATINO - PUERTO RICAN",
                 "HISPANIC/LATINO - SALVADORAN", "HISPANIC OR LATINO"),
    `African American` = c("BLACK/AFRICAN AMERICAN"),
    Other = c("AMERICAN INDIAN/ALASKA NATIVE FEDERALLY RECOGNIZED TRIBE",
              "UNABLE TO OBTAIN", "UNKNOWN/NOT SPECIFIED", "MIDDLE EASTERN",
              "MULTI RACE ETHNICITY",
              "NATIVE HAWAIIAN OR OTHER PACIFIC ISLANDER", "OTHER",
              "PATIENT DECLINED TO ANSWER",
              "PORTUGUESE", "SOUTH AMERICAN",
              "AMERICAN INDIAN/ALASKA NATIVE",
              "AMERICAN INDIAN/ALASKA NATIVE FEDERALLY RECOGNIZED TRIBE",
              "CARIBBEAN ISLAND", "BLACK/AFRICAN", "BLACK/CAPE VERDEAN",
              "BLACK/HAITIAN")
  )
  map <- unlist(groups)
  names(map) <- rep(names(groups), times = lapply(groups, length))
  
  x[, ethnicity := names(map)[match(ethnicity, map)]]
}

race_miiv_cb <- function(x, val_var, env) {
  
  groups <- list(
    Caucasian = c("WHITE", "WHITE - BRAZILIAN", "WHITE - EASTERN EUROPEAN",
                  "WHITE - OTHER EUROPEAN", "WHITE - RUSSIAN"),
    Asian = c("ASIAN", "ASIAN - ASIAN INDIAN", "ASIAN - CHINESE",
              "ASIAN - KOREAN", "ASIAN - SOUTH EAST ASIAN"),
    Hispanic = c("HISPANIC OR LATINO", "HISPANIC/LATINO",
                 "HISPANIC/LATINO - CENTRAL AMERICAN",
                 "HISPANIC/LATINO - COLUMBIAN", "HISPANIC/LATINO - CUBAN",
                 "HISPANIC/LATINO - DOMINICAN", "HISPANIC/LATINO - GUATEMALAN",
                 "HISPANIC/LATINO - HONDURAN", "HISPANIC/LATINO - MEXICAN",
                 "HISPANIC/LATINO - PUERTO RICAN",
                 "HISPANIC/LATINO - SALVADORAN", "SOUTH AMERICAN"),
    `African American` = c("BLACK/AFRICAN", "BLACK/AFRICAN AMERICAN",
                           "BLACK/CAPE VERDEAN", "BLACK/CARIBBEAN ISLAND"),
    Other = c("OTHER", "UNKNOWN", "UNABLE TO OBTAIN", "MULTIPLE RACE/ETHNICITY",
              "NATIVE HAWAIIAN OR OTHER PACIFIC ISLANDER",
              "PATIENT DECLINED TO ANSWER", "PORTUGUESE")
  )
  
  map <- unlist(groups)
  names(map) <- rep(names(groups), times = lapply(groups, length))
  
  x[, race := names(map)[match(race, map)]]
}

race_eicu_cb <- function(x, val_var, env) {
  
  x[ethnicity %in% c("Native American", "Other/Unknown"), ethnicity := "Other"]
}

acute_dayone <- function(sofa, ...) {
  
  ind_var <- index_var(sofa)
  sofa <- sofa[get(ind_var) == hours(24L)]
  sofa[, acu_24 := sofa]
  sofa[, c(ind_var, "sofa") := NULL]
  sofa
}

mimic_adm_epi_cb <- function(x, ...) {

  x <- merge(x, list(...)$env$icustays[, c("icustay_id", "intime")], 
             by = "icustay_id")
  x <- setorderv(x, cols = c("subject_id", "intime"))
  x[, adm_episode := seq_len(.N), by = "subject_id"]
  
  x <- x[, c(id_vars(x), "adm_episode"), with=FALSE]
  rename_cols(x, "subject_id", "adm_episode")
}

mimic_charlson_dir <- function(x, ...) {

  ch9 <- icd::icd9_comorbid_charlson(x)
  
  make_long <- function(x, id_name) {
    
    res <- id_tbl(id = as.integer(rownames(x)))
    res <- cbind(res, x)
    res <- rename_cols(res, id_name, "id")
    res <- data.table::melt.data.table(res, id.vars = id_name)
    as_id_tbl(res)
  }
  
  ch <- make_long(ch9, id_vars(x))
  ch <- ch[, list(cmb = max(value, na.rm = TRUE)), 
           by = c(id_vars(ch), "variable")]
  ch[, list(icd9_code = sum(cmb, na.rm = TRUE)), by = c(id_vars(ch))]
}

miiv_charlson_dir <- function(x, ...) {
  
  ch9 <- icd::icd9_comorbid_charlson(x[icd_version == 9])
  ch10 <- icd::icd10_comorbid_charlson(x[icd_version == 10])
  
  make_long <- function(x, id_name) {
    
    res <- id_tbl(id = as.integer(rownames(x)))
    res <- cbind(res, x)
    res <- rename_cols(res, id_name, "id")
    res <- melt.data.table(res, id.vars = id_name)
    as_id_tbl(res)
  }
  
  ch <- rbind(
    make_long(ch9, id_vars(x)),
    make_long(ch10, id_vars(x))
  )
  
  ch <- ch[, list(cmb = max(value, na.rm = TRUE)), by = c(id_vars(ch), "variable")]
  ch[, list(icd_code = sum(cmb, na.rm = TRUE)), by = c(id_vars(ch))]
}

anzics_adm_diag <- function(x, val_var, env, ...) {
  
  vsurg <- c(1203.01, 1203.02, 1204.01, 1204.02, 1205.01, 1205.02, 
             1206.01, 1206.05, 1206.06, 1206.07, 1206.08, 1206.09, 
             1207.01, 1207.02, 1207.03, 1208.03, 1208.04, 1208.10, 
             1208.11, 1208.12, 1208.13, 1208.14, 1208.17, 1208.18, 
             1209.01, 1209.02, 1210.01, 1210.02, 1211.01, 1211.02, 
             1212.01, 1212.02, 1212.04, 1212.05, 1212.06, 1212.07, 
             1213.01, 1213.02)
  
  tsurg <- c(1302.01, 1302.02, 1302.03, 1303.01, 1303.02, 1304.02, 
             1304.03, 1304.04, 1304.06, 1304.07, 1304.08, 1304.09, 
             1304.11)
  
  omed <- c(202.02, 202.03, 202.04, 202.05, 312.01, 312.02, 312.03, 312.04, 
            312.05, 405.01, 802.03, 802.04, 802.05, 802.06, 802.07, 802.08, 
            802.1, 901.05)
  
  
  x <- merge(
    x, load_concepts("apache_iii_subcode", "anzics", verbose = FALSE),
    all.x = TRUE
  )
  
  diag_map <- list(
    CMED = c(101, 102, 103, 104, 106, 107, 108, 109, 110, 111),
    CSURG = c(1202, 1203, 1204, 1205, 1206, 1207, 1208, 1209, 1210, 1211, 1212, 
              1213),
    # DENT = c(),
    # ENT = c(),
    # EYE = c(),
    GU = c(1701, 1702, 1703, 1704, 1705, 902, 903),
    GYN = c(902, 903, 1101, 1102),
    MED = c(201, 202, 203, 204, 206, 207, 208, 209, 210, 211, 212, 213, 301, 
            302, 303, 304, 305, 306, 307, 308, 309, 310, 311, 312, 313, 501, 
            502, 503, 504, 701, 702, 703, 704, 801, 802, 901, 1101, 1102),
    # NB = c(),
    # NBB = c(),
    NMED = c(401, 402, 403, 404, 405, 406, 407, 408, 409, 410),
    NSURG = c(1501, 1502, 1503, 1504, 1505, 1506),
    OBS = c(902, 903),
    ORTHO = c(1902, 1903, 1904),
    # OMED = c(),
    # PSURG = c(),
    # PSYCH = c(),
    # TSURG = c(),
    # VSURG = c(),
    SURG = c(1301, 1302, 1303, 1304, 1401, 1403, 1404, 1405, 1406, 1407, 1408, 
             1409, 1410, 1411, 1412, 1413),
    TRAUM = c(601, 602, 603, 604, 605, 1601, 1602, 1603, 1604, 1605),
    OTH = c(0)
  )
  
  diag_dt <- data.table::data.table()
  for (i in seq_along(diag_map)) {
    diag_dt <- rbind(
      diag_dt,
      data.table(target = names(diag_map)[i], AP3DIAG = diag_map[[i]])
    )
  }

  x <- merge(x, diag_dt, by = val_var)
  x[apache_iii_subcode %in% vsurg, target := "VSURG"]
  x[apache_iii_subcode %in% tsurg, target := "TSURG"]
  x[apache_iii_subcode %in% omed, target := "OMED"]
  x[, AP3DIAG := NULL]
  rename_cols(x, "AP3DIAG", "target")
}

mimic_adm_diag <- function(x, val_var, ...) {
  
  mapp <- list(
    OTH = c("DENT", "PSYCH", "NBB", "NB", "ENT", "GU", "PSURG"),
    GYN = c("GYN", "OBS")
  )
  for (i in seq_along(mapp)) {
    x[get(val_var) %in% mapp[[i]], c(val_var) := names(mapp)[i]]
  }
  x
}

# ANZICS APD callbacks
init_proj <- function(file = ".gitignore") {
  
  root <- rprojroot::find_root(rprojroot::has_file(file))
  invisible(lapply(list.files(file.path(root, "r"), full.names = TRUE), source))
}

anzics_sex <- function(x, ...) {

  x[, SEX := ifelse(SEX == "M", "Male", "Female")]
  x
}

anzics_diagnosis_decode <- function(x) {
  
  df <- anzics$d_diagnoses
  df$diagnosis_name[match(x, df$diagnosis_code)]
}

anzics_binary <- function(x, val_var, ...) {
  
  x[, c(val_var) := as.logical(ifelse(get(val_var) == 1, 1, 0))]
  x
}

anzics_adm <- function(x, val_var, ...) {
  
  diag_to_adm <- function(x) ifelse(x < 1200, "med", "surg")
  
  x[, adm := diag_to_adm(get(val_var))]
  x[, c(val_var) := NULL]
  x <- setnames(x, "adm", val_var)
  x
}

anzics_is_vent_cb <- function(...) {
  
  res <- Reduce(function(x, y) merge(x, y, all = TRUE), list(...)[1:3])
  res[, is_vent := is_invasive > 0 | is_invasive2 > 0 | is_noninvasive > 0]
  res[is.na(is_vent), is_vent := FALSE]
  res[, c(id_vars(res), "is_vent"), with=FALSE]
}


# SICdb callbacks
sic_death_cb <- function(x, ...) {
  
  x[, HospitalDischargeType := HospitalDischargeType == 2028]
  x[, OffsetAfterFirstAdmission := NA]
  x[HospitalDischargeType == TRUE, 
    OffsetAfterFirstAdmission := TimeOfStay]
  x[!is.na(OffsetOfDeath) & HospitalDischargeType == TRUE, 
    OffsetAfterFirstAdmission := OffsetOfDeath]
  x
}

sic_adm_cb <- function(x, val_var, ...) {
  
  x[, c(val_var) := NULL]
  x[, c(val_var) := "surg"]
}

sic_los_icu_cb <- function(x, ...) {
  
  x[, TimeOfStay := as.numeric(TimeOfStay / (60 * 60 * 24))]
}

sic_gcs_cb <- function(x, ...) {
  
  x[, c(index_var(x)) := hours(0L)]
  x
}

sic_samp_cb <- function(x, val_var, ...) {
  
  x1 <- data.table::copy(x)
  x2 <- data.table::copy(x)
  x3 <- data.table::copy(x)
  
  x1 <- x1[, c(index_var(x1)) := get(index_var(x1)) - hours(24L)]
  x2 <- x2[, c(index_var(x2)) := get(index_var(x2)) - hours(72L)]
  x3 <- x3[, c(index_var(x3)) := get(index_var(x3)) - hours(48L)]
  
  x <- rbind(x1, x2, x3)
  x[, c(val_var) := as.logical(get(val_var))]
  x[, c(val_var) := FALSE]
  x
}

sic_mv_cb <- function(x, val_var, dur_var, add_var, ...) {
  
  x[, c(dur_var) := min_as_mins(get(add_var))]
  x[, c(val_var) := as.character(get(val_var))]
  x[, c(val_var) := "invasive"]
  x
}

sic_inr_pt_cb <- function(x, val_var, ...) {
  
  x[, c(val_var) := 1 / (get(val_var) / 100)]
}

# admission episode callbacks
sic_adm_epi_cb <- function(x, val_var, grp_var, off_var, ...) {
  
  x <- data.table::setorderv(x, c("PatientID", "CaseID"))
  x[, c(val_var) := seq_along(get(val_var)), by = grp_var]
  data.table::setorderv(x, "CaseID")
}

miiv_adm_epi_cb <- function(x, ...) {
  
  x <- merge(x, list(...)$env$icustays[, c("stay_id", "intime")], by = "stay_id")
  x <- setorderv(x, cols = c("subject_id", "intime"))
  x[, adm_episode := seq_len(.N), by = "subject_id"]
  
  x <- x[, c(id_vars(x), "adm_episode"), with=FALSE]
  rename_cols(x, "subject_id", "adm_episode")
}

# MAP - \beta * NorEq
map_beta_200 <- function (..., match_win = hours(2L), beta = 200, 
                          interval = NULL) {
  
  cnc <- c("map", "norepi_equiv")
  res <- collect_dots(cnc, interval, ...)
  
  assert_that(is_interval(match_win), match_win > check_interval(res))
  
  on12 <- paste(meta_vars(res[[1L]]), "==", meta_vars(res[[2L]]))
  on21 <- paste(meta_vars(res[[2L]]), "==", meta_vars(res[[1L]]))
  res <- rbind(res[[1L]][res[[2L]], on = on12, roll = match_win],
               res[[2L]][res[[1L]], on = on21, roll = match_win])
  res <- unique(res)
  res[is.na(get(cnc[2L])), cnc[2L]] <- 0 # impute a 0 value for vasos
  
  res <- res[!is.na(get(cnc[1L])) & !is.na(get(cnc[2L])), ]
  res <- res[, `:=`(c(paste0("map_beta", beta)), 
                    get(cnc[1L]) - beta*get(cnc[2L]))]
  res <- rm_cols(res, cnc)
  res
}

map_beta_100 <- function (..., match_win = hours(2L), beta = 100, 
                          interval = NULL) {
  
  cnc <- c("map", "norepi_equiv")
  res <- collect_dots(cnc, interval, ...)
  
  assert_that(is_interval(match_win), match_win > check_interval(res))
  
  on12 <- paste(meta_vars(res[[1L]]), "==", meta_vars(res[[2L]]))
  on21 <- paste(meta_vars(res[[2L]]), "==", meta_vars(res[[1L]]))
  res <- rbind(res[[1L]][res[[2L]], on = on12, roll = match_win],
               res[[2L]][res[[1L]], on = on21, roll = match_win])
  res <- unique(res)
  res[is.na(get(cnc[2L])), cnc[2L]] <- 0 # impute a 0 value for vasos
  
  res <- res[!is.na(get(cnc[1L])) & !is.na(get(cnc[2L])), ]
  res <- res[, `:=`(c(paste0("map_beta", beta)), 
                    get(cnc[1L]) - beta*get(cnc[2L]))]
  res <- rm_cols(res, cnc)
  res
}

map_beta_50 <- function (..., match_win = hours(2L), beta = 50, 
                         interval = NULL) {
  
  cnc <- c("map", "norepi_equiv")
  res <- collect_dots(cnc, interval, ...)
  
  assert_that(is_interval(match_win), match_win > check_interval(res))
  
  on12 <- paste(meta_vars(res[[1L]]), "==", meta_vars(res[[2L]]))
  on21 <- paste(meta_vars(res[[2L]]), "==", meta_vars(res[[1L]]))
  res <- rbind(res[[1L]][res[[2L]], on = on12, roll = match_win],
               res[[2L]][res[[1L]], on = on21, roll = match_win])
  res <- unique(res)
  res[is.na(get(cnc[2L])), cnc[2L]] <- 0
  
  res <- res[!is.na(get(cnc[1L])) & !is.na(get(cnc[2L])), ]
  res <- res[, `:=`(c(paste0("map_beta", beta)), 
                    get(cnc[1L]) - beta*get(cnc[2L]))]
  res <- rm_cols(res, cnc)
  res
}

aptt_inr_cb <- function (..., match_win = hours(6L), interval = NULL) {
  
  cnc <- c("ptt", "inr_pt")
  res <- ricu:::collect_dots(cnc, interval, ...)
  assert_that(is_interval(match_win), match_win > ricu:::check_interval(res))
  
  on12 <- paste(meta_vars(res[[1L]]), "==", meta_vars(res[[2L]]))
  on21 <- paste(meta_vars(res[[2L]]), "==", meta_vars(res[[1L]]))
  
  res <- rbind(res[[1L]][res[[2L]], on = on12, roll = match_win],
               res[[2L]][res[[1L]], on = on21, roll = match_win])
  res <- unique(res)
  
  res <- res[!is.na(get(cnc[1L])) & !is.na(get(cnc[2L])), ]
  res <- res[, `:=`(c("aptt_inr"), get(cnc[1L])*get(cnc[2L]))]
  res <- rm_cols(res, cnc)
  res
}

neut_lymph_cb <- function (..., match_win = hours(6L), interval = NULL) {
  
  cnc <- c("neut", "lymph")
  res <- ricu:::collect_dots(cnc, interval, ...)
  assert_that(is_interval(match_win), match_win > ricu:::check_interval(res))
  
  on12 <- paste(meta_vars(res[[1L]]), "==", meta_vars(res[[2L]]))
  on21 <- paste(meta_vars(res[[2L]]), "==", meta_vars(res[[1L]]))
  
  res <- rbind(res[[1L]][res[[2L]], on = on12, roll = match_win],
               res[[2L]][res[[1L]], on = on21, roll = match_win])
  res <- unique(res)
  
  res <- res[!is.na(get(cnc[1L])) & !is.na(get(cnc[2L])), ]
  res <- res[, `:=`(c("neut_div_lymph"), get(cnc[1L])/get(cnc[2L]))]
  res <- rm_cols(res, cnc)
  res
}

plt_inr_cb <- function (..., match_win = hours(6L), interval = NULL) {
  
  cnc <- c("plt", "inr_pt")
  res <- ricu:::collect_dots(cnc, interval, ...)
  assert_that(is_interval(match_win), match_win > ricu:::check_interval(res))
  
  on12 <- paste(meta_vars(res[[1L]]), "==", meta_vars(res[[2L]]))
  on21 <- paste(meta_vars(res[[2L]]), "==", meta_vars(res[[1L]]))
  
  res <- rbind(res[[1L]][res[[2L]], on = on12, roll = match_win],
               res[[2L]][res[[1L]], on = on21, roll = match_win])
  res <- unique(res)
  
  res <- res[is.na(get(cnc[2L])), c(cnc[2L]) := 1]
  res <- res[!is.na(get(cnc[1L])) & !is.na(get(cnc[2L])), ]
  res <- res[, `:=`(c("plt_div_inr"), get(cnc[1L])/get(cnc[2L]))]
  res <- rm_cols(res, cnc)
  res
}

ts_to_win_12hours <- function(x, dur_var, ...) {
  
  x[, c(list(...)$val_var) := NULL]
  x[, c(list(...)$val_var) := TRUE]
  x[, c(dur_var) := NULL]
  x[, c(dur_var) := mins(720L)]
  
  as_win_tbl(x, dur_var = dur_var, by_ref = TRUE)
}

hirid_pharma_win12 <- function(x, dur_var, group_var, ...) {
  
  x[, c(list(...)$val_var) := NULL]
  x[, c(list(...)$val_var) := TRUE]
  x[, c(dur_var) := NULL]
  
  x[, c(dur_var) := max(get(index_var(x))) - min(get(index_var(x))) +
      hours(12L), by = c(group_var)]
  
  x <- x[, head(.SD, n = 1L), by = c(group_var)]
  
  x[, c(dur_var) := `units<-`(get(dur_var), "mins")]
  
  as_win_tbl(x, dur_var = dur_var, by_ref = TRUE)
}

sed_rass <- function(rass, ..., thresh = -2, win_dur = hours(6L)) {
  
  res <- rass[, c("dur_var", "sed_rass", "rass") := list(
    win_dur, rass <= thresh, NULL
  )]
  
  as_win_tbl(res, dur_var = "dur_var", by_ref = TRUE)
}

gcs_cb_generator <- function(name, sed_cncpt = "sed_gcs") {
  
  assert_that(is.string(name), is.string(sed_cncpt))
  
  function(..., valid_win = hours(6L),
           sed_impute = c("none", "verb", "max", "prev"),
           set_na_max = TRUE, interval = NULL) {
    
    zero_to_na <- function(x) replace(x, x == 0, NA_real_)
    
    sed_impute <- match.arg(sed_impute)
    
    if (is.na(sed_cncpt)) {
      assert_that(identical(sed_impute, "none"))
      cnc <- c("egcs", "vgcs", "mgcs", "tgcs")
    } else {
      cnc <- c("egcs", "vgcs", "mgcs", "tgcs", sed_cncpt)
    }
    
    res <- ricu:::collect_dots(cnc, interval, ...)
    
    assert_that(is_interval(valid_win), valid_win > ricu:::check_interval(res),
                is.flag(set_na_max))
    
    sed <- res[[cnc[5L]]]
    res <- ricu:::reduce(merge, res[cnc[-5L]], all = TRUE)
    
    expr <- substitute(list(egcs = fun(egcs), vgcs = fun(vgcs),
                            mgcs = fun(mgcs), tgcs = fun(tgcs)),
                       list(fun = ricu:::locf))
    
    res <- slide(res, !!expr, before = valid_win)
    
    if (identical(sed_impute, "none")) {
      
      cnc <- cnc[-5L]
      
    } else {
      
      sed <- sed[is_true(get(cnc[5L])), ]
      
      if (is_win_tbl(sed)) {
        sed <- expand(sed, aggregate = "any")
      }
      
      res <- merge(res, sed, all.x = TRUE)
    }
    
    if (identical(sed_impute, "max")) {
      
      res <- res[is_true(get(cnc[5L])), c(cnc[4]) := 15]
      
    } else if (identical(sed_impute, "verb")) {
      
      res <- res[is_true(get(cnc[5L])), c(cnc[c(2L, 4L)]) := list(5, NA_real_)]
      
    } else if (identical(sed_impute, "prev")) {
      
      idv <- id_vars(res)
      res <- res[, c(cnc[-5L]) := lapply(.SD, replace_na, 0),
                 .SDcols = cnc[-5L]]
      res <- res[is_true(get(cnc[5L])), c(cnc[-5L]) := NA_real_]
      res <- res[, c(cnc[-5L]) := lapply(.SD, replace_na, type = "locf"),
                 .SDcols = cnc[-5L], by = c(idv)]
      res <- res[, c(cnc[-5L]) := lapply(.SD, zero_to_na), .SDcols = cnc[-5L]]
    }
    
    if (set_na_max) {
      res <- res[, c(cnc[1L:3L]) := Map(replace_na, .SD, c(4, 5, 6)),
                 .SDcols = cnc[1L:3L]]
    }
    
    res <- res[is.na(get(cnc[4L])), c(cnc[4L]) := rowSums(.SD),
               .SDcols = cnc[1L:3L]]
    
    if (set_na_max) {
      res <- res[, c(cnc[4L]) := list(replace_na(get(cnc[4L]), 15))]
    }
    
    res <- rename_cols(res, name, cnc[4L], by_ref = TRUE)
    res <- rm_cols(res, cnc[-4L], by_ref = TRUE)
    
    res
  }
}