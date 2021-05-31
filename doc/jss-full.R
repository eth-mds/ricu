## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

library(ricu)
library(data.table)
library(forestmodel)
library(survival)
library(ggplot2)
library(kableExtra)

## ----data-avail, echo = FALSE-------------------------------------------------
srcs  <- c("mimic", "mimic_demo", "eicu", "eicu_demo", "hirid", "aumc")
avail <- is_data_avail(srcs)

srcs_avail <- function(x) all(avail[x])

paste1 <- function(x) {

  x <- paste0("`", x, "`")

  if (length(x) <= 2L) {
    return(paste0(x, collapse = " and "))
  }

  paste0(paste0(head(x, n = length(x) - 2L), collapse = ", "), ", ",
         paste0(tail(x, n = 2L), collapse = " and "))
}

if (!srcs_avail(srcs)) {

  msg <- paste(
    "Note: Examples in this vignette require that datasets",
    paste1(srcs[!avail]), "are available. Chunks that depend on certain",
    "datasets will not be evaluated if the corresponding dataset is missing.",
    "The full version of this vignette is available from",
    "https://CRAN.R-project.org/package=ricu/vignettes/jss.pdf"
  )

  message(paste(strwrap(msg), collapse = "\n"))
}

## ----demo-data, eval = FALSE--------------------------------------------------
#  install.packages(
#    c("mimic.demo", "eicu.demo"),
#    repos = "https://eth-mds.github.io/physionet-demo"
#  )

## ----mimic-tbls, eval = srcs_avail("mimic")-----------------------------------
mimic

## ----eicu-tbls, eval = srcs_avail("eicu")-------------------------------------
eicu

## ----hirid-tbls, eval = srcs_avail("hirid")-----------------------------------
hirid

## ----aumc-tbls, eval = srcs_avail("aumc")-------------------------------------
aumc

## ----src-overview, eval = srcs_avail(c("mimic", "eicu", "hirid", "aumc")), echo = FALSE, results = "asis"----
as_quant <- function(x) {

  if (identical(length(x), 0L)) {
    return("-")
  }

  if (is_id_tbl(x)) {
    x <- data_col(x)
  }

  res <- format_2(quantile(x, probs = seq(0.25, 0.75, 0.25), na.rm = TRUE))

  paste0(res[2L], "\n(", res[1L], " - ", res[3L], ")")
}

big_mark <- function(x) {

  if (identical(length(x), 0L)) {
    return("-")
  }

  formatC(x, big.mark = ",", format = "d")
}

format_2 <- function(x) {
  formatC(x, digits = 2L, format = "f")
}

summarize <- function(src, avail) {

  ids <- as_id_cfg(src)
  cnc <- avail[, src]

  los_icu <- load_concepts("los_icu", src, verbose = FALSE)

  hosp_len <- if ("hadm" %in% names(ids)) {
    load_concepts("los_hosp", src, id_type = "hadm", verbose = FALSE)
  }

  sex <- if ("patient" %in% names(ids)) {
    load_concepts("sex", src, id_type = "patient", verbose = FALSE)
  }

  fil <- list.files(src_data_dir(src), recursive = TRUE, full.names = TRUE)
  siz <- sum(vapply(fil, file.size, numeric(1L))) * 1e-9

  vit <- load_concepts("hr", src, interval = mins(1L), verbose = FALSE)
  vit <- vit[, 1 / diff(as.double(get(index_var(vit)), units = "hours")),
             by = c(id_var(vit))]

  lab <- load_concepts("bili", src, interval = mins(1L), verbose = FALSE)
  lab <- lab[, 1 / diff(as.double(get(index_var(lab)), units = "hours")),
             by = c(id_var(lab))]

  c(`Number of tables` = big_mark(length(as_src_env(src))),
    `Disk storage [GB]` = format_2(siz),
    `Available concepts` = sum(cnc),
    `ICU` = big_mark(nrow(los_icu)),
    `Hospital` = big_mark(nrow(hosp_len)),
    `Unique patients` = big_mark(nrow(sex)),
    `ICU stays` = as_quant(los_icu),
    `Hospital stays` = as_quant(hosp_len),
    `Vital signs\n(heart rate)` = as_quant(vit),
    `Lab tests\n(bilirubin)` = as_quant(lab)
  )
}

srcs <- c("MIMIC", "eICU", "HiRID", "AUMC")

dict <- load_dictionary(tolower(srcs))
avai <- concept_availability(dict, include_rec = FALSE)
summ <- vapply(tolower(srcs), summarize, character(10L), avai)

colnames(summ)     <- srcs
rownames(summ)[3L] <- paste0(rownames(summ)[3L],
                             footnote_marker_symbol(1, "html"))

n_rec_cpt <- nrow(concept_availability(dict, include_rec = TRUE)) -
             nrow(avai)

tbl <- kable(summ, format = "html", escape = FALSE)
tbl <- pack_rows(tbl, "Admission counts", 4, 6)
tbl <- pack_rows(tbl, "Stay lengths [hr]", 7, 8)
tbl <- pack_rows(tbl, "Frequency [1/hr]", 9, 10)
tbl <- footnote(tbl, symbol = paste(
  "These values represent the number of atomic concepts per data source.",
  "Additionally,", n_rec_cpt, "recursive concepts are available, which",
  "build on source-specific atomic concepts in a source-agnostic manner."),
  footnote_as_chunk = TRUE, escape = FALSE
)
kable_styling(tbl)

## ----mimic-adm, eval = srcs_avail("mimic_demo")-------------------------------
mimic_demo$admissions

## ----mimic-sub, eval = srcs_avail("mimic_demo")-------------------------------
subset(mimic_demo$admissions, subject_id > 44000, language:ethnicity)

## ----mimic-tidy, eval = srcs_avail("mimic_demo")------------------------------
subject_id <- 44000:45000
subset(mimic_demo$admissions, .data$subject_id %in% .env$subject_id,
       subject_id:dischtime)

## ----load-src, eval = srcs_avail("mimic_demo")--------------------------------
load_src("admissions", "mimic_demo", subject_id > 44000,
         cols = c("hadm_id", "admittime", "dischtime"))

## ----load-dt, eval = srcs_avail("mimic_demo")---------------------------------
load_difftime("admissions", "mimic_demo", subject_id > 44000,
              cols = c("hadm_id", "admittime", "dischtime"))

## ----load-dt-print, eval = srcs_avail("mimic_demo"), echo = FALSE-------------
load_difftime("admissions", "mimic_demo", subject_id > 44000,
              cols = c("hadm_id", "admittime", "dischtime"))[]

## ----load-id, eval = srcs_avail("mimic_demo")---------------------------------
load_id("admissions", "mimic_demo", subject_id > 44000,
        cols = c("admittime", "dischtime"), id_var = "hadm_id")

## ----load-id-print, eval = srcs_avail("mimic_demo"), echo = FALSE-------------
load_id("admissions", "mimic_demo", subject_id > 44000,
        cols = c("admittime", "dischtime"), id_var = "hadm_id")[]

## ----mimic-cfg----------------------------------------------------------------
cfg <- load_src_cfg("mimic_demo")
str(cfg, max.level = 2L, width = 70L)
mi_cfg <- cfg[["mimic_demo"]]

## ----mimic-ids----------------------------------------------------------------
as_id_cfg(mi_cfg)

## ----mimic-col----------------------------------------------------------------
as_col_cfg(mi_cfg)

## ----mimic-tbl----------------------------------------------------------------
as_tbl_cfg(mi_cfg)

## ---- eval = srcs_avail(c("mimic_demo", "eicu_demo"))-------------------------
load_concepts("hr", c("mimic_demo", "eicu_demo"), verbose = FALSE)

## ----id-tbl-------------------------------------------------------------------
(dat <- ts_tbl(a = 1:5, b = hours(1:5), c = rnorm(5)))
dat[["b"]] <- dat[["b"]] + mins(30)
dat

## ---- eval = srcs_avail(c("mimic_demo", "eicu_demo"))-------------------------
dict <- load_dictionary(c("mimic_demo", "eicu_demo"))
head(dict)
explain_dictionary(head(dict))

## ---- eval = srcs_avail(c("mimic_demo", "eicu_demo"))-------------------------
table(vapply(dict, `[[`, character(1L), "category"))

## ---- eval = srcs_avail("mimic_demo")-----------------------------------------
load_concepts(c("alb", "glu"), "mimic_demo", interval = mins(15L),
              verbose = FALSE)

## ---- eval = srcs_avail("mimic_demo")-----------------------------------------
load_concepts(c("age", "glu"), "mimic_demo", verbose = FALSE)

## ---- eval = srcs_avail("mimic_demo")-----------------------------------------
load_concepts(c("abx", "vent_ind", "norepi_rate", "norepi_dur"),
              "mimic_demo", verbose = FALSE)

## ---- eval = srcs_avail("mimic_demo")-----------------------------------------
load_concepts(c("sirs", "death"), "mimic_demo", verbose = FALSE,
              keep_components = TRUE)

## ----cox-model, eval = srcs_avail("mimic")------------------------------------
src <- "mimic"

cohort <- load_id("icustays", src, dbsource == "metavision",
                  cols = NULL)
cohort <- load_concepts("age", src, patient_ids = cohort,
                        verbose = FALSE)

dat <- load_concepts(c("lact", "death", "sofa", "sex"), src,
                     patient_ids = cohort[age > 25 & age < 65],
                     verbose = FALSE)

dat <- dat[, head(.SD, n = match(TRUE, death, .N)), by = c(id_vars(dat))]
dat <- fill_gaps(dat)

dat <- replace_na(dat, c(NA, FALSE), type = c("locf", "const"),
                  by_ref = TRUE, vars = c("lact", "death"),
                  by = id_vars(dat))

cox_mod <- coxph(
  Surv(charttime - 1L, charttime, death) ~ lact + sofa,
  data = dat
)

## ----cox_plot, eval = srcs_avail("mimic"), echo = FALSE, warning = FALSE, message = FALSE, fig.width = 8, fig.height = 6----
theme_fp <- function(...) {
  theme_bw(...) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.y = element_blank(), axis.title.x = element_blank(),
        axis.text.y = element_blank(), axis.ticks.y = element_blank())
}

forest_model(cox_mod, theme = theme_fp(14))

## ----ins24, eval = srcs_avail("mimic")----------------------------------------
ins_breaks <- c(0, 1, 10, 20, 40, Inf)

ins_cb <- function(ins, ...) {

  day_one <- function(x) x >= hours(0L) & x <= hours(24L)

  idx_var <- index_var(ins)
  ids_var <- id_vars(ins)

  ins <- ins[
    day_one(get(idx_var)), list(ins24 = sum(ins)), by = c(ids_var)
  ]

  ins <- ins[,
    ins24 := list(cut(ins24, breaks = ins_breaks, right = FALSE))
  ]

  ins
}

ins24 <- load_dictionary(src, "ins")
ins24 <- concept("ins24", ins24, "insulin in first 24h", aggregate = "sum",
                 callback = ins_cb, target = "id_tbl", class = "rec_cncpt")

## ----diab, eval = srcs_avail("mimic")-----------------------------------------
grep_diab <- function(x) grepl("^250\\.?[0-9]{2}$", x)

diab  <- item(src, table = "diagnoses_icd",
              callback = transform_fun(grep_diab), class = "col_itm")
diab  <- concept("diab", diab, "diabetes", target = "id_tbl",
                 class = "lgl_cncpt")

dat <- load_concepts(c(ins24, diab), id_type = "icustay", verbose = FALSE)
dat <- replace_na(dat, "[0,1)", vars = "ins24")
dat

## ----diabetes-visualize, echo = FALSE, eval = srcs_avail("mimic"), fig.width = 8, fig.height = 6----
dat <- dat[, weight := 1 / .N, by = diab]
ggplot(dat, aes(x = ins24, fill = diab)) +
  stat_count(aes(weight = weight), alpha = 0.75, position = "dodge") +
  labs(x = "Amount of administered insulin in first 24h of ICU stay [units]",
       y = "Proportion of patients",
       fill = "Diabetic") +
  theme_bw(14)

## ----session-info-------------------------------------------------------------
sessionInfo()

