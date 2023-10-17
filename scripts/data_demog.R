#!/usr/bin/env Rscript
## Packages
library("here")
library("data.table")
library("stringr")
library("magrittr")
library("lubridate")
library("gtsummary")

### Paths
path_data   <- here("data/raw")
path_qc     <- here("data/qc-ratings_clean/raw/")

### Raw: NACC data
## Demographics
# Load data
nacc_demog  <- fread(here(path_data, "nacc/NACC.csv"),
                     select = c("NACCID", "VISITDAY", "VISITMO", "VISITYR",
                                "SEX", "NACCUDSD"))

# Clean data
nacc_demog  <- nacc_demog[, .(ID = NACCID,
                              SEX = factor(SEX, levels = c(1:2),
                                           labels = c("Male", "Female")),
                              DX = factor(NACCUDSD, levels = c(1:4),
                                          labels = c("Normal Cognition",
                                                     "Impaired-not-MCI",
                                                     "Mild Cognitive Impairment",
                                                     "Dementia")),
                              DATE = ymd(sprintf("%d%02d%02d",
                                                 VISITYR, VISITMO, VISITDAY)))]

## MRI metadata
# Load data
nacc_mri    <- fread(here(path_data, "nacc/NACCmri.csv"),
                     select = c("NACCID", "NACCMRFI",
                                "MRIDY", "MRIMO", "MRIYR",
                                "NACCMRIA", "MRIFIELD"))

# Clean data
nacc_mri    <- nacc_mri[, .(ID        = NACCID,
                            AGE       = NACCMRIA,
                            MRI_ID    = str_extract(NACCMRFI, "^.*(?=.zip)"),
                            MRI_DATE  = ymd(sprintf("%d%02d%02d",
                                                    MRIYR, MRIMO, MRIDY)),
                            TESLA     = factor(MRIFIELD, levels = c(1:2, 5, 7),
                               labels = c("1.5T", "3T", "Other", "Other")))]


## 338 cases
# Load data
nacc_338    <- fread(here(path_qc, "NACC-338_qc-ratings.csv"))
nacc_338    <- nacc_338[, .(MRI_ID = str_extract(Image, "^mri\\d*(?=_)"),
                            MRI_DATE = ymd(str_extract(Image, "\\d{8}")))]

# Merge MRI metadata
nacc_338    <- nacc_mri[nacc_338, on = .(MRI_ID, MRI_DATE)]

# Find and merge subs with unmatched dates
missing     <- nacc_338[is.na(ID), MRI_ID]
nacc_338    <- rbindlist(list(nacc_338[!is.na(ID),
                                       .(ID, MRI_DATE, AGE, TESLA)],
                              nacc_mri[MRI_ID %in% missing,
                                       .(ID, MRI_DATE, AGE, TESLA)]))

# Merge Demographic data
nacc_338    <- nacc_demog[nacc_338, on = "ID"]

# Find closest date of MRI scan to clinical visit
nacc_338[, DATE_DIFF := abs(difftime(DATE, MRI_DATE, units = "days"))]
nacc_338    <- nacc_338[nacc_338[, .I[DATE_DIFF == min(DATE_DIFF)],
                                 .(ID, MRI_DATE)]$V1]

## 99 cases
# Load data
nacc_99     <- fread(here(path_data, "nacc/CaseIDs.csv"), select = "ID")
nacc_99     <- nacc_99[, .(MRI_ID = str_extract(ID, "^mri\\d*(?=_)"),
                           MRI_DATE = ymd(str_extract(ID, "\\d{8}")))]

# Merge MRI metadata
nacc_99     <- nacc_mri[nacc_99, on = .(MRI_ID, MRI_DATE)]

# Find and merge subs with unmatched dates
missing     <- nacc_99[is.na(ID), MRI_ID]
nacc_99     <- rbindlist(list(nacc_99[!is.na(ID),
                                      .(ID, MRI_DATE, AGE, TESLA)],
                              nacc_mri[MRI_ID %in% missing,
                                       .(ID, MRI_DATE, AGE, TESLA)]))

# Merge Demographic data
nacc_99     <- nacc_demog[nacc_99, on = "ID"]

# Find closest date of MRI scan to clinical visit
nacc_99[, DATE_DIFF := abs(difftime(DATE, MRI_DATE, units = "days"))]
nacc_99     <- nacc_99[nacc_99[, .I[DATE_DIFF == min(DATE_DIFF)],
                               .(ID, MRI_DATE)]$V1]
rm(missing, nacc_demog, nacc_mri)

nacc        <- rbindlist(list(nacc_338[, .(AGE, SEX, DX, COHORT = "Initial")],
                              nacc_99[, .(AGE, SEX, DX, COHORT = "Selected")]))

nacc |>
  tbl_summary(by = COHORT,
              label = list(SEX ~ "Sex",
                           AGE ~ "Age (years)",
                           DX ~ "Clinical Label"),
              statistic = all_continuous() ~ "{mean} ({sd})",
              missing_text = "Missing") |>
  modify_header(label ~ "**Raw MRI**\n**NACC**") |>
  #modify_spanning_header(c("stat_1", "stat_2", "stat_3") ~ "**Clinical Label**") |>
  #add_n() |>
  add_p() |>
  as_flex_table() |>
  flextable::save_as_docx(path = "data/derivatives/nacc_demog-table.docx")



### Raw & Linear Registration: ADNI data
## Get VISCODES needed for ADNIMERGE
# MRI metadata
adnimri     <- fread(here(path_data, "adni/MRILIST.csv"),
                     select = c("SUBJECT", "SCANDATE",
                                "SERIESID", "MAGSTRENGTH"),
                     col.names = c("ID", "SCANDATE", "SERIESID", "TESLA"))

# ADNIMERGE (initial pass)
adnimerge   <- fread(here(path_data, "adni/ADNIMERGE.csv"),
                     select = c("PTID", "VISCODE", "EXAMDATE"),
                     col.names = c("ID", "VISCODE", "DATE"))

# 10,196 QCed IDs
adni        <- fread(here(path_qc, "ADNI_qc-ratings.csv"), select = "Image")
adni[, `:=`(ID        = str_extract(Image, "\\d{3}_S_\\d+"),
            SCANDATE  = ymd(str_extract(Image, "\\d{8}")),
            SERIESID  = str_extract(Image, "(?<=_)S\\d+(?=_t1)"))]

# ID of subject without SCANDATE nor SERIESID
sub_missed1 <- adni[is.na(SCANDATE) & is.na(SERIESID), .(ID)]

# List with SERIESID info and FILENAME of original data
sid_dt      <- fread(here(path_data, "adni/raw_simon.csv"), header = F,
                     col.names = c("ID", "SERIESID", "PATH"))
sid_dt[, VISCODE := str_extract(PATH, "(?<=\\.)[^.]+(?=\\.T1)")]

# Data cleaning (using SERIESID)
# Extract VISCODE from FILENAME
adni_sid    <- adni[!is.na(SERIESID)]
adni_sid    <- sid_dt[adni_sid, on = .(ID, SERIESID)]

adni_sid[, SERIESID := as.integer(str_extract(SERIESID, "\\d+"))]
adni_sid    <- unique(adnimri[adni_sid,
                              on = .(ID, SERIESID)])
adni_sid[, SCANDATE := ymd(SCANDATE)]

# Extract SCANDATE for missed subject
sub_missed2 <- adni_sid[is.na(VISCODE), .(ID, SERIESID)]
sub_missed2 <- adnimri[sub_missed2, on = .(ID, SERIESID)]
sub_missed2[, SCANDATE := ymd(SCANDATE)]

# Data cleaning (using closest SCANDATE to VISIT_DATE)
# Add previous missing subject
adni_date   <- rbindlist(list(adni[!is.na(SCANDATE), .(ID, SCANDATE)],
                              sub_missed2[, .(ID, SCANDATE)]))
adni_date   <- adnimerge[adni_date, on = "ID", allow.cartesian = T]
adni_date[, DATE_diff := abs(difftime(DATE, SCANDATE, units = "days"))]
adni_date   <- adni_date[adni_date[order(DATE_diff),
                                   .I[1],
                                   .(ID, SCANDATE)]$V1]
adnimri[, SERIESID := NULL]
adnimri     <- unique(adnimri)
adni_tesla  <- adnimri[!is.na(TESLA)][adni_date, on = .(ID, SCANDATE)]
adni_n      <- adni_tesla[, .N, .(ID, SCANDATE)
                          ][adni_tesla, on = .(ID, SCANDATE)]
adni_n[N > 1, TESLA := 3.0]
adni_tesla  <- unique(adni_n)
adni_tesla[, SCANDATE := ymd(SCANDATE)]

#rm(sub_missed2, adnimri, sid_dt)

# Merge cleaned data
adni_all    <- rbindlist(list(adni_tesla[!is.na(VISCODE),
                                         .(ID, VISCODE, SCANDATE, TESLA)],
                              adni_sid[!is.na(VISCODE),
                                       .(ID, VISCODE, SCANDATE, TESLA)]))
adni_na     <- adni_date[is.na(VISCODE), .(ID, SCANDATE)]
#rm(adni_date, adni_sid)

## Extract demog info
# ADNIMERGE (final pass)
adnimerge   <- fread(here(path_data, "adni/ADNIMERGE.csv"),
                     select = c("PTID", "VISCODE", "DX", "DX_bl", "AGE",
                                "PTGENDER"),
                     col.names = c("ID", "VISCODE", "DX", "DX_bl", "AGE",
                                   "SEX"))

adni_demog  <- adnimerge[adni_all, on = .(ID, VISCODE)]
adni_10196  <- rbindlist(list(adni_demog, adni_na, sub_missed1), fill = T)
adni_10196[TESLA < 2, TESLA := 1.5]
adni_10196[TESLA > 2, TESLA := 3.0]
#rm(adni, adni_demog, adni_na, sub_missed1)

adni_10196[, .(SEX, AGE, DX = factor(DX,
                                     levels = c("CN", "MCI", "Dementia"),
                                     labels = c("Normal Cognition",
                                                "Mild Cognitive Impairment",
                                                "Dementia")))] |>
  tbl_summary(#by = COHORT,
              label = list(SEX ~ "Sex",
                           AGE ~ "Age (years)",
                           DX ~ "Clinical Label"),
              statistic = all_continuous() ~ "{mean} ({sd})",
              missing_text = "Missing") |>
  modify_header(label ~ "**Raw MRI & Lin. Registration**\n**ADNI**") |>
  #modify_spanning_header(c("stat_1", "stat_2", "stat_3") ~ "**Clinical Label**") |>
  #add_n() |>
  #add_p() |>
  as_flex_table() |>
  flextable::save_as_docx(path = "data/derivatives/adni-10k_demog-table.docx")


### Linear Registration: ADNI & HCP & PPMI & PreventAD data
## Cases
reg_cases   <- fread(here(path_data, "list_cases_linreg.csv"))

## ADNI
reg_adni    <- reg_cases[Database == "ADNI",
                         .(ID = Subject, VISCODE = Session)]
reg_adni    <- adni_10196[reg_adni, on = .(ID, VISCODE)]
reg_adni_n  <- reg_adni[, .N, .(ID, VISCODE)][reg_adni, on = .(ID, VISCODE)]
reg_adni_n[N == 2, TESLA := 3.0]
reg_adni_n[, SCANDATE := NULL]
reg_adni    <- unique(reg_adni_n)

reg_adni[, `:=`(DATABASE = "ADNI", VISCODE = NULL, N = NULL)]

# For missing DX, use baseline Diagnosis
# All are m03 & EMCI -> MCI
reg_adni[DX == "", DX := DX_bl]
reg_adni[DX == "EMCI", DX := "MCI"]
reg_adni[, DX_bl := NULL]

## HCP
reg_hcp     <- reg_cases[Database == "HCP",
                         .(ID = as.integer(Subject),
                           #VISCODE = "bl",
                           DX = "CN")]

# Demographic data
hcp_demog   <- fread(here(path_data, "hcp/hcp_demog.csv"),
                     select = c("Subject", "Age", "Gender"),
                     col.names = c("ID", "AGE_bins", "SEX"))

reg_hcp     <- hcp_demog[reg_hcp, on = "ID"]
reg_hcp[, `:=`(DATABASE = "HCP",
               SEX = factor(SEX, levels = c("M", "F"),
                            labels = c("Male", "Female")))]

## PPMI
reg_ppmi    <- reg_cases[Database == "PPMI_3T",
                         .(ID = as.integer(Subject), VISIT = ymd(Session))]

ppmi_demog  <- fread(here(path_data, "ppmi/ppmi_demog.csv"),
                     select = c("ID", "visit", "Cohort", "GENDER", "Age"),
                     col.names = c("ID", "VISIT", "DX", "SEX", "AGE"))
ppmi_demog[, VISIT := ymd(VISIT)]

reg_ppmi    <- ppmi_demog[reg_ppmi, on = .(ID, VISIT),
                          .(ID, DX, AGE,
                            SEX = factor(SEX, levels = c("M", "F"),
                                         labels = c("Male", "Female")),
                            DATABASE = "PPMI")]

## PreventAD
reg_prevad  <- reg_cases[Database == "PreventAD",
                         .(ID_old = as.integer(Subject), VISIT = Session)]
link_prevad <- fread(here(path_data, "preventad/link_imgs.csv"),
                     col.names = c("ID_old", "ID_new"))
reg_prevad  <- link_prevad[reg_prevad, on = "ID_old"]
prevad_demog <- fread(here(path_data, "preventad/preventad_demog.csv"),
                      select = c(3:5, 10),
                      col.names = c("ID_new", "SEX", "AGE_months", "VISIT"))
prevad_demog[, AGE := AGE_months / 12]
reg_prevad  <- prevad_demog[reg_prevad, on = .(ID_new, VISIT),
                            .(ID = ID_old, AGE, SEX, DATABASE = "PREVENT-AD")]
reg_prevad[ID != 977578, DX := "CN"]

## Merge all
reg_99      <- rbindlist(list(reg_adni, reg_hcp, reg_ppmi, reg_prevad),
                         use.names = TRUE, fill = TRUE)

# Age in bins to be consistent with HCP data
reg_99[AGE_bins %in% c("22-25", "26-30"), AGE_bins := "21-30"]
reg_99[AGE_bins == "31-35", AGE_bins := "31-40"]
reg_99[AGE < 51, AGE_bins := "41-50"]
reg_99[AGE > 50 & AGE < 61, AGE_bins := "51-60"]
reg_99[AGE > 60 & AGE < 71, AGE_bins := "61-70"]
reg_99[AGE > 70 & AGE < 81, AGE_bins := "71-80"]
reg_99[AGE > 80 & AGE < 91, AGE_bins := "81-90"]


reg_99[, .(SEX, AGE, DATABASE,
           DX = factor(DX,
                       levels = c("CN", "MCI", "Dementia", "PD"),
                       labels = c("Normal Cognition",
                                  "Mild Cognitive Impairment",
                                  "Dementia", "Parkinson's")))] |>
  tbl_summary(by = DATABASE,
              label = list(SEX ~ "Sex",
                           AGE ~ "Age (years)",
                           DX ~ "Clinical Label"),
              statistic = all_continuous() ~ "{mean} ({sd})",
              missing_text = "Missing") |>
  modify_header(label ~ "**Lin. Registration**\n**Selected Cases**") |>
  #modify_spanning_header(c("stat_1", "stat_2",
                           #"stat_3", "stat_4") ~ "**Database**") |>
  #add_n() |>
  add_p() |>
  as_flex_table() |>
  flextable::save_as_docx(path = "data/derivatives/linreg99_demog-table.docx")

### Skull segmentation
skull_cases <- fread(here(path_qc, "../skull_segmentation/ADNI_qc-ratings.csv"))

skull_cases[, `:=`(ID        = str_extract(Image, "\\d{3}_S_\\d+"),
                   SCANDATE  = ymd(str_extract(Image, "\\d{8}")))]

skull_demog <- unique(adni_10196[skull_cases, on = .(ID, SCANDATE)
                      ][!is.na(SEX), .(ID, SCANDATE, DX, AGE, SEX)])

skull       <- skull_demog[skull_cases, on = .(ID, SCANDATE)]
skull6968   <- skull[, .(SEX, AGE, DX, COHORT = "All")]
skull1746   <- skull[Expert1 != "", .(SEX, AGE, DX,
                                      COHORT = "Subset")]

skull_all   <- rbindlist(list(skull6968, skull1746))
skull_all[, DX := factor(DX,
                       levels = c("CN", "MCI", "Dementia"),
                       labels = c("Normal Cognition",
                                  "Mild Cognitive Impairment",
                                  "Dementia"))]
skull_all |>
  tbl_summary(by = COHORT,
              label = list(SEX ~ "Sex",
                           AGE ~ "Age (years)",
                           DX ~ "Clinical Label"),
              statistic = all_continuous() ~ "{mean} ({sd})",
              missing_text = "Missing") |>
  modify_header(label ~ "**Skull Segmentation**\n**ADNI**") |>
  #modify_spanning_header(c("stat_1", "stat_2",
                           #"stat_3", "stat_4") ~ "**Database**") |>
  #add_n() |>
  add_p() |>
  as_flex_table() |>
  flextable::save_as_docx(path = "data/derivatives/skull_demog-table.docx")


