#!/usr/bin/env Rscript

## Packages
library("here")
library("data.table")
library("readr")

## Agreement data
fname1 <- here("data/derivatives/qc_count_acq_dt.rds")
fname2 <- here("data/derivatives/qc_count_reg_dt.rds")
if (!file.exists(fname1) || !file.exists(fname2)) {
  source(here("scripts/data_acquisition99.R"))
} else {
  acq_99_count <- read_rds(fname1)
  reg_99_count <- read_rds(fname2)
}

# Session
fname3 <- here("data/derivatives/acq_99_time_dt.rds")
fname4 <- here("data/derivatives/reg_99_time_dt.rds")
if (!file.exists(fname3)) {
  source(here("scripts/timing.R"))
} else {
  acq_99_time_dt <- read_rds(fname3)
  reg_99_time_dt <- read_rds(fname4)
}

rm(fname1, fname2, fname3, fname4)

## Correlation of Agreement (Count) and Time

## raw MRI
acq_99 <- acq_99_time_dt[Rater %like% "Rater",
                         .(Total = sum(as.numeric(Diff), na.rm = T),
                           Median = median(as.numeric(Diff), na.rm = T)),
                         Rater]
acq_99[, Agree := acq_99_count[-1, E01]]

acq_99[, cor.test(Agree, Total, method = "spearman")]
acq_99[, cor.test(Agree, Median, method = "spearman")]

# linear registration
reg_99 <- reg_99_time_dt[Rater %like% "Rater",
                         .(Total = sum(as.numeric(Diff), na.rm = T),
                           Median = median(as.numeric(Diff), na.rm = T)),
                         Rater]
reg_99[, Agree := reg_99_count[-1, E01]]

reg_99[, cor.test(Agree, Total, method = "spearman")]
reg_99[, cor.test(Agree, Median, method = "spearman")]
