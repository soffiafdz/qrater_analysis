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
fname3 <- here("data/derivatives/qc_time_dt.rds")
if (!file.exists(fname3)) {
  source(here("scripts/timing.R"))
} else {
  sessions <- read_rds(fname3)
}

rm(fname1, fname2, fname3)

## Correlation of Agreement (Count) and Time (hours)
# raw MRI
cor.test(acq_99_count[-1, E01],
         sessions[Rater %like% "Rater" & Dataset == "Training" & Task == "Acquisition",
                  Time], method = "pearson")

# linear registration
cor.test(reg_99_count[-1, E01],
         sessions[Rater %like% "Rater" & Dataset == "Training" & Task == "Registration",
                  Time], method = "pearson")
