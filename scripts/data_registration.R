#!/usr/bin/env Rscript

## Packages
library("here")
library("data.table")
library("readr")

# Read raw data
regist <- fread(here("data/raw/adni_wmh_all-raters_2021-12-15.csv"))
setnames(regist, c("Image", "Rater", "Rating", "Time"))

# Extract single rater
regist <- regist[Rater == "Cassandra"]
regist[, Rater := "Rater11"]

# Timing
setorder(regist, cols = "Time")
regist[, Diff := Time - shift(Time)]

# Write RDS objects
write_rds(regist, here("data/derivatives/registration_dt.rds"))