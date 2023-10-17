#!/usr/bin/env Rscript

## Packages
library("here")
library("data.table")
library("readr")

# Read raw data
rskull <- fread(here("data/raw/qc_ratings/skull/ADNI_Skull_Experts_2022-09-23.csv"))
setnames(rskull, c("Image", "Rating", "Rater", "Time"))

# Louis: 1746; Mahsa 6968
rskull[Rater == "louis", Rater := "Rater01"]
rskull[Rater == "Mahsa", Rater := "Rater12"]

# Timing
setorder(rskull, cols = "Time")
for (rater in c("Rater01", "Rater12")) {
    rskull[Rater == rater, Diff := Time - shift(Time)]
    rm(rater)
}

# Write RDS object
write_rds(rskull, here("data/derivatives/segmentation_dt.rds"))
