#!/usr/bin/env Rscript

## Packages
library("here")
library("data.table")
library("readr")
#library("magrittr")

# Ratings
acq_388     <- fread(here("data/qc-ratings_clean/raw/NACC-338_qc-ratings.csv"))

# CaseIDs
case_ids    <- fread(here("data/raw/nacc/CaseIDs.csv"))

# Merge data
acq_388     <- case_ids[acq_388,
                        on = .(ID = Image),
                        .(Image_fname = ID, Image_case = CaseID,
                          Expert1, Expert2)]
rm(case_ids)

# Write RDS
write_rds(acq_388, here("data/derivatives/acquisition_388_dt.rds"))
