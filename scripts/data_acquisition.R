#!/usr/bin/env Rscript

## Packages
library("here")
library("data.table")
library("readr")

# Read raw data
acquis1 <- fread(here("data/raw/ADNI_Raw_Louis_Ratings_2022-11-23.csv"))
setnames(acquis1, c("Image", "Rater", "Rating", "Comment", "Time"))

acquis <- fread(here("data/raw/Raw_ADNI3_rating_2022-06-29.csv"))
acquis[, V4 := NULL]
setnames(acquis, c("Image", "Rater", "Rating", "Time", "Comment"))

acquis <- rbindlist(list(acquis1, acquis), use.names = TRUE)

# Codify raters
acquis <- acquis[(! Rater %in% c("Mahdiye", "Mahsa")) & Rating != "Pending"]
rater_codes <- sprintf("Rater%02d", 1:8)
rater_usernames <- c("louis", "Sofia", "reza", "Neda", "vmadge",
                     "alexliv", "dandrews.qrater", "estonge")

for (i in seq_along(rater_codes)) {
    acquis[Rater == rater_usernames[[i]], Rater := rater_codes[[i]]]
}

# Timing
setorder(acquis, cols = "Time")
for (rater in acquis[, unique(Rater)]) {
    acquis[Rater == rater, Diff := Time - shift(Time)]
}

# Write RDS object
write_rds(acquis, here("data/derivatives/acquisition_dt.rds"))
