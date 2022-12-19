#!/usr/bin/env Rscript

## Packages
library("here")
library("data.table")
library("readr")
library("stringr")
library("magrittr")

# Read raw data

regis <- here("data/raw") %>%
  list.files(full.names = TRUE) %>%
  str_subset("ADNI_Linear_\\S*_Ratings") %>%
  lapply(fread) %>%
  rbindlist

# Remove comments [Why?]
#regis[, V4 := NULL]
setnames(regis, c("Image", "Rater", "Rating", "Comment", "Timestamp"))

# Remove pending rating (N=1)
regis <- regis[Rating != "Pending"]

# Remove Rater01
regis <- regis[Rater != "louis"]

# Renaming
raters_names <- regis[, unique(Rater)]

# Same order as acquisition:
# Sof, reza, neda, vmadge, alex, dandrews, estonge, vfonov
new_order <- c(6, 5, 4, 7, 1, 2, 3, 8)
raters_names <- raters_names[new_order]
rm(new_order)

# Timing
setorder(regis, cols = "Timestamp")
for (i in seq_along(raters_names)) {
  rater <- raters_names[[i]]
  print(rater)
  regis[Rater == rater, `:=`(Rater = sprintf("Rater%02d", i + 1),
                             Diff = Timestamp - shift(Timestamp))]
}
rm(i, rater)
rm(raters_names)

# Write RDS
write_rds(regis, here("data/derivatives/registration_dt.rds"))
