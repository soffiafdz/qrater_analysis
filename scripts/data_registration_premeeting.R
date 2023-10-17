#!/usr/bin/env Rscript

## Packages
library("here")
library("data.table")
library("readr")
library("stringr")
library("magrittr")
library("lubridate")

# Read raw data

regis_list <- here("data/raw/qc_ratings/registration/pre_meeting") %>%
  list.files(full.names = TRUE) %>%
  str_subset("ADNI_Linear_\\S*_history_2023-08-03") %>%
  lapply(fread)

# Raters - Alphabetical order:
# Alex, Daniel, Etienne, Neda, Reza, Sofia, Victoria, Vlad
# Raters - Consistency order:
# Sofia, Reza, Neda, Victoria, Alex, Daniel, Etienne, Vlad
raters_order <- c(5:7, 3:1, 4, 8)
for (i in seq_along(regis_list)) {
  regis_list[[i]][, V4 := sprintf("Rater%02d", raters_order[i])]
}
rm(raters_order)

regis_pre <- rbindlist(regis_list)
setnames(regis_pre, c("Image", "Rating", "Timestamp", "Rater"))
regis_pre <- regis_pre[regis_pre[Timestamp < ymd(20221020),
                                 .I[Timestamp == max(Timestamp)],
                                 .(Image, Rater)]$V1]
rm(regis_list)

write_rds(regis_pre, here("data/derivatives/registration_premeeting_dt.rds"))
