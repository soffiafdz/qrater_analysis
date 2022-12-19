#!/usr/bin/env Rscript

## Packages
library("here")
library("data.table")
library("readr")

## Rater 1
# Read raw data
rater1_1 <- fread(here("data/raw/Acquisition99_v2_all-raters_2022-09-23.csv"))
rater1_2 <- fread(here("data/raw/Acquisition99_v2_louis_2022-10-28.csv"))
rater1_c <- fread(here("data/raw/Acquisition99_v2_louis_consensus.csv"),
                  header = FALSE)

# Remove extra cols
rater1_2[, `:=`(V3 = NULL, V4 = NULL, V5 = NULL)]

# Change colnames
setnames(rater1_1, c("Image", "Rating", "Rater", "Comment", "Time"))
setnames(rater1_2, c("Image", "Rating", "Rater", "Comment", "Time"))
setnames(rater1_c, c("Image", "Rating", "Comment"))

# Assign Session No
rater1_1[, Session := "First"]
rater1_2[, Session := "Second"]
rater1_c[, Session := "Consensus"]

# Join two timepoints
rater1 <- rbindlist(list(rater1_1, rater1_2, rater1_c),
                    use.names = TRUE, fill = TRUE)
rm(rater1_1, rater1_2, rater1_c)

# Rename Rater1
rater1[, Rater := "Rater01"]

# Timing
setorder(rater1, cols = "Time")
rater1[!is.na(Time) & Session == "First", Diff := Time - shift(Time)]
rater1[!is.na(Time) & Session == "Second", Diff := Time - shift(Time)]

## Rater 2-10
# Read raw data
raters2_10 <- fread(here("data/raw/T1_Final_Recoded_all-raters_2022-04-05.csv"))
setnames(raters2_10, c("Image", "Rater", "Rating", "Comment", "Time"))
raters2_10[, Session := NA]

# Check numbers
# raters2_10[, .N, by = Rater]

# Extract rater names
raters2_10_names <- raters2_10[, unique(Rater)]

# Change Rater 4's Warning -> Pass
raters2_10[Rater == "vfonov" & Rating == "Warning", Rating := "Pass"]

# Move vfonov and Mahdiye to the end
new_order <- c(1,2,4,5,7,8,9,3,6)
raters2_10_names <- raters2_10_names[new_order]
rm(new_order)

# Timing
setorder(raters2_10, cols = "Time")
for (i in seq_along(raters2_10_names)) {
    rater <- raters2_10_names[[i]]
    raters2_10[Rater == rater,
        `:=`(
            Rater = sprintf("Rater%02d", i + 1),
            Diff = Time - shift(Time))]
    rm(i, rater)
}
#rm(raters2_10_names)


# Add rater 1
acq_99 <- rbindlist(list(rater1, raters2_10), fill = TRUE)
rm(rater1, raters2_10)

# Leading 0 for Cases
acq_99[, Image := purrr::map_chr(
  stringr::str_split(Image, "_"),
  ~ paste(.[[1]], sprintf("%02d", as.integer(.[[2]])), sep = "_")
)]

# Matrix of comments
setorder(acq_99, cols = "Rater")
acq_99_comments <- acq_99[
  (!Session %in% c("First", "Second")) & Image %in% acq_99[
    Session == "Consensus" & Rating == "Fail", Image],
  .(Image, Rater, Comment)] %>%
    dcast(Image ~ Rater, value.var = "Comment")
#acq_99_comments[1]

## TSV
write_delim(acq_99_comments, here("data/derivatives/acq_99_comments.csv"),
            delim = "\t")

# Write RDS objects
write_rds(acq_99, here("data/derivatives/acquisition_99_dt.rds"))
