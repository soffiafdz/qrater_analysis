#!/usr/bin/env Rscript

## Packages
library("here")
library("data.table")
library("readr")
library("magrittr")

## Rater 1
# Read raw data
expert_1 <- fread(here("data/raw/Acquisition99_v2_all-raters_2022-09-23.csv"))
expert_2 <- fread(here("data/raw/Acquisition99_v2_louis_2022-10-28.csv"))
expert_c <- fread(here("data/raw/Acquisition99_v2_louis_consensus.csv"),
                  header = FALSE)

# Remove extra cols
expert_2[, `:=`(V3 = NULL, V4 = NULL, V5 = NULL)]

# Change colnames
setnames(expert_1, c("Image", "Rating", "Rater", "Comment", "Time"))
setnames(expert_2, c("Image", "Rating", "Rater", "Comment", "Time"))
setnames(expert_c, c("Image", "Rating", "Comment"))

# Assign Session No
expert_1[, Session := "First"]
expert_2[, Session := "Second"]
expert_c[, Session := "Consensus"]

# Join two timepoints
expert <- rbindlist(list(expert_1, expert_2, expert_c),
                    use.names = TRUE, fill = TRUE)
rm(expert_1, expert_2, expert_c)

# Rename Expert
expert[, Rater := "Expert01"]

# Timing
setorder(expert, cols = "Time")
expert[!is.na(Time) & Session == "First", Diff := Time - shift(Time)]
expert[!is.na(Time) & Session == "Second", Diff := Time - shift(Time)]

## Rater 2-10
# Read raw data
trainees <- fread(here("data/raw/T1_Final_Recoded_all-raters_2022-04-05.csv"))
setnames(trainees, c("Image", "Rater", "Rating", "Comment", "Time"))
trainees[, Session := NA]

# Check numbers
# trainees[, .N, by = Rater]

# Extract rater names
trainees_names <- trainees[, unique(Rater)]

# Change Rater 4's Warning -> Pass
trainees[Rater == "vfonov" & Rating == "Warning", Rating := "Pass"]

# Move vfonov and Mahdiye to the end
new_order <- c(1,2,4,5,7,8,9,3,6)
trainees_names <- trainees_names[new_order]
rm(new_order)

# Timing
setorder(trainees, cols = "Time")
for (i in seq_along(trainees_names)) {
    rater <- trainees_names[[i]]
    trainees[Rater == rater,
             `:=`(Rater = sprintf("Rater%02d", i),
                  Diff = Time - shift(Time))]
    rm(i, rater)
}
#rm(trainees_names)


# Add rater 1
acq_99 <- rbindlist(list(expert, trainees), fill = TRUE)
rm(expert, trainees)

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
