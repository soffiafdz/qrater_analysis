#!/usr/bin/env Rscript

## Packages
library("here")
library("data.table")
library("readr")
library("purrr")
library("stringr")

# Read raw data
path     <- "data/raw/qc_ratings/registration/balanced_data"
expert_1 <- fread(here(path, "MULTI_99_Linear_Expert_all-raters_2022-09-23.csv"))
expert_2 <- fread(here(path, "MULTI_99_Linear_Expert_all-raters_2022-10-12.csv"))
expert_c <- fread(here(path, "MULTI_99_Linear_Expert_consensus.csv"),
                  header = FALSE)
ratings <- fread(here(path, "MULTI_99_Linear_all-raters_2022-10-14.csv"))
history <- fread(here(path, "MULTI_99_Linear_all-raters_History_2022-10-14.csv"))

# Remove unused cols
expert_2[, `:=`(V3 = NULL, V4 = NULL, V5 = NULL)]

# Change colnames
setnames(expert_1, c("Image", "Rating", "Rater", "Comment", "Timestamp"))
setnames(expert_2, c("Image", "Rating", "Rater", "Comment", "Timestamp"))
setnames(expert_c, c("Image", "Rating", "Comment"))
setnames(ratings, c("Image", "Rating", "Rater", "Comment", "Timestamp"))
setnames(history, c("Image", "Rater", "Rating", "Comment", "Timestamp"))

# Merge history and ratings (and remove duplicate rows)
trainees <- unique(rbindlist(list(ratings, history), use.names = TRUE))

# Remove duplicate rating for Case_33:Reza
# Ground-truth is Fail (Checked current rating on Qrater)
trainees <- trainees[!(Image == 'Case_33' & Rater == 'reza' & Rating == 'Pass')]


# Extract most recent & non-Pending or Warning ratings from history
trainees <- trainees[trainees[!Rating %in% c("Pending", "Warning"),
                              .I[Timestamp == max(Timestamp)],
                              by = .(Image, Rater)]$V1]

# Mark passing session
expert_1[, Session := "First"]
expert_2[, Session := "Second"]
expert_c[, `:=`(Rater = "louis", Session = "Consensus")]
trainees[, Session := NA]

# Check numbers
#trainees[, .N, by = Rater]

# Add rater 1
reg_99 <- rbindlist(list(expert_1, expert_2, expert_c, trainees),
                    use.names = TRUE, fill = TRUE)
rm(expert_1, expert_2, expert_c, trainees)

# Extract rater names and change them
raters_names <- reg_99[, unique(Rater)]

# Same order as acquisition:
# Sof, reza, neda, vmadge, alex, dandrews, estonge, vfonov
new_order <- c(1, 9, 4, 6, 8, 3, 5, 7, 2)
raters_names <- raters_names[new_order]
rm(new_order)

# Timing
setorder(reg_99, cols = "Timestamp")
for (i in seq_along(raters_names)) {
    rater <- raters_names[[i]]
    reg_99[Rater == rater,
        `:=`(
            Rater = fifelse(i == 1, "Expert01", sprintf("Rater%02d", i - 1)),
            Diff = Timestamp - shift(Timestamp))]
}
rm(i, rater)
rm(raters_names)

# Load original QC
case_ids <- here("data/raw/list_cases_linreg.csv") |>
  fread(select = c("ID", "QC"), col.names = c("Image", "Orig_QC"))

reg_99 <- case_ids[reg_99, on = "Image"]
rm(case_ids)

# Leading 0 for Cases
reg_99[, Image := map_chr(
  str_split(Image, "_"),
  ~ paste(.[[1]], sprintf("%03d", as.integer(.[[2]])), sep = "_")
)]

# Matrix of comments
setorder(reg_99, cols = "Rater")
reg_99_comments <- reg_99[
  (!Session %in% c("First", "Second")) & Image %in% reg_99[
    Session == "Consensus" & Rating == "Fail", Image],
  .(Image, Rater, Comment)] %>%
    dcast(Image ~ Rater, value.var = "Comment")
reg_99_comments[1]


# Write CSV file
#write_csv(reg_99, here("data/derivatives/registration_99.csv"))
write_delim(reg_99_comments, here("data/derivatives/reg_99_comments.csv"),
            delim = "\t")
#write_delim(reg_99_wide, here("data/derivatives/reg_99_wide.csv"),
            #delim = "\t")


# Write RDS objects
write_rds(reg_99, here("data/derivatives/registration_99_dt.rds"))
