#!/usr/bin/env Rscript

## Packages
library("here")
library("data.table")
library("readr")
library("purrr")
library("stringr")

# Read raw data
#rater1 <- fread(here("data/raw/QC_Training_Louis_all-raters_2022-08-21.csv"))
rater1_1 <- fread(here("data/raw/ADNI3_99_LinReg_v2_all-raters_2022-09-23.csv"))
rater1_2 <- fread(here("data/raw/ADNI3_99_LinReg_v2_louis_2022-10-12.csv"))
rater1_c <- fread(here("data/raw/ADNI3_99_LinReg_v2_louis_consensus.csv"),
                  header = FALSE)
ratings <- fread(here("data/raw/ADNI3_99_LinReg_all-raters_2022-10-14.csv"))
history <- fread(here("data/raw/ADNI3_99_LinReg_all-raters_History_2022-10-14.csv"))

# Remove unused cols
rater1_2[, `:=`(V3 = NULL, V4 = NULL, V5 = NULL)]

# Change colnames
setnames(rater1_1, c("Image", "Rating", "Rater", "Comment", "Timestamp"))
setnames(rater1_2, c("Image", "Rating", "Rater", "Comment", "Timestamp"))
setnames(rater1_c, c("Image", "Rating", "Comment"))
setnames(ratings, c("Image", "Rating", "Rater", "Comment", "Timestamp"))
setnames(history, c("Image", "Rater", "Rating", "Comment", "Timestamp"))

# Merge history and ratings (and remove duplicate rows)
raters2_9 <- unique(rbindlist(list(ratings, history), use.names = TRUE))

# Remove duplicate rating for Case_33:Reza
# Ground-truth is Fail (Checked current rating on Qrater)
raters2_9 <- raters2_9[!(Image == 'Case_33' & Rater == 'reza' & Rating == 'Pass')]


# Extract most recent & non-Pending or Warning ratings from history
raters2_9 <- raters2_9[raters2_9[!Rating %in% c("Pending", "Warning"),
                             .I[Timestamp == max(Timestamp)],
                             by = .(Image, Rater)]$V1]

# Mark passing session
rater1_1[, Session := "First"]
rater1_2[, Session := "Second"]
rater1_c[, `:=`(Rater = "louis", Session = "Consensus")]
raters2_9[, Session := NA]

# Check numbers
#raters2_9[, .N, by = Rater]

# Add rater 1
reg_99 <- rbindlist(list(rater1_1, rater1_2, rater1_c, raters2_9),
                    use.names = TRUE, fill = TRUE)
rm(rater1_1, rater1_2, rater1_c, raters2_9)

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
    #print(rater)
    reg_99[Rater == rater,
        `:=`(
            Rater = sprintf("Rater%02d", i),
            Diff = Timestamp - shift(Timestamp))]
}
rm(i, rater)
rm(raters_names)

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
write_delim(reg_99_wide, here("data/derivatives/reg_99_wide.csv"),
            delim = "\t")


# Write RDS objects
write_rds(reg_99, here("data/derivatives/registration_99_dt.rds"))
