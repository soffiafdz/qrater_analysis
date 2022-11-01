#!/usr/bin/env Rscript

## Packages
library("here")
library("data.table")
library("stringr")
library("purrr")
library("lubridate")
library("ggplot2")
library("kableExtra")

## Raw T1 ADNI Data
# Louis: 1275
raw1 <- fread(here("data/raw/Rater1_Raw_all-raters_2021-12-16.csv"))
setnames(raw1, c("Image", "Rater", "Rating",  "Comment", "Time"))
raw1[, .N, by = Rater]

# Add time difference with previous row
raw1_louis <- raw1[Rater == "louis"]
setorder(raw1_louis, cols = "Time")
raw1_louis[, Diff := Time - shift(Time)]


# N and mean of duration higher than 30 minutes
raw1_louis[Diff > duration(60, "minutes"), Diff]
raw1_louis[Diff > duration(60, "minutes"), median(Diff)]

# Duration by rating
raw1_louis[Diff < duration(60, "minutes"),
           .(.N, Time = mean(Diff), Dataset = "Acquisition_1"),
           by = Rating]

set1 <- raw1_louis[Diff < duration(60, "minutes"),
                   .(.N, Time = Diff, Dataset = "Acquisition_1"),
                   by = Rating]
raw1_louis[Diff < duration(60, "minutes"), mean(Diff)]


# Ana: 1623
raw2 <- fread(here("data/raw/NACC_T1_all-raters_2021-12-15.csv"))
setnames(raw2, c("Image", "Rater", "Rating", "Time"))

raw2[, .N, by = Rater]

# Add time difference with previous row
raw2_ana <- raw2[Rater == "anamanera"]
setorder(raw2_ana, cols = "Time")
raw2_ana[, Diff := Time - shift(Time)]


# N and mean of duration higher than 30 minutes
raw2_ana[Diff > duration(60, "minutes"), Diff]
raw2_ana[Diff > duration(60, "minutes"), median(Diff)]

# Duration by rating
raw2_ana[Diff < duration(60, "minutes"),
         .(.N, Time = mean(Diff), Dataset = "Acquisition_2"),
         by = Rating]

set2 <- raw2_ana[Diff < duration(60, "minutes"),
                 .(.N, Time = Diff, Dataset = "Acquisition_2"),
                 by = Rating]

## Registration
# Cass: 3K
reg1 <- fread(here("data/raw/adni_wmh_all-raters_2021-12-15.csv"))
setnames(reg1, c("Image", "Rater", "Rating", "Time"))

# Cassandra: 3k
reg1[, .N, by = Rater]

# Add time difference with previous row
reg1_cass <- reg1[Rater == "Cassandra"]
setorder(reg1_cass, cols = "Time")
reg1_cass[, Diff := Time - shift(Time)]

# N and mean of duration higher than 30 minutes
reg1_cass[Diff > duration(60, "minutes"), .N]
reg1_cass[Diff > duration(60, "minutes"), median(Diff)]

# Duration by rating
reg1_cass[Diff < duration(60, "minutes"),
         .(.N, Time = mean(Diff), Dataset = "Registration"),
         by = Rating]

set3 <- reg1_cass[Diff < duration(60, "minutes"),
                 .(.N, Time = Diff, Dataset = "Registration"),
                 by = Rating]

## Inter/intra - rater agreement
# 99 images
adni3_all <- fread(here("data/raw/QC_Raw_Images.csv"))
adni3_codes <- adni3_all[, .(ID, Image = CaseID)]

adni3_rater1_1 <- fread(here("data/raw/T1_Louis_all-raters_2022-07-04.csv"))
setnames(adni3_rater1_1, c("ID", "Rater", "Rating", "Comment", "Time"))
adni3_rater1_1 <- adni3_rater1_1[Rating != "Pending"]
adni3_rater1_1[, Rater := "Rater01_1"]

adni3_rater1_2 <- fread(here("data/raw/T1_Louis_99_all-raters_2022-07-04.csv"))
setnames(adni3_rater1_2, c("ID", "Rater", "Rating", "Comment", "Time"))
adni3_rater1_2 <- adni3_rater1_2[Rating != "Pending"][Rating != "Warning"]
adni3_rater1_2[, Rater := "Rater01_2"]

# Just the same 99 images of both datasets
adni3_rater1_1 <- adni3_rater1_1[ID %in% adni3_rater1_2[, ID]]

# Extract codes from adni3_all
adni3_rater1_1 <- adni3_codes[adni3_rater1_1, on = "ID"]
adni3_rater1_2 <- adni3_codes[adni3_rater1_2, on = "ID"]

# Missing Case_{22, 29, 39, 73, 89}
missing_cases <- paste("Case", c(22, 29, 39, 73, 89), sep = "_")
missing_cases_dt <- adni3_all[,
  .(ID, Rater01_1 = QC, Image = CaseID, Rater01_2 = QC_Louis)][
  Image %in% missing_cases]
missing_cases_dt <- melt(missing_cases_dt, id.vars = c("ID", "Image"),
  variable.name = "Rater", value.name = "Rating")

adni3_rater1 <- rbindlist(
  list(adni3_rater1_1, adni3_rater1_2, missing_cases_dt), fill = TRUE)

## Timing
setorder(adni3_rater1, cols = "Time")
for (i in 1:2) {
  adni3_rater1[
    !is.na(Time) & Rater == paste("Rater01", i, sep = "_"),
    Diff := Time - shift(Time)]
}

# Raters 2-10
adni3_99 <- fread(here("data/raw/T1_Final_Recoded_all-raters_2022-04-05.csv"))
setnames(adni3_99, c("Image", "Rater", "Rating", "Comment", "Time"))

# Check numbers and extract raters
adni3_99[, .N, by = Rater]
adni3_99_raters <- adni3_99[, unique(Rater)]

# Duration by rating
setorder(adni3_99, cols = "Time")
for (i in seq_along(adni3_99_raters)) {
  rater <- adni3_99_raters[[i]]
  adni3_99[Rater == rater,
           `:=`(Rater = sprintf("Rater%02d", i + 1), Diff = Time - shift(Time))]
}

# Change Rater 4's Warning -> Pass
adni3_99[Rater == "Rater04" & Rating == "Warning", Rating := "Pass"]

# Add rater 1
adni3_99 <- adni3_codes[adni3_99, on = "Image"]
adni3 <- rbindlist(list(adni3_rater1, adni3_99))

## Timing
adni3_time <- adni3[!is.na(Diff),
.(ID, Rater = as.character(Rater), Rating, Diff)]

# N and mean of duration higher than 60 minutes
adni3_time[Diff > duration(60, "minutes"), .N, by = Rater]
adni3_time[Diff > duration(60, "minutes"), median(Diff), by = Rater]

# Duration by rating
adni3_time[Diff < duration(60, "minutes"),
         .(.N, Time = mean(Diff), Dataset = "Acquisition_99"),
         by = .(Rater, Rating)]

set4 <- adni3_time[Diff < duration(60, "minutes"),
         .(.N, Time = Diff, Dataset = "Acquisition_99"),
         by = .(Rater, Rating)]

## Agreement
adni3_agree <- adni3[!is.na(Image), .(Image, Rater, Rating)]
adni3_agree[, Image := map_chr(
  str_split(Image, "_"),
  ~ paste(.[[1]], sprintf("%02d", as.integer(.[[2]])), sep = "_")
)]
setorder(adni3_agree, cols = "Rater")
adni3_agree_bin <- copy(adni3_agree)
adni3_agree_bin[
  Rating == "Pass", Rating := 1][
  Rating == "Fail", Rating := 0][
  , Rating := as.integer(Rating)]
adni3_agree_wide <- dcast(adni3_agree_bin, Image ~ Rater, value.var = "Rating")
adni3_agree_matrix <- adni3_agree_wide[, -c(1)]

adni3_kappas <- vector("list", length(adni3_agree_matrix))
adni3_count <- vector("list", length(adni3_agree_matrix))
for (i in seq_along(adni3_agree_matrix)) {
  adni3_kappas[[i]] <- vector("list", length(adni3_agree_matrix))
  adni3_count[[i]] <- vector("list", length(adni3_agree_matrix))
  for (j in seq_along(adni3_agree_matrix)) {
    cols <- names(adni3_agree_matrix)[c(i, j)]
    adni3_kappas[[i]][[j]] <- irr::kappa2(adni3_agree_matrix[, ..cols])$value
    adni3_count[[i]][[j]] <- sum(
      adni3_agree_matrix[[i]] == adni3_agree_matrix[[j]])
  }
}
adni3_kappas <- setDT(lapply(adni3_kappas, unlist))
names(adni3_kappas) <- names(adni3_agree_matrix)

adni3_count <- setDT(lapply(adni3_count, unlist))
names(adni3_count) <- names(adni3_agree_matrix)

## Plots

# Three datasets
datasets <- rbindlist(list(set1, set2, set3))
ggplot(datasets[Time < 180], aes(x = Dataset, y = Time, fill = Rating)) +
  # geom_jitter(width = 0.15, shape = 21,
  #             size = 0.2, stroke = 1, alpha = 0.3) +
  geom_boxplot(outlier.shape = NA, alpha = 0.3) +
  # facet_grid(cols = vars(Dataset), scales = "free") +
  labs(y = "Time (s)", title = "Rating time per image done by experts",
       caption = "Time spent on a single image before rating it.") +
theme_bw() +
theme(text = element_text(size = 16),
  plot.caption = element_text(hjust = 0.5),
  legend.position = "bottom")

ggsave(here("plots/time_rating.png"), width = 10, height = 12)

# Ridges
# p <-
# ggplot(adni3_time[Diff < 3600], aes(x = Diff, y = Rater)) +
#   stat_density_ridges(quantile_lines = TRUE,jittered_points = TRUE,
#   position = position_points_jitter(width = 0.05, height = 0),
#   point_shape = "|", point_size = 3, point_alpha = 1) +
#   facet_grid(cols = vars(Rating)) +
#   labs(x = "Time (s)", title = "Rating time per image done by trainees",
#     caption = paste("Time spent on a single image before rating it.",
#     "Lines in the density plots denote Q1, Q2 (median) and Q3.", sep = " ")) +
#   theme_linedraw()

ggplot(adni3_time[Diff < 4000], aes(x = Rater, y = Diff, fill = Rating)) +
  # geom_jitter(width = 0.1, shape = 21,
  #             size = 0.3, stroke = 1, alpha = 0.5) +
  geom_boxplot(outlier.shape = NA, alpha = 0.3) +
  ylim(c(0, 600)) +
  coord_flip() +
  # facet_grid(cols = vars(Rating), scales = "free") +
  labs(y = "Time (s)", title = "Rating time per image done by trainees",
       caption = "Time spent on a single image before rating it.") +
theme_bw() +
theme(text = element_text(size = 16),
  plot.caption = element_text(hjust = 0.5),
  legend.position = "bottom")

ggsave(here("plots/time_rating2.png"), width = 12, height = 10)

## Heatmap
rater_id <- names(adni3_count)
rater_id <- str_replace(rater_id, "Rater", "R")
adni3_count[, Rater := rater_id]
adni3_count_melt <- melt(adni3_count, id.vars = "Rater",
  variable.factor = FALSE)

ggplot(adni3_count_melt, aes(x = Rater, y = variable)) +
geom_tile(aes(fill = value)) +
geom_text(aes(label = value)) +
scale_fill_gradient(low = "white", high = "red") +
labs(y = "Rater", fill = "Agreement",
  title = "Intra-/Inter- Rater Agreement",
  caption = "Number of images (out of 99) rated in agreement between raters.") +
theme_bw() +
theme(text = element_text(size = 16),
  plot.caption = element_text(hjust = 0.5),
  legend.position = "bottom")

ggsave(here("plots/time_heatmap.png"), width = 12, height = 10)

## Pie plots
ggplot(
  adni3_agree[Rater != "Rater01_2", .N, by = .(Image, Rating)],
  aes(x = "", y = N, fill = Rating)) +
  geom_col() + coord_polar(theta = "y") +
  scale_fill_manual(values = c("red", "green")) +
  facet_wrap(vars(Image), ncol = 10) +
  theme_void() +
  theme(strip.text.x = element_blank())

# Number of images with full agreement
adni3_agree[
  Rater != "Rater01_2", .N, by = .(Image, Rating)][
  N == 10, .N, by = Rating]

adni3_agree[
  Rater != "Rater01_2", .N, by = .(Image, Rating)][
  N > 6, .N, by = Rating]
