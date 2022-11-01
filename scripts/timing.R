#!/usr/bin/env Rscript

## Packages
library("here")
library("data.table")
library("readr")
library("lubridate")
library("ggplot2")

## Acquisition 99 / ADNI
# Data.table 99
fnames <- c("acquisition_99_dt.rds", "rater1_dt.rds")
fpaths <- here("data/derivatives", fnames)

if (!file.exists(fpaths[[1]])) {
  source(here("scripts/data_acquisition99.R"))
} else {
  acq_99 <- read_rds(fpaths[[1]])
  rater1 <- read_rds(fpaths[[2]])
}

rm(fnames, fpaths)

acq_99 <- acq_99[!Rater == "Rater01", .(Image, Rater, Rating, Diff)]
trainees <- acq_99[Diff < duration(15, "minutes"),
  .(.N, Time = Diff, Dataset = "Training"), by = .(Rater, Rating)]

expert <- rater1[Rater == "Rater01_1", Rater := "Rater01"][
  Diff < duration(15, "minutes") & Rater == "Rater01",
  .(.N, Time = Diff, Dataset = "Training Dataset"),
  by = .(Rater, Rating)]

time_99 <- rbindlist(list(expert, trainees))

ggplot(time_99, aes(x = Rating, y = Time, fill = Rating)) +
  geom_boxplot() +
  facet_wrap(. ~ Rater, scales = "free", nrow = 10, ) +
  coord_flip() +
  labs(title = "Training Dataset", y = "Time (s)") +
  scale_fill_manual(values = c("darkred", "darkgreen")) +
  theme_linedraw() +
  theme(text = element_text(size = 16),
    legend.position = "none")

ggsave(here("plots/time_99.png"), width = 7, height = 14)


# Data.table ADNI
fpath <- here("data/derivatives/acquisition_dt.rds")

if (!file.exists(fpath)) {
  source(here("scripts/data_acquisition.R"))
} else {
  acquis <- read_rds(fpath)
}

rm(fpath)

acquis$Rating <- factor(acquis$Rating, levels = c("Fail", "Warning", "Pass"))
adni <- acquis[,
  .(.N, Time = Diff, Dataset = "Full Dataset"),
  by = .(Rater, Rating)]

# Table
setorder(acq_99, Rater)
acq_99 <- acq_99[!Rater == "Rater01", .(Image, Rater, Rating, Diff)]
trainees_99_prop <- acq_99[, .N, by = .(Rater, Rating)]
trainees_99_time <- acq_99[Diff < duration(15, "minutes"),
  .(.N, Time = mean(Diff), SD = format(sd(Diff), scientific = FALSE)),
  by = .(Rater, Rating)]
trainees_99 <- trainees_99_prop[trainees_99_time, on = .(Rater, Rating)]

expert_99_prop <- rater1[Rater == "Rater01", .N, by = .(Rater, Rating)]
expert_99_time <- rater1[Diff < duration(15, "minutes") & Rater == "Rater01",
  .(.N, Time = mean(Diff), SD = format(sd(Diff), scientific = FALSE)),
  by = .(Rater, Rating)]
expert_99 <- expert_99_prop[expert_99_time, on = .(Rater, Rating)]

time_99 <- rbindlist(list(expert_99, trainees_99))
write_csv(time_99, here("data/derivatives/timing_99.csv"))

setorder(acquis, Rater)
table_adni_prop <- acquis[, .N, by = .(Rater, Rating)]
table_adni_time <- acquis[Diff < duration(15, "minutes"),
  .(.N, Time = mean(Diff), SD = format(sd(Diff), scientific = FALSE)),
  by = .(Rater, Rating)]

table_adni <- table_adni_prop[table_adni_time, on = .(Rater, Rating)]
write_csv(table_adni, here("data/derivatives/timing_adni.csv"))

# Plot
ggplot(adni, aes(x = Rating, y = Time, fill = Rating)) +
  geom_boxplot() +
  facet_wrap(. ~ Rater, scales = "free", nrow = 7, ) +
  coord_flip() +
  scale_fill_manual(values = c("darkred", "yellow", "darkgreen")) +
  labs(title = "Full Dataset", y = "Time (s)") +
  theme_linedraw() +
  theme(text = element_text(size = 16),
    legend.position = "top")

ggsave(here("plots/time_adni.png"), width = 7, height = 14)


## Registration and Segmentation
# Data.table
fnames <- c("registration_dt.rds", "segmentation_dt.rds")
fpaths <- here("data/derivatives", fnames)

if (!file.exists(fpaths[[1]])) {
  source(here("scripts/data_registration.R"))
} else {
  regist <- read_rds(fpaths[[1]])
}

if (!file.exists(fpaths[[2]])) {
  source(here("scripts/data_segmentation.R"))
} else {
  rskull <- read_rds(fpaths[[2]])
}

rm(fnames, fpaths)

# Plot
q25 <- quantile(rater1[Rater == "Rater01", Diff], 0.25, na.rm = TRUE)
q75 <- quantile(rater1[Rater == "Rater01", Diff], 0.75, na.rm = TRUE)
set1 <- rater1[Rater == "Rater01" , .(Rater, Rating, Diff, Dataset = "Acquisition")]
set1_r <- set1[Diff < q75 + 3 * (q75 - q25)]

q25 <- quantile(regist[, Diff], 0.25, na.rm = TRUE)
q75 <- quantile(regist[, Diff], 0.75, na.rm = TRUE)
set2 <- regist[, .(Rater, Rating, Diff, Dataset = "Registration")]
set2_r <- set2[Diff < q75 + 3 * (q75 - q25)]

q25 <- quantile(rskull[, Diff], 0.25, na.rm = TRUE)
q75 <- quantile(rskull[, Diff], 0.75, na.rm = TRUE)
set3 <- rskull[, .(Rater, Rating, Diff, Dataset = "Segmentation")]
set3_r <- set3[Diff < q75 + 3 * (q75 - q25)]

datasets <- rbindlist(list(set1_r, set2_r, set3_r))
ggplot(datasets,
  aes(x = Rater, y = Diff)) +
  geom_violin(aes(fill = Rating), trim = F, alpha = .5) +
  geom_boxplot(aes(colour = Rating), position = position_dodge(width = 0.9),
               width = 0.2, outlier.shape = NA, alpha = .5) +
  facet_wrap(. ~ Dataset, scales = "free", nrow = 1, ) +
  labs(y = "Time (s)") +
  scale_fill_manual(values = c("darkred", "darkgreen", "yellow")) +
  scale_colour_manual(values = c("darkred", "darkgreen", "yellow4")) +
theme_linedraw() +
theme(text = element_text(size = 18))
ggsave(here("plots/time_experts.png"), width = 8, height = 5)
