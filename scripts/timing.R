#!/usr/bin/env Rscript

## Packages
library("here")
library("data.table")
library("readr")
library("lubridate")
library("ggplot2")

## Acquisition 99 / ADNI
# Data.table 99
fname <- "acquisition_99_dt.rds"
fpath <- here("data/derivatives", fname)

if (!file.exists(fpath)) {
  source(here("scripts/data_acquisition99.R"))
} else {
  acq_99 <- read_rds(fpath)
}

rm(fname, fpath)

acq_99[Session == "First" & Rater == "Expert01", Rater := "Expert01_1"]
acq_99[Session == "Second" & Rater == "Expert01", Rater := "Expert01_2"]
acq_99 <- acq_99[Rater != "Expert01"]

acq_99_N <- acq_99[, .N, by = .(Rater, Rating)]
acq_99_N[, .(X = mean(N), SD = sd(N)), by = Rating]

acq_99_time <- acq_99[Diff < duration(15, "minutes"),
  .(Image, Rater = as.character(Rater), Rating, Diff,
    Dataset = "Training", Task = "Acquisition")]

acq_99_time[, .(X = median(Diff), SD = sd(Diff)), by = .(Rater, Rating)]
acq_99_time[, .(X = median(Diff), SD = sd(Diff)),
            by = .(Rater, Rating)][, .(X = mean(X), SD = sd(X)), by = Rating]

# Data.table ADNI
fpath <- here("data/derivatives/acquisition_dt.rds")

if (!file.exists(fpath)) {
  source(here("scripts/data_acquisition.R"))
} else {
  acquis <- read_rds(fpath)
}

rm(fpath)

acquis_N <- acquis[, .N, by = .(Rater, Rating)]
acquis_N[, .(X = mean(N), SD = sd(N)), by = Rating]

acquis_time <- acquis[Diff < duration(15, "minutes"),
  .(Image, Rater = as.character(Rater), Rating, Diff,
    Dataset = "Full", Task = "Acquisition")]

acquis_time[, .(X = median(Diff), SD = sd(Diff)), by = .(Rater, Rating)]
acquis_time[, .(X = median(Diff), SD = sd(Diff)),
            by = .(Rater, Rating)][, .(X = mean(X), SD = sd(X)), by = Rating]

## Registration 99 / ADNI
# Data.table 99
fpath <- here("data/derivatives/registration_99_dt.rds")

if (!file.exists(fpath)) {
  source(here("scripts/data_registration99.R"))
} else {
  reg_99 <- read_rds(fpath)
}

rm(fpath)

reg_99[Session == "First" & Rater == "Expert01", Rater := "Expert01_1"]
reg_99[Session == "Second" & Rater == "Expert01", Rater := "Expert01_2"]
reg_99 <- reg_99[Rater != "Expert01"]

reg_99_N <- reg_99[, .N, by = .(Rater, Rating)]
reg_99_N[, .(X = mean(N), SD = sd(N)), by = Rating]

reg_99_time <- reg_99[Diff < duration(15, "minutes"),
  .(Image, Rater = as.character(Rater), Rating, Diff,
    Dataset = "Training", Task = "Registration")]

reg_99_time[, .(X = median(Diff), SD = sd(Diff)), by = .(Rater, Rating)]
reg_99_time[, .(X = median(Diff), SD = sd(Diff)),
            by = .(Rater, Rating)][, .(X = mean(X), SD = sd(X)), by = Rating]

# Data.table ADNI
fpath <- here("data/derivatives/registration_dt.rds")

if (!file.exists(fpath)) {
  source(here("scripts/data_registration.R"))
} else {
  regis <- read_rds(fpath)
}

rm(fpath)

regis_N <- regis[, .N, by = .(Rater, Rating)]
regis_N[, .(X = mean(N), SD = sd(N)), by = Rating]

regis_time <- regis[Diff < duration(15, "minutes"),
  .(Image, Rater = as.character(Rater), Rating, Diff,
    Dataset = "Full", Task = "Registration")]

regis_time[, .(X = median(Diff), SD = sd(Diff)), by = .(Rater, Rating)]
regis_time[, .(X = median(Diff), SD = sd(Diff)),
           by = .(Rater, Rating)][, .(X = mean(X), SD = sd(X)), by = Rating]

## Merge datasets
timeDT <- rbindlist(list(acq_99_time, acquis_time, reg_99_time, regis_time))
timeDT[, Rating := factor(Rating, levels = c("Pass", "Warning", "Fail"))]
timeDT[, Dataset := factor(Dataset, levels = c("Training", "Full"))]

# QC time sessions by Rater, task, dataset
sessions <- timeDT[order(Task, Dataset, Rater),
                   .(Time = as.numeric(sum(Diff), units = "hours")),
                   .(Rater, Dataset, Task)]

avg_sess <- sessions[, .(Time = mean(Time)), .(Dataset, Task)]

sessions[, `:=`(Hrs = floor(Time),
                Mins = floor((Time * 60) %% 60),
                Secs = floor((Time * 3600) %% 60))]

avg_sess[, `:=`(Hrs = floor(Time),
                Mins = floor((Time * 60) %% 60),
                Secs = floor((Time * 3600) %% 60))]


write_rds(sessions, here("data/derivatives/qc_time_dt.rds"))

# Acq99 time between expert sessions
acq_99_time[startsWith(Rater, "Expert01"), median(Diff), by = Rater]
wilcox.test(acq_99_time[Rater == "Expert01_1", as.numeric(Diff)],
            acq_99_time[Rater == "Expert01_2", as.numeric(Diff)])

# Acq99 time between ratings (with expert):
acq_99_time[, median(Diff), by = Rating]
wilcox.test(acq_99_time[Rating == "Pass", as.numeric(Diff)],
            acq_99_time[Rating == "Fail", as.numeric(Diff)])

# Acq99 time between ratings (only trainees):
acq_99_time[!startsWith(Rater, "Expert01"), median(Diff), by = Rating]

wilcox.test(acq_99_time[(!startsWith(Rater, "Expert01")) & Rating == "Pass",
            as.numeric(Diff)],
            acq_99_time[(!startsWith(Rater, "Expert01")) & Rating == "Fail",
            as.numeric(Diff)])

# Acq99 expert sessions
acq_99_time[startsWith(Rater, "Expert01"), median(Diff), by = Rater]
wilcox.test(acq_99_time[Rater == "Expert01_1", as.numeric(Diff)],
            acq_99_time[Rater == "Expert01_2", as.numeric(Diff)])

# adni acquisition time between ratings:
acquis_time[, median(Diff), by = Rating]
kruskal.test(acquis_time[, .(Diff, Rating)])

# ADNI vs training (Expert only)
acq_99_time[startsWith(Rater, "Expert01"), median(Diff)]
acquis_time[Rater == "Expert01", median(Diff)]
wilcox.test(acq_99_time[startsWith(Rater, "Expert01"), as.numeric(Diff)],
            acquis_time[Rater == "Expert01", as.numeric(Diff)])

# ADNI vs training
raters_adni <- acquis_time[, unique(Rater)]
acq_99_time[Rater %in% raters_adni, median(Diff)]
acquis_time[, median(Diff)]

wilcox.test(acq_99_time[Rater %in% raters_adni, as.numeric(Diff)],
            acquis_time[, as.numeric(Diff)])

# Acq99 time between expert sessions
reg_99_time[startsWith(Rater, "Expert01"), median(Diff), by = Rater]
wilcox.test(reg_99_time[Rater == "Expert01_1", as.numeric(Diff)],
            reg_99_time[Rater == "Expert01_2", as.numeric(Diff)])

# Reg99 time between ratings (with expert):
reg_99_time[, median(Diff), by = Rating]
wilcox.test(reg_99_time[Rating == "Pass", as.numeric(Diff)],
            reg_99_time[Rating == "Fail", as.numeric(Diff)])

# Reg99 time between ratings (only trainees):
reg_99_time[!startsWith(Rater, "Expert01"), median(Diff), by = Rating]
wilcox.test(reg_99_time[(!startsWith(Rater, "Expert01")) & Rating == "Pass",
                        as.numeric(Diff)],
            reg_99_time[(!startsWith(Rater, "Expert01")) & Rating == "Fail",
                        as.numeric(Diff)])

# Reg99 expert sessions
reg_99_time[startsWith(Rater, "Expert01"), median(Diff), by = Rater]
wilcox.test(reg_99_time[Rater == "Expert01_1", as.numeric(Diff)],
            reg_99_time[Rater == "Expert01_2", as.numeric(Diff)])

# adni acquisition time between ratings:
regis_time[, median(Diff), by = Rating]
wilcox.test(regis_time[Rating == "Pass", as.numeric(Diff)],
            regis_time[Rating == "Fail", as.numeric(Diff)])

# ADNI vs training
raters_adni <- regis_time[, unique(Rater)]
reg_99_time[Rater %in% raters_adni, median(Diff)]
regis_time[, median(Diff)]

wilcox.test(reg_99_time[Rater %in% raters_adni, as.numeric(Diff)],
            regis_time[, as.numeric(Diff)])


## Plots
# Clean acq99
q25 <- quantile(acq_99_time[, Diff], 0.25)
q75 <- quantile(acq_99_time[, Diff], 0.75)
timeDT[Dataset == "Training" & Task == "Acquisition",
       plot := Diff < q75 + 3 * (q75 - q25)]

# Clean acquis
q25 <- quantile(acquis_time[, Diff], 0.25)
q75 <- quantile(acquis_time[, Diff], 0.75)
timeDT[Dataset == "Full" & Task == "Acquisition",
       plot := Diff < q75 + 3 * (q75 - q25)]


ggplot(timeDT[Task == "Acquisition" & plot == T],
       aes(x = Rater, y= Diff, fill = Rating)) +
  geom_boxplot(outlier.shape = NA) +
  coord_flip() +
  facet_wrap(. ~ Dataset, scales = "free") +
  labs(y = "Time (s)", title = "Task: Raw MRI QC") +
  scale_fill_manual(values = c("darkgreen", "yellow3", "darkred")) +
  theme_linedraw() +
  theme(text = element_text(size = 21), legend.position = "bottom")

ggsave(here("plots/time_acquisition.png"), width = 12, height = 15)

# Clean reg99
q25 <- quantile(reg_99_time[, Diff], 0.25)
q75 <- quantile(reg_99_time[, Diff], 0.75)
timeDT[Dataset == "Training" & Task == "Registration",
       plot := Diff < q75 + 3 * (q75 - q25)]

q25 <- quantile(regis_time[, Diff], 0.25)
q75 <- quantile(regis_time[, Diff], 0.75)
timeDT[Dataset == "Full" & Task == "Registration",
       plot := Diff < q75 + 3 * (q75 - q25)]

ggplot(timeDT[Task == "Registration" & plot == T],
       aes(x = Rater, y= Diff, fill = Rating)) +
  geom_boxplot(outlier.shape = NA) +
  coord_flip() +
  facet_wrap(. ~ Dataset, scales = "free") +
  labs(y = "Time (s)", title = "Task: Linear registration QC") +
  scale_fill_manual(values = c("darkgreen", "darkred")) +
  theme_linedraw() +
  theme(text = element_text(size = 21), legend.position = "bottom")

ggsave(here("plots/time_registration.png"), width = 12, height = 15)

## Registration and Segmentation
# Data.table
fname <- "segmentation_dt.rds"
fpath <- here("data/derivatives", fname)

if (!file.exists(fpath)) {
  source(here("scripts/data_segmentation.R"))
} else {
  rskull <- read_rds(fpath)
}

rm(fname, fpath)

q25 <- quantile(rskull[, Diff], 0.25, na.rm = TRUE)
q75 <- quantile(rskull[, Diff], 0.75, na.rm = TRUE)
rskull[, plot := Diff < q75 + 3 * (q75 - q25)]

ggplot(rskull[plot == T], aes(x = Rater, y = Diff, fill = Rating)) +
  #geom_violin(aes(fill = Rating), trim = F, alpha = .5) +
  #geom_boxplot(aes(colour = Rating), position = position_dodge(width = 0.9),
               #width = 0.2, outlier.shape = NA, alpha = .5) +
  geom_boxplot(outlier.shape = NA) +
  coord_flip() +
  labs(y = "Time (s)", title = "Task: Skull segmentation QC") +
  scale_fill_manual(values = c("darkred", "darkgreen")) +
  scale_colour_manual(values = c("darkred", "darkgreen")) +
theme_linedraw() +
theme(text = element_text(size = 18))
ggsave(here("plots/time_segmentation.png"), width = 8, height = 5)
