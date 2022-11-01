#!/usr/bin/env Rscript

## Packages
library("here")
library("magrittr")
library("data.table")
library("lubridate")
library("ggplot2")
library("kableExtra")

## White Matter Hyperintensities ADNI
wmh <- fread(here("data/adni_wmh_all-raters_2021-12-15.csv"))
setnames(wmh, c("Image", "Rater", "Rating", "Time"))

# Cassandra: 3k
wmh[, .N, by = Rater]

# Add time difference with previous row
wmh_cass <- wmh[Rater == "Cassandra"]
setorder(wmh_cass, cols = "Time")
wmh_cass[, Diff := Time - shift(Time)]


# N and mean of duration higher than 30 minutes
wmh_cass[Diff > duration(60, "minutes"), .N]
wmh_cass[Diff > duration(60, "minutes"), median(Diff)]

# Duration by rating
wmh_cass[Diff < duration(60, "minutes"),
         .(.N, Time = mean(Diff), Dataset = "Registration"),
         by = Rating]

set3 <- wmh_cass[Diff < duration(60, "minutes"),
                 .(.N, Time = Diff, Dataset = "Registration"),
                 by = Rating]

## Raw T1 ADNI Data
raw1 <- fread(here("data/Rater1_Raw_all-raters_2021-12-16.csv"))
setnames(raw1, c("Image", "Rater", "Rating",  "Comment", "Time"))

# Louis: 1275
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


## ADNI: NACC T1
nacc <- fread(here("data/NACC_T1_all-raters_2021-12-15.csv"))
setnames(nacc, c("Image", "Rater", "Rating", "Time"))

# Ana: 1623
nacc[, .N, by = Rater]

# Add time difference with previous row
nacc_ana <- nacc[Rater == "anamanera"]
setorder(nacc_ana, cols = "Time")
nacc_ana[, Diff := Time - shift(Time)]


# N and mean of duration higher than 30 minutes
nacc_ana[Diff > duration(60, "minutes"), Diff]
nacc_ana[Diff > duration(60, "minutes"), median(Diff)]

# Duration by rating
nacc_ana[Diff < duration(60, "minutes"),
         .(.N, Time = mean(Diff), Dataset = "Acquisition_2"),
         by = Rating]

set2 <- nacc_ana[Diff < duration(60, "minutes"),
                 .(.N, Time = Diff, Dataset = "Acquisition_2"),
                 by = Rating]

## Redskull
rskull <- fread(here("data/redskull_all-raters_2021-12-15.csv"))
setnames(rskull, c("Image", "Rater", "Rating", "Time"))

# Louis: 1746; Mahsa 6968
rskull[, .N, by = Rater]

# Add time difference with previous row
setorder(rskull, cols = "Time")
rskull[Rater == "louis", Diff := Time - shift(Time)]
rskull[Rater == "Mahsa", Diff := Time - shift(Time)]


# N and mean of duration higher than 30 minutes
rskull[Diff > duration(60, "minutes"), Diff, by = Rater]
rskull[Diff > duration(60, "minutes"), median(Diff), by = Rater]

# Duration by rating
rskull[Diff < duration(60, "minutes"), mean(Diff), by = .(Rater, Rating)]

set4 <- rskull[Diff < duration(60, "minutes"),
                 .(.N, Time = Diff, Dataset = "Skull", Rater),
                 by = Rating]

## Inter-rater reliability: 0.836 Kappa
rskull_wide <- dcast(rskull, Image ~ Rater, value.var = "Rating")
irr::kappa2(rskull_wide[!is.na(louis), .(louis, Mahsa)],
            weight = "unweighted")


## Merging tables
set1[, Rater := "Rater1"]
set2[, Rater := "Rater2"]
set3[, Rater := "Rater3"]
set4[Rater == "louis", Rater := "Rater1"][Rater == "Mahsa", Rater := "Rater4"]
datasets <- rbindlist(list(set1, set2, set3, set4))

p <- ggplot(datasets[Time < 180], aes(x = Rating, y = Time)) +
  geom_jitter(aes(colour = Rater),
              width = 0.3, shape = 21,
              size = 0.5, stroke = 1, alpha = 0.5) +
  geom_boxplot(outlier.shape = NA, alpha = 0.3) +
  facet_grid(cols = vars(Dataset), scales = "free") +
  labs(y = "Time (s)", title = "Rating time per image",
       caption = paste("Time spent on a single image before rating it.",
                       "For visibility, the points above 180 seconds are",
                       "not shown (N = 76).", sep = " ")) +
  theme_linedraw()

ggsave(here("plots/time_rating.png"), width = 10, height = 9)

## Table

datasets[, .(.N, "Average time" = mean(Time)), by = .(Dataset, Rating)] %>%
kbl(digits = 1) %>%
kable_classic(full_width = FALSE) %>%
footnote(general = paste("Any lapse of time higher than 60 minutes",
                         "was taken as time spent away from the computer",
                         "and discarded before calculations.", sep = " ")) %>%
save_kable(here("plots/table_times.png"), bs_theme = "flatly")
