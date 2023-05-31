#!/usr/bin/env Rscript

## Packages
library("here")
library("data.table")
library("magrittr")
library("purrr")
library("stringr")
library("readr")
library("ggplot2")
library("gridExtra")

## Acquisition 99
# Data.table
fpath <- here("data/derivatives/acquisition_99_dt.rds")
if (!file.exists(fpath)) {
  source(here("scripts/data_acquisition99.R"))
} else {
  acq_99 <- read_rds(fpath)
}
#rm(fpath)

## Intra-rater agreement
acq_99_bin_1 <- acq_99[Session %in% c("First", "Second"),
                         .(Image, Session, Rating)]
acq_99_bin_1[
  Rating == "Pass", Rating := 1][
  Rating == "Fail", Rating := 0][
  , Rating := as.integer(Rating)
               ]
acq_99_wide_1 <- dcast(acq_99_bin_1, Image ~ Session,
                         value.var = "Rating")
irr::kappa2(acq_99_wide_1[, -1])

## Gold_standard: new images
acq_99[
  Session == "Consensus" & Image %in% acq_99_wide_1[First != Second, Image],
  .N,
  by = Rating
]

## Gold_standard: proportion
acq_99[Session == "Consensus", .N, by = Rating]

## Inter-rater agreement

# Matrix of ratings
acq_99_bin <- acq_99[!Session %in% c("First", "Second"),
                     .(Image, Rater, Rating)]
acq_99_bin[
  Rating == "Pass", Rating := 1][
  Rating == "Fail", Rating := 0][
  , Rating := as.integer(Rating)]
acq_99_wide <- dcast(acq_99_bin, Image ~ Rater, value.var = "Rating")[, -1]

# Calculate Cohen's Kappas & Raw agreement
acq_99_kappa <- acq_99_count <- vector("list", length(acq_99_wide))
for (i in seq_along(acq_99_wide)) {
  acq_99_kappa[[i]] <- acq_99_count[[i]] <- vector("list", length(acq_99_wide))
  for (j in seq_along(acq_99_wide)) {
    cols <- names(acq_99_wide)[c(i, j)]
    acq_99_kappa[[i]][[j]] <-
      round(irr::kappa2(acq_99_wide[, ..cols])$value, digits = 2)
    acq_99_count[[i]][[j]] <- sum(acq_99_wide[[i]] == acq_99_wide[[j]])
  }
}

names(acq_99_wide) <- acq_99_wide %>%
  names() %>%
  str_replace("Rater", "R") %>%
  str_replace("Expert", "E")

acq_99_kappa <- setDT(lapply(acq_99_kappa, unlist))
names(acq_99_kappa) <- names(acq_99_wide)

acq_99_count <- setDT(lapply(acq_99_count, unlist))
names(acq_99_count) <- names(acq_99_wide)

write_rds(acq_99_count, here("data/derivatives/qc_count_acq_dt.rds"))

## Off Qrater QC
# Rater 1:
acq_99_off1 <- fread(here("data/raw/off-qrater-qc_rater1_raw-mri.csv"))

acq_99_off1 <- acq_99[Rater == "Rater1", .(Image, Prev = Rating)
                      ][acq_99_off1, on = "Image", .(Image, Prev, Off = QC)]

acq_99_off1 <- acq_99[Session == "Consensus", .(Image, GS = Rating)
                      ][acq_99_off1, on = "Image",
                      .(Image, Prev, GS, Off)
                      ][, `:=`(Off = fifelse(Off == "Pass", 1, 0),
                               Prev = fifelse(Off == "Pass", 1, 0),
                               GS = fifelse(GS == "Pass", 1, 0))]

# Expert1
acq_99_off2 <- fread(here("data/raw/off-qrater-qc_expert1_raw-mri.csv"),
                     nrows = 90)



acq_99_off2 <- acq_99[Session == "Consensus", .(Image, GS = Rating)
                      ][acq_99_off2, on = "Image",
                      .(Image, GS, Off = QC)
                      ][, `:=`(Off = fifelse(Off == "fail", 0, 1),
                               GS = fifelse(GS == "Pass", 1, 0))]

# Agreement Gold-Standard
# Count
acq_99_off1[Off == GS, .N]
acq_99_off2[Off == GS, .N]

# Kappa
irr::kappa2(acq_99_off1[, .(Off, GS)])
irr::kappa2(acq_99_off2[, .(Off, GS)])

# Agreement Previous task
# Count
acq_99_off1[Off == Prev, .N]

# Kappa
irr::kappa2(acq_99_off1[, .(Off, Prev)])

# Inter-rater agreement
acq_99_off <- acq_99_off2[acq_99_off1,
                          on = "Image",
                          .(Image, R1 = Off, E1 = i.Off)]

acq_99_off[E1 == R1, .N]
irr::kappa2(acq_99_off[, .(R1, E1)])

## Plots
# Heatmap
rater_id <- names(acq_99_wide)

acq_99_count[, Rater := rater_id]
acq_99_count[] %>%
  melt(id.vars = "Rater", variable.factor = FALSE) %>%
  ggplot(aes(Rater, variable)) +
  geom_tile(aes(fill = value)) +
  geom_text(aes(label = value), size = 10) +
  scale_fill_gradient(low = "white", high = "red") +
  labs(y = "Rater", fill = "Agreement:\nCount") +
  theme_bw() +
  theme(text = element_text(size = 28), legend.position = "left")

ggsave(here("plots/heatmap_count.png"), width = 18, height = 15)

acq_99_kappa[, Rater := rater_id]
acq_99_kappa[] %>%
  melt(id.vars = "Rater", variable.factor = FALSE) %>%
  ggplot(aes(Rater, variable)) +
  geom_tile(aes(fill = value)) +
  geom_text(aes(label = value), size = 10) +
  scale_fill_gradient(low = "white", high = "red") +
  labs(y = "Rater", fill = "Agreement:\nKappa") +
  theme_bw() +
  theme(text = element_text(size = 28),
    legend.position = "right")

ggsave(here("plots/heatmap_kappa.png"), width = 18, height = 15)

# Pie charts
agree    <- acq_99[Session == "First", .(Image, First = Rating)
                   ][acq_99[Session == "Second", .(Image, Second = Rating)],
                   on = "Image"
                   ][First == Second, .(Image, Expert = "*")]

exp1 <- agree[acq_99[Session == "Consensus", .(Image, Rating)],
                  on = "Image"]
exp1[is.na(Expert), Expert := "†"]
exp1_1pass <- exp1[Expert == '*', Image]
exp1_2pass <- exp1[Expert == "†", Image]

acq_99_n <- acq_99[! Session %in% c("First", "Second"),
                   .N, by = .(Image, Rating)]
acq_99_n <- exp1[acq_99_n, on = .(Image, Rating)]
acq_99_n[is.na(Expert), Expert := ""]

acq_99_pass <- acq_99_n[Rating == "Pass"]

acq_99_n$Image <- factor(acq_99_n$Image,
  levels = c(paste("Case", c(79, 28), sep = "_"),
             acq_99_pass[Expert == ""][Image %in% exp1_1pass][order(N), Image],
             acq_99_pass[Expert == ""][Image %in% exp1_2pass][order(N), Image],
             acq_99_pass[Expert != ""][order(-Expert, N), Image]))

acq_99_n %>%
  ggplot(aes(x = "", y = N, fill = Rating)) +
  geom_col() + coord_polar(theta = "y") +
  geom_text(aes(label = Expert),
            position = position_stack(vjust = 0.5),
            size = 9) +
  scale_fill_manual(values = c("#9d0000", "#028202")) +
  facet_wrap(vars(Image), ncol = 11) +
  ggtitle("Experiment 1: Raw MRI") +
  theme_void() +
  theme(text = element_text(size = 32),
    strip.text.x = element_blank(),
    legend.position = "bottom")

ggsave(here("plots/pies_agreement.png"), width = 18, height = 15)

# Number of images with full agreement
acq_99_n[N == 10, .N, by = Rating] #28:99
acq_99_n[N >= 8, .N, by = Rating] #69:99


## Registration
fname <- "registration_99_dt.rds"
fpath <- here("data/derivatives", fname)

if (!file.exists(fpath)) {
    source(here("scripts/data_registration99.R"))
} else {
    reg_99 <- read_rds(fpath)
}

rm(fname, fpath)

## Intra-rater agreement
# Matrix of ratings
reg_99_bin <- copy(reg_99)
reg_99_bin[
  Rating == "Pass", Rating := 1][
  Rating == "Fail", Rating := 0][
  , Rating := as.integer(Rating)]

reg_99_bin_1 <- reg_99_bin[Session %in% c("First", "Second")]
reg_99_wide_1 <- dcast(reg_99_bin_1,
                       Image ~ Session,
                       value.var = "Rating")

irr::kappa2(reg_99_wide_1[, -1])
# K=0.873, z=8.69
sum(reg_99_wide_1[[1]] == reg_99_wide_1[[2]])
# 93/99

## Gold standard: new images
reg_99[
  Session == "Consensus" & Image %in% reg_99_wide_1[First != Second, Image],
  .N,
  by = Rating
]

## Gold standard: proportion
reg_99[Session == "Consensus", .N, by = Rating]

## Inter-rater agreement
# Matrix of ratings
reg_99_wide <- dcast(reg_99_bin[!Session %in% c("First", "Second")],
                     Image ~ Rater, value.var = "Rating")[, -1]

# Calculate Cohen's Kappas & Raw agreement
reg_99_kappa <- reg_99_count <- vector("list", length(reg_99_wide))
for (i in seq_along(reg_99_wide)) {
  reg_99_kappa[[i]] <- reg_99_count[[i]] <- vector("list", length(reg_99_wide))
  for (j in seq_along(reg_99_wide)) {
    cols <- names(reg_99_wide)[c(i, j)]
    reg_99_kappa[[i]][[j]] <- round(irr::kappa2(reg_99_wide[, ..cols])$value,
      digits = 2)
    reg_99_count[[i]][[j]] <- sum(reg_99_wide[[i]] == reg_99_wide[[j]])
  }
}

names(reg_99_wide) <- reg_99_wide %>%
  names() %>%
  str_replace("Rater", "R") %>%
  str_replace("Expert", "E")

reg_99_kappa <- setDT(lapply(reg_99_kappa, unlist))
names(reg_99_kappa) <- names(reg_99_wide)

reg_99_count <- setDT(lapply(reg_99_count, unlist))
names(reg_99_count) <- names(reg_99_wide)

write_rds(reg_99_count, here("data/derivatives/qc_count_reg_dt.rds"))

## Off Qrater QC
# Rater1
reg_99_off1 <- fread(here("data/raw/off-qrater-qc_rater1_lin-reg.csv"))

reg_99_off1 <- reg_99[Rater == "Rater1", .(Image, Prev = Rating)
                      ][reg_99_off1, on = "Image", .(Image, Prev, Off = QC)]

reg_99_off1 <- reg_99[Session == "Consensus", .(Image, GS = Rating)
                      ][reg_99_off1, on = "Image",
                      .(Image, Prev, GS, Off)
                      ][, `:=`(Off = fifelse(Off == "Pass", 1, 0),
                               Prev = fifelse(Off == "Pass", 1, 0),
                               GS = fifelse(GS == "Pass", 1, 0))]

# Expert1
reg_99_off2 <- fread(here("data/raw/off-qrater-qc_expert1_lin-reg.csv"),
                     nrows = 44)

reg_99_off2 <- reg_99[Session == "Consensus", .(Image, GS = Rating)
                      ][reg_99_off2, on = "Image",
                      .(Image, GS, Off = QC)
                      ][, `:=`(Off = fifelse(Off == "fail", 0, 1),
                               GS = fifelse(GS == "Pass", 1, 0))]

# Agreement Gold-Standard
# Count
reg_99_off1[Off == GS, .N]
reg_99_off2[Off == GS, .N]

# Kappa
irr::kappa2(reg_99_off1[, .(Off, GS)])
irr::kappa2(reg_99_off2[, .(Off, GS)])

# Agreement Previous task
# Count
reg_99_off1[Off == Prev, .N]

# Kappa
irr::kappa2(reg_99_off1[, .(Off, Prev)])

# Inter-rater agreement
reg_99_off <- reg_99_off2[reg_99_off1,
                          on = "Image",
                          .(Image, R1 = Off, E1 = i.Off)]

reg_99_off[E1 == R1, .N]
irr::kappa2(reg_99_off[, .(R1, E1)])

## Plots
# Heatmap
rater_id <- names(reg_99_wide)

reg_99_count[, Rater := rater_id]
reg_99_count[] %>%
  melt(id.vars = "Rater", variable.factor = FALSE) %>%
  ggplot(aes(Rater, variable)) +
  geom_tile(aes(fill = value)) +
  geom_text(aes(label = value), size = 10) +
  scale_fill_gradient(low = "white", high = "red") +
  labs(y = "Rater", fill = "Agreement:\nCount") +
  theme_bw() +
  theme(text = element_text(size = 28),
    legend.position = "left")

ggsave(here("plots/heatmap_count_registration.png"), width = 18, height = 15)

reg_99_kappa[, Rater := rater_id]
reg_99_kappa[] %>%
  melt(id.vars = "Rater", variable.factor = FALSE) %>%
  ggplot(aes(Rater, variable)) +
  geom_tile(aes(fill = value)) +
  geom_text(aes(label = value), size = 10) +
  scale_fill_gradient(low = "white", high = "red") +
  labs(y = "Rater", fill = "Agreement:\nKappa") +
  theme_bw() +
  theme(text = element_text(size = 28),
    legend.position = "right")

ggsave(here("plots/heatmap_kappa_registration.png"), width = 18, height = 15)

# Pie charts
agree    <- reg_99[Session == "First", .(Image, First = Rating)
                   ][reg_99[Session == "Second", .(Image, Second = Rating)],
                   on = "Image"
                   ][First == Second, .(Image, Expert = "*")]

exp1 <- agree[reg_99[Session == "Consensus", .(Image, Rating)],
                  on = "Image"]
exp1[is.na(Expert), Expert := "†"]
exp1_1pass <- exp1[Expert == '*', Image]
exp1_2pass <- exp1[Expert == "†", Image]

reg_99_n <- reg_99[! Session %in% c("First", "Second"),
                   .N, by = .(Image, Rating)]
reg_99_n <- exp1[reg_99_n, on = .(Image, Rating)]
reg_99_n[is.na(Expert), Expert := ""]

reg_99_pass <- reg_99_n[Rating == "Pass"]

reg_99_n$Image <- factor(reg_99_n$Image,
  levels = c(reg_99_n[Rating == "Fail" & N == 9][order(Expert), Image],
             reg_99_pass[Expert == ""][Image %in% exp1_1pass][order(N), Image],
             reg_99_pass[Expert == ""][Image %in% exp1_2pass][order(N), Image],
             reg_99_pass[Expert != ""][order(N, -Expert), Image]))

reg_99_n %>%
  ggplot(aes(x = "", y = N, fill = Rating)) +
  geom_col() + coord_polar(theta = "y") +
  geom_text(aes(label = Expert), position = position_stack(vjust = 0.5), size = 9) +
  scale_fill_manual(values = c("#9d0000", "#028202")) +
  facet_wrap(vars(Image), ncol = 11) +
  ggtitle("Experiment 2: Linear registration") +
  theme_void() +
  theme(text = element_text(size = 32),
    strip.text.x = element_blank(),
    legend.position = "bottom")

ggsave(here("plots/pies_agreement_registration.png"), width = 18, height = 15)

# Number of images with full agreement
reg_99_n[N == 9, .N, by = Rating] #28:99
reg_99_n[N >= 7, .N, by = Rating] #68:99


## Redskull
# Data.table
fpath <- here("data/derivatives", "segmentation_dt.rds")

if (!file.exists(fpath)) {
  source(here("scripts/data_segmentation.R"))
} else {
  rskull <- read_rds(fpath)
}

rm(fpath)

rskull_wide <- dcast(rskull, Image ~ Rater, value.var = "Rating")

# Inter-rater reliability: 0.836 Kappa
irr::kappa2(rskull_wide[!is.na(Rater01), .(Rater01, Rater12)],
  weight = "unweighted")
