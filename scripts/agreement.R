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
fpath <-here("data/derivatives/acquisition_99_dt.rds")
if (!file.exists(fpath)) {
  source(here("scripts/data_acquisition99.R"))
} else {
  acq_99 <- read_rds(fpath)
}
#rm(fpath)

## Intra-rater agreement
rater1_bin <- acq_99[Rater == "Rater01", .(Image, Session, Rating)]
rater1_bin[
  Rating == "Pass", Rating := 1][
  Rating == "Fail", Rating := 0][
  , Rating := as.integer(Rating)]
rater1_wide <- dcast(rater1_bin, Image ~ Session, value.var = "Rating")[, -1]
irr::kappa2(rater1_wide)

## Inter-rater agreement
# Matrix of comments
setorder(acq_99, cols = "Rater")
acq_99_comments <- acq_99[Session == 1, .(Image, Rater, Comment)] %>%
  dcast(Image ~ Rater, value.var = "Comment")
acq_99_comments[1]

# Matrix of ratings
acq_99_bin <- acq_99[Session == 1, .(Image, Rater, Rating)]
acq_99_bin[
  Rating == "Pass", Rating := 1][
  Rating == "Fail", Rating := 0][
  , Rating := as.integer(Rating)]
acq_99_wide <- dcast(acq_99_bin, Image ~ Rater, value.var = "Rating")[, -1]

# Calculate Cohen's Kappas & Raw agreement
acq_99_kappa <- vector("list", length(acq_99_wide))
acq_99_count <- vector("list", length(acq_99_wide))
for (i in seq_along(acq_99_wide)) {
  acq_99_kappa[[i]] <- vector("list", length(acq_99_wide))
  acq_99_count[[i]] <- vector("list", length(acq_99_wide))
  for (j in seq_along(acq_99_wide)) {
    cols <- names(acq_99_wide)[c(i, j)]
    acq_99_kappa[[i]][[j]] <- round(irr::kappa2(acq_99_wide[, ..cols])$value,
      digits = 2)
    acq_99_count[[i]][[j]] <- sum(acq_99_wide[[i]] == acq_99_wide[[j]])
  }
}

names(acq_99_wide) <- acq_99_wide %>%
  names() %>%
  str_replace("Rater", "R")

acq_99_kappa <- setDT(lapply(acq_99_kappa, unlist))
names(acq_99_kappa) <- names(acq_99_wide)

acq_99_count <- setDT(lapply(acq_99_count, unlist))
names(acq_99_count) <- names(acq_99_wide)


## Plots
# Heatmap
rater_id <- names(acq_99_wide)

acq_99_count[, Rater := rater_id]
acq_99_count[] %>%
  melt(id.vars = "Rater", variable.factor = FALSE) %>%
  ggplot(aes(Rater, variable)) +
  geom_tile(aes(fill = value)) +
  geom_text(aes(label = value)) +
  scale_fill_gradient(low = "white", high = "red") +
  labs(y = "Rater", fill = "Agreement") +
  theme_bw() +
  theme(text = element_text(size = 13), legend.position = "left")

ggsave(here("plots/heatmap_count.png"), width = 12, height = 10)

acq_99_kappa[, Rater := rater_id]
acq_99_kappa[] %>%
  melt(id.vars = "Rater", variable.factor = FALSE) %>%
  ggplot(aes(Rater, variable)) +
  geom_tile(aes(fill = value)) +
  geom_text(aes(label = value)) +
  scale_fill_gradient(low = "white", high = "red") +
  labs(y = "Rater", fill = "Agreement") +
  theme_bw() +
  theme(text = element_text(size = 13),
    legend.position = "right")

ggsave(here("plots/heatmap_kappa.png"), width = 12, height = 10)

# Pie charts
#ratings1 <- acq_99[Rater == "Rater01" & Session == 1,
                   #.(Image, Rating, Rater1 = "*")]
ratings1 <- acq_99[Rater == "Rater01", .N, by = .(Image, Rating)]
ratings1[N == 2, Rater1 := "**"]
ratings1[N == 1, Rater1 := "*"]
ratings1[, N := NULL]

acq_99_n <- acq_99[, .N, by = .(Image, Rating)]
acq_99_n <- ratings1[acq_99_n, on = .(Image, Rating)]
acq_99_n[is.na(Rater1), Rater1 := ""]

acq_99_pass <- acq_99_n[Rating == "Pass"]
setorder(acq_99_pass, cols = "N")
setorder(acq_99_pass, cols = "Rater1")

acq_99_n$Image <- factor(acq_99_n$Image,
  levels = c(paste("Case", c(79, 28), sep = "_"), acq_99_pass[, unique(Image)]))

acq_99_n %>%
  ggplot(aes(x = "", y = N, fill = Rating)) +
  geom_col() + coord_polar(theta = "y") +
  geom_text(aes(label = Rater1), position = position_stack(vjust = 0.5)) +
  scale_fill_manual(values = c("#9d0000", "#028202")) +
  facet_wrap(vars(Image), ncol = 10) +
  ggtitle("Agreement: Acquisition dataset") +
  theme_void() +
  theme(text = element_text(size = 23),
    strip.text.x = element_blank(),
    legend.position = "bottom")

ggsave(here("plots/pies_agreement.png"), width = 12, height = 10)

# Number of images with full agreement
acq_99_n[N == 10, .N, by = Rating] #28:99
acq_99_n[N >= 8, .N, by = Rating] #69:99

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
