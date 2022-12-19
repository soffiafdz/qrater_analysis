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

# Matrix of comment agreement
acq_99_comm <-
  fread(here("data/derivatives/acq_99_comments_reviewed.csv")) %>%
  melt(id.vars = "Image", variable.name = "Rater", value.name = "Comm_Agree")

acq_99_comm_bin <- acq_99_comm[acq_99_bin, on = .(Image, Rater)]
acq_99_comm_bin_par <- copy(acq_99_comm_bin)
acq_99_comm_bin_par[
  Comm_Agree %in% c("*", "**") & Rating == 1, Rating := 0][
  , Comm_Agree := NULL
]
acq_99_wide_cp <- dcast(acq_99_comm_bin_par,
                        Image ~ Rater,
                        value.var = "Rating")[, -1]

acq_99_comm_bin_full <- copy(acq_99_comm_bin)
acq_99_comm_bin_full[
  Comm_Agree == "**" & Rating == 1, Rating := 0][
  , Comm_Agree := NULL
]
acq_99_wide_cf <- dcast(acq_99_comm_bin_full,
                        Image ~ Rater,
                        value.var = "Rating")[, -1]

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
  str_replace("Rater", "R")

acq_99_kappa <- setDT(lapply(acq_99_kappa, unlist))
names(acq_99_kappa) <- names(acq_99_wide)

acq_99_count <- setDT(lapply(acq_99_count, unlist))
names(acq_99_count) <- names(acq_99_wide)

# Count & Kappa: Partial comment agreement
acq_99_kappa_cp <- acq_99_count_cp <- vector("list", length(acq_99_wide_cp))
for (i in seq_along(acq_99_wide_cp)) {
  acq_99_kappa_cp[[i]] <- acq_99_count_cp[[i]] <-
    vector("list", length(acq_99_wide_cp))
  for (j in seq_along(acq_99_wide_cp)) {
    cols <- names(acq_99_wide_cp)[c(i, j)]
    acq_99_kappa_cp[[i]][[j]] <-
      round(irr::kappa2(acq_99_wide_cp[, ..cols])$value, digits = 2)
    acq_99_count_cp[[i]][[j]] <-
      sum(acq_99_wide_cp[[i]] == acq_99_wide_cp[[j]])
  }
}

names(acq_99_wide_cp) <- acq_99_wide_cp %>%
  names() %>%
  str_replace("Rater", "R")

acq_99_kappa_cp <- setDT(lapply(acq_99_kappa_cp, unlist))
names(acq_99_kappa_cp) <- names(acq_99_wide_cp)

acq_99_count_cp <- setDT(lapply(acq_99_count_cp, unlist))
names(acq_99_count_cp) <- names(acq_99_wide_cp)

# Count & Kappa: Full comment agreement
acq_99_kappa_cf <- acq_99_count_cf <- vector("list", length(acq_99_wide_cf))
for (i in seq_along(acq_99_wide_cf)) {
  acq_99_kappa_cf[[i]] <- acq_99_count_cf[[i]] <-
    vector("list", length(acq_99_wide_cf))
  for (j in seq_along(acq_99_wide_cf)) {
    cols <- names(acq_99_wide_cf)[c(i, j)]
    acq_99_kappa_cf[[i]][[j]] <-
      round(irr::kappa2(acq_99_wide_cf[, ..cols])$value, digits = 2)
    acq_99_count_cf[[i]][[j]] <-
      sum(acq_99_wide_cf[[i]] == acq_99_wide_cf[[j]])
  }
}

names(acq_99_wide_cf) <- acq_99_wide_cf %>%
  names() %>%
  str_replace("Rater", "R")

acq_99_kappa_cf <- setDT(lapply(acq_99_kappa_cf, unlist))
names(acq_99_kappa_cf) <- names(acq_99_wide_cf)

acq_99_count_cf <- setDT(lapply(acq_99_count_cf, unlist))
names(acq_99_count_cf) <- names(acq_99_wide_cf)

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
  labs(y = "Rater", fill = "Agreement") +
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
  labs(y = "Rater", fill = "Agreement") +
  theme_bw() +
  theme(text = element_text(size = 28),
    legend.position = "right")

ggsave(here("plots/heatmap_kappa.png"), width = 18, height = 15)

# Heatmaps: Comment agreement partial
acq_99_count_cp[, Rater := rater_id]
acq_99_count_cp[] %>%
  melt(id.vars = "Rater", variable.factor = FALSE) %>%
  ggplot(aes(Rater, variable)) +
  geom_tile(aes(fill = value)) +
  geom_text(aes(label = value), size = 10) +
  scale_fill_gradient(low = "white", high = "red") +
  labs(y = "Rater", fill = "Agreement") +
  theme_bw() +
  theme(text = element_text(size = 28), legend.position = "left")

ggsave(here("plots/heatmap_count_cp.png"), width = 18, height = 15)

acq_99_kappa_cp[, Rater := rater_id]
acq_99_kappa_cp[] %>%
  melt(id.vars = "Rater", variable.factor = FALSE) %>%
  ggplot(aes(Rater, variable)) +
  geom_tile(aes(fill = value)) +
  geom_text(aes(label = value), size = 10) +
  scale_fill_gradient(low = "white", high = "red") +
  labs(y = "Rater", fill = "Agreement") +
  theme_bw() +
  theme(text = element_text(size = 28),
    legend.position = "right")

ggsave(here("plots/heatmap_kappa_cp.png"), width = 18, height = 15)

# Heatmaps: Comment agreement full
acq_99_count_cf[, Rater := rater_id]
acq_99_count_cf[] %>%
  melt(id.vars = "Rater", variable.factor = FALSE) %>%
  ggplot(aes(Rater, variable)) +
  geom_tile(aes(fill = value)) +
  geom_text(aes(label = value), size = 10) +
  scale_fill_gradient(low = "white", high = "red") +
  labs(y = "Rater", fill = "Agreement") +
  theme_bw() +
  theme(text = element_text(size = 28), legend.position = "left")

ggsave(here("plots/heatmap_count_cf.png"), width = 18, height = 15)

acq_99_kappa_cf[, Rater := rater_id]
acq_99_kappa_cf[] %>%
  melt(id.vars = "Rater", variable.factor = FALSE) %>%
  ggplot(aes(Rater, variable)) +
  geom_tile(aes(fill = value)) +
  geom_text(aes(label = value), size = 10) +
  scale_fill_gradient(low = "white", high = "red") +
  labs(y = "Rater", fill = "Agreement") +
  theme_bw() +
  theme(text = element_text(size = 28),
    legend.position = "right")

ggsave(here("plots/heatmap_kappa_cf.png"), width = 18, height = 15)

# Pie charts
ratings1 <- acq_99[Session == "Consensus", .(Image, Rating, Rater1 = "*")]

acq_99_n <- acq_99[! Session %in% c("First", "Second"),
                   .N, by = .(Image, Rating)]
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
  geom_text(aes(label = Rater1), position = position_stack(vjust = 0.5), size = 9) +
  scale_fill_manual(values = c("#9d0000", "#028202")) +
  facet_wrap(vars(Image), ncol = 11) +
  ggtitle("Agreement: Acquisition dataset") +
  theme_void() +
  theme(text = element_text(size = 32),
    strip.text.x = element_blank(),
    legend.position = "bottom")

ggsave(here("plots/pies_agreement.png"), width = 18, height = 15)

# Number of images with full agreement
acq_99_n[N == 10, .N, by = Rating] #28:99
acq_99_n[N >= 8, .N, by = Rating] #69:99

# Pie charts: partial comment agreement
acq_99_comm <- acq_99_comm[acq_99, on = .(Image, Rater)]
acq_99_cp <- copy(acq_99_comm)
acq_99_cp[Comm_Agree %in% c("*", "**") & Rating == "Pass", Rating := "Fail"]
acq_99_ncp <- acq_99_cp[! Session %in% c("First", "Second"),
                          .N, by = .(Image, Rating)]
acq_99_ncp <- ratings1[acq_99_ncp, on = .(Image, Rating)]
acq_99_ncp[is.na(Rater1), Rater1 := ""]

acq_99_pass_cp <- acq_99_ncp[Rating == "Pass"]
setorder(acq_99_pass_cp, cols = "N")
setorder(acq_99_pass_cp, cols = "Rater1")

acq_99_ncp$Image <- factor(acq_99_ncp$Image,
  levels = c(acq_99_ncp[Rating == "Fail" & N == 10, Image],
             acq_99_pass_cp[, unique(Image)]))

acq_99_ncp %>%
  ggplot(aes(x = "", y = N, fill = Rating)) +
  geom_col() + coord_polar(theta = "y") +
  geom_text(aes(label = Rater1), position = position_stack(vjust = 0.5), size = 9) +
  scale_fill_manual(values = c("#9d0000", "#028202")) +
  facet_wrap(vars(Image), ncol = 11) +
  ggtitle("Agreement: Acquisition dataset (Partial comments)") +
  theme_void() +
  theme(text = element_text(size = 32),
    strip.text.x = element_blank(),
    legend.position = "bottom")

ggsave(here("plots/pies_agreement_cp.png"), width = 18, height = 15)

# Pie charts: full comment agreement
acq_99_cf <- copy(acq_99_comm)
acq_99_cf[Comm_Agree == "**" & Rating == "Pass", Rating := "Fail"]
acq_99_ncf <- acq_99_cf[! Session %in% c("First", "Second"),
                          .N, by = .(Image, Rating)]
acq_99_ncf <- ratings1[acq_99_ncf, on = .(Image, Rating)]
acq_99_ncf[is.na(Rater1), Rater1 := ""]

acq_99_pass_cf <- acq_99_ncf[Rating == "Pass"]
setorder(acq_99_pass_cf, cols = "N")
setorder(acq_99_pass_cf, cols = "Rater1")

acq_99_ncf$Image <- factor(acq_99_ncf$Image,
  levels = c(acq_99_ncf[Rating == "Fail" & N == 10, Image],
             acq_99_pass_cf[, unique(Image)]))

acq_99_ncf %>%
  ggplot(aes(x = "", y = N, fill = Rating)) +
  geom_col() + coord_polar(theta = "y") +
  geom_text(aes(label = Rater1), position = position_stack(vjust = 0.5), size = 9) +
  scale_fill_manual(values = c("#9d0000", "#028202")) +
  facet_wrap(vars(Image), ncol = 11) +
  ggtitle("Agreement: Acquisition dataset (Full comments)") +
  theme_void() +
  theme(text = element_text(size = 32),
    strip.text.x = element_blank(),
    legend.position = "bottom")

ggsave(here("plots/pies_agreement_cf.png"), width = 18, height = 15)

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

# Matrix of comments agreement
reg_99_comm <-
  fread(here("data/derivatives/reg_99_comments_reviewed.csv")) %>%
  melt(id.vars = "Image", variable.name = "Rater", value.name = "Comm_Agree")

reg_99_comm_bin <- reg_99_comm[reg_99_bin[!Session %in% c("First", "Second")],
                               on = .(Image, Rater)]
reg_99_comm_bin_par <- copy(reg_99_comm_bin)
reg_99_comm_bin_par[
  Comm_Agree %in% c("*", "**") & Rating == 1, Rating := 0][
  , Comm_Agree := NULL
]
reg_99_wide_cp <- dcast(reg_99_comm_bin_par,
                        Image ~ Rater,
                        value.var = "Rating")[, -1]

reg_99_comm_bin_full <- copy(reg_99_comm_bin)
reg_99_comm_bin_full[
  Comm_Agree == "**" & Rating == 1, Rating := 0][
  , Comm_Agree := NULL
]
reg_99_wide_cf <- dcast(reg_99_comm_bin_full,
                        Image ~ Rater,
                        value.var = "Rating")[, -1]

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
  str_replace("Rater", "R")

reg_99_kappa <- setDT(lapply(reg_99_kappa, unlist))
names(reg_99_kappa) <- names(reg_99_wide)

reg_99_count <- setDT(lapply(reg_99_count, unlist))
names(reg_99_count) <- names(reg_99_wide)

# Count & Kappa: Partial comment agreement
reg_99_kappa_cp <- reg_99_count_cp <- vector("list", length(reg_99_wide_cp))
for (i in seq_along(reg_99_wide_cp)) {
  reg_99_kappa_cp[[i]] <- reg_99_count_cp[[i]] <-
    vector("list", length(reg_99_wide_cp))
  for (j in seq_along(reg_99_wide_cp)) {
    cols <- names(reg_99_wide_cp)[c(i, j)]
    reg_99_kappa_cp[[i]][[j]] <-
      round(irr::kappa2(reg_99_wide_cp[, ..cols])$value, digits = 2)
    reg_99_count_cp[[i]][[j]] <-
      sum(reg_99_wide_cp[[i]] == reg_99_wide_cp[[j]])
  }
}

names(reg_99_wide_cp) <- reg_99_wide_cp %>%
  names() %>%
  str_replace("Rater", "R")

reg_99_kappa_cp <- setDT(lapply(reg_99_kappa_cp, unlist))
names(reg_99_kappa_cp) <- names(reg_99_wide_cp)

reg_99_count_cp <- setDT(lapply(reg_99_count_cp, unlist))
names(reg_99_count_cp) <- names(reg_99_wide_cp)

# Count & Kappa: Full comment agreement
reg_99_kappa_cf <- reg_99_count_cf <- vector("list", length(reg_99_wide_cf))
for (i in seq_along(reg_99_wide_cf)) {
  reg_99_kappa_cf[[i]] <- reg_99_count_cf[[i]] <-
    vector("list", length(reg_99_wide_cf))
  for (j in seq_along(reg_99_wide_cf)) {
    cols <- names(reg_99_wide_cf)[c(i, j)]
    reg_99_kappa_cf[[i]][[j]] <-
      round(irr::kappa2(reg_99_wide_cf[, ..cols])$value, digits = 2)
    reg_99_count_cf[[i]][[j]] <-
      sum(reg_99_wide_cf[[i]] == reg_99_wide_cf[[j]])
  }
}

names(reg_99_wide_cf) <- reg_99_wide_cf %>%
  names() %>%
  str_replace("Rater", "R")

reg_99_kappa_cf <- setDT(lapply(reg_99_kappa_cf, unlist))
names(reg_99_kappa_cf) <- names(reg_99_wide_cf)

reg_99_count_cf <- setDT(lapply(reg_99_count_cf, unlist))
names(reg_99_count_cf) <- names(reg_99_wide_cf)


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
  labs(y = "Rater", fill = "Agreement") +
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
  labs(y = "Rater", fill = "Agreement") +
  theme_bw() +
  theme(text = element_text(size = 28),
    legend.position = "right")

ggsave(here("plots/heatmap_kappa_registration.png"), width = 18, height = 15)

# Heatmaps: Comment agreement partial
reg_99_count_cp[, Rater := rater_id]
reg_99_count_cp[] %>%
  melt(id.vars = "Rater", variable.factor = FALSE) %>%
  ggplot(aes(Rater, variable)) +
  geom_tile(aes(fill = value)) +
  geom_text(aes(label = value), size = 10) +
  scale_fill_gradient(low = "white", high = "red") +
  labs(y = "Rater", fill = "Agreement") +
  theme_bw() +
  theme(text = element_text(size = 28), legend.position = "left")

ggsave(here("plots/heatmap_count_regist_cp.png"), width = 18, height = 15)

reg_99_kappa_cp[, Rater := rater_id]
reg_99_kappa_cp[] %>%
  melt(id.vars = "Rater", variable.factor = FALSE) %>%
  ggplot(aes(Rater, variable)) +
  geom_tile(aes(fill = value)) +
  geom_text(aes(label = value), size = 10) +
  scale_fill_gradient(low = "white", high = "red") +
  labs(y = "Rater", fill = "Agreement") +
  theme_bw() +
  theme(text = element_text(size = 28),
    legend.position = "right")

ggsave(here("plots/heatmap_kappa_regist_cp.png"), width = 18, height = 15)

# Heatmaps: Comment agreement full
reg_99_count_cf[, Rater := rater_id]
reg_99_count_cf[] %>%
  melt(id.vars = "Rater", variable.factor = FALSE) %>%
  ggplot(aes(Rater, variable)) +
  geom_tile(aes(fill = value)) +
  geom_text(aes(label = value), size = 10) +
  scale_fill_gradient(low = "white", high = "red") +
  labs(y = "Rater", fill = "Agreement") +
  theme_bw() +
  theme(text = element_text(size = 28), legend.position = "left")

ggsave(here("plots/heatmap_count_regist_cf.png"), width = 18, height = 15)

reg_99_kappa_cf[, Rater := rater_id]
reg_99_kappa_cf[] %>%
  melt(id.vars = "Rater", variable.factor = FALSE) %>%
  ggplot(aes(Rater, variable)) +
  geom_tile(aes(fill = value)) +
  geom_text(aes(label = value), size = 10) +
  scale_fill_gradient(low = "white", high = "red") +
  labs(y = "Rater", fill = "Agreement") +
  theme_bw() +
  theme(text = element_text(size = 28),
    legend.position = "right")

ggsave(here("plots/heatmap_kappa_regist_cf.png"), width = 18, height = 15)

# Pie charts
ratings1 <- reg_99[Session == "Consensus", .(Image, Rating, Rater1 = "*")]

reg_99_n <- reg_99[! Session %in% c("First", "Second"),
                   .N, by = .(Image, Rating)]
reg_99_n <- ratings1[reg_99_n, on = .(Image, Rating)]
reg_99_n[is.na(Rater1), Rater1 := ""]

reg_99_pass <- reg_99_n[Rating == "Pass"]
setorder(reg_99_pass, cols = "N")
setorder(reg_99_pass, cols = "Rater1")

reg_99_n$Image <- factor(reg_99_n$Image,
    levels = c(reg_99_n[Rating == "Fail" & N == 9, Image],
        reg_99_pass[, unique(Image)]))

reg_99_n %>%
  ggplot(aes(x = "", y = N, fill = Rating)) +
  geom_col() + coord_polar(theta = "y") +
  geom_text(aes(label = Rater1), position = position_stack(vjust = 0.5), size = 9) +
  scale_fill_manual(values = c("#9d0000", "#028202")) +
  facet_wrap(vars(Image), ncol = 11) +
  ggtitle("Agreement: Registration dataset") +
  theme_void() +
  theme(text = element_text(size = 32),
    strip.text.x = element_blank(),
    legend.position = "bottom")

ggsave(here("plots/pies_agreement_registration.png"), width = 18, height = 15)

# Number of images with full agreement
reg_99_n[N == 9, .N, by = Rating] #28:99
reg_99_n[N >= 7, .N, by = Rating] #68:99

# Pie charts: partial comment agreement
reg_99_comm <- reg_99_comm[reg_99, on = .(Image, Rater)]
reg_99_cp <- copy(reg_99_comm)
reg_99_cp[Comm_Agree %in% c("*", "**") & Rating == "Pass", Rating := "Fail"]
reg_99_ncp <- reg_99_cp[! Session %in% c("First", "Second"),
                          .N, by = .(Image, Rating)]
reg_99_ncp <- ratings1[reg_99_ncp, on = .(Image, Rating)]
reg_99_ncp[is.na(Rater1), Rater1 := ""]

reg_99_pass_cp <- reg_99_ncp[Rating == "Pass"]
setorder(reg_99_pass_cp, cols = "N")
setorder(reg_99_pass_cp, cols = "Rater1")

reg_99_ncp$Image <- factor(reg_99_ncp$Image,
  levels = c(reg_99_ncp[Rating == "Fail" & N == 9, Image],
             reg_99_pass_cp[, unique(Image)]))

reg_99_ncp %>%
  ggplot(aes(x = "", y = N, fill = Rating)) +
  geom_col() + coord_polar(theta = "y") +
  geom_text(aes(label = Rater1), position = position_stack(vjust = 0.5), size = 9) +
  scale_fill_manual(values = c("#9d0000", "#028202")) +
  facet_wrap(vars(Image), ncol = 11) +
  ggtitle("Agreement: Registration dataset (Partial comments)") +
  theme_void() +
  theme(text = element_text(size = 32),
    strip.text.x = element_blank(),
    legend.position = "bottom")

ggsave(here("plots/pies_agreement_regist_cp.png"), width = 18, height = 15)

# Pie charts: full comment agreement
reg_99_cf <- copy(reg_99_comm)
reg_99_cf[Comm_Agree == "**" & Rating == "Pass", Rating := "Fail"]
reg_99_ncf <- reg_99_cf[! Session %in% c("First", "Second"),
                          .N, by = .(Image, Rating)]
reg_99_ncf <- ratings1[reg_99_ncf, on = .(Image, Rating)]
reg_99_ncf[is.na(Rater1), Rater1 := ""]

reg_99_pass_cf <- reg_99_ncf[Rating == "Pass"]
setorder(reg_99_pass_cf, cols = "N")
setorder(reg_99_pass_cf, cols = "Rater1")

reg_99_ncf$Image <- factor(reg_99_ncf$Image,
  levels = c(reg_99_ncf[Rating == "Fail" & N == 9, Image],
             reg_99_pass_cf[, unique(Image)]))

reg_99_ncf %>%
  ggplot(aes(x = "", y = N, fill = Rating)) +
  geom_col() + coord_polar(theta = "y") +
  geom_text(aes(label = Rater1), position = position_stack(vjust = 0.5), size = 9) +
  scale_fill_manual(values = c("#9d0000", "#028202")) +
  facet_wrap(vars(Image), ncol = 11) +
  ggtitle("Agreement: Registration dataset (Full comments)") +
  theme_void() +
  theme(text = element_text(size = 32),
    strip.text.x = element_blank(),
    legend.position = "bottom")

ggsave(here("plots/pies_agreement_regist_cf.png"), width = 18, height = 15)


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
