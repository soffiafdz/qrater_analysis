#!/usr/bin/env Rscript

## Packages
library("here")
library("data.table")
library("irr")
library("purrr")
library("stringr")
library("readr")
library("ggplot2")
library("ggforce")
library("cowplot")
library("flextable")

## Acquisition 338
# Data.table
fpath <- here("data/derivatives/acquisition_388_dt.rds")
if (!file.exists(fpath)) {
  source(here("scripts/data_acquisition388.R"))
} else {
  acq_388 <- read_rds(fpath)
}
rm(fpath)

## Intra-rater agreement
kappa2(acq_388[, .(Expert1, Expert2)])
# Kappa = 0.401, Z = 9.83

## Acquisition 99
# Data.table
fpath <- here("data/derivatives/acquisition_99_dt.rds")
if (!file.exists(fpath)) {
  source(here("scripts/data_acquisition99.R"))
} else {
  acq_99 <- read_rds(fpath)
}
rm(fpath)

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
kappa2(acq_99_wide_1[, -1])
# Kappa = 0.81, Z = 8.14

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

# Calculate Fleiss' Kappa
kappam.fleiss(acq_99_wide)
# Kappa = 0.439; Z = 32.4

# Calculate Count for Raw agreement
acq_99_corr <- acq_99_count <- vector("list", length(acq_99_wide))
for (i in seq_along(acq_99_wide)) {
  acq_99_corr[[i]] <- acq_99_count[[i]] <- vector("list", length(acq_99_wide))
  for (j in seq_along(acq_99_wide)) {
    acq_99_count[[i]][[j]] <- sum(acq_99_wide[[i]] == acq_99_wide[[j]])
    acq_99_corr[[i]][[j]] <- cor(acq_99_wide[[i]], acq_99_wide[[j]],
                                 method = "spearman")
  }
}

names(acq_99_wide) <- acq_99_wide |>
  names() |>
  str_replace("Rater", "R") |>
  str_replace("Expert", "E")

acq_99_count <- setDT(lapply(acq_99_count, unlist))
acq_99_count[upper.tri(acq_99_count, diag = TRUE)] <- NA
names(acq_99_count) <- names(acq_99_wide)
write_rds(acq_99_count, here("data/derivatives/qc_count_acq_dt.rds"))

acq_99_corr <- setDT(lapply(acq_99_corr, unlist))
acq_99_corr[upper.tri(acq_99_corr, diag = TRUE)] <- NA
names(acq_99_corr) <- names(acq_99_wide)

## Off Qrater QC
# Rater 1:
path        <- "data/raw/qc_ratings/raw/off_qrater"
acq_99_off1 <- fread(here(path, "OffQrater_Raw_Rater.csv"))

acq_99_off1 <- acq_99[Rater == "Rater01", .(Image, Prev = Rating)
                      ][acq_99_off1, on = "Image", .(Image, Prev, Off = QC)]

acq_99_off1 <- acq_99[Session == "Consensus", .(Image, GS = Rating)
                      ][acq_99_off1, on = "Image",
                      .(Image, Prev, GS, Off)
                      ][, `:=`(Off = fifelse(Off == "Pass", 1, 0),
                               Prev = fifelse(Prev == "Pass", 1, 0),
                               GS = fifelse(GS == "Pass", 1, 0))]

# Expert1
acq_99_off2 <- fread(here(path, "OffQrater_Raw_Expert.csv"),
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
kappa2(acq_99_off1[, .(Off, Prev)])

# Inter-rater agreement
acq_99_off <- acq_99_off2[acq_99_off1,
                          on = "Image",
                          .(Image, R1 = Off, E1 = i.Off)]

acq_99_off[E1 == R1, .N]
irr::kappa2(acq_99_off[, .(R1, E1)])

## Plots
# Heatmap
acq_99_count[, Rater := names(acq_99_wide)]
acq_99_count_l <- melt(acq_99_count, id.vars = "Rater",
                       variable.factor = FALSE, na.rm = TRUE,
                       variable.name = "Rater2", value.name = "Count")
acq_99_count_l[, Perc := sprintf("%.0f%%", Count / 99 * 100)]

p_count <- ggplot(acq_99_count_l, aes(Rater, Rater2)) +
  theme_classic() +
  theme(text = element_text(size = 14), legend.position = "right") +
  geom_tile(aes(fill = Count)) + geom_text(aes(label = Perc), size = 3.5) +
  scale_fill_gradient(low = "white", high = "#028202") +
  labs(y = "Rater", fill = "Agreement\n(Percentage)")

ggsave("plots/raw_tiles-agreement.png",
       width = 8, height = 8, units = "in", dpi = 600)

acq_99_corr[, Rater := names(acq_99_wide)]
acq_99_corr_l <- melt(acq_99_corr, id.vars = "Rater",
                      variable.factor = FALSE, na.rm = TRUE,
                      variable.name = "Rater2", value.name = "Corr")
acq_99_corr_l[, Corr := round(Corr, 2)]

p_corr <- ggplot(acq_99_corr_l, aes(Rater, Rater2)) +
  theme_classic() +
  theme(text = element_text(size = 14), legend.position = "right") +
  geom_tile(aes(fill = Corr)) + geom_text(aes(label = Corr), size = 3.5) +
  scale_fill_gradient(low = "white", high = "#028202") +
  labs(y = "Rater", fill = "Agreement\n(Correlation)")

ggsave("plots/raw_tiles-agreement2.png",
       width = 8, height = 8, units = "in", dpi = 600)

# Bar charts
acq_agree   <- acq_99[Session == "First", .(Image, First = Rating)
                      ][acq_99[Session == "Second",
                               .(Image, Second = Rating)],
                      on = "Image"
                      ][First == Second, .(Image, Expert = "*")]

acq_exp1    <- acq_agree[acq_99[Session == "Consensus", .(Image, Rating)],
                         on = "Image"]
acq_exp1[is.na(Expert), Expert := "†"]

acq_exp1[Expert == "*", GS := Rating]
acq_exp1[Expert == "†" & Rating == "Fail", GS := "rFail"]
acq_exp1[Expert == "†" & Rating == "Pass", GS := "rPass"]

acq_99_n <- acq_99[Rater != "Expert01", .N, .(Orig_QC, Image, Rating)]
acq_99_plot <- acq_exp1[, .(Image, GS)][acq_99_n, on = "Image"]
acq_99_plot[, `:=`(Image = factor(Image,
                                  levels = c(acq_99_plot[GS %like% "Pass"
                                                         ][Rating == "Pass"
                                                         ][order(N), Image],
                                             acq_99_plot[GS %like% "Fail"
                                                         ][Rating == "Fail"
                                                         ][order(-N), Image])),
                   GS = factor(GS, levels = c("Pass", "rPass", "Fail", "rFail"),
                               labels = sprintf("Expert: %s",
                                                c("Pass",
                                                  "Pass (after revision)",
                                                  "Fail",
                                                  "Fail (after revision)"))))]

p_pass  <- ggplot(acq_99_plot[Orig_QC == "Pass"],
                  aes(Image, N, fill = Rating)) +
  theme_classic() +
  theme(text = element_text(size = 9),
        #axis.text.y = element_blank(),
        #axis.ticks.y = element_blank(),
        legend.position = "left") +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_fill_manual(values = c("#9d0000", "#028202")) +
  scale_y_continuous(breaks = 1:9) +
  scale_x_discrete(labels = 1:99) +
  facet_col(vars(GS), scales = "free", space = "free") +
  labs(title = 'Selected "Pass" Cases',
       x = "Cases", y = "Ratings",
       fill = "Trainees\nratings")

p_bline  <- ggplot(acq_99_plot[Orig_QC == "Borderline"],
                   aes(Image, N, fill = Rating)) +
  theme_classic() +
  theme(text = element_text(size = 9),
        #axis.text.y = element_blank(),
        #axis.ticks.y = element_blank(),
        legend.position = "none") +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_fill_manual(values = c("#9d0000", "#028202")) +
  scale_y_continuous(breaks = 1:9) +
  scale_x_discrete(labels = 1:99) +
  facet_col(vars(GS), scales = "free", space = "free") +
  labs(title = 'Selected Borderline Cases',
       x = "Cases", y = "Ratings")

p_fail  <- ggplot(acq_99_plot[Orig_QC == "Fail"], aes(Image, N, fill = Rating)) +
  theme_classic() +
  theme(text = element_text(size = 9),
        #axis.text.y = element_blank(),
        #axis.ticks.y = element_blank(),
        legend.position = "none") +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_fill_manual(values = c("#9d0000", "#028202")) +
  scale_y_continuous(breaks = 1:9) +
  scale_x_discrete(labels = 1:99) +
  facet_col(vars(GS), scales = "free", space = "free") +
  labs(title = 'Selected "Fail" Cases',
       x = "Cases", y = "Ratings")

plot_grid(p_pass, p_bline, p_fail, nrow = 1, rel_widths = c(1.3, 1, 1))
#plot_grid(p_pass,
          #plot_grid(p_fail, p_rev_p, p_rev_f,
                    #nrow = 3, rel_heights = c(2, 1, 1)),
                    #rel_widths = c(1.3, 1))

ggsave("plots/raw_bars-agreement.png",
       width = 8, height = 8, units = "in", dpi = 600)

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

kappa2(reg_99_wide_1[, -1])
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

# Calculate Fleiss' Kappa
kappam.fleiss(reg_99_wide)
# Kappa = 0.563; Z = 33.6

# Calculate Cohen's Kappas & Raw agreement
reg_99_corr <- reg_99_count <- vector("list", length(reg_99_wide))
for (i in seq_along(reg_99_wide)) {
  reg_99_corr[[i]] <- reg_99_count[[i]] <- vector("list", length(reg_99_wide))
  for (j in seq_along(reg_99_wide)) {
    reg_99_count[[i]][[j]] <- sum(reg_99_wide[[i]] == reg_99_wide[[j]])
    reg_99_corr[[i]][[j]] <- cor(reg_99_wide[[i]], reg_99_wide[[j]],
                                 method = "spearman")
  }
}

names(reg_99_wide) <- reg_99_wide |>
  names() |>
  str_replace("Rater", "R") |>
  str_replace("Expert", "E")

reg_99_count <- setDT(lapply(reg_99_count, unlist))
reg_99_count[upper.tri(reg_99_count, diag = TRUE)] <- NA
names(reg_99_count) <- names(reg_99_wide)
write_rds(reg_99_count, here("data/derivatives/qc_count_reg_dt.rds"))

reg_99_corr <- setDT(lapply(reg_99_corr, unlist))
reg_99_corr[upper.tri(reg_99_corr, diag = TRUE)] <- NA
names(reg_99_corr) <- names(reg_99_wide)

## Off Qrater QC
# Rater1
path        <- "data/raw/qc_ratings/registration/off_qrater"
reg_99_off1 <- fread(here(path, "OffQrater_Linear_Rater.csv"))

reg_99_off1 <- reg_99[Rater == "Rater01", .(Image, Prev = Rating)
                      ][reg_99_off1, on = "Image", .(Image, Prev, Off = QC)]

reg_99_off1 <- reg_99[Session == "Consensus", .(Image, GS = Rating)
                      ][reg_99_off1, on = "Image",
                      .(Image, Prev, GS, Off)
                      ][, `:=`(Off = fifelse(Off == "Pass", 1, 0),
                               Prev = fifelse(Prev == "Pass", 1, 0),
                               GS = fifelse(GS == "Pass", 1, 0))]

# Expert1
reg_99_off2 <- fread(here(path, "OffQrater_Linear_Expert.csv"),
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
reg_99_count[, Rater := names(reg_99_wide)]
reg_99_count_l  <- melt(reg_99_count, id.vars = "Rater",
                        variable.factor = FALSE, na.rm = TRUE,
                        variable.name = "Rater2", value.name = "Count")
reg_99_count_l[, Perc := sprintf("%.0f%%", Count / 99 * 100)]

p_count <- ggplot(reg_99_count_l, aes(Rater, Rater2)) +
  theme_classic() +
  theme(text = element_text(size = 14), legend.position = "right") +
  geom_tile(aes(fill = Count)) + geom_text(aes(label = Perc), size = 3.5) +
  scale_fill_gradient(low = "white", high = "#028202") +
  labs(y = "Rater", fill = "Agreement\n(Percentage)")

ggsave("plots/lreg_tiles-agreement.png",
       width = 8, height = 8, units = "in", dpi = 600)

reg_99_corr[, Rater := names(reg_99_wide)]
reg_99_corr_l  <- melt(reg_99_corr, id.vars = "Rater",
                        variable.factor = FALSE, na.rm = TRUE,
                        variable.name = "Rater2", value.name = "Corr")
reg_99_corr_l[, Corr := round(Corr, 2)]

p_corr <- ggplot(reg_99_corr_l, aes(Rater, Rater2)) +
  theme_classic() +
  theme(text = element_text(size = 14), legend.position = "right") +
  geom_tile(aes(fill = Corr)) + geom_text(aes(label = Corr), size = 3.5) +
  scale_fill_gradient(low = "white", high = "#028202") +
  labs(y = "Rater", fill = "Agreement\n(Correlation)")

ggsave("plots/lreg_tiles-agreement2.png",
       width = 8, height = 8, units = "in", dpi = 600)

# Bar charts
reg_agree   <- reg_99[Session == "First", .(Image, First = Rating)
                      ][reg_99[Session == "Second",
                               .(Image, Second = Rating)],
                      on = "Image"
                      ][First == Second, .(Image, Expert = "*")]

reg_exp1    <- reg_agree[reg_99[Session == "Consensus", .(Image, Rating)],
                         on = "Image"]
reg_exp1[is.na(Expert), Expert := "†"]

reg_exp1[Expert == "*", GS := Rating]
reg_exp1[Expert == "†" & Rating == "Fail", GS := "rFail"]
reg_exp1[Expert == "†" & Rating == "Pass", GS := "rPass"]

reg_99_n <- reg_99[Rater != "Expert01", .N, .(Orig_QC, Image, Rating)]
reg_99_plot <- reg_exp1[, .(Image, GS)][reg_99_n, on = "Image"]
reg_99_plot[, `:=`(Image = factor(Image,
                                  levels = c(reg_99_plot[GS %like% "Pass"
                                                         ][Rating == "Pass"
                                                         ][order(N), Image],
                                             reg_99_plot[GS %like% "Fail"
                                                         ][Rating == "Fail"
                                                         ][order(-N), Image])),
                   GS = factor(GS, levels = c("Pass", "rPass", "Fail", "rFail"),
                               labels = sprintf("Expert: %s",
                                                c("Pass",
                                                  "Pass (after revision)",
                                                  "Fail",
                                                  "Fail (after revision)"))))]

p_pass  <- ggplot(reg_99_plot[Orig_QC == "Pass"],
                  aes(Image, N, fill = Rating)) +
  theme_classic() +
  theme(text = element_text(size = 9),
        #axis.text.y = element_blank(),
        #axis.ticks.y = element_blank(),
        legend.position = "left") +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_fill_manual(values = c("#9d0000", "#028202")) +
  scale_y_continuous(breaks = 1:8) +
  scale_x_discrete(labels = 1:99) +
  facet_col(vars(GS), scales = "free", space = "free") +
  labs(title = 'Selected "Pass" Cases',
       x = "Cases", y = "Ratings",
       fill = "Trainees\nratings")

p_bline  <- ggplot(reg_99_plot[Orig_QC == "Borderline"],
                  aes(Image, N, fill = Rating)) +
  theme_classic() +
  theme(text = element_text(size = 9),
        #axis.text.y = element_blank(),
        #axis.ticks.y = element_blank(),
        legend.position = "none") +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_fill_manual(values = c("#9d0000", "#028202")) +
  scale_y_continuous(breaks = 1:8) +
  scale_x_discrete(labels = 1:99) +
  facet_col(vars(GS), scales = "free", space = "free") +
  labs(title = 'Selected Borderline Cases',
       x = "Cases", y = "Ratings")

p_fail  <- ggplot(reg_99_plot[Orig_QC == "Fail"],
                  aes(Image, N, fill = Rating)) +
  theme_classic() +
  theme(text = element_text(size = 9),
        #axis.text.y = element_blank(),
        #axis.ticks.y = element_blank(),
        legend.position = "none") +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_fill_manual(values = c("#9d0000", "#028202")) +
  scale_y_continuous(breaks = 1:8) +
  scale_x_discrete(labels = 1:99) +
  facet_col(vars(GS), scales = "free", space = "free") +
  labs(title = 'Selected "Fail" Cases',
       x = "Cases", y = "Ratings")

plot_grid(p_pass, p_bline, p_fail, nrow = 1, rel_widths = c(1.3, 1, 1))

ggsave("plots/lreg_bars-agreement.png",
       width = 8, height = 8, units = "in", dpi = 600)


# Number of images with full agreement
reg_99_n[N == 9, .N, by = Rating] #28:99
reg_99_n[N >= 7, .N, by = Rating] #68:99


## Pre/post protocol meeting agreement
fnames <- sprintf("registration_%s.rds", c("dt", "premeeting_dt"))
fpaths <- here("data/derivatives", fnames)

if (!file.exists(fpaths[1])) {
  source(here("scripts/data_registration.R"))
} else {
    reg_post <- read_rds(fpaths[1])
}

if (!file.exists(fpaths[2])) {
  source(here("scripts/data_registration_premeeting.R"))
} else {
    reg_pre <- read_rds(fpaths[2])
}

rm(fnames, fpaths)

reg_pre[, Timestamp := NULL]
reg_pre <- reg_pre[!Rating == "Pending"]
#reg_pre <- reg_pre[!Rating %in% c("Warning", "Pending")]
setnames(reg_pre, "Rating", "Pre")

reg_post <- reg_post[reg_post[, .I[Timestamp == max(Timestamp)],
                              .(Image, Rater)]$V1]
reg_post[, c("Comment", "Diff", "Timestamp") := NULL]
setnames(reg_post, "Rating", "Post")

reg_pre_n <- reg_pre[, .N, .(Rater, Pre)] |>
               dcast(Rater ~ Pre, value.var = "N")
reg_pre_avg <- reg_pre_n[, lapply(.SD, mean, na.rm = TRUE), .SDcols = 2:4]
reg_pre_avg[, Rater := "Average"]

reg_pre_n <- rbindlist(list(reg_pre_n, reg_pre_avg), use.names = TRUE)
reg_pre_n[is.na(Warning), Warning := 0]
reg_pre_n[, Total := Pass + Fail + Warning]
setnames(reg_pre_n, 2:5, str_c("pre", names(reg_pre_n)[-1]))
#rm(reg_pre_n, reg_pre_avg)

reg_pre_post <- unique(reg_post[reg_pre, on = .(Image, Rater)])

reg_post_n <- reg_pre_post[, .N, .(Rater, Post)] |>
                dcast(Rater ~ Post, value.var = "N")
reg_post_avg <- reg_post_n[, lapply(.SD, mean, na.rm = TRUE), .SDcols = 2:3]
reg_post_avg[, Rater := "Average"]
reg_post_n <- rbindlist(list(reg_post_n, reg_post_avg), use.names = TRUE)
reg_post_n[, Total := Pass + Fail]
setnames(reg_post_n, 2:4, str_c("post", names(reg_post_n)[-1]))

reg_meeting_n <- reg_pre_n[reg_post_n, on = "Rater"]
#rm(reg_pre, reg_pre_n, reg_pre_avg, reg_post, reg_post_n, reg_post_avg)

reg_change <- reg_pre_post[Pre != "Warning"
                           ][Post == Pre, .(Agreed = .N), Rater
                           ][reg_pre_n, on = "Rater",
                           .(Rater, N = (preTotal - preWarning) - Agreed)]
reg_change[Rater == "Average", N := reg_change[-.N, mean(N)]]

reg_meeting <- reg_change[reg_meeting_n, on = "Rater",
                      .(Rater = str_replace(Rater, "0", "-"),
                        Before_Pass = sprintf("%.0f (%.0f%%)", prePass,
                                              prePass / preTotal * 100),
                        Before_Warning = sprintf("%.0f (%.0f%%)", preWarning,
                                                 preWarning / preTotal * 100),
                        Before_Fail = sprintf("%.0f (%.0f%%)", preFail,
                                                 preFail / preTotal * 100),
                        After_Pass = sprintf("%.0f (%.0f%%)", postPass,
                                              postPass / postTotal * 100),
                        After_Fail = sprintf("%.0f (%.0f%%)", postFail,
                                                 postFail / postTotal * 100),
                        Change = sprintf("%.0f (%.0f%%)", N,
                                         N / (preTotal - preWarning) * 100))]

reg_meeting |>
  flextable() |>
  separate_header() |>
  labelizor(part = "header", labels = c("Change" = "Images changed")) |>
  bold(part = "header") |>
  italic(~ Rater == "Average") |>
  hline(i = ~ before(Rater, "Average"),
        border = fp_border_default(width = 2)) |>
  autofit() |>
  save_as_docx(path = "data/derivatives/agreement_pre-post_meeting.docx")

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
