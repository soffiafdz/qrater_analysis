## Packages
library("here")
library("data.table")
library("magrittr")
library("purrr")
library("stringr")
library("readr")
library("ggplot2")
library("gridExtra")

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

reg_99_bin_1 <- reg_99_bin[Rater == "Rater01"]
reg_99_wide_1 <- dcast(reg_99_bin_1,
                       Image ~ Session,
                       value.var = "Rating")[, -1]

irr::kappa2(reg_99_wide_1)
# K=0.873, z=8.69
sum(reg_99_wide_1[[1]] == reg_99_wide_1[[2]])
# 93/99

## Inter-rater agreement
# Matrix of comments
setorder(reg_99, cols = "Rater")
reg_99_comments <- reg_99[Session == 1, .(Image, Rater, Comment)] %>%
  dcast(Image ~ Rater, value.var = "Comment")
reg_99_comments[1]
# Matrix of ratings
reg_99_wide <- dcast(reg_99_bin[Session == 1], Image ~ Rater, value.var = "Rating")[, -1]

# Calculate Cohen's Kappas & Raw agreement
reg_99_kappa <- vector("list", length(reg_99_wide))
reg_99_count <- vector("list", length(reg_99_wide))
for (i in seq_along(reg_99_wide)) {
  reg_99_kappa[[i]] <- vector("list", length(reg_99_wide))
  reg_99_count[[i]] <- vector("list", length(reg_99_wide))
  for (j in seq_along(reg_99_wide)) {
    cols <- names(reg_99_wide)[c(i, j)]
    reg_99_kappa[[i]][[j]] <- round(irr::kappa2(reg_99_wide[, ..cols])$value,
      digits = 2)
    reg_99_count[[i]][[j]] <- sum(reg_99_wide[[i]] == reg_99_wide[[j]])
  }
}

# names(reg_99_wide) <- reg_99_wide %>%
#   names() %>%
#   str_replace("Rater", "R")

reg_99_kappa <- setDT(lapply(reg_99_kappa, unlist))
names(reg_99_kappa) <- names(reg_99_wide)

reg_99_count <- setDT(lapply(reg_99_count, unlist))
names(reg_99_count) <- names(reg_99_wide)


## Plots
# Heatmap
rater_id <- names(reg_99_wide)

reg_99_count[, Rater := rater_id]
reg_99_count[] %>%
  melt(id.vars = "Rater", variable.factor = FALSE) %>%
  ggplot(aes(Rater, variable)) +
  geom_tile(aes(fill = value)) +
  geom_text(aes(label = value)) +
  scale_fill_gradient(low = "white", high = "red") +
  labs(y = "Rater", fill = "Agreement") +
  theme_bw() +
  theme(text = element_text(size = 16),
    legend.position = "none")

ggsave(here("plots/heatmap_count_registration.png"), width = 12, height = 10)

reg_99_kappa[, Rater := rater_id]
reg_99_kappa[] %>%
  melt(id.vars = "Rater", variable.factor = FALSE) %>%
  ggplot(aes(Rater, variable)) +
  geom_tile(aes(fill = value)) +
  geom_text(aes(label = value)) +
  scale_fill_gradient(low = "white", high = "red") +
  labs(y = "Rater", fill = "Agreement") +
  theme_bw() +
  theme(text = element_text(size = 23),
    legend.position = "none")

ggsave(here("plots/heatmap_kappa_registration.png"), width = 15, height = 9)

# Pie charts

#ratings1 <- reg_99[Rater == "Rater01" & Session == 1,
                   #.(Image, Rating, Rater1 = "*")]
ratings1 <- reg_99[Rater == "Rater01", .N, by = .(Image, Rating)]
ratings1[N == 2, Rater1 := "**"]
ratings1[N == 1, Rater1 := "*"]
ratings1[, N := NULL]

reg_99_n <- reg_99[, .N, by = .(Image, Rating)]
reg_99_n <- ratings1[reg_99_n, on = .(Image, Rating)]
reg_99_n[is.na(Rater1), Rater1 := ""]

reg_99_pass <- reg_99_n[Rating == "Pass"]
setorder(reg_99_pass, cols = "N")
setorder(reg_99_pass, cols = "Rater1")

reg_99_n$Image <- factor(reg_99_n$Image,
    levels = c(reg_99_n[Rating == "Fail" & N > 8, Image],
        reg_99_pass[, unique(Image)]))

reg_99_n %>%
  ggplot(aes(x = "", y = N, fill = Rating)) +
  geom_col() + coord_polar(theta = "y") +
  geom_text(aes(label = Rater1), position = position_stack(vjust = 0.5)) +
  scale_fill_manual(values = c("#9d0000", "#028202")) +
  facet_wrap(vars(Image), ncol = 10) +
  ggtitle("Agreement: Registration dataset") +
  theme_void() +
  theme(text = element_text(size = 23),
    strip.text.x = element_blank(),
    legend.position = "bottom")

ggsave(here("plots/pies_agreement_registration.png"), width = 12, height = 10)

# Number of images with full agreement
reg_99_n[N == 9, .N, by = Rating] #28:99
reg_99_n[N >= 7, .N, by = Rating] #68:99

## CSV
write_csv(reg_99_comments, here("data/derivatives/reg_99_comments.csv"))
write_csv(reg_99_wide, here("data/derivatives/reg_99_wide.csv"))
