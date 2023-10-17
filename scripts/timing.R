#!/usr/bin/env Rscript

## Packages
library("here")
library("data.table")
library("readr")
library("stringr")
library("lubridate")
library("hms")
library("ggplot2")
library("flextable")
library("dunn.test")
library("rstatix")

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

acq_99[Session == "First" & Rater == "Expert01", Rater := "Expert-1 (S1)"]
acq_99[Session == "Second" & Rater == "Expert01", Rater := "Expert-1 (S2)"]
acq_99 <- acq_99[Rater != "Expert01"]

#acq_99_N <- acq_99[, .N, by = .(Rater, Rating)]
#acq_99_N[, .(X = mean(N), SD = sd(N)), by = Rating]

acq_99[Diff > duration(15, "minutes"), Rest := TRUE]
acq_99_time <- acq_99[, .(Image, Rater = str_replace(Rater, "0", "-"),
                          Rating, Diff, Rest)]
acq_99_time[!is.na(Rest), Diff := NA]
write_rds(acq_99_time, here("data/derivatives/acq_99_time_dt.rds"))

# Session and Pass vs Fail (Wilcoxon test)
acq_99_summ1 <-
  acq_99_time[, .(N = .N,
                  Rests = sum(Rest, na.rm = TRUE),
                  Session = as_hms(sum(Diff, na.rm = TRUE)),
                  PvF = suppressWarnings(
                          wilcox.test(as.numeric(Diff) ~ Rating,
                                      na.action = na.omit))$p.value), Rater]

acq_99_all1 <- acq_99_summ1[, .(Rater = "All",
                                N = 99,
                                Rests = round(mean(Rests)),
                                Session = as_hms(round(mean(Session))))]
acq_99_all1[, PvF := acq_99_time[, wilcox.test(as.numeric(Diff) ~ Rating,
                                               na.action = na.omit)$p.value]]

acq_99_summ1 <- rbindlist(list(acq_99_summ1, acq_99_all1), use.names = TRUE)

acq_99_summ2 <- acq_99_time[, .(N = .N,
                               Median = as_hms(median(Diff, na.rm = TRUE)),
                               Sd = hms(sd(Diff, na.rm = TRUE))),
                           .(Rater, Rating)] |>
               dcast(Rater ~ Rating, value.var = c("N", "Median", "Sd"))

acq_99_all2 <- acq_99_time[, .(Rater = "All",
                               Median = as_hms(median(Diff, na.rm = TRUE)),
                               Sd = hms(sd(Diff, na.rm = TRUE))), Rating] |>
               dcast(Rater ~ Rating, value.var = c("Median", "Sd"))

acq_99_all2 <- cbind(acq_99_all2,
                     acq_99_summ2[, lapply(.SD, mean), .SDcols = 2:3])

acq_99_summ2 <- rbindlist(list(acq_99_summ2, acq_99_all2), use.names = TRUE)

time_format <- "%02d:%02.0f (%02d:%02.0f)"
acq_99_summ <- acq_99_summ1[acq_99_summ2, on = "Rater",
                            .(Rater, Session, Rests,
                              Pass_N = round(N_Pass),
                              Pass_Time = sprintf(time_format,
                                                  minute(Median_Pass),
                                                  second(Median_Pass),
                                                  minute(Sd_Pass),
                                                  second(Sd_Pass)),
                              Fail_N = round(N_Fail),
                              Fail_Time = sprintf(time_format,
                                                  minute(Median_Fail),
                                                  second(Median_Fail),
                                                  minute(Sd_Fail),
                                                  second(Sd_Fail)),
                              p = round(PvF, digits = 3))]

rm(acq_99, acq_99_summ1, acq_99_all1, acq_99_summ2, acq_99_all2)

acq_99_summ |>
  flextable() |>
  separate_header() |>
  labelizor(part = "header", labels = c("Session" = "Total Time",
                                        "p" = "p-value")) |>
  bold(part = "header") |>
  italic(part = "header", j = "p") |>
  italic(~ Rater == "All") |>
  hline(i = ~ before(Rater, "All"), border = fp_border_default(width = 2)) |>
  bold(~ p < 0.05, j = "p") |>
  footnote(part = "header", i = 1, j = 2,
           value = as_paragraph("HH:MM:SS"), ref_symbols = "1") |> #, inline = TRUE) |>
  footnote(part = "header", i = c(1, 2, 2), j = c(3, 4, 6),
           value = as_paragraph("n"), ref_symbols = "2") |> #, inline = TRUE) |>
  footnote(part = "header", i = 2, j = c(5, 7),
           value = as_paragraph("Median (SD): MM:SS"), ref_symbols = "3") |> #, inline = TRUE) |>
  footnote(part = "header", i = 1, j = 8,
           value = as_paragraph("Wilcoxon rank-sum test"), ref_symbols = "4") |> #, inline = TRUE) |>
  footnote(~ Rater == "All", j = c(2:4, 6), value = as_paragraph("Average"),
           ref_symbols = "5") |> #, inline = TRUE) |>
  autofit() |>
  save_as_docx(path = "data/derivatives/acq_99_time.docx")

# Data.table ADNI
fpath <- here("data/derivatives/acquisition_dt.rds")

if (!file.exists(fpath)) {
  source(here("scripts/data_acquisition.R"))
} else {
  acquis <- read_rds(fpath)
}

rm(fpath)

acquis[Diff > duration(15, "minutes"), Rest := TRUE]
acquis_time <- acquis[, .(Image, Rater = str_replace(Rater, "0", "-"),
                          Rating, Diff, Rest)]
acquis_time[!is.na(Rest), Diff := NA]
write_rds(acquis_time, here("data/derivatives/acquis_time_dt.rds"))

#Session and Pass vs Warning vs Fail (Kruskal-Wallis rank sum test)
acquis_summ1 <-
  acquis_time[, .(N = .N,
                  Rests = sum(Rest, na.rm = TRUE),
                  Session = as_hms(sum(Diff, na.rm = TRUE)),
                  PvWvF = suppressWarnings(
                            kruskal.test(as.numeric(Diff) ~ Rating,
                                         na.action = na.omit))$p.value), Rater]

# Wilcox for Expert-1 and Rater-4 who don't have Warnings
no_w_raters <- c("Expert-1", "Rater-4")
acquis_wilcox <- acquis_time[Rater %in% no_w_raters,
                             .(PvWvF = wilcox.test(as.numeric(Diff) ~ Rating,
                                                   na.action = na.omit)$p.value),
                             Rater]

acquis_summ1[Rater %in% no_w_raters, PvWvF := acquis_wilcox$PvWvF]

acquis_all1 <- acquis_summ1[, .(Rater = "All", N = sum(N),
                                Rests = round(mean(Rests)),
                                Session = as_hms(round(mean(Session))))]
acquis_all1[, PvWvF := acquis_time[, kruskal.test(as.numeric(Diff) ~ Rating,
                                                  na.action = na.omit)$p.value]]

acquis_summ1 <- rbindlist(list(acquis_summ1[order(Rater)], acquis_all1),
                          use.names = TRUE)

acquis_summ2 <- acquis_time[, .(N = .N,
                                Median = as_hms(median(Diff, na.rm = TRUE)),
                                Sd = hms(sd(Diff, na.rm = TRUE))),
                            .(Rater, Rating)] |>
                  dcast(Rater ~ Rating, value.var = c("N", "Median", "Sd"))

acquis_all2 <- acquis_time[, .(Rater = "All", N = .N,
                               Median = as_hms(median(Diff, na.rm = TRUE)),
                               Sd = hms(sd(Diff, na.rm = TRUE))), Rating] |>
                dcast(Rater ~ Rating, value.var = c("N", "Median", "Sd"))

acquis_summ2 <- rbindlist(list(acquis_summ2, acquis_all2), use.names = TRUE)

acquis_summ <- acquis_summ1[acquis_summ2, on = "Rater",
                            .(Rater, Session, Rests,
                              Pass_N = sprintf("%d (%.0f%%)",
                                               N_Pass, N_Pass / N * 100),
                              Pass_Time = sprintf(time_format,
                                                  minute(Median_Pass),
                                                  second(Median_Pass),
                                                  minute(Sd_Pass),
                                                  second(Sd_Pass)),
                              Warning_N = sprintf("%d (%.0f%%)",
                                                  N_Warning,
                                                  N_Warning / N * 100),
                              Warning_Time = sprintf(time_format,
                                                     minute(Median_Warning),
                                                     second(Median_Warning),
                                                     minute(Sd_Warning),
                                                     second(Sd_Warning)),
                              Fail_N = sprintf("%d (%.0f%%)",
                                               N_Fail, N_Fail / N * 100),
                              Fail_Time = sprintf(time_format,
                                                  minute(Median_Fail),
                                                  second(Median_Fail),
                                                  minute(Sd_Fail),
                                                  second(Sd_Fail)),
                              p = sprintf("%04.3f", PvWvF))]

acquis_summ[Rater %in% no_w_raters, `:=`(Warning_N = "0 (0%)",
                                        Warning_Time = "-")]

rm(acquis, acquis_wilcox, acquis_summ1, acquis_all1, acquis_summ2, acquis_all2,
   no_w_raters)

acquis_summ |>
  flextable() |>
  separate_header() |>
  labelizor(part = "header", labels = c("Session" = "Total Time",
                                        "p" = "p-value")) |>
  bold(part = "header") |>
  italic(part = "header", j = "p") |>
  italic(~ Rater == "All") |>
  hline(i = ~ before(Rater, "All"), border = fp_border_default(width = 2)) |>
  bold(~ p < 0.05, j = "p") |>
  labelizor(j = "p", labels = c("0.000" = "<0.001")) |>
  footnote(part = "header", i = 1, j = 2,
           value = as_paragraph("HH:MM:SS"), ref_symbols = "1") |> #, inline = TRUE) |>
  footnote(part = "header", i = 1, j = 3,
           value = as_paragraph("n"), ref_symbols = "2") |> #, inline = TRUE) |>
  footnote(part = "header", i = 2, j = c(4, 6, 8),
           value = as_paragraph("n (%)"), ref_symbols = "3") |> #, inline = TRUE) |>
  footnote(part = "header", i = 2, j = c(5, 7, 9),
           value = as_paragraph("Median (SD): MM:SS"), ref_symbols = "4") |> #, inline = TRUE) |>
  footnote(part = "header", j = 10,
           value = as_paragraph("Wilcoxon rank-sum test; Kruskal-Wallis rank-sum test"),
           ref_symbols = "5") |>#, inline = TRUE) |>
  footnote(part = "body", i = 9, j = c(2:3), value = as_paragraph("Average"),
           ref_symbols = "6") |>#, inline = TRUE) |>

  autofit() |>
  save_as_docx(path = "data/derivatives/acq_adni_time.docx")

## Post-hoc analysis
raters_kruskal <- acquis_summ[, unique(Rater)][c(2:4, 6, 8)]
acquis_posthoc <- acquis_time[Rater %in% raters_kruskal,
                              dunn.test(as.numeric(Diff), Rating,
                                        method = "bonferroni"), Rater]
acquis_posthoc_all <- acquis_time[, dunn.test(as.numeric(Diff), Rating,
                                              method = "bonferroni")]
acquis_posthoc_all[, Rater := "All"]
acquis_posthoc <- rbindlist(list(acquis_posthoc[order(Rater)],
                                 acquis_posthoc_all),
                            use.names = TRUE)
rm(raters_kruskal, acquis_posthoc_all)

acquis_posthoc |>
  flextable() |>
  colformat_double(digits = 3) |>
  labelizor(part = "header", labels = c("chi2" = "Chi-squared",
                                        "P" = "p-value",
                                        "P.adjusted" = "Adjusted p-value",
                                        "comparisons" = "Comparisons")) |>
  bold(part = "header") |>
  italic(part = "header", j = 4:5) |>
  merge_v(j = 1:2) |>
  italic(~ Rater == "All") |>
  hline(i = 15, border = fp_border_default(width = 2)) |>
  fix_border_issues() |>
  bold(~ P < 0.05, j = 4) |>
  bold(~ P.adjusted < 0.05, j = 5) |>
  labelizor(j = 4:5, labels = c("0.000" = "<0.001")) |>
  autofit() |>
  save_as_docx(path = "data/derivatives/acq_adni_posthoc.docx")



## Registration 99 / ADNI
# Data.table 99
fpath <- here("data/derivatives/registration_99_dt.rds")

if (!file.exists(fpath)) {
  source(here("scripts/data_registration99.R"))
} else {
  reg_99 <- read_rds(fpath)
}

rm(fpath)

reg_99[Session == "First" & Rater == "Expert01", Rater := "Expert-1 (S1)"]
reg_99[Session == "Second" & Rater == "Expert01", Rater := "Expert-1 (S2)"]
reg_99 <- reg_99[Rater != "Expert01"]

reg_99[Diff > duration(15, "minutes"), Rest := TRUE]
reg_99_time <- reg_99[, .(Image, Rater = str_replace(Rater, "0", "-"),
                          Rating, Diff, Rest)]
reg_99_time[!is.na(Rest), Diff := NA]
write_rds(reg_99_time, here("data/derivatives/reg_99_time_dt.rds"))

reg_99_summ1 <-
  reg_99_time[, .(N = .N,
                  Rests = sum(Rest, na.rm = TRUE),
                  Session = as_hms(sum(Diff, na.rm = TRUE)),
                  PvF = suppressWarnings(
                          wilcox.test(as.numeric(Diff) ~ Rating,
                                      na.action = na.omit))$p.value), Rater]

reg_99_all1 <- reg_99_summ1[, .(Rater = "All",
                                N = 99,
                                Rests = round(mean(Rests)),
                                Session = as_hms(round(mean(Session))))]
reg_99_all1[, PvF := reg_99_time[, wilcox.test(as.numeric(Diff) ~ Rating,
                                               na.action = na.omit)$p.value]]

reg_99_summ1 <- rbindlist(list(reg_99_summ1, reg_99_all1), use.names = TRUE)

reg_99_summ2 <- reg_99_time[, .(N = .N,
                                Median = as_hms(median(Diff, na.rm = TRUE)),
                                Sd = hms(sd(Diff, na.rm = TRUE))),
                            .(Rater, Rating)] |>
                  dcast(Rater ~ Rating, value.var = c("N", "Median", "Sd"))

reg_99_all2 <- reg_99_time[, .(Rater = "All",
                               Median = as_hms(median(Diff, na.rm = TRUE)),
                               Sd = hms(sd(Diff, na.rm = TRUE))), Rating] |>
                  dcast(Rater ~ Rating, value.var = c("Median", "Sd"))

reg_99_all2 <- cbind(reg_99_all2,
                      reg_99_summ2[, lapply(.SD, mean), .SDcols = 2:3])

reg_99_summ2 <- rbindlist(list(reg_99_summ2, reg_99_all2), use.names = TRUE)

reg_99_summ <- reg_99_summ1[reg_99_summ2, on = "Rater",
                            .(Rater, Session, Rests,
                              Pass_N = round(N_Pass),
                              Pass_Time = sprintf(time_format,
                                                  minute(Median_Pass),
                                                  second(Median_Pass),
                                                  minute(Sd_Pass),
                                                  second(Sd_Pass)),
                              Fail_N = round(N_Fail),
                              Fail_Time = sprintf(time_format,
                                                  minute(Median_Fail),
                                                  second(Median_Fail),
                                                  minute(Sd_Fail),
                                                  second(Sd_Fail)),
                              p = round(PvF, digits = 3))]

rm(reg_99, reg_99_summ1, reg_99_all1, reg_99_summ2, reg_99_all2)

reg_99_summ |>
  flextable() |>
  separate_header() |>
  labelizor(part = "header", labels = c("Session" = "Total Time",
                                        "p" = "p-value")) |>
  bold(part = "header") |>
  italic(part = "header", j = "p") |>
  italic(~ Rater == "All") |>
  hline(i = ~ before(Rater, "All"), border = fp_border_default(width = 2)) |>
  bold(~ p < 0.05, j = "p") |>
  footnote(part = "header", i = 1, j = 2,
           value = as_paragraph("HH:MM:SS"), ref_symbols = "1") |> #, inline = TRUE) |>
  footnote(part = "header", i = c(1, 2, 2), j = c(3, 4, 6),
           value = as_paragraph("n"), ref_symbols = "2") |> #, inline = TRUE) |>
  footnote(part = "header", i = 2, j = c(5, 7),
           value = as_paragraph("Median (SD): MM:SS"), ref_symbols = "3") |> #, inline = TRUE) |>
  footnote(part = "header", i = 1, j = 8,
           value = as_paragraph("Wilcoxon rank-sum test"), ref_symbols = "4") |> #, inline = TRUE) |>
  footnote(~ Rater == "All", j = c(2:4, 6), value = as_paragraph("Average"),
           ref_symbols = "5") |> #, inline = TRUE) |>
  autofit() |>
  save_as_docx(path = "data/derivatives/reg_99_time.docx")

# Data.table ADNI
fpath <- here("data/derivatives/registration_dt.rds")

if (!file.exists(fpath)) {
  source(here("scripts/data_registration.R"))
} else {
  regis <- read_rds(fpath)
}
rm(fpath)

regis[Diff > duration(15, "minutes"), Rest := TRUE]
regis_time <- regis[, .(Image, Rater = str_replace(Rater, "0", "-"),
                        Rating, Diff, Rest)]
regis_time[!is.na(Rest), Diff := NA]
write_rds(regis_time, here("data/derivatives/regis_time_dt.rds"))

regis_summ1 <-
  regis_time[, .(N = .N,
                 Rests = sum(Rest, na.rm = TRUE),
                 Session = as_hms(sum(Diff, na.rm = TRUE)),
                 PvF = suppressWarnings(
                         wilcox.test(as.numeric(Diff) ~ Rating,
                                     na.action = na.omit))$p.value), Rater]

regis_all1 <- regis_summ1[, .(Rater = "All", N = sum(N),
                              Rests = round(mean(Rests)),
                              Session = as_hms(round(mean(Session))))]
regis_all1[, PvF := regis_time[, wilcox.test(as.numeric(Diff) ~ Rating,
                                             na.action = na.omit)$p.value]]

regis_summ1 <- rbindlist(list(regis_summ1[order(Rater)], regis_all1),
                         use.names = TRUE)

regis_summ2 <- regis_time[, .(N = .N,
                              Median = as_hms(median(Diff, na.rm = TRUE)),
                              Sd = hms(sd(Diff, na.rm = TRUE))),
                          .(Rater, Rating)] |>
                  dcast(Rater ~ Rating, value.var = c("N", "Median", "Sd"))

regis_all2 <- regis_time[, .(Rater = "All", N = .N,
                             Median = as_hms(median(Diff, na.rm = TRUE)),
                             Sd = hms(sd(Diff, na.rm = TRUE))), Rating] |>
                dcast(Rater ~ Rating, value.var = c("N", "Median", "Sd"))

regis_summ2 <- rbindlist(list(regis_summ2, regis_all2), use.names = TRUE)

regis_summ <- regis_summ1[regis_summ2, on = "Rater",
                          .(Rater, Session, Rests,
                            Pass_N = sprintf("%d (%.0f%%)",
                                             N_Pass, N_Pass / N * 100),
                            Pass_Time = sprintf(time_format,
                                                minute(Median_Pass),
                                                second(Median_Pass),
                                                minute(Sd_Pass),
                                                second(Sd_Pass)),
                            Fail_N = sprintf("%d (%.0f%%)",
                                             N_Fail, N_Fail / N * 100),
                            Fail_Time = sprintf(time_format,
                                                minute(Median_Fail),
                                                second(Median_Fail),
                                                minute(Sd_Fail),
                                                second(Sd_Fail)),
                            p = sprintf("%04.3f", PvF))]

rm(regis, regis_summ1, regis_all1, regis_summ2, regis_all2)

regis_summ |>
  flextable() |>
  separate_header() |>
  labelizor(part = "header", labels = c("Session" = "Total Time",
                                        "p" = "p-value")) |>
  bold(part = "header") |>
  italic(part = "header", j = "p") |>
  italic(~ Rater == "All") |>
  hline(i = ~ before(Rater, "All"), border = fp_border_default(width = 2)) |>
  bold(~ p < 0.05, j = "p") |>
  labelizor(j = "p", labels = c("0.000" = "<0.001")) |>
  footnote(part = "header", i = 1, j = 2,
           value = as_paragraph("HH:MM:SS"), ref_symbols = "1") |> #, inline = TRUE) |>
  footnote(part = "header", i = 1, j = 3,
           value = as_paragraph("n"), ref_symbols = "2") |> #, inline = TRUE) |>
  footnote(part = "header", i = 2, j = c(4, 6),
           value = as_paragraph("n (%)"), ref_symbols = "3") |> #, inline = TRUE) |>
  footnote(part = "header", i = 2, j = c(5, 7),
           value = as_paragraph("Median (SD): MM:SS"), ref_symbols = "4") |> #, inline = TRUE) |>
  footnote(part = "header", i = 1, j = "p",
           value = as_paragraph("Wilcoxon rank-sum test"), ref_symbols = "5") |> #, inline = TRUE) |>
  footnote(~ Rater == "All", j = c(2:3), value = as_paragraph("Average"),
           ref_symbols = "6") |> #, inline = TRUE) |>
  autofit() |>
  save_as_docx(path = "data/derivatives/reg_adni_time.docx")

## Skull Segmentation
# Data.table
fname <- "segmentation_dt.rds"
fpath <- here("data/derivatives", fname)

if (!file.exists(fpath)) {
  source(here("scripts/data_segmentation.R"))
} else {
  rskull <- read_rds(fpath)
}

rm(fname, fpath)

rskull[Diff > duration(15, "minutes"), Rest := TRUE]
rskull_time <- rskull[, .(Image, Rater = str_c("Expert-", str_sub(Rater, -1)),
                          Rating, Diff, Rest)]
rskull_time[!is.na(Rest), Diff := NA]

rskull_summ1 <-
  rskull_time[, .(N = .N,
                  Rests = sum(Rest, na.rm = TRUE),
                  Session = as_hms(sum(Diff, na.rm = TRUE)),
                  PvF = wilcox.test(as.numeric(Diff) ~ Rating,
                                    na.action = na.omit)$p.value), Rater]

rskull_summ2 <- rskull_time[, .(N = .N,
                                Median = as_hms(median(Diff, na.rm = TRUE)),
                                Sd = hms(sd(Diff, na.rm = TRUE))),
                            .(Rater, Rating)] |>
                  dcast(Rater ~ Rating, value.var = c("N", "Median", "Sd"))

rskull_summ <- rskull_summ1[rskull_summ2, on = "Rater",
                            .(Rater, Session, Rests,
                              Pass_N = sprintf("%d (%.0f%%)",
                                               N_Pass, N_Pass / N * 100),
                              Pass_Time = sprintf(time_format,
                                                  minute(Median_Pass),
                                                  second(Median_Pass),
                                                  minute(Sd_Pass),
                                                  second(Sd_Pass)),
                              Fail_N = sprintf("%d (%.0f%%)",
                                               N_Fail, N_Fail / N * 100),
                              Fail_Time = sprintf(time_format,
                                                  minute(Median_Fail),
                                                  second(Median_Fail),
                                                  minute(Sd_Fail),
                                                  second(Sd_Fail)),
                              p = sprintf("%04.3f", PvF))]

rm(rskull, rskull_summ1, rskull_summ2, time_format)

rskull_summ |>
  flextable() |>
  separate_header() |>
  labelizor(part = "header", labels = c("Session" = "Total Time",
                                        "p" = "p-value")) |>
  bold(part = "header") |>
  italic(part = "header", j = "p") |>
  bold(~ p < 0.05, j = "p") |>
  labelizor(j = "p", labels = c("0.000" = "<0.001")) |>
  footnote(part = "header", i = 1, j = 2,
           value = as_paragraph("HH:MM:SS"), ref_symbols = "1") |> #, inline = TRUE) |>
  footnote(part = "header", i = 1, j = 3,
           value = as_paragraph("n"), ref_symbols = "2") |> #, inline = TRUE) |>
  footnote(part = "header", i = 2, j = c(4, 6),
           value = as_paragraph("n (%)"), ref_symbols = "3") |> #, inline = TRUE) |>
  footnote(part = "header", i = 2, j = c(5, 7),
           value = as_paragraph("Median (SD): MM:SS"), ref_symbols = "4") |> #, inline = TRUE) |>
  footnote(part = "header", i = 1, j = "p",
           value = as_paragraph("Wilcoxon rank-sum test"), ref_symbols = "5") |> #, inline = TRUE) |>
  autofit() |>
  save_as_docx(path = "data/derivatives/seg_adni_time.docx")

## Other analyses
# Acquisition99
# Acq99 time between expert sessions
acq_99_time[Rater %like% "Expert", median(Diff, na.rm = TRUE), Rater]
wilcox.test(acq_99_time[Rater %like% "S1", as.numeric(Diff)],
            acq_99_time[Rater %like% "S2", as.numeric(Diff)],
            paired = TRUE)
wilcox_effsize(acq_99_time[Rater %like% "Expert",
                           .(Diff = as.numeric(Diff), Rater)],
               Diff ~ Rater, paired = TRUE)


# Acq99 time between ratings (with expert):
acq_99_time[, median(Diff, na.rm = TRUE), by = Rating]
wilcox.test(acq_99_time[Rating == "Pass", as.numeric(Diff)],
            acq_99_time[Rating == "Fail", as.numeric(Diff)])
wilcox_effsize(acq_99_time[, .(Diff = as.numeric(Diff), Rating)], Diff ~ Rating)

## Acq99 time between ratings (only trainees):
acq_99_time[Rater %like% "Rater", median(Diff, na.rm = TRUE), Rating]

wilcox.test(acq_99_time[Rater %like% "Rater" & Rating == "Pass",
            as.numeric(Diff)],
            acq_99_time[Rater %like% "Rater" & Rating == "Fail",
            as.numeric(Diff)])
wilcox_effsize(acq_99_time[Rater %like% "Rater",
               .(Diff = as.numeric(Diff), Rating)], Diff ~ Rating)


## adni acquisition time between ratings:
acquis_time[, median(Diff, na.rm = TRUE), Rating]
kruskal.test(Diff ~ Rating, acquis_time)
kruskal_effsize(acquis_time, Diff ~ Rating)

## ADNI Acquisition

## ADNI vs training (Expert only)
#acq_99_time[startsWith(Rater, "Expert01"), median(Diff)]
#acquis_time[Rater == "Expert01", median(Diff)]
#wilcox.test(acq_99_time[startsWith(Rater, "Expert01"), as.numeric(Diff)],
            #acquis_time[Rater == "Expert01", as.numeric(Diff)])

## ADNI vs training
raters_adni <- acquis_time[Rater %like% "Rater", unique(Rater)]
acq_99_acquis_time <- rbindlist(list(acq_99_time[Rater %in% c("Expert-1 (S1)",
                                                              raters_adni),
                                                 .(Task = "Balanced",
                                                   Time = as.numeric(Diff))],
                                     acquis_time[,
                                                 .(Task = "ADNI",
                                                   Time = as.numeric(Diff))]))
acq_99_acquis_time[, median(Time, na.rm = TRUE), Task]
wilcox.test(Time ~ Task, acq_99_acquis_time)
wilcox_effsize(acq_99_acquis_time, Time ~ Task)

## Registration 99cases

## Reg99 time between expert sessions
reg_99_time[Rater %like% "Expert", median(Diff, na.rm = TRUE), Rater]
wilcox.test(as.numeric(Diff) ~ Rater,
            reg_99_time[Rater %like% "Expert"],
            paired = TRUE)
wilcox_effsize(reg_99_time[Rater %like% "Expert",
                           .(Diff = as.numeric(Diff), Rater)], Diff ~ Rater)

## Reg99 time between ratings (with expert):
reg_99_time[, median(Diff, na.rm = TRUE), Rating]
wilcox.test(as.numeric(Diff) ~ Rating, reg_99_time)
wilcox_effsize(reg_99_time[, .(Diff = as.numeric(Diff), Rating)], Diff ~ Rating)

## Reg99 time between ratings (only trainees):
reg_99_time[Rater %like% "Rater", median(Diff, na.rm = TRUE), Rating]
wilcox.test(as.numeric(Diff) ~ Rating, reg_99_time[Rater %like% "Rater"])
wilcox_effsize(reg_99_time[Rater %like% "Rater",
                           .(Diff = as.numeric(Diff), Rating)], Diff ~ Rating)

## ADNI Registration
## ADNI vs training
raters_adni <- regis_time[, unique(Rater)]
reg_99_regis_time <- rbindlist(list(reg_99_time[Rater %in% raters_adni,
                                                .(Task = "Balanced",
                                                  Time = as.numeric(Diff))],
                                    regis_time[, .(Task = "ADNI",
                                                   Time = as.numeric(Diff))]))
reg_99_regis_time[, median(Time, na.rm = TRUE), Task]
wilcox.test(Time ~ Task, reg_99_regis_time)
wilcox_effsize(reg_99_regis_time, Time ~ Task)

## adni acquisition time between ratings:
regis_time[, median(Diff, na.rm = TRUE), Rating]
wilcox.test(as.numeric(Diff) ~ Rating, regis_time)
wilcox_effsize(regis_time[, .(Diff = as.numeric(Diff), Rating)], Diff ~ Rating)

## Rskull
rskull_time[, median(Diff, na.rm = TRUE), .(Rating, Rater)]

# Expert-1
wilcox.test(as.numeric(Diff) ~ Rating, rskull_time[Rater %like% "1"])
wilcox_effsize(rskull_time[Rater %like% "1", .(Diff = as.numeric(Diff), Rating)],
               Diff ~ Rating)

# Expert-2
wilcox.test(as.numeric(Diff) ~ Rating, rskull_time[Rater %like% "2"])
wilcox_effsize(rskull_time[Rater %like% "2", .(Diff = as.numeric(Diff), Rating)],
               Diff ~ Rating)
