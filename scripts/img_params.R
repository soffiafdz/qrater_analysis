#!/usr/bin/env Rscript

## Packages
library("here")
library("data.table")
library("flextable")

params <- here("data/raw/databases_params.csv") |> fread()

params |>
  flextable() |>
  bold(part = "header") |>
  bold(j = 1) |>
  footnote(part = "body", j = 1, i = 3,
           value = as_paragraph("mm"), ref_symbols = "1") |>
  footnote(part = "body", j = 1, i = 5:6,
           value = as_paragraph("cm2"), ref_symbols = "2") |>
  footnote(part = "body", j = 1, i = 7:8,
           value = as_paragraph("ms"), ref_symbols = "3") |>
  autofit() |>
  save_as_docx(path = "data/derivatives/imgs_params.docx")
