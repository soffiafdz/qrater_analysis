#!/usr/bin/env Rscript

## Packages
library("here")
library("data.table")
library("flextable")

params <- here("data/raw/databases_params.csv") |> fread()

params[Dataset != "Previous QC"] |>
  flextable() |>
  bold(part = "header") |>
  bold(j = 1) |>
  autofit() |>
  save_as_docx(path = "data/derivatives/imgs_params.docx")
