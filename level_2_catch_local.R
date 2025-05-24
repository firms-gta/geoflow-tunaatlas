#!/usr/bin/env Rscript

# ─── Chargement des packages nécessaires ─────────────────────────────────────
library(here)
library(futile.logger)
library(geoflow)
library(jsonlite)
library(purrr)
library(dotenv)
library(data.table)
library(dplyr)
library(DBI)
library(RPostgreSQL)
library(rpostgis)
library(ggplot2)
library(readr)
library(sf)
library(bookdown)
library(knitr)
library(tidyr)
library(CWP.dataset)
library(utils)
library(ows4R)
library(callr)
library(renv)
# install.packages("pak")
# pak::pak("bastienird/CWP.dataset")
# Activate the project environment (if using project-specific libraries)
# renv::activate()
# Restore the project library (if using renv)
renv::restore()

# Define all required packages (excluding 'base' and 'utils' as they are always available)
required_packages <- c(
  "remotes", "tinytex", "googledrive", "gsheet", "readr", "plotrix", "janitor", 
  "dotenv", "data.table", "here", "xfun", "RPostgreSQL", "RPostgres", "DBI", 
  "rpostgis", "terra", "sf", "RSQLite", "webshot", "usethis", "ows4R", "sp", 
  "flextable", "dplyr", "stringr", "tibble", "bookdown", "knitr", 
  "purrr", "readxl", "odbc", "rlang", "kableExtra", "tidyr", "ggplot2", "fs" ,
  "stats", "RColorBrewer", "cowplot", "tmap", "curl", "officer", 
  "gdata", "R3port", "reshape2", "tools", "plogr", "futile.logger", "lubridate", "data.table"
)

# Function to check, install (if necessary), and load a package
install_and_load <- function(package) {
  if (!require(package, character.only = TRUE)) {
    # install.packages(package)
    library(package, character.only = TRUE)
  }
}

# Apply the function to each required package
sapply(required_packages, install_and_load)
require(geoflow)
Sys.setlocale("LC_ALL", "C.UTF-8")         # pour être sûr  
# ─── Préparation des dossiers ────────────────────────────────────────────────
if (!dir.exists(here("results"))) {
  dir.create(here("results"), recursive = TRUE)
}
if (!dir.exists(here("data"))) {
  dir.create(here("data"), recursive = TRUE)
}

# ─── Restaurer l’environnement renv ──────────────────────────────────────────
if (!requireNamespace("renv", quietly = TRUE)) install.packages("renv")
renv::restore()

# ─── Chargement des fonctions externes ──────────────────────────────────────
source(here::here("tunaatlas_scripts/generation/create_global_tuna_atlas_dataset_v2025.R"))

# ─── 1) Initialisation du workflow ───────────────────────────────────────────
config <- initWorkflow(here("catch_ird_level2_local.json"))

# ─── 2) Extraction de l’entité et suppression des relations inutiles ────────
entityfile <- config$metadata$content$entities[[1]]
entityfile$relations <- NULL
entity <- entityfile

# ─── 3) Extraction de l’action et de ses options ────────────────────────────
action <- entity$data$actions[[1]]
opts   <- action$options

# ─── 4) Téléchargement du fichier nominal catch si nécessaire ───────────────
keynominal <- opts$keynominal
doinominal <- opts$doinominal
nominal_catch_file <- here("data", keynominal)
dir.create("data")
if (!file.exists(nominal_catch_file)) {
  message("Téléchargement de ", keynominal, " depuis Zenodo…")
  zen4R::download_zenodo(doi = doinominal, files = keynominal, path = here("data"))
} else {
  message("Fichier nominal déjà présent : ", nominal_catch_file)
}

# ─── 5) Téléchargement et décompression de All_rawdata_for_level2.zip ───────
zipfile <- here("data", "All_rawdata_for_level2.zip")
url     <- "https://zenodo.org/record/15496164/files/All_rawdata_for_level2.zip"

if (!file.exists(zipfile)) {
  message("Téléchargement de All_rawdata_for_level2.zip…")
  utils::download.file(url, zipfile, mode = "wb")
} else {
  message("ZIP déjà présent : ", zipfile)
}

files_in_data <- list.files(here("data"), all.files = TRUE, no.. = TRUE)
only_zip <- identical(files_in_data, basename(zipfile))

if (only_zip) {
  message("Décompression de All_rawdata_for_level2.zip dans data/ …")
  utils::unzip(zipfile, exdir = here("data"))
} else {
  message("data/ contient déjà d’autres fichiers, skip décompression.")
}

# ─── 6) Exécution du script principal de génération ─────────────────────────
message("Lancement de create_global_tuna_atlas_dataset_v2025() …")
create_global_tuna_atlas_dataset_v2025(action, entity, config)

output_file <- here("data", paste0(entity$identifiers[["id"]], "_harmonized.csv"))
if (!file.exists(output_file)) {
  stop("Erreur : le fichier attendu n'a pas été généré", output_file)
}
message("Fichier généré : ", output_file)

# ─── 7) Lecture et sauvegarde des résultats finaux ──────────────────────────
df <- read.csv(output_file, stringsAsFactors = FALSE)
write.csv(df, here("results", "tuna_atlas_results.csv"), row.names = FALSE)
message("Résultats sauvegardés dans results/tuna_atlas_results.csv")

# ─── 8) Génération des rapports PDF ─────────────────────────────────────────
con <- config$software$output$dbi

entity_path <- paste0("report/entities/", entity$identifiers[["id"]]) #entity_path

dir.create(entity_path, recursive = TRUE)

file.copy(from = "Markdown", to = entity_path, recursive = TRUE)

message("Génération du rapport court…")
CWP.dataset::summarising_step(
  main_dir             = "report",
  connectionDB         = con,
  config               = config,
  sizepdf              = "short",
  savestep             = TRUE,
  usesave              = TRUE,
  source_authoritylist = "all"
)

message("Génération du rapport moyen…")
CWP.dataset::summarising_step(
  main_dir             = "report",
  connectionDB         = con,
  config               = config,
  sizepdf              = "middle",
  savestep             = TRUE,
  usesave              = TRUE,
  source_authoritylist = "all",
  fast_and_heavy       = FALSE
)

pdfs <- list.files(
  path       = here("_book"),
  pattern    = "\\.pdf$",
  full.names = TRUE
)

message("PDFs générés : ", paste(pdfs, collapse = ", "))
