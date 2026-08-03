#!/usr/bin/env Rscript

# Lightweight checks for the launcher interface. These tests intentionally avoid
# loading the full scientific stack so they can run before a long Docker build.

launcher_environment <- new.env(parent = globalenv())

sys.source(
  here::here("R/launching_workflows/zenodo_helpers.R"),
  envir = launcher_environment
)

stopifnot(
  is.function(launcher_environment$get_zenodo_record_id),
  is.function(launcher_environment$select_zenodo_input_archive)
)

stopifnot(
  identical(
    launcher_environment$get_zenodo_record_id("10.5281/zenodo.20834708"),
    "20834708"
  ),
  identical(
    launcher_environment$get_zenodo_record_id(
      "https://zenodo.org/records/20834708"
    ),
    "20834708"
  ),
  identical(launcher_environment$get_zenodo_record_id("20834708"), "20834708")
)

fake_files <- list(
  list(key = "processed.csv"),
  list(key = "all_raw_data_GTA.zip")
)

selected <- launcher_environment$select_zenodo_input_archive(fake_files)
stopifnot(identical(selected$key, "all_raw_data_GTA.zip"))

selected_explicit <- launcher_environment$select_zenodo_input_archive(
  fake_files,
  doi_file = "processed.csv"
)
stopifnot(identical(selected_explicit$key, "processed.csv"))

required_environment_variables <- c(
  "GTA_STEPS",
  "GTA_DATA_SOURCE",
  "GTA_DATA_PATH",
  "GTA_DOI",
  "GTA_DOI_FILE",
  "GTA_BOOTSTRAP_RESTORE_RENV"
)

cli_text <- paste(
  readLines("R/launching_workflows/run_gta_2026_workflow_cli.R", warn = FALSE),
  collapse = "\n"
)

stopifnot(all(vapply(
  required_environment_variables,
  grepl,
  logical(1),
  x = cli_text,
  fixed = TRUE
)))

message("Launcher smoke tests passed.")

