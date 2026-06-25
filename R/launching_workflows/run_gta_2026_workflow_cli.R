#!/usr/bin/env Rscript
# =============================================================================
# GTA 2026 DOCKER / CLI ENTRYPOINT
# =============================================================================
#
# Small command-line wrapper around run_gta_workflow().
#
# Docker examples:
#
#   docker run \
#     -v /home/bastien/data/GTA_2026:/data/GTA_2026 \
#     -e GTA_STEPS=rawdata \
#     mon_image
#
#   docker run \
#     -v /home/bastien/GTA_2026.zip:/data/GTA_2026.zip \
#     -e GTA_STEPS=level1 \
#     mon_image
#
#   docker run \
#     -v /home/bastien/data/GTA_2026:/data/GTA_2026 \
#     -e GTA_STEPS=rawdata,level1 \
#     -e GTA_SUMMARISE_INVALID_RAW=true \
#     mon_image
#
# =============================================================================

parse_bool_env <- function(name, default = FALSE) {
  value <- Sys.getenv(name, unset = NA_character_)
  
  if (is.na(value) || !nzchar(value)) {
    return(default)
  }
  
  tolower(value) %in% c("true", "1", "yes", "y")
}

parse_nullable_env <- function(name) {
  value <- Sys.getenv(name, unset = NA_character_)
  
  if (is.na(value) || !nzchar(value)) {
    return(NULL)
  }
  
  value
}

parse_steps_env <- function() {
  steps_raw <- Sys.getenv("GTA_STEPS", unset = "rawdata")
  trimws(strsplit(steps_raw, ",", fixed = TRUE)[[1]])
}

source("R/launching_workflows/GTA_2026_creation.R")

run_gta_workflow(
  steps_to_run = parse_steps_env(),
  summarise_invalid_raw = parse_bool_env("GTA_SUMMARISE_INVALID_RAW", FALSE),
  stop_on_missing_inputs = parse_bool_env("GTA_STOP_ON_MISSING_INPUTS", TRUE),
  data_source = Sys.getenv("GTA_DATA_SOURCE", unset = "auto"),
  data_path = parse_nullable_env("GTA_DATA_PATH"),
  doi = parse_nullable_env("GTA_DOI"),
  bootstrap_restore_renv = parse_bool_env("GTA_BOOTSTRAP_RESTORE_RENV", TRUE)
)