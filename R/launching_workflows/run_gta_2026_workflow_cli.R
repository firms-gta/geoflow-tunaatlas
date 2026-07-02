#!/usr/bin/env Rscript
# =============================================================================
# GTA 2026 DOCKER / CLI ENTRYPOINT
# =============================================================================
#
# Small command-line wrapper around run_gta_workflow().
#
# Docker examples
# ---------------
#
# 1) Run from an already extracted GTA_2026 data directory.
#    The host directory is mounted directly where the workflow expects it:
#
#   mkdir -p /home/grasset/gta_output/jobs
#   chmod -R u+rwX /home/grasset/gta_output/jobs
#
#   docker run --rm \
#     -v /home/grasset/data/GTA_2026:/home/rstudio/geoflow-tunaatlas/data/GTA_2026 \
#     -v /home/grasset/gta_output/jobs:/home/rstudio/geoflow-tunaatlas/jobs \
#     -e GTA_STEPS=rawdata \
#     -e GTA_DATA_SOURCE=volume_dir \
#     -e GTA_DATA_PATH=/home/rstudio/geoflow-tunaatlas/data/GTA_2026 \
#     gta-workflow:latest
#
#
# 2) Run from a mounted GTA_2026 zip archive.
#    The archive is read from /data/GTA_2026.zip, then extracted inside
#    /home/rstudio/geoflow-tunaatlas/data/GTA_2026.
#    The data/ and jobs/ directories are mounted so outputs remain available
#    after the container stops.
#
#   mkdir -p /home/grasset/gta_runtime_data
#   mkdir -p /home/grasset/gta_output/jobs
#   chmod -R u+rwX /home/grasset/gta_output/jobs
#   chmod -R u+rwX /home/grasset/gta_runtime_data
# 
#   docker run --rm \
#     -v /home/grasset/Downloads/all_raw_data_GTA.zip:/data/GTA_2026.zip:ro \
#     -v /home/grasset/gta_runtime_data:/home/rstudio/geoflow-tunaatlas/data \
#     -v /home/grasset/gta_output/jobs:/home/rstudio/geoflow-tunaatlas/jobs \
#     -e GTA_STEPS=rawdata \
#     -e GTA_DATA_SOURCE=volume_zip \
#     -e GTA_DATA_PATH=/data/GTA_2026.zip \
#     gta-workflow:latest
#
#
# 3) Run rawdata and invalid-data summaries from the mounted zip.
#
#   mkdir -p /home/grasset/gta_runtime_data
#   mkdir -p /home/grasset/gta_output/jobs
#
#   docker run --rm \
#     -v /home/grasset/Downloads/all_raw_data_GTA.zip:/data/GTA_2026.zip:ro \
#     -v /home/grasset/gta_runtime_data:/home/rstudio/geoflow-tunaatlas/data \
#     -v /home/grasset/gta_output/jobs:/home/rstudio/geoflow-tunaatlas/jobs \
#     -e GTA_STEPS=rawdata \
#     -e GTA_DATA_SOURCE=volume_zip \
#     -e GTA_DATA_PATH=/data/GTA_2026.zip \
#     -e GTA_SUMMARISE_INVALID_RAW=true \
#     gta-workflow:latest
#
#
# 4) Run several steps in sequence.
#
#   mkdir -p /home/grasset/gta_runtime_data
#   mkdir -p /home/grasset/gta_output/jobs
#
#   docker run --rm \
#     -v /home/grasset/Downloads/all_raw_data_GTA.zip:/data/GTA_2026.zip:ro \
#     -v /home/grasset/gta_runtime_data:/home/rstudio/geoflow-tunaatlas/data \
#     -v /home/grasset/gta_output/jobs:/home/rstudio/geoflow-tunaatlas/jobs \
#     -e GTA_STEPS=rawdata,level1 \
#     -e GTA_DATA_SOURCE=volume_zip \
#     -e GTA_DATA_PATH=/data/GTA_2026.zip \
#     gta-workflow:latest

# Run summaries for with volume and jobs
# docker run --rm --network none \
# -v /home/grasset/gta_runtime_data:/home/rstudio/geoflow-tunaatlas/data \
# -v /home/grasset/Downloads/all_raw_data_GTA:/home/rstudio/geoflow-tunaatlas/data/GTA_2026 \
# -v /home/grasset/gta_output/jobs:/home/rstudio/geoflow-tunaatlas/jobs \
# -e GTA_STEPS=summaries \
# -e GTA_DATA_SOURCE=volume_dir \
# -e GTA_DATA_PATH=/home/rstudio/geoflow-tunaatlas/data/GTA_2026 \
# -e GTA_BOOTSTRAP_RESTORE_RENV=false \
# -e GTA_TUNAATLAS_LEVEL0_CATCH="jobs/20260630200458level_0_catch_2026" \
# gta-reporting:latest
#
#
# 5) Run from a Zenodo archive URL.
#    The archive is downloaded inside the container, extracted into
#    data/GTA_2026, then the workflow is launched using data_source = volume_dir.
#
#    This works with:
#      - all_raw_data_GTA.tar.gz
#      - all_raw_data_GTA.zip
#
#    Prefer the direct Zenodo file URL rather than the DOI landing page.
#
#   mkdir -p /home/grasset/gta_runtime_data
#   mkdir -p /home/grasset/gta_output/jobs
#
#   docker run --rm \
#     -v /home/grasset/gta_runtime_data:/home/rstudio/geoflow-tunaatlas/data \
#     -v /home/grasset/gta_output/jobs:/home/rstudio/geoflow-tunaatlas/jobs \
#     -e GTA_STEPS=rawdata \
#     -e GTA_ARCHIVE_URL="https://zenodo.org/records/XXXXXXXX/files/all_raw_data_GTA.tar.gz?download=1" \
#     gta-workflow:latest \
#     bash -lc '
#       set -euo pipefail
#
#       DATA_DIR=/home/rstudio/geoflow-tunaatlas/data/GTA_2026
#       TMP_DIR=/tmp/gta_archive
#
#       rm -rf "$TMP_DIR" "$DATA_DIR"
#       mkdir -p "$TMP_DIR/extract" "$DATA_DIR"
#
#       echo "Downloading GTA archive..."
#
#       case "$GTA_ARCHIVE_URL" in
#         *.zip* )
#           ARCHIVE="$TMP_DIR/archive.zip"
#           wget -O "$ARCHIVE" "$GTA_ARCHIVE_URL"
#           echo "Extracting zip archive..."
#           unzip -q "$ARCHIVE" -d "$TMP_DIR/extract"
#           ;;
#         *.tar.gz* | *.tgz* )
#           ARCHIVE="$TMP_DIR/archive.tar.gz"
#           wget -O "$ARCHIVE" "$GTA_ARCHIVE_URL"
#           echo "Extracting tar.gz archive..."
#           tar -xzf "$ARCHIVE" -C "$TMP_DIR/extract"
#           ;;
#         * )
#           echo "Unsupported archive format: $GTA_ARCHIVE_URL"
#           exit 1
#           ;;
#       esac
#
#       if [ -d "$TMP_DIR/extract/all_raw_data_GTA" ]; then
#         cp -a "$TMP_DIR/extract/all_raw_data_GTA/." "$DATA_DIR/"
#       else
#         cp -a "$TMP_DIR/extract/." "$DATA_DIR/"
#       fi
#
#       echo "Launching GTA workflow..."
#
#       GTA_DATA_SOURCE=volume_dir \
#       GTA_DATA_PATH="$DATA_DIR" \
#       Rscript -e "options(gta_data_dir = Sys.getenv('GTA_DATA_PATH'), gta.src_data_dir = Sys.getenv('GTA_DATA_PATH')); source('R/launching_workflows/run_gta_2026_workflow_cli.R')"
#     '
#
#
# Notes
# -----
# - /home/grasset/gta_output/jobs will contain the workflow job outputs.
# - /home/grasset/gta_runtime_data will contain the extracted GTA_2026 data
#   and any files written under the project data/ directory.
# - The :ro flag on the zip mount means read-only; the container cannot modify
#   the original archive.
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