# =============================================================================
# GTA 2026 WORKFLOW RUNNER
# =============================================================================
#
# Purpose
# -------
# This script runs the main Global Tuna Atlas 2026 processing chain:
#   1. Pre-harmonisation of nominal catch, georeferenced catch, and effort data.
#   2. Generation of harmonised effort, nominal catch, and catch Levels 0, 1, 2.
#   3. Production of short QA summaries and comparison plots.
#
# Main design choices
# -------------------
# - Database upload is enabled only when the expected production/sandbox database
#   context is detected and the DBI connection is valid.
# - If the database is unavailable, workflows are retried after removing DBI
#   software blocks from the JSON configuration.
# - Repeated Level 0 / Level 1 / Level 2 summary code is factorised to avoid
#   maintaining the same logic several times.
# - Optional analyses are grouped at the end so that the main workflow remains
#   easy to read and modify.
#
# =============================================================================


# =============================================================================
# 0. BOOTSTRAP AND PACKAGE INITIALISATION
# =============================================================================

# Load the pre-harmonisation bootstrap script first because it may define
# project-specific functions, options, paths, or geoflow-related helpers required
# by the rest of the workflow.
source("R/tunaatlas_scripts/pre-harmonization/bootstrap_preharmo.R")

# Load here before using project-relative paths.
require(here)

# Restore the renv environment to make sure the package versions used by this
# workflow match the versions recorded in renv.lock.
library(renv)
renv::restore()

# Keep the complete package list in one place. This makes Docker/debug runs easier
# because missing dependencies are detected at the beginning of the script rather
# than halfway through a long workflow.
required_packages <- c(
  "remotes", "tinytex", "googledrive", "gsheet", "readr", "plotrix",
  "janitor", "dotenv", "data.table", "here", "xfun", "RPostgreSQL",
  "RPostgres", "DBI", "rpostgis", "terra", "sf", "RSQLite", "webshot",
  "usethis", "ows4R", "sp", "flextable", "dplyr", "stringr", "tibble",
  "bookdown", "knitr", "purrr", "readxl", "odbc", "rlang", "kableExtra",
  "tidyr", "ggplot2", "fs", "stats", "RColorBrewer", "cowplot", "tmap",
  "curl", "officer", "gdata", "R3port", "reshape2", "tools", "plogr",
  "futile.logger", "lubridate", "geoflow"
)

# Load one package and fail early if it cannot be loaded. The function name keeps
# the historical behaviour, but no installation is attempted here: renv should be
# responsible for dependency restoration.
load_required_package <- function(package) {
  if (!require(package, character.only = TRUE)) {
    stop("Package could not be loaded: ", package, call. = FALSE)
  }
}

invisible(lapply(unique(required_packages), load_required_package))


# =============================================================================
# 1. ENVIRONMENT VARIABLES, PROJECT HELPERS, AND GEOFLOW PATCHES
# =============================================================================

# Prefer the local SDI lab environment file when it exists. This lets Julien or
# other users test against a specific database without modifying the script.
default_env_file <- ".env"
if (file.exists(here::here("geoserver_sdi_lab.env"))) {
  default_env_file <- "geoserver_sdi_lab.env"
}

# Load environment variables when available. A missing .env file should not stop
# local non-DB runs, because the workflow can still be executed without upload.
tryCatch(
  dotenv::load_dot_env(file = here::here(default_env_file)),
  error = function(e) message("No environment file loaded: ", e$message)
)

# Helper scripts used for timing and post-run job renaming.
source(here::here("R/running_time_of_workflow.R"))
source(here::here("R/executeAndRename.R"))

preharmo_json_files <- c(
  "config/Nominal_catch_2026.json",
  "config/All_raw_data_georef.json",
  "config/All_raw_data_georef_effort.json"
)

preharmo_check <- check_files_from_json(preharmo_json_files) # Checking the json files for nominal, level 0, 1 and 2

source(here::here("R/launching_workflows/workflow_helpers.R"))

# =============================================================================
# 4. OPTIONAL DATABASE MODEL AND MAPPING WORKFLOWS
# =============================================================================
#
# These workflows are kept commented because they are not part of the standard
# 2026 run, as the worflow now use directly the github. They can be re-enabled when code lists, DB model resources, or mapping
# tables need to be refreshed in the DB.
#
# db_model <- execute_workflow_maybe_upload(
#   file = here::here("config/tunaatlas_qa_dbmodel+codelists.json"),
#   rename_suffix = "db_model_codelists"
# )
# running_time_of_workflow(db_model)
#
# mappings <- executeWorkflow(here::here("config/tunaatlas_qa_mappings.json"))
# mappings <- executeAndRename(mappings, "_mappings")
# running_time_of_workflow(mappings)


# =============================================================================
# 5. PRE-HARMONISATION WORKFLOWS
# =============================================================================

# -----------------------------------------------------------------------------
# 5.1 Nominal catch pre-harmonisation
# -----------------------------------------------------------------------------
# Produces the raw nominal catch dataset after source-specific pre-harmonisation.
# This output is later used to generate the harmonised nominal and Level 0 catch
# datasets.
raw_nominal_catch <- execute_workflow_maybe_upload(
  file = here::here("config/Nominal_catch_2026.json"),
  rename_suffix = "_raw_nominal_catch_2026_final"
)
running_time_of_workflow(raw_nominal_catch)

# Optional invalid-data report. The invalid records are also available in each
# job's All_invalid_data.qs file, so this does not need to be run systematically.
# summarise_invalid(raw_nominal_catch)


# -----------------------------------------------------------------------------
# 5.2 Georeferenced catch pre-harmonisation
# -----------------------------------------------------------------------------
# Produces the harmonised raw georeferenced catch inputs used for Level 1 and
# Level 2 processing.
raw_data_georef <- execute_workflow_maybe_upload(
  file = here::here("config/All_raw_data_georef.json"),
  rename_suffix = "_raw_data_georef_final"
)
running_time_of_workflow(raw_data_georef)

# summarise_invalid(raw_data_georef)


# -----------------------------------------------------------------------------
# 5.3 Georeferenced effort pre-harmonisation
# -----------------------------------------------------------------------------
# Produces the harmonised raw effort inputs. This is kept separate from catch
# because effort has its own source structure and checks.
raw_data_georef_effort <- execute_workflow_maybe_upload(
  file = here::here("config/All_raw_data_georef_effort.json"),
  rename_suffix = "_raw_data_georef_effort_final"
)
running_time_of_workflow(raw_data_georef_effort)

# summarise_invalid(raw_data_georef_effort)


# -----------------------------------------------------------------------------
# 5.4 Optional QA documentation generation for pre-harmonisation functions
# -----------------------------------------------------------------------------
# Re-enable this block only when pre-harmonisation functions have changed and the
# corresponding Rmd documentation needs to be regenerated and pushed to GitHub.
#
# source(here::here("R/tunaatlas_scripts/pre-harmonization/rewrite_functions_as_rmd.R"))
# dir.create(file.path(raw_nominal_catch, "data"), showWarnings = FALSE, recursive = TRUE)
# rewrite_functions_as_rmd(raw_nominal_catch)
# dir.create(file.path(raw_data_georef, "data"), showWarnings = FALSE, recursive = TRUE)
# rewrite_functions_as_rmd(raw_data_georef)
# dir.create(file.path(raw_data_georef_effort, "data"), showWarnings = FALSE, recursive = TRUE)
# rewrite_functions_as_rmd(raw_data_georef_effort)


# =============================================================================
# 6. HARMONISED DATASET GENERATION
# =============================================================================

# Make sure the working directory is the project root before launching workflows
# that rely on relative paths inside JSON configuration files.
setwd(here::here())


# -----------------------------------------------------------------------------
# 6.1 Effort dataset
# -----------------------------------------------------------------------------
# Generates the final harmonised effort dataset after pre-harmonisation.
tunaatlas_effort_path <- execute_workflow_maybe_upload(
  file = here::here("config/create_effort_dataset_2026.json"),
  rename_suffix = "new_efforts_final"
)


# -----------------------------------------------------------------------------
# 6.2 Nominal catch dataset
# -----------------------------------------------------------------------------
# Generates the final harmonised nominal catch dataset. This dataset is used as a
# key reference for later comparisons with Level 2 raised georeferenced catches.
tunaatlas_nominal_path <- execute_workflow_maybe_upload(
  file = here::here("config/create_nominal_dataset_2026.json"),
  rename_suffix = "nominal_final"
)


# -----------------------------------------------------------------------------
# 6.3 Catch Level 0
# -----------------------------------------------------------------------------
# Level 0 is the harmonised nominal catch product following the GTA data model.
tunaatlas_level0_catch_path <- execute_workflow_maybe_upload(
  file = here::here("config/catch_ird_level0_local.json"),
  rename_suffix = "level_0_catch_2026"
)

run_step_summary(
  workflow_file = "config/catch_ird_level0_local.json",
  workflow_output = tunaatlas_level0_catch_path,
  source_authority = "all"
)


# -----------------------------------------------------------------------------
# 6.4 Optional mapping between georeferenced and nominal strata
# -----------------------------------------------------------------------------
# This block is currently kept as a reminder for future development. It should be
# re-enabled only when the georef-to-nominal mapping proposal is needed between
# Level 0 and Level 1 / Level 2 processing.
#
# res_map <- propose_georef_to_nominal_mappings_clean(
#   georef = georef_dataset,
#   nominal = nominal_catch,
#   id_cols = c("source_authority", "group_species_iattc_sharks", "year"),
#   candidate_cols_order = c(
#     "fishing_mode_wcpfc_issue_unk_solved",
#     "gear_type",
#     "geographic_identifier_nom",
#     "fishing_fleet"
#   )
# )


# -----------------------------------------------------------------------------
# 6.5 Catch Level 1
# -----------------------------------------------------------------------------
# Level 1 corresponds to georeferenced catch data with standardised units and GTA
# harmonisation rules applied.
tunaatlas_level1_catch_path <- execute_workflow_maybe_upload(
  file = here::here("config/catch_ird_level1_local.json"),
  rename_suffix = "level_1_catch_2026"
)

run_step_summary(
  workflow_file = "config/catch_ird_level1_local.json",
  workflow_output = tunaatlas_level1_catch_path,
  source_authority = "all"
)


# -----------------------------------------------------------------------------
# 6.6 Catch Level 2
# -----------------------------------------------------------------------------
# Level 2 corresponds to georeferenced catch raised against nominal catches. This
# is the main product used for georef-versus-nominal consistency checks.
tunaatlas_level2_catch_path <- execute_workflow_maybe_upload(
  file = here::here("config/catch_ird_level2_local.json"),
  rename_suffix = "level_2_catch_2026"
)

run_step_summary(
  workflow_file = "config/catch_ird_level2_local.json",
  workflow_output = tunaatlas_level2_catch_path,
  source_authority = "all"
)


# =============================================================================
# 7. QUALITY CONTROL AND COMPARISON REPORTS
# =============================================================================

# Columns that may be useful when generating detailed reports. Keep this list
# close to the reporting section so that it can be re-enabled without searching
# through the whole script.
# colnames_to_keep_report <- c(
#   "source_authority", "fishing_fleet_label", "fishing_mode_label",
#   "geographic_identifier", "measurement_unit", "measurement_value",
#   "gridtype", "species_label", "gear_type_label",
#   "measurement_processing_level"
# )

# Plot differences between the nominal reference files and the Level 2 output.
# This helps identify species or authorities where the raised georeferenced data
# diverges from nominal totals.
source(here::here("R/ongoing_projects/plot_diffs_from_nominal_files.R"))

comparisons <- c(
  "sums_species.csv" = here::here(
    paste0(
      tunaatlas_nominal_path,
      "/entities/global_nominal_catch_firms_level0_2026/Markdown/Filtering_SBF_data/sums_species.csv"
    )
  ),
  "sums_source_auth.csv" = here::here(
    paste0(
      tunaatlas_nominal_path,
      "/entities/global_nominal_catch_firms_level0_2026/Markdown/Filtering_SBF_data/sums_source_auth.csv"
    )
  ),
  "sums.csv" = here::here(
    paste0(
      tunaatlas_nominal_path,
      "/entities/global_nominal_catch_firms_level0_2026/Markdown/Filtering_SBF_data/sums.csv"
    )
  )
)

res <- plot_diffs_from_nominal_files(
  main_dir = paste0(
    tunaatlas_level2_catch_path,
    "/entities/global_catch_ird_level2_1950_2024/Markdown"
  ),
  comparison_files = comparisons,
  value_col = "sum_t",
  filter_list = list(
    sums_species = c("YFT", "SKJ", "BET", "SWO", "ALB", "SBF"),
    sums_source_auth = NULL,
    sums = NULL
  )
)

# Save the global evolution plot in data/ so it can be reused outside the job
# folder, for instance in reports, emails, or GitHub issues.
ggplot2::ggsave(
  filename = file.path("data", "plot_georef_vs_nominal_evolution.png"),
  plot = res$diff_plots$sums,
  width = 16,
  height = 12,
  dpi = 300
)

# Run the entity-level georef-versus-nominal analysis and save the RDS output for
# later inspection. This is useful when the plot reveals unexpected differences.
source(here::here("R/ongoing_projects/check_georef_vs_nominal_entity.R"))

level2_entity_paths <- list(
  paste0(tunaatlas_level2_catch_path, "/entities/global_catch_ird_level2_1950_2024")
)

results <- lapply(level2_entity_paths, run_analysis)
saveRDS(results, "data/check_georef_vs_nominal_entity.rds")


# =============================================================================
# 8. OPTIONAL TARGETED REPORTS AND DEBUGGING BLOCKS
# =============================================================================
#
# The blocks below are examples of targeted summaries that can be re-enabled when
# investigating a specific species, source authority, or filtering issue. They are
# intentionally kept commented so that the default run remains lightweight.

# -----------------------------------------------------------------------------
# 8.1 Summary by source authority
# -----------------------------------------------------------------------------
# for (source_authority in c("ICCAT", "IATTC", "IOTC", "WCPFC", "CCSBT")) {
#   run_step_summary(
#     workflow_file = "config/catch_ird_level2_local.json",
#     workflow_output = tunaatlas_level2_catch_path,
#     source_authority = source_authority
#   )
# }


# -----------------------------------------------------------------------------
# 8.2 Targeted Swordfish / WCPFC report
# -----------------------------------------------------------------------------
# config_level2 <- init_workflow_maybe_without_dbi(
#   here::here("config/catch_ird_level2_local.json")
# )
# on.exit(unlink(config_level2$job, recursive = TRUE), add = TRUE)
#
# config_level2$metadata$content$entities[[1]]$data$actions[[1]]$options$parameter_filtering <-
#   list(species_label = c("Swordfish"), source_authority = c("WCPFC"))
#
# CWP.dataset::summarising_step(
#   main_dir = tunaatlas_level2_catch_path,
#   connectionDB = config_level2$software$output$dbi %||% NULL,
#   config = config_level2,
#   sizepdf = "short",
#   savestep = FALSE,
#   usesave = FALSE,
#   source_authoritylist = "all",
#   nameoutput = "swo_wcpfc"
# )


# -----------------------------------------------------------------------------
# 8.3 Targeted Silky shark / IATTC report
# -----------------------------------------------------------------------------
# config_level2 <- init_workflow_maybe_without_dbi(
#   here::here("config/catch_ird_level2_local.json")
# )
# on.exit(unlink(config_level2$job, recursive = TRUE), add = TRUE)
#
# config_level2$metadata$content$entities[[1]]$data$actions[[1]]$options$parameter_filtering <-
#   list(species_label = c("Silky shark"), source_authority = c("IATTC"))
#
# CWP.dataset::summarising_step(
#   main_dir = tunaatlas_level2_catch_path,
#   connectionDB = config_level2$software$output$dbi %||% NULL,
#   config = config_level2,
#   sizepdf = "short",
#   savestep = FALSE,
#   usesave = FALSE,
#   source_authoritylist = "all",
#   nameoutput = "SilkyIATTConlynoSKH"
# )


# =============================================================================
# 9. NOTES FOR FUTURE RESTRUCTURING
# =============================================================================
#
# Suggested next step if the script keeps growing:
#   - Move package/environment setup to R/workflow_setup.R.
#   - Move DB/geoflow helpers to R/workflow_helpers.R.
#   - Move summary/reporting helpers to R/workflow_reporting.R.
#   - Keep this file as a short orchestration script containing only the ordered
#     execution of workflows.
#
# This would make the main script much shorter and would simplify testing of the
# helper functions independently from the full GTA workflow.
#
# =============================================================================
