# =============================================================================
# GTA 2026 WORKFLOW RUNNER
# =============================================================================
#
# Purpose
# -------
# This script runs the main Global Tuna Atlas 2026 processing chain.
#
# It can be used in two ways:
#   1. Interactively, by editing the default parameters below and sourcing this file.
#   2. Programmatically, by sourcing this file and calling run_gta_workflow().
#
# Docker examples
# ---------------
#
#   docker run \
#     -v /home/bastien/data/GTA_2026:/data/GTA_2026 \
#     -e GTA_STEPS=rawdata \
#     mon_image
#
#   docker run \
#     -v /home/bastien/GTA_2026.zip:/data/GTA_2026.zip \
#     -e GTA_STEPS=rawdata \
#     mon_image
#
# =============================================================================


# =============================================================================
# 0. DEFAULT USER PARAMETERS
# =============================================================================

steps_to_run <- c(
  # "all",
  "rawdata"
  # "effort",
  # "nominal",
  # "level0",
  # "level1",
  # "level2",
  # "reports"
)

data_source <- "auto"
data_path <- NULL
doi <- NULL

summarise_invalid_raw <- FALSE
stop_on_missing_inputs <- TRUE
bootstrap_restore_renv <- TRUE

env_or_null <- function(x) {
  value <- Sys.getenv(x, unset = "")
  if (identical(value, "")) NULL else value
}

existing_paths <- list(
  raw_nominal_catch = env_or_null("GTA_RAW_NOMINAL_CATCH"),
  raw_data_georef = env_or_null("GTA_RAW_DATA_GEOREF"),
  raw_data_georef_effort = env_or_null("GTA_RAW_DATA_GEOREF_EFFORT"),
  tunaatlas_effort = env_or_null("GTA_TUNAATLAS_EFFORT"),
  tunaatlas_nominal = env_or_null("GTA_TUNAATLAS_NOMINAL"),
  tunaatlas_level0_catch = env_or_null("GTA_TUNAATLAS_LEVEL0_CATCH"),
  tunaatlas_level1_catch = env_or_null("GTA_TUNAATLAS_LEVEL1_CATCH"),
  tunaatlas_level2_catch = env_or_null("GTA_TUNAATLAS_LEVEL2_CATCH")
)


# =============================================================================
# 1. DATA SOURCE PREPARATION
# =============================================================================

download_gta_data_from_doi <- function(doi, dst_data_dir) {
  stop(
    "DOI download is not implemented yet.\n",
    "Requested DOI: ", doi, "\n",
    "Expected destination: ", dst_data_dir,
    call. = FALSE
  )
}

flatten_single_root_dir <- function(path) {
  entries <- list.files(
    path,
    all.files = FALSE,
    no.. = TRUE,
    full.names = TRUE
  )
  
  if (length(entries) == 1 && dir.exists(entries[1])) {
    root_dir <- entries[1]
    
    message("Flattening archive root directory: ", basename(root_dir))
    
    inner_entries <- list.files(
      root_dir,
      all.files = TRUE,
      no.. = TRUE,
      full.names = TRUE
    )
    
    for (entry in inner_entries) {
      target <- file.path(path, basename(entry))
      
      if (!file.rename(entry, target)) {
        if (dir.exists(entry)) {
          dir.create(target, recursive = TRUE, showWarnings = FALSE)
          file.copy(
            from = list.files(entry, all.files = TRUE, no.. = TRUE, full.names = TRUE),
            to = target,
            recursive = TRUE,
            overwrite = TRUE
          )
          unlink(entry, recursive = TRUE, force = TRUE)
        } else {
          file.copy(entry, target, overwrite = TRUE)
          unlink(entry, force = TRUE)
        }
      }
    }
    
    unlink(root_dir, recursive = TRUE, force = TRUE)
  }
  
  invisible(path)
}

extract_gta_archive <- function(archive, dst_data_dir) {
  if (!file.exists(archive)) {
    stop("Archive not found: ", archive, call. = FALSE)
  }
  
  message("Cleaning data directory: ", dst_data_dir)
  unlink(dst_data_dir, recursive = TRUE, force = TRUE)
  dir.create(dst_data_dir, recursive = TRUE, showWarnings = FALSE)
  
  message("Extracting archive: ", archive)
  
  status <- system2(
    command = "unzip",
    args = c("-q", "-o", archive, "-d", dst_data_dir)
  )
  
  if (!identical(status, 0L)) {
    stop("Archive extraction failed: ", archive, call. = FALSE)
  }
  
  flatten_single_root_dir(dst_data_dir)
  
  top_files <- list.files(dst_data_dir, recursive = FALSE, all.files = FALSE)
  
  message("Top-level files/directories after extraction: ", length(top_files))
  message("Data directory ready: ", dst_data_dir)
  
  invisible(dst_data_dir)
}

prepare_gta_data_source <- function(data_source = "auto",
                                    data_path = NULL,
                                    doi = NULL,
                                    dst_data_dir = NULL) {
  
  if (!requireNamespace("here", quietly = TRUE)) {
    stop("Package 'here' is required before preparing the data source.", call. = FALSE)
  }
  
  if (is.null(dst_data_dir)) {
    dst_data_dir <- here::here("data", "GTA_2026")
  }
  
  data_source <- match.arg(
    data_source,
    choices = c("auto", "volume_dir", "volume_zip", "doi")
  )
  
  if (identical(data_source, "auto")) {
    
    if (dir.exists("/data/GTA_2026")) {
      message("Using mounted GTA data directory: /data/GTA_2026")
      return(normalizePath("/data/GTA_2026", mustWork = TRUE))
    }
    
    if (file.exists("/data/GTA_2026.zip")) {
      message("Using mounted GTA zip file: /data/GTA_2026.zip")
      
      extract_gta_archive(
        archive = "/data/GTA_2026.zip",
        dst_data_dir = dst_data_dir
      )
      
      return(normalizePath(dst_data_dir, mustWork = TRUE))
    }
    
    if (!is.null(doi)) {
      message("No mounted GTA data found. Downloading data from DOI: ", doi)
      dir.create(dst_data_dir, recursive = TRUE, showWarnings = FALSE)
      download_gta_data_from_doi(doi = doi, dst_data_dir = dst_data_dir)
      return(normalizePath(dst_data_dir, mustWork = TRUE))
    }
    
    stop(
      "No GTA data source found.\n",
      "Expected one of:\n",
      "  - mounted directory: /data/GTA_2026\n",
      "  - mounted zip:       /data/GTA_2026.zip\n",
      "  - explicit DOI via doi = '...'\n",
      call. = FALSE
    )
  }
  
  if (identical(data_source, "volume_dir")) {
    
    if (is.null(data_path)) {
      stop("data_path is required when data_source = 'volume_dir'.", call. = FALSE)
    }
    
    if (!dir.exists(data_path)) {
      stop("Mounted GTA data directory not found: ", data_path, call. = FALSE)
    }
    
    message("Using explicit GTA data directory: ", data_path)
    
    required_test_file <- file.path(
      data_path,
      "iotc_nominal_catch_firms_level0_2026-04-13.csv"
    )
    
    if (!file.exists(required_test_file)) {
      stop(
        "The GTA data directory exists, but the expected files are not at the top level.\n",
        "Missing test file: ", required_test_file, "\n",
        "Check that data_path points directly to the folder containing the raw files.",
        call. = FALSE
      )
    }
    
    return(normalizePath(data_path, mustWork = TRUE))
  }
  
  if (identical(data_source, "volume_zip")) {
    
    if (is.null(data_path)) {
      stop("data_path is required when data_source = 'volume_zip'.", call. = FALSE)
    }
    
    extract_gta_archive(
      archive = data_path,
      dst_data_dir = dst_data_dir
    )
    
    return(normalizePath(dst_data_dir, mustWork = TRUE))
  }
  
  if (identical(data_source, "doi")) {
    
    if (is.null(doi)) {
      stop("doi is required when data_source = 'doi'.", call. = FALSE)
    }
    
    dir.create(dst_data_dir, recursive = TRUE, showWarnings = FALSE)
    download_gta_data_from_doi(doi = doi, dst_data_dir = dst_data_dir)
    return(normalizePath(dst_data_dir, mustWork = TRUE))
  }
}


# =============================================================================
# 2. MAIN FUNCTION
# =============================================================================

run_gta_workflow <- function(steps_to_run = c("rawdata"),
                             summarise_invalid_raw = FALSE,
                             stop_on_missing_inputs = TRUE,
                             data_source = "auto",
                             data_path = NULL,
                             doi = NULL,
                             bootstrap_restore_renv = TRUE,
                             existing_paths = list()) {
  
  run_step <- function(step) {
    "all" %in% steps_to_run || step %in% steps_to_run
  }
  
  run_any_step <- function(steps) {
    "all" %in% steps_to_run || any(steps %in% steps_to_run)
  }
  
  default_existing_paths <- list(
    raw_nominal_catch = NULL,
    raw_data_georef = NULL,
    raw_data_georef_effort = NULL,
    tunaatlas_effort = NULL,
    tunaatlas_nominal = NULL,
    tunaatlas_level0_catch = NULL,
    tunaatlas_level1_catch = NULL,
    tunaatlas_level2_catch = NULL
  )
  
  existing_paths <- utils::modifyList(default_existing_paths, existing_paths)
  env_existing_paths <- list(
    raw_nominal_catch = env_or_null("GTA_RAW_NOMINAL_CATCH"),
    raw_data_georef = env_or_null("GTA_RAW_DATA_GEOREF"),
    raw_data_georef_effort = env_or_null("GTA_RAW_DATA_GEOREF_EFFORT"),
    tunaatlas_effort = env_or_null("GTA_TUNAATLAS_EFFORT"),
    tunaatlas_nominal = env_or_null("GTA_TUNAATLAS_NOMINAL"),
    tunaatlas_level0_catch = env_or_null("GTA_TUNAATLAS_LEVEL0_CATCH"),
    tunaatlas_level1_catch = env_or_null("GTA_TUNAATLAS_LEVEL1_CATCH"),
    tunaatlas_level2_catch = env_or_null("GTA_TUNAATLAS_LEVEL2_CATCH")
  )
  
  env_existing_paths <- env_existing_paths[!vapply(env_existing_paths, is.null, logical(1))]
  existing_paths <- utils::modifyList(existing_paths, env_existing_paths)
  
  get_existing_or_stop <- function(object_name, existing_path, step_name) {
    if (exists(object_name, inherits = FALSE)) {
      return(get(object_name, inherits = FALSE))
    }
    
    if (!is.null(existing_path)) {
      return(existing_path)
    }
    
    warning(
      "Missing path for step '", step_name, "'.\n",
      "Either run the upstream workflow in the same execution, or provide the ",
      "corresponding existing path in `existing_paths`.",
      call. = FALSE
    )
    
    return(NULL)
  }
  
  # ---------------------------------------------------------------------------
  # Minimal setup and data-source detection
  # ---------------------------------------------------------------------------
  
  require(here)
  setwd(here::here())
  
  gta_data_dir <- prepare_gta_data_source(
    data_source = data_source,
    data_path = data_path,
    doi = doi,
    dst_data_dir = here::here("data", "GTA_2026")
  )
  
  # This option is read by bootstrap_preharmo.R.
  # It ensures that Docker volumes or unzipped data are used instead of falling
  # back to the Blue-Cloud cache path.
  options(
    gta.src_data_dir = gta_data_dir,
    gta_data_dir = gta_data_dir,
    gta.bootstrap_restore_renv = bootstrap_restore_renv
  )
  
  source("R/tunaatlas_scripts/pre-harmonization/bootstrap_preharmo.R")
  
  library(renv)
  
  if (bootstrap_restore_renv) {
    renv::restore()
  }
  
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
  
  load_required_package <- function(package) {
    if (!require(package, character.only = TRUE)) {
      stop("Package could not be loaded: ", package, call. = FALSE)
    }
  }
  
  invisible(lapply(unique(required_packages), load_required_package))
  
  default_env_file <- ".env"
  if (file.exists(here::here("geoserver_sdi_lab.env"))) {
    default_env_file <- "geoserver_sdi_lab.env"
  }
  
  tryCatch(
    dotenv::load_dot_env(file = here::here(default_env_file)),
    error = function(e) message("No environment file loaded: ", e$message)
  )
  
  source(here::here("R/running_time_of_workflow.R"))
  source(here::here("R/executeAndRename.R"))
  source(here::here("R/launching_workflows/workflow_helpers.R"))
  
  setwd(here::here())
  
  # ---------------------------------------------------------------------------
  # Mandatory JSON input checks
  # ---------------------------------------------------------------------------
  
  preharmo_json_files <- c(
    "config/Nominal_catch_2026.json",
    "config/All_raw_data_georef.json",
    "config/All_raw_data_georef_effort.json"
  )
  
  message("Checking pre-harmonisation JSON input files before launching workflows...")
  
  preharmo_check <- check_files_from_json(
    json_files = preharmo_json_files,
    data_dir = "data"
  )
  
  if (length(preharmo_check$missing) > 0) {
    missing_message <- paste(
      "Missing required input files detected before workflow launch:",
      paste0("  - ", preharmo_check$missing, collapse = "\n"),
      sep = "\n"
    )
    
    if (stop_on_missing_inputs) {
      stop(missing_message, call. = FALSE)
    }
    
    warning(missing_message, call. = FALSE)
  }
  
  # ---------------------------------------------------------------------------
  # Pre-harmonisation workflows
  # ---------------------------------------------------------------------------
  
  if (run_any_step(c("rawdata", "raw_effort"))) {
    raw_data_georef_effort <- execute_workflow_maybe_upload(
      file = here::here("config/All_raw_data_georef_effort.json"),
      rename_suffix = "_raw_data_georef_effort_final"
    )
    
    running_time_of_workflow(raw_data_georef_effort)
    
    if (summarise_invalid_raw) {
      summarise_invalid(raw_data_georef_effort)
    }
  }
  
  if (run_any_step(c("rawdata", "raw_nominal"))) {
    raw_nominal_catch <- execute_workflow_maybe_upload(
      file = here::here("config/Nominal_catch_2026.json"),
      rename_suffix = "_raw_nominal_catch_2026_final"
    )
    
    running_time_of_workflow(raw_nominal_catch)
    
    if (summarise_invalid_raw) {
      summarise_invalid(raw_nominal_catch)
    }
  }
  
  if (run_any_step(c("rawdata", "raw_georef"))) {
    raw_data_georef <- execute_workflow_maybe_upload(
      file = here::here("config/All_raw_data_georef.json"),
      rename_suffix = "_raw_data_georef_final"
    )
    
    running_time_of_workflow(raw_data_georef)
    
    if (summarise_invalid_raw) {
      summarise_invalid(raw_data_georef)
    }
  }
  
  if (run_step("qa_rmd")) {
    raw_nominal_catch <- get_existing_or_stop(
      "raw_nominal_catch",
      existing_paths$raw_nominal_catch,
      "qa_rmd / raw nominal"
    )
    
    raw_data_georef <- get_existing_or_stop(
      "raw_data_georef",
      existing_paths$raw_data_georef,
      "qa_rmd / raw georef"
    )
    
    raw_data_georef_effort <- get_existing_or_stop(
      "raw_data_georef_effort",
      existing_paths$raw_data_georef_effort,
      "qa_rmd / raw effort"
    )
    
    source(here::here("R/tunaatlas_scripts/pre-harmonization/rewrite_functions_as_rmd.R"))
    
    dir.create(file.path(raw_nominal_catch, "data"), showWarnings = FALSE, recursive = TRUE)
    rewrite_functions_as_rmd(raw_nominal_catch)
    
    dir.create(file.path(raw_data_georef, "data"), showWarnings = FALSE, recursive = TRUE)
    rewrite_functions_as_rmd(raw_data_georef)
    
    dir.create(file.path(raw_data_georef_effort, "data"), showWarnings = FALSE, recursive = TRUE)
    rewrite_functions_as_rmd(raw_data_georef_effort)
  }
  
  # ---------------------------------------------------------------------------
  # Harmonised dataset generation
  # ---------------------------------------------------------------------------
  
  if (run_step("effort")) {
    tunaatlas_effort_path <- execute_workflow_maybe_upload(
      file = here::here("config/create_effort_dataset_2026.json"),
      rename_suffix = "new_efforts_final"
    )
  }
  
  if (run_step("nominal")) {
    tunaatlas_nominal_path <- execute_workflow_maybe_upload(
      file = here::here("config/create_nominal_dataset_2026.json"),
      rename_suffix = "nominal_final"
    )
  }
  
  if (run_step("level0")) {
    tunaatlas_level0_catch_path <- execute_workflow_maybe_upload(
      file = here::here("config/catch_ird_level0_local.json"),
      rename_suffix = "level_0_catch_2026"
    )
    if (run_step("summaries")) {
      if (!is.null(tunaatlas_level0_catch_path)) {
    run_step_summary(
      workflow_file = "config/catch_ird_level0_local.json",
      workflow_output = tunaatlas_level0_catch_path,
      source_authority = "all"
    )
      }
    }
  }
  
  if (run_step("level1")) {
    tunaatlas_level1_catch_path <- execute_workflow_maybe_upload(
      file = here::here("config/catch_ird_level1_local.json"),
      rename_suffix = "level_1_catch_2026"
    )
    if (run_step("summaries")) {
      if (!is.null(tunaatlas_level1_catch_path)) {
    run_step_summary(
      workflow_file = "config/catch_ird_level1_local.json",
      workflow_output = tunaatlas_level1_catch_path,
      source_authority = "all"
    )
      }
    }
  }
  
  if (run_step("level2")) {
      
    tunaatlas_level2_catch_path <- execute_workflow_maybe_upload(
      file = here::here("config/catch_ird_level2_local.json"),
      rename_suffix = "level_2_catch_2026"
    )
    if (run_step("summaries")) {
      if (!is.null(tunaatlas_level2_catch_path)) {
        
    run_step_summary(
      workflow_file = "config/catch_ird_level2_local.json",
      workflow_output = tunaatlas_level2_catch_path,
      source_authority = "all"
    )
      }
    }

  }
  
  # ---------------------------------------------------------------------------
  # Summary-only reruns
  # ---------------------------------------------------------------------------
  
  if (run_step("summaries")) {
    tunaatlas_level0_catch_path <- get_existing_or_stop(
      "tunaatlas_level0_catch_path",
      existing_paths$tunaatlas_level0_catch,
      "summaries / level0"
    )
    
    tunaatlas_level1_catch_path <- get_existing_or_stop(
      "tunaatlas_level1_catch_path",
      existing_paths$tunaatlas_level1_catch,
      "summaries / level1"
    )
    
    tunaatlas_level2_catch_path <- get_existing_or_stop(
      "tunaatlas_level2_catch_path",
      existing_paths$tunaatlas_level2_catch,
      "summaries / level2"
    )
    if (!is.null(tunaatlas_level0_catch_path)) {
      
    run_step_summary(
      workflow_file = "config/catch_ird_level0_local.json",
      workflow_output = tunaatlas_level0_catch_path,
      source_authority = "all"
    )
    }
    if (!is.null(tunaatlas_level1_catch_path)) {
      
    run_step_summary(
      workflow_file = "config/catch_ird_level1_local.json",
      workflow_output = tunaatlas_level1_catch_path,
      source_authority = "all"
    )
    }
    if (!is.null(tunaatlas_level2_catch_path)) {
      
    run_step_summary(
      workflow_file = "config/catch_ird_level2_local.json",
      workflow_output = tunaatlas_level2_catch_path,
      source_authority = "all"
    )
    }
  }
  
  # ---------------------------------------------------------------------------
  # Quality control and comparison reports
  # ---------------------------------------------------------------------------
  
  if (run_step("reports")) {
    tunaatlas_nominal_path <- get_existing_or_stop(
      "tunaatlas_nominal_path",
      existing_paths$tunaatlas_nominal,
      "reports / nominal"
    )
    
    tunaatlas_level2_catch_path <- get_existing_or_stop(
      "tunaatlas_level2_catch_path",
      existing_paths$tunaatlas_level2_catch,
      "reports / level2"
    )
    
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
    
    ggplot2::ggsave(
      filename = file.path("data", "plot_georef_vs_nominal_evolution.png"),
      plot = res$diff_plots$sums,
      width = 16,
      height = 12,
      dpi = 300
    )
    
    source(here::here("R/ongoing_projects/check_georef_vs_nominal_entity.R"))
    
    level2_entity_paths <- list(
      paste0(tunaatlas_level2_catch_path, "/entities/global_catch_ird_level2_1950_2024")
    )
    
    results <- lapply(level2_entity_paths, run_analysis)
    saveRDS(results, "data/check_georef_vs_nominal_entity.rds")
  }
  
  output_paths <- list(
    gta_data_dir = gta_data_dir,
    raw_nominal_catch = if (exists("raw_nominal_catch", inherits = FALSE)) raw_nominal_catch else NULL,
    raw_data_georef = if (exists("raw_data_georef", inherits = FALSE)) raw_data_georef else NULL,
    raw_data_georef_effort = if (exists("raw_data_georef_effort", inherits = FALSE)) raw_data_georef_effort else NULL,
    tunaatlas_effort = if (exists("tunaatlas_effort_path", inherits = FALSE)) tunaatlas_effort_path else NULL,
    tunaatlas_nominal = if (exists("tunaatlas_nominal_path", inherits = FALSE)) tunaatlas_nominal_path else NULL,
    tunaatlas_level0_catch = if (exists("tunaatlas_level0_catch_path", inherits = FALSE)) tunaatlas_level0_catch_path else NULL,
    tunaatlas_level1_catch = if (exists("tunaatlas_level1_catch_path", inherits = FALSE)) tunaatlas_level1_catch_path else NULL,
    tunaatlas_level2_catch = if (exists("tunaatlas_level2_catch_path", inherits = FALSE)) tunaatlas_level2_catch_path else NULL
  )
  
  message("Selected GTA workflow steps completed: ", paste(steps_to_run, collapse = ", "))
  
  invisible(output_paths)
}


# =============================================================================
# 3. INTERACTIVE EXECUTION
# =============================================================================

if (sys.nframe() == 0) {
  run_gta_workflow(
    steps_to_run = steps_to_run,
    summarise_invalid_raw = summarise_invalid_raw,
    stop_on_missing_inputs = stop_on_missing_inputs,
    data_source = data_source,
    data_path = data_path,
    doi = doi,
    bootstrap_restore_renv = bootstrap_restore_renv,
    existing_paths = existing_paths
  )
}

# =============================================================================
