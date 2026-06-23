#!/usr/bin/env Rscript
# =============================================================================
# PRE-HARMONISATION BOOTSTRAP
# =============================================================================
#
# Purpose
# -------
# Prepare pre-harmonisation resources before running the GTA workflow.
#
# This script is intentionally safe in non-interactive environments:
#   - If the Blue-Cloud cache is unavailable, the copy step is skipped.
#   - If the cache is available and files would be overwritten:
#       * interactive mode asks for confirmation;
#       * non-interactive mode skips the copy unless force_overwrite = TRUE.
#
# This prevents Docker, CI, or server runs from hanging on readline().
#
# Typical usage
# -------------
# Interactive:
#   source("R/tunaatlas_scripts/pre-harmonization/bootstrap_preharmo.R")
#
# Docker / non-interactive, no cache copy:
#   options(gta.copy_cache = FALSE)
#   source("R/tunaatlas_scripts/pre-harmonization/bootstrap_preharmo.R")
#
# Docker / non-interactive, forced cache copy:
#   options(gta.copy_cache = TRUE, gta.force_overwrite = TRUE)
#   source("R/tunaatlas_scripts/pre-harmonization/bootstrap_preharmo.R")
#
# =============================================================================


# =============================================================================
# 0. OPTIONS
# =============================================================================

# Whether the script is allowed to copy data from the Blue-Cloud cache.
# NULL means:
#   - copy if cache exists and the user confirms interactively;
#   - skip in non-interactive mode unless force_overwrite is TRUE.
copy_cache <- getOption("gta.copy_cache", NULL)

# Whether existing destination files can be overwritten without asking.
force_overwrite <- isTRUE(getOption("gta.force_overwrite", FALSE)) ||
  "--force" %in% commandArgs(trailingOnly = TRUE)

# Whether renv restore should be run from the bootstrap.
# In Docker images where renv has already been restored during build, this can be
# disabled with options(gta.bootstrap_restore_renv = FALSE).
restore_renv <- isTRUE(getOption("gta.bootstrap_restore_renv", TRUE))

src_data_dir <- getOption("gta.src_data_dir")

if (is.null(src_data_dir)) {
  src_data_dir <- path.expand(
    "~/blue-cloud-dataspace/GlobalFisheriesAtlas/data_pre_harmo_tRFMOs/All_raw_data_GTA_2026/"
  )
}

# =============================================================================
# 1. BASIC INITIALISATION
# =============================================================================

if (restore_renv) {
  renv::restore(prompt = FALSE)
}

message("=== Pre-harmonization bootstrap ===")

require(here)

repo_root <- here::here()

dst_data_parent <- file.path(repo_root, "data")
dst_data_dir <- file.path(dst_data_parent, "GTA_2026")

functions_file <- file.path(
  repo_root,
  "R",
  "tunaatlas_scripts",
  "pre-harmonization",
  "replace_preharmo_files_from_gta_2026_folder.R"
)


# =============================================================================
# 2. HELPER FUNCTIONS
# =============================================================================

cache_is_available <- function(src_data_dir, functions_file) {
  dir.exists(src_data_dir) && file.exists(functions_file)
}

find_common_csv_files <- function(src_data_dir, dst_data_dir) {
  src_files <- list.files(src_data_dir, pattern = "\\.csv$", recursive = TRUE)
  
  dst_files <- if (dir.exists(dst_data_dir)) {
    list.files(dst_data_dir, pattern = "\\.csv$", recursive = TRUE)
  } else {
    character(0)
  }
  
  intersect(basename(src_files), basename(dst_files))
}

confirm_cache_copy <- function(src_data_dir, dst_data_dir, force_overwrite = FALSE) {
  common_files <- find_common_csv_files(src_data_dir, dst_data_dir)
  
  if (length(common_files) == 0) {
    message("No common CSV files between cache and repository. No overwrite risk.")
    return(TRUE)
  }
  
  if (force_overwrite) {
    message("Force overwrite enabled: skipping interactive confirmation.")
    return(TRUE)
  }
  
  if (!interactive()) {
    message(
      "Cache copy skipped: non-interactive mode and overwrite would be required.\n",
      "Use options(gta.copy_cache = TRUE, gta.force_overwrite = TRUE) to force it."
    )
    return(FALSE)
  }
  
  msg <- paste0(
    "\n⚠️ Overwrite ", length(common_files), " CSV files in:\n",
    dst_data_dir, "\n",
    "from cache:\n",
    src_data_dir, "\n",
    "Type 'yes' to continue: "
  )
  
  answer <- trimws(tolower(readline(msg)))
  
  identical(answer, "yes")
}

copy_top_level_cache_files <- function(src_data_dir, dst_data_dir) {
  message("Step 1/3 - Checking top-level files in source cache...")
  
  src_files <- list.files(
    src_data_dir,
    full.names = TRUE,
    recursive = FALSE,
    include.dirs = FALSE
  )
  
  if (length(src_files) == 0) {
    stop("No top-level files found in source cache directory.", call. = FALSE)
  }
  
  dir.create(dst_data_dir, recursive = TRUE, showWarnings = FALSE)
  
  message("Found ", length(src_files), " top-level files to copy.")
  message("Starting file copy...")
  
  start_time <- Sys.time()
  
  for (i in seq_along(src_files)) {
    src_file <- src_files[i]
    file_name <- basename(src_file)
    dst_file <- file.path(dst_data_dir, file_name)
    
    file_size_mb <- round(file.info(src_file)$size / 1024^2, 2)
    
    message(sprintf("[%d/%d] Copying %s (%.2f MB)",
                    i, length(src_files), file_name, file_size_mb))
    
    file.copy(src_file, dst_file, overwrite = TRUE)
  }
  
  end_time <- Sys.time()
  
  message("✅ Cache copy completed.")
  message("Total time: ", round(difftime(end_time, start_time, units = "secs"), 2), " seconds.")
}

run_preharmo_file_replacement <- function(dst_data_dir, functions_file) {
  message("Step 2/3 - Loading pre-harmonization replacement functions...")
  source(functions_file)
  
  suppressPackageStartupMessages(require(here))
  
  message("Step 3/3 - Running replace_preharmo_files_from_gta_2026_folder()...")
  
  replace_preharmo_files_from_gta_2026_folder(
    gta_dir = dst_data_dir,
    preharmo_dir = here::here("R/tunaatlas_scripts/pre-harmonization"),
    dry_run = FALSE
  )
  
  message("✅ Pre-harmonization file replacement completed.")
}


# =============================================================================
# 3. CACHE COPY DECISION
# =============================================================================

if (!dir.exists(repo_root)) {
  stop("Repository root not found: ", repo_root, call. = FALSE)
}

dir.create(dst_data_parent, recursive = TRUE, showWarnings = FALSE)

same_data_dir <- normalizePath(src_data_dir, mustWork = FALSE) ==
  normalizePath(dst_data_dir, mustWork = FALSE)

cache_available <- cache_is_available(src_data_dir, functions_file)

if (!cache_available) {
  message(
    "Pre-harmonisation bootstrap skipped.\n",
    "Reason: source data directory or replacement function file is not available.\n",
    "Source directory: ", src_data_dir, "\n",
    "Functions file: ", functions_file
  )
  
} else if (same_data_dir) {
  message(
    "Source and destination data directories are identical.\n",
    "No copy needed: ", dst_data_dir
  )
  
  run_preharmo_file_replacement(dst_data_dir, functions_file)
  
} else {
  should_copy_cache <- isTRUE(copy_cache) ||
    (is.null(copy_cache) && interactive()) ||
    (is.null(copy_cache) && force_overwrite)
  
  if (!should_copy_cache) {
    message(
      "Cache copy skipped.\n",
      "Reason: gta.copy_cache is FALSE/NULL in non-interactive mode.\n",
      "Set options(gta.copy_cache = TRUE, gta.force_overwrite = TRUE) to enable it."
    )
    
  } else if (confirm_cache_copy(src_data_dir, dst_data_dir, force_overwrite)) {
    copy_top_level_cache_files(src_data_dir, dst_data_dir)
    run_preharmo_file_replacement(dst_data_dir, functions_file)
    
  } else {
    message("Cache copy skipped by user choice.")
  }
}
