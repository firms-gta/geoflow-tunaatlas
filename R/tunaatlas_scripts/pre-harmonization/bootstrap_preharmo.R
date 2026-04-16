#!/usr/bin/env Rscript
# bootstrap_preharmo.R
# Purpose: one-command bootstrap for pre-harmonization (copy data + run helpers)
renv::restore(prompt = FALSE)
message("=== Pre-harmonization bootstrap ===")
require(here)
# ---- Paths (edit if needed) ----
repo_root <- here::here()

src_data_dir <- path.expand("~/blue-cloud-dataspace/GlobalFisheriesAtlas/data_pre_harmo_tRFMOs/GTA_2026")
dst_data_parent <- file.path(repo_root, "data")
dst_data_dir <- file.path(dst_data_parent, "GTA_2026")

functions_file <- file.path(
  here::here(), "R", "tunaatlas_scripts", "pre-harmonization", "replace_preharmo_files_from_gta_2026_folder.R"
)

# ---- Checks ----
if (!dir.exists(repo_root)) stop("Repository root not found: ", repo_root)
if (!file.exists(functions_file)) stop("Functions file not found: ", functions_file)
if (!dir.exists(src_data_dir)) stop("Source data folder not found: ", src_data_dir)

dir.create(dst_data_parent, recursive = TRUE, showWarnings = FALSE)

# ---- Step 1: copy data files (TOP LEVEL ONLY, with logs) ----

message("Step 1/3 - Checking top-level files in source...")

src_files <- list.files(
  src_data_dir,
  full.names = TRUE,
  recursive = FALSE,
  include.dirs = FALSE
)

if (length(src_files) == 0) {
  stop("No top-level files found in source directory.")
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

message("✅ Copy completed.")
message("Total time: ", round(difftime(end_time, start_time, units = "secs"), 2), " seconds.")
# ---- Step 2: load functions ----
message("Step 2/3 - Loading pre-harmonization functions...")
source(functions_file)

# here is optional, but you asked to keep it
suppressPackageStartupMessages(require(here))

# ---- Step 3: run pre-harmo helper ----
message("Step 3/3 - Running copy_prehamo_data_files()...")
replace_preharmo_files_from_gta_2026_folder(gta_dir =dst_data_dir , preharmo_dir = here::here("R/tunaatlas_scripts/pre-harmonization"),
                                            dry_run=FALSE)

message("✅ Bootstrap finished successfully.")