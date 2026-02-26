#!/usr/bin/env Rscript
# bootstrap_preharmo.R
# Purpose: one-command bootstrap for pre-harmonization (copy data + run helpers)

message("=== Pre-harmonization bootstrap ===")

# ---- Paths (edit if needed) ----
repo_root <- path.expand("~/GitHubRepos/geoflow-tunaatlas/stable/geoflow-tunaatlas")

src_data_dir <- path.expand("~/blue-cloud-dataspace/GlobalFisheriesAtlas/data_pre_harmo_tRFMOs/GTA_2026")
dst_data_parent <- file.path(repo_root, "data")
dst_data_dir <- file.path(dst_data_parent, "GTA_2026")

functions_file <- file.path(
  repo_root, "R", "tunaatlas_scripts", "pre-harmonization", "rewrite_functions_as_rmd.R"
)

# ---- Checks ----
if (!dir.exists(repo_root)) stop("Repository root not found: ", repo_root)
if (!file.exists(functions_file)) stop("Functions file not found: ", functions_file)
if (!dir.exists(src_data_dir)) stop("Source data folder not found: ", src_data_dir)

dir.create(dst_data_parent, recursive = TRUE, showWarnings = FALSE)

# ---- Step 1: copy data (only if missing) ----
if (!dir.exists(dst_data_dir)) {
  message("Step 1/3 - Copying data folder to repo...")
  message("  From: ", src_data_dir)
  message("  To  : ", dst_data_parent)
  
  # Use cp -r for speed and to preserve structure
  cmd <- sprintf("cp -r '%s' '%s/'", src_data_dir, dst_data_parent)
  status <- system(cmd)
  
  if (status != 0) stop("Data copy failed (cp returned non-zero exit status).")
  message("  ✅ Data copied.")
} else {
  message("Step 1/3 - Data already present, skipping copy:")
  message("  ", dst_data_dir)
}

# ---- Step 2: load functions ----
message("Step 2/3 - Loading pre-harmonization functions...")
source(functions_file)

# here is optional, but you asked to keep it
suppressPackageStartupMessages(require(here))

# ---- Step 3: run pre-harmo helper ----
message("Step 3/3 - Running copy_prehamo_data_files()...")
copy_prehamo_data_files(dst_data_dir)

message("✅ Bootstrap finished successfully.")