source("renv/activate.R")
options(knitr.duplicate.label = "allow")

data_dir <- file.path("data", "GTA_2026")

cat("
======================================================================
  PRE-HARMONIZATION WORKFLOW – DATA REQUIREMENTS
======================================================================

This repository runs correctly as-is.
However, to execute the FULL pre-harmonization workflow,
the required input dataset (GTA_2026) must be available locally.

")

if (dir.exists(data_dir)) {
  
  cat("
  ✓ GTA_2026 dataset detected in data/GTA_2026/.

  To integrate the dataset properly into the project structure,
  please run:

      source('R/tunaatlas_scripts/pre-harmonization/bootstrap_preharmo.R')

  This will:
    • Verify the data structure
    • Copy/prepare required files
    • Configure the workflow environment

")
  
} else {
  
  cat("
  ⚠ GTA_2026 dataset not detected.

  To run the complete workflow, you need to provide the input data.

  OPTION 1 (Recommended) — Use the Blue-Cloud D4Science workspace:
  https://blue-cloud.d4science.org/group/bluecloud-gateway

    This ensures you are working in the same environment as the
    original workflow (same data access, same structure).

    After logging in, run:

        source('R/tunaatlas_scripts/pre-harmonization/bootstrap_preharmo.R')

  OPTION 2 — Manual setup:

    Download the GTA_2026 dataset manually and place it in:

        data/GTA_2026/

    Then run the bootstrap script above.

")
  
}

cat("
======================================================================

")
