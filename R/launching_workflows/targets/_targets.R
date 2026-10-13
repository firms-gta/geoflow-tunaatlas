# =============================================================================
# GTA workflow driven by {targets}
# =============================================================================
#
# Each workflow step is one target. A step is re-run only when one of these
# changed: its geoflow configuration and every file it depends on (entity
# CSVs, action and generation scripts, R files they source, code-list files,
# see targets_deps.R), the launcher code, or an upstream step.
#
# Dependencies between steps (an arrow = "re-runs if the other one re-ran"):
#
#              raw_nominal     raw_effort     raw_georef ──────────┐
#                    │              │              │               │
#                    v              v              v               │
#                 nominal         effort         level0            │
#                    │                             │               │
#                    │                             v               │
#                    │                          level1             │
#                    │                                             v
#                    └───────────────────────────────────────>  level2
#
#   * raw_nominal re-runs nominal and level2;
#   * raw_effort re-runs effort only;
#   * raw_georef re-runs level0, level1 and level2.
#
# No database: every step runs with GTA_NO_DB=true (database software removed,
# no upload), so a result never depends on whether PostGIS was reachable and
# the cache stays valid when the database volume is deleted. The database and
# the services (GeoServer, GeoNetwork, Zenodo) are publication, done with the
# classic launcher (run_gta_2026_workflow_cli.R, GTA_STEPS=DB,...,services).
#
# Raw input files: each raw_* step tracks only the files named in its own
# entity CSV (./data/GTA_2026/...), so a new IOTC effort file re-runs
# raw_effort but not raw_georef. Outputs of other steps are not tracked as
# files: the links above are the only way a step re-runs because of another.
#
# Meant for computation and debugging (RStudio), not for publication. Do not
# run this file directly. Use, from RStudio:
#   source("R/launching_workflows/targets/targets_rstudio.R"); gta_tar_make("level0")
# or from a terminal:
#   GTA_STEPS=level0 Rscript R/launching_workflows/targets/run_targets_cli.R
#
# Known limits:
#   * Scripts reached through computed paths that are not written as a file
#     name anywhere are not tracked. Invalidate by hand if needed, e.g.
#     targets::tar_invalidate(level0, store = "/cache/_targets").
#   * Raw files downloaded from a DOI (GTA_DATA_SOURCE=doi) are not on disk
#     when the files are listed, so they are not tracked.
#   * summaries / reports / qa_rmd steps are not covered: use the classic
#     launcher for them.
# =============================================================================

library(targets)

# Defines run_gta_workflow() and reads the GTA_* environment variables
# (data_source, data_path, doi, doi_file, ...). It does not start the workflow
# when sourced.
source("R/launching_workflows/GTA_2026_creation.R")
source("R/launching_workflows/targets/targets_deps.R")

# Computation only, never the database (see workflow_helpers.R). Set here so
# that it applies in the R process where tar_make() runs the steps.
Sys.setenv(GTA_NO_DB = "true")

# Runs one step and returns the path(s) of the job directory it created.
# The other arguments are only there to declare the dependencies.
gta_step <- function(step, upstream = NULL, inputs = NULL, launcher = NULL) {
  old_wd <- getwd()
  on.exit(setwd(old_wd), add = TRUE)

  out <- run_gta_workflow(
    steps_to_run = step,
    summarise_invalid_raw = summarise_invalid_raw,
    stop_on_missing_inputs = stop_on_missing_inputs,
    data_source = data_source,
    data_path = data_path,
    doi = doi,
    doi_file = doi_file,
    bootstrap_restore_renv = bootstrap_restore_renv,
    existing_paths = existing_paths
  )

  out <- out[setdiff(names(out), "gta_data_dir")]  # always set, not a job
  out <- unlist(lapply(Filter(Negate(is.null), out), as.character))
  if (length(out) == 0) {
    stop("Step '", step, "' did not return any job directory.")
  }
  out
}

# Files of a step's configuration(s), re-listed at every run (cheap) so that a
# newly sourced script is picked up; the step itself only re-runs if one of
# the listed files changed.
cfg_target <- function(name, ..., include_data = FALSE) {
  configs <- c(...)
  tar_target_raw(
    name,
    bquote(unique(unlist(lapply(.(configs), gta_config_deps,
                                include_data = .(include_data))))),
    format = "file",
    cue = tar_cue(mode = "always")
  )
}

list(
  # --- files each step depends on ---------------------------------------------
  tar_target(launcher_files, gta_launcher_deps(), format = "file",
             cue = tar_cue(mode = "always")),
  cfg_target("cfg_raw_nominal", "config/Nominal_catch_2026.json",           include_data = TRUE),
  cfg_target("cfg_raw_effort",  "config/All_raw_data_georef_effort.json",  include_data = TRUE),
  cfg_target("cfg_raw_georef",  "config/All_raw_data_georef.json",         include_data = TRUE),
  cfg_target("cfg_nominal",     "config/create_nominal_dataset_2026.json"),
  cfg_target("cfg_effort",      "config/create_effort_dataset_2026.json"),
  cfg_target("cfg_level0",      "config/catch_ird_level0_local.json"),
  cfg_target("cfg_level1",      "config/catch_ird_level1_local.json"),
  cfg_target("cfg_level2",      "config/catch_ird_level2_local.json"),

  # --- workflow steps -----------------------------------------------------------
  tar_target(raw_nominal, gta_step("raw_nominal",
                                   inputs = cfg_raw_nominal,
                                   launcher = launcher_files)),
  tar_target(raw_effort,  gta_step("raw_effort",
                                   inputs = cfg_raw_effort,
                                   launcher = launcher_files)),
  tar_target(raw_georef,  gta_step("raw_georef",
                                   inputs = cfg_raw_georef,
                                   launcher = launcher_files)),

  tar_target(nominal, gta_step("nominal", upstream = raw_nominal,
                               inputs = cfg_nominal, launcher = launcher_files)),
  tar_target(effort,  gta_step("effort",  upstream = raw_effort,
                               inputs = cfg_effort, launcher = launcher_files)),
  tar_target(level0,  gta_step("level0",  upstream = raw_georef,
                               inputs = cfg_level0, launcher = launcher_files)),
  tar_target(level1,  gta_step("level1",  upstream = level0,
                               inputs = cfg_level1, launcher = launcher_files)),
  tar_target(level2,  gta_step("level2",  upstream = list(raw_georef, nominal),
                               inputs = cfg_level2, launcher = launcher_files))
)
