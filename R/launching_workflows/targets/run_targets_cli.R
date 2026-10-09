#!/usr/bin/env Rscript
# =============================================================================
# GTA 2026 entrypoint using {targets} (computation, without database)
# =============================================================================
#
# Same GTA_* variables as run_gta_2026_workflow_cli.R. Steps already done and
# still up to date are skipped.
#
#   GTA_STEPS          steps to bring up to date (their upstream steps are
#                      also run if they are missing or outdated)
#   GTA_TARGETS_STORE  where {targets} keeps its state. Default: /cache/_targets
#                      (runtime/cache on the host, so it survives the container)
#   GTA_TARGETS_RESET  "true" to forget everything and run all selected steps
#
# Steps run without database (GTA_NO_DB=true). DB and services are
# publication: run them with run_gta_2026_workflow_cli.R.
# =============================================================================

step_to_target <- list(
  rawdata = c("raw_nominal", "raw_effort", "raw_georef"),
  raw_nominal = "raw_nominal", raw_effort = "raw_effort", raw_georef = "raw_georef",
  nominal = "nominal", effort = "effort",
  level0 = "level0", level1 = "level1", level2 = "level2"
)

steps <- trimws(strsplit(Sys.getenv("GTA_STEPS", "rawdata"), ",")[[1]])
unknown <- setdiff(steps, names(step_to_target))
if (length(unknown) > 0) {
  stop(
    "Steps not available with {targets}: ", paste(unknown, collapse = ", "),
    ". Use R/launching_workflows/run_gta_2026_workflow_cli.R for them."
  )
}
wanted <- unique(unlist(step_to_target[steps], use.names = FALSE))

store <- Sys.getenv("GTA_TARGETS_STORE", "")
if (!nzchar(store)) {
  store <- if (dir.exists("/cache")) "/cache/_targets" else "_targets"
}
script <- "R/launching_workflows/targets/_targets.R"

if (tolower(Sys.getenv("GTA_TARGETS_RESET", "false")) %in% c("true", "1", "yes")) {
  message("GTA_TARGETS_RESET: removing the {targets} store ", store)
  unlink(store, recursive = TRUE)
}

message("{targets} store: ", store)
message("Targets requested: ", paste(wanted, collapse = ", "))

targets::tar_make(
  names = tidyselect::all_of(wanted),
  script = script,
  store = store
)

# Summary: which job directory belongs to which step.
meta <- targets::tar_meta(store = store, fields = c("name", "seconds", "error"))
print(meta[meta$name %in% wanted, ])
for (t in wanted) {
  cat(t, ":", paste(targets::tar_read_raw(t, store = store), collapse = "\n    "), "\n")
}
