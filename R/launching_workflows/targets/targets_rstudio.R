# =============================================================================
# {targets} from RStudio: computation and debugging only, never for publication
# =============================================================================
#
#   source("R/launching_workflows/targets/targets_rstudio.R")
#
#   gta_tar_graph()                  # dependency graph, outdated steps in colour
#   gta_tar_outdated()               # steps that would re-run
#   gta_tar_make("level0")           # brings level0 (and what it needs) up to date
#   gta_tar_make(c("level1", "nominal"))
#   gta_tar_make("level0", in_session = TRUE)  # runs in this session: browser(),
#                                              # debug() and traceback() work
#   gta_tar_job("level0")            # job directory of the last level0 run
#   gta_tar_invalidate("level0")     # force level0 (and downstream) to re-run
#
# Steps: raw_nominal, raw_effort, raw_georef (or "rawdata" for the three),
# nominal, effort, level0, level1, level2. They run without database
# (GTA_NO_DB=true), so the cache stays valid whatever happens to PostGIS.
#
# State: /cache/_targets in the Docker stack, otherwise _targets/ at the root of
# the repository (git-ignored). GTA_TARGETS_STORE overrides it.
#
# Publication (DB, services, Zenodo) always goes through the classic launcher,
# from scratch, without this cache.
# =============================================================================

.gta_tar_script <- function() here::here("R/launching_workflows/targets/_targets.R")

.gta_tar_store <- function() {
  store <- Sys.getenv("GTA_TARGETS_STORE", "")
  if (nzchar(store)) return(store)
  if (dir.exists("/cache")) "/cache/_targets" else here::here("_targets")
}

.gta_tar_names <- function(steps) {
  steps <- unique(unlist(lapply(steps, function(s) {
    if (identical(s, "rawdata")) c("raw_nominal", "raw_effort", "raw_georef") else s
  })))
  known <- c("raw_nominal", "raw_effort", "raw_georef",
             "nominal", "effort", "level0", "level1", "level2")
  unknown <- setdiff(steps, known)
  if (length(unknown) > 0) {
    stop("Unknown step(s): ", paste(unknown, collapse = ", "),
         ". Available: rawdata, ", paste(known, collapse = ", "), call. = FALSE)
  }
  steps
}

gta_tar_make <- function(steps = "level0", in_session = FALSE) {
  old <- setwd(here::here())
  on.exit(setwd(old), add = TRUE)
  wanted <- .gta_tar_names(steps)
  if (isTRUE(in_session)) {
    targets::tar_make(names = tidyselect::any_of(wanted), script = .gta_tar_script(),
                      store = .gta_tar_store(), callr_function = NULL)
  } else {
    targets::tar_make(names = tidyselect::any_of(wanted), script = .gta_tar_script(),
                      store = .gta_tar_store())
  }
  invisible(lapply(stats::setNames(nm = .gta_tar_names(steps)), gta_tar_job))
}

gta_tar_outdated <- function(steps = NULL) {
  old <- setwd(here::here())
  on.exit(setwd(old), add = TRUE)
  targets::tar_outdated(
    names = if (is.null(steps)) NULL else tidyselect::all_of(.gta_tar_names(steps)),
    script = .gta_tar_script(), store = .gta_tar_store(),
    reporter = "forecast"
  )
}

gta_tar_graph <- function() {
  old <- setwd(here::here())
  on.exit(setwd(old), add = TRUE)
  targets::tar_visnetwork(script = .gta_tar_script(), store = .gta_tar_store(),
                          targets_only = TRUE)
}

gta_tar_job <- function(step) {
  names <- .gta_tar_names(step)
  jobs <- lapply(stats::setNames(nm = names), function(n) {
    tryCatch(targets::tar_read_raw(n, store = .gta_tar_store()),
             error = function(e) NULL)
  })
  if (length(jobs) == 1) jobs[[1]] else jobs
}

gta_tar_invalidate <- function(steps) {
  targets::tar_invalidate(tidyselect::all_of(.gta_tar_names(steps)),
                          store = .gta_tar_store())
}
