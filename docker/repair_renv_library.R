#!/usr/bin/env Rscript
# =============================================================================
# docker/repair_renv_library.R
#
# Makes an existing renv library usable without downloading every package
# again (~2 GB):
#   1. renv::restore() installs only the packages that are missing or at
#      another version than in renv.lock;
#   2. every package is then loaded; only those that fail to load (for example
#      compiled against other system libraries) are rebuilt, at their renv.lock
#      version.
#
# Typical use: speed up tests in a container that mounts a renv library kept
# on your machine (e.g. a library first built on another system), instead of
# rebuilding the image from scratch.
#
# Run it from the repository root, where renv is active (.Rprofile):
#   Rscript docker/repair_renv_library.R              # every package of renv.lock
#   Rscript docker/repair_renv_library.R sf qs dplyr  # only these packages
#   Rscript docker/repair_renv_library.R --check      # report only, change nothing
#
# Exit status 0 when every package loads at the end, 1 otherwise.
# Adapted from firms-gta/tunaatlas_pie_map_shiny (testing_loading_of_all_packages.R).
# =============================================================================

args <- commandArgs(trailingOnly = TRUE)
check_only <- "--check" %in% args
packages <- setdiff(args, "--check")

locked <- renv::lockfile_read()$Packages
if (length(packages) == 0L) packages <- setdiff(names(locked), "R")

if (!check_only) renv::restore(prompt = FALSE)

loads <- function(pkg) {
  tryCatch({
    suppressPackageStartupMessages(loadNamespace(pkg))
    TRUE
  }, error = function(e) {
    message(sprintf("  %s: %s", pkg, conditionMessage(e)))
    FALSE
  })
}

failed <- Filter(Negate(loads), packages)
message(sprintf("%d package(s) checked, %d failed to load.",
                length(packages), length(failed)))

if (length(failed) > 0L && !check_only) {
  in_lock <- intersect(failed, names(locked))
  message("Rebuilding: ", paste(failed, collapse = ", "))
  if (length(in_lock) > 0L) {
    # renv.lock version, rebuilt even if a copy is in the cache
    renv::restore(packages = in_lock, rebuild = TRUE, prompt = FALSE)
  }
  for (pkg in setdiff(failed, in_lock)) {
    tryCatch(renv::install(pkg, prompt = FALSE),
             error = function(e) message(sprintf("  %s: %s", pkg, conditionMessage(e))))
  }
  failed <- Filter(Negate(loads), failed)
}

if (length(failed) > 0L) {
  message("Still failing: ", paste(failed, collapse = ", "))
  quit(status = 1L)
}
message("Every package loads.")
