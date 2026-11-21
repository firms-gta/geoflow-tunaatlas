#!/usr/bin/env Rscript
# =============================================================================
# Compare the outputs of two workflow runs, for example master (A) and a
# branch (B) run on the same input data with compose/run_workflow.sh.
#
# Usage: Rscript tests/compare_runs.R <runtime/jobs of A> <runtime/jobs of B>
#
# * Jobs are matched by name, without their timestamp prefix.
# * Data files (.csv, .qs, .rds) are read and compared as data: rows sorted,
#   column order kept, numbers with a 1e-9 tolerance. Dates and job ids in
#   file names do not count.
# * Other output files (.gpkg, .parquet, .nc) are only checked for presence.
#
# Exit status 0 when both runs produced the same jobs, the same files and the
# same data; 1 otherwise.
# =============================================================================

args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 2L) stop("usage: compare_runs.R <jobs_dir_A> <jobs_dir_B>")

data_ext <- "\\.(csv|qs|rds)$"
presence_ext <- "\\.(gpkg|parquet|nc)$"

list_jobs <- function(root) {
  dirs <- list.dirs(root, recursive = FALSE)
  # The most recent run wins when the same job was run twice.
  dirs <- sort(dirs)
  keys <- sub("^[0-9]{14}_?", "", basename(dirs))
  keys[keys == ""] <- "(unnamed)"  # e.g. the services job
  stats::setNames(dirs, keys)[!duplicated(keys, fromLast = TRUE)]
}

read_any <- function(path) {
  if (grepl("\\.csv$", path)) {
    data.table::fread(path, colClasses = "character", showProgress = FALSE)
  } else if (grepl("\\.qs$", path)) {
    qs::qread(path)
  } else {
    readRDS(path)
  }
}

normalise <- function(x) {
  if (inherits(x, "sf")) x <- sf::st_drop_geometry(x)
  if (!is.data.frame(x)) return(x)
  x <- as.data.frame(x, stringsAsFactors = FALSE)
  for (j in seq_along(x)) if (is.factor(x[[j]])) x[[j]] <- as.character(x[[j]])
  if (nrow(x) > 0L && ncol(x) > 0L) {
    x <- x[do.call(order, c(unname(as.list(x)), list(na.last = TRUE))), , drop = FALSE]
  }
  rownames(x) <- NULL
  x
}

same_data <- function(a, b) {
  res <- tryCatch(
    all.equal(normalise(read_any(a)), normalise(read_any(b)),
              tolerance = 1e-9, check.attributes = FALSE),
    error = function(e) paste("read error:", conditionMessage(e))
  )
  if (isTRUE(res)) TRUE else paste(head(res, 3), collapse = " | ")
}

jobs_a <- list_jobs(args[1])
jobs_b <- list_jobs(args[2])
problems <- character(0)
note <- function(...) problems <<- c(problems, paste0(...))

for (j in setdiff(names(jobs_a), names(jobs_b))) note("job only in A: ", j)
for (j in setdiff(names(jobs_b), names(jobs_a))) note("job only in B: ", j)

n_compared <- 0L
for (j in intersect(names(jobs_a), names(jobs_b))) {
  pattern <- paste0(data_ext, "|", presence_ext)
  fa <- list.files(jobs_a[[j]], pattern = pattern, recursive = TRUE)
  fb <- list.files(jobs_b[[j]], pattern = pattern, recursive = TRUE)
  for (f in setdiff(fa, fb)) note(j, ": file only in A: ", f)
  for (f in setdiff(fb, fa)) note(j, ": file only in B: ", f)
  for (f in intersect(fa, fb)) {
    if (!grepl(data_ext, f)) next
    res <- same_data(file.path(jobs_a[[j]], f), file.path(jobs_b[[j]], f))
    n_compared <- n_compared + 1L
    if (!isTRUE(res)) note(j, ": ", f, ": ", res)
  }
}

cat(sprintf("Jobs: %d in A, %d in B. Data files compared: %d.\n",
            length(jobs_a), length(jobs_b), n_compared))
if (length(problems) == 0L) {
  cat("IDENTICAL: same jobs, same files, same data.\n")
  quit(status = 0L)
}
cat(sprintf("%d difference(s):\n", length(problems)))
cat(paste0("  - ", problems), sep = "\n")
quit(status = 1L)
