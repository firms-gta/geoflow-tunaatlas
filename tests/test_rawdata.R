#!/usr/bin/env Rscript
# =============================================================================
# tests/test_rawdata.R
#
# Runs the geoflow action of every raw dataset (config/entities/entities_raw_*.csv)
# on the committed sample data, outside geoflow, and compares what it writes
# with the expected outputs in tests/expected/rawdata/<entity>/.
#
# The action gets a minimal stand-in for the geoflow entity: data$source,
# getJobDataResource(), setTemporalExtent() and addResource(), the only parts
# of the entity that the raw dataset actions use. Inputs come from
# tests/sample_data (files of data/GTA_2026) and data/ (code lists).
#
# Usage, from the repository root:
#   Rscript tests/test_rawdata.R                    # every raw dataset
#   Rscript tests/test_rawdata.R catch_5deg_1m_ps_wcpfc_level0 ...
#   Rscript tests/test_rawdata.R --update [ids...]  # (re)write the expected outputs
#
# Exit status 0 when every output matches, 1 otherwise. When a change is meant
# to change an output, run --update and commit the new expected files: the
# change then shows in the diff.
# =============================================================================

args <- commandArgs(trailingOnly = TRUE)
update <- "--update" %in% args
only <- setdiff(args, "--update")
expected_root <- "tests/expected/rawdata"

# --- Same R session as in the workflow -----------------------------------------
# The launcher attaches these packages and defines log_info() before geoflow
# runs the actions; some actions rely on them (e.g. read_excel() unqualified).
launcher_packages <- c(
  "tinytex", "googledrive", "gsheet", "readr", "plotrix", "janitor",
  "dotenv", "data.table", "here", "xfun", "RPostgreSQL", "RPostgres", "DBI",
  "terra", "sf", "RSQLite", "ows4R", "sp", "flextable", "dplyr", "stringr",
  "tibble", "bookdown", "knitr", "purrr", "readxl", "rlang", "kableExtra",
  "tidyr", "ggplot2", "fs", "stats", "RColorBrewer", "cowplot", "tmap",
  "curl", "officer", "reshape2", "tools", "plogr", "futile.logger",
  "lubridate", "geoflow"
)
invisible(suppressMessages(suppressWarnings(
  lapply(launcher_packages, require, character.only = TRUE, quietly = TRUE)
)))
source("workflow/launchers/workflow_helpers.R")
here::i_am("tests/test_rawdata.R")   # repository root, before any setwd()

# --- Raw dataset entities ---------------------------------------------------------
read_entities <- function() {
  files <- Sys.glob("config/entities/entities_raw_*.csv")
  rows <- do.call(rbind, lapply(files, function(f) {
    x <- utils::read.csv(f, stringsAsFactors = FALSE, check.names = FALSE, encoding = "UTF-8")
    x[, c("Identifier", "Data")]
  }))
  field <- function(data, key) {
    m <- regmatches(data, regexpr(sprintf("%s:[^\n]*", key), data))
    if (length(m) == 0) return(NA_character_)
    sub("_$", "", sub(sprintf("^%s:", key), "", m))
  }
  data.frame(
    id = sub("^id:", "", sub("_\n.*$", "", sub("\n.*$", "", rows$Identifier))),
    sources = vapply(rows$Data, field, "", key = "source", USE.NAMES = FALSE),
    script = sub("^[^@]*@", "", vapply(rows$Data, field, "", key = "action", USE.NAMES = FALSE)),
    stringsAsFactors = FALSE
  )
}

input_file <- function(source) {
  if (startsWith(source, "./data/GTA_2026/")) {
    file.path("tests/sample_data", basename(source))
  } else {
    sub("^\\./", "", source)
  }
}

# --- Run one action outside geoflow --------------------------------------------
run_entity <- function(e) {
  workdir <- tempfile("rawdata_")
  dir.create(workdir)
  sources <- strsplit(e$sources, ",", fixed = TRUE)[[1]]
  for (s in sources) {
    if (!file.copy(input_file(s), file.path(workdir, basename(s)))) {
      stop("missing input: ", input_file(s), call. = FALSE)
    }
  }
  inputs <- list.files(workdir)
  record <- new.env()
  record$resources <- list()
  entity <- list(
    data = list(source = as.list(basename(sources))),
    getJobDataResource = function(config, filename) file.path(workdir, filename),
    setTemporalExtent = function(x) record$extent <- x,
    addResource = function(name, value) record$resources[[name]] <- value
  )
  action_fun <- eval(parse(sub("^\\./", "", e$script), encoding = "UTF-8"))
  owd <- setwd(workdir)
  on.exit(setwd(owd), add = TRUE)
  invisible(utils::capture.output(suppressMessages(suppressWarnings(
    action_fun(action = NULL, entity = entity, config = NULL)
  ))))
  # Outputs: the files the action wrote (not its inputs, as given)
  outputs <- setdiff(list.files(workdir), inputs)
  list(
    files = stats::setNames(file.path(workdir, outputs), outputs),
    extent = if (is.null(record$extent)) "" else record$extent,
    resources = paste(sort(names(record$resources)), collapse = ",")
  )
}

# --- Compare or update ------------------------------------------------------------
entities <- read_entities()
if (length(only) > 0) entities <- entities[entities$id %in% only, ]
if (nrow(entities) == 0) stop("no raw dataset entity selected", call. = FALSE)

problems <- character(0)
for (i in seq_len(nrow(entities))) {
  e <- entities[i, ]
  out <- tryCatch(run_entity(e), error = function(err) err)
  dir_expected <- file.path(expected_root, e$id)
  if (inherits(out, "error")) {
    problems <- c(problems, sprintf("%s: action failed: %s", e$id, conditionMessage(out)))
    next
  }
  summary <- c(paste("extent:", out$extent), paste("resources:", out$resources),
               paste("outputs:", paste(sort(names(out$files)), collapse = ",")))
  if (update) {
    unlink(dir_expected, recursive = TRUE)
    dir.create(dir_expected, recursive = TRUE)
    writeLines(summary, file.path(dir_expected, "summary.txt"))
    for (f in names(out$files)) file.copy(out$files[[f]], file.path(dir_expected, f))
    cat("updated ", e$id, "\n")
    next
  }
  if (!dir.exists(dir_expected)) {
    problems <- c(problems, sprintf("%s: no expected outputs (run --update)", e$id))
    next
  }
  if (!identical(summary, readLines(file.path(dir_expected, "summary.txt")))) {
    problems <- c(problems, sprintf("%s: extent, resources or output files differ", e$id))
  }
  for (f in names(out$files)) {
    exp_f <- file.path(dir_expected, f)
    if (file.exists(exp_f) && !identical(unname(tools::md5sum(out$files[[f]])), unname(tools::md5sum(exp_f)))) {
      problems <- c(problems, sprintf("%s: %s differs from the expected output", e$id, f))
    }
  }
  cat(if (any(startsWith(problems, paste0(e$id, ":")))) "FAIL " else "ok   ", e$id, "\n")
}

if (length(problems) > 0) {
  cat("\n", length(problems), " problem(s):\n", paste0("  - ", problems, collapse = "\n"), "\n", sep = "")
  quit(status = 1L)
}
cat("\nAll", nrow(entities), "raw datasets", if (update) "updated." else "match their expected outputs.", "\n")
