#' Write the harmonisation report (R Markdown) of a raw dataset
#'
#' Builds, from a core function `gta_rawdata_*()`, the R Markdown document
#' given to the tRFMOs: it shows the documentation and the code of the core
#' function, runs it on the raw file, maps the codes to the global code lists
#' and saves the results. The code shown is the core function itself, so the
#' report always matches what the workflow runs.
#'
#' @param dataset Folder of the dataset in R/tunaatlas_scripts/pre-harmonization,
#'   `<tRFMO>/<catch|effort|nominal>/<dataset>`, e.g. `"wcpfc/catch/ps"`. The core
#'   function is `gta_rawdata_<tRFMO>_<type>_<dataset>()`, in
#'   `R/core/rawdata_<tRFMO>_<type>_<dataset>.R`. A dataset already in the CWP
#'   format has the folder `<tRFMO>/<type>/cwp_format` and the shared core
#'   function `gta_rawdata_cwp_format()`.
#' @param raw_files Name(s) of the raw file(s), in the `data/` folder of the dataset,
#'   in the order of the arguments of the core function.
#' @param output_file Path of the `.Rmd` to write; by default
#'   `report_<tRFMO>_<type>_<dataset>.Rmd` in the folder of the dataset.
#' @return The path of the report, invisibly.
gta_rawdata_report <- function(dataset, raw_files, output_file = NULL) {
  root <- "R/tunaatlas_scripts/pre-harmonization"
  parts <- strsplit(dataset, "/", fixed = TRUE)[[1]]
  slug <- paste(parts, collapse = "_")
  type <- parts[2]
  cwp <- parts[3] == "cwp_format"   # already in CWP format: shared core function
  core <- if (cwp) "cwp_format" else slug
  fun_name <- paste0("gta_rawdata_", core)
  core_file <- file.path("R/core", paste0("rawdata_", core, ".R"))
  if (is.null(output_file)) {
    output_file <- here::here(root, dataset, paste0("report_", slug, ".Rmd"))
  }
  lines <- readLines(here::here(core_file), warn = FALSE, encoding = "UTF-8")
  doc <- sub("^#' ?", "", lines[grepl("^#'", lines)])
  code <- lines[!grepl("^#'", lines)]
  title <- doc[1]
  description <- doc[-1]

  fact <- if (type == "effort") "effort" else "catch"
  base <- tools::file_path_sans_ext(raw_files[1])
  data_path <- function(file) {
    sprintf("here::here('%s', %s, 'data', '%s')",
            root, paste0("'", parts, "'", collapse = ", "), file)
  }
  chunk <- function(...) c("```{r}", ..., "```", "")

  rmd <- c(
    "---",
    sprintf("title: \"%s\"", gsub("\"", "'", title)),
    "output:",
    "  html_document:",
    "    code_folding: hide",
    "---",
    "",
    "```{r setup, include=FALSE}",
    "knitr::opts_chunk$set(echo = TRUE, message = FALSE, warning = FALSE)",
    "```",
    "",
    "# Introduction",
    "",
    if (cwp) c(
      sprintf("This document shows the small changes made to the %s dataset, already in the CWP format", toupper(parts[1])),
      "and with the CWP codes, before it enters the Global Tuna Atlas (the changes are summarised below)."
    ) else c(
      "This document transforms a dataset that is not in the CWP format into the CWP format:",
      "it first changes the structure of the data, then maps its codes to the CWP code lists."
    ),
    sprintf("It is generated from the function `%s()` (`%s`), the code that the Global Tuna Atlas workflow runs.", fun_name, core_file),
    "",
    "The raw dataset is read from the `data/` folder next to this document, which holds the first",
    "lines of each file under the name given by the tRFMO at export (this name may change).",
    if (cwp) "The result is saved in the same folder." else
      "A summary of the mapping is given at the end; the results are saved in the same folder.",
    "",
    "*Knit the document from the repository (here::here() locates its root), or run it chunk by chunk.*",
    "",
    "# The function",
    "",
    description,
    "",
    chunk(
      "# Packages the function uses unqualified, attached as in the workflow",
      "suppressPackageStartupMessages({ library(data.table); library(reshape2); library(dplyr); library(tidyr); library(readxl) })",
      "source(here::here('R/core/gta_data.R'))",
      code
    ),
    "# Harmonisation",
    "",
    chunk(
      if (length(raw_files) == 1) {
        c(sprintf("path_to_raw_dataset <- %s", data_path(raw_files)),
          sprintf("georef_dataset <- %s(path_to_raw_dataset)", fun_name))
      } else {
        c(sprintf("path_to_raw_dataset%d <- %s", seq_along(raw_files), vapply(raw_files, data_path, "")),
          sprintf("georef_dataset <- %s(%s)", fun_name,
                  paste0("path_to_raw_dataset", seq_along(raw_files), collapse = ", ")))
      },
      sprintf("write.csv(georef_dataset, %s, row.names = FALSE)", data_path(paste0(base, "_harmonized.csv")))
    ),
    if (cwp) list(
    "# What the function changes",
    "",
    "Rows, columns and total of `measurement_value`, by unit, before and after the function:",
    "",
    chunk(
      "raw <- readr::read_csv(path_to_raw_dataset, guess_max = 0, show_col_types = FALSE)",
      "raw_value <- suppressWarnings(as.numeric(raw$measurement_value))",
      "data.frame(",
      "  ` ` = c(\"rows\", \"rows with a measurement_value of 0 (removed)\", \"columns\"),",
      "  raw = c(nrow(raw), sum(raw_value == 0, na.rm = TRUE), ncol(raw)),",
      "  harmonized = c(nrow(georef_dataset), sum(georef_dataset$measurement_value == 0, na.rm = TRUE), ncol(georef_dataset)),",
      "  check.names = FALSE)",
      "cat(\"Columns removed:\", paste(setdiff(colnames(raw), colnames(georef_dataset)), collapse = \", \"), \"\\n\")",
      "cat(\"Columns added:\", paste(setdiff(colnames(georef_dataset), colnames(raw)), collapse = \", \"), \"\\n\")",
      "merge(",
      "  stats::aggregate(list(raw = raw_value), list(measurement_unit = raw$measurement_unit), sum, na.rm = TRUE),",
      "  stats::aggregate(list(harmonized = georef_dataset$measurement_value), list(measurement_unit = georef_dataset$measurement_unit), sum, na.rm = TRUE),",
      "  all = TRUE)",
      "if (\"measurement_processing_level\" %in% colnames(georef_dataset)) table(measurement_processing_level = georef_dataset$measurement_processing_level, useNA = \"ifany\")"
    )
    ) else list(
    "# Mapping to the CWP code lists",
    "",
    chunk(
      "source(here::here('R/tunaatlas_scripts/pre-harmonization/map_codelists_no_DB.R'))",
      sprintf("fact <- \"%s\"", fact),
      "mapping_codelist <- map_codelists_no_DB(fact, dataset_to_map = georef_dataset, mapping_keep_src_code = FALSE, summary_mapping = TRUE, source_authority_to_map = c(\"IATTC\", \"CCSBT\", \"WCPFC\"))"
    ),
    "Unmapped values, then the results:",
    "",
    chunk(
      "georef_dataset <- mapping_codelist$dataset_mapped %>% dplyr::mutate(fishing_fleet = ifelse(fishing_fleet == 'UNK', 'NEI', fishing_fleet), gear_type = ifelse(gear_type == 'UNK', '99.9', gear_type))",
      "# The summaries are NULL when nothing was mapped (e.g. tRFMOs not in source_authority_to_map)",
      sprintf("if (!is.null(mapping_codelist$recap_mapping)) data.table::fwrite(mapping_codelist$recap_mapping, %s)", data_path(paste0(base, "_recap_mapping.csv"))),
      sprintf("if (!is.null(mapping_codelist$not_mapped_total)) data.table::fwrite(mapping_codelist$not_mapped_total, %s)", data_path(paste0(base, "_not_mapped_total.csv"))),
      sprintf("data.table::fwrite(georef_dataset, %s)", data_path(paste0(base, "_CWP_dataset.csv"))),
      "head(mapping_codelist$recap_mapping)"
    )
    )
  )
  rmd <- unlist(rmd)
  while (length(rmd) > 0 && rmd[length(rmd)] == "") rmd <- rmd[-length(rmd)]
  writeLines(rmd, output_file, useBytes = TRUE)
  invisible(output_file)
}

#' Folder of the harmonisation report of a raw dataset
#'
#' A dataset with its own core function has its report in the folder of its
#' action, `<tRFMO>/<type>/<dataset>`. The datasets already in the CWP format
#' share one action (`cwp_format/`): each has its report in
#' `<tRFMO>/<catch|nominal>/cwp_format`, the tRFMO being the prefix of the raw
#' file name (e.g. `iccat_catch_all_1m_firms_level0_20260422.csv`).
#'
#' @param script_path Path of the geoflow action of the entity.
#' @param raw_files Name(s) of the raw file(s) of the entity.
#' @return The folder, relative to R/tunaatlas_scripts/pre-harmonization.
gta_rawdata_report_dataset <- function(script_path, raw_files) {
  root <- "R/tunaatlas_scripts/pre-harmonization/"
  dataset <- sub(paste0("^.*", root), "", dirname(sub("^\\./", "", script_path)))
  if (dataset == "cwp_format") {
    raw <- basename(raw_files[1])
    type <- if (grepl("nominal", raw)) "nominal" else "catch"
    dataset <- paste(sub("_.*$", "", raw), type, "cwp_format", sep = "/")
  }
  dataset
}
