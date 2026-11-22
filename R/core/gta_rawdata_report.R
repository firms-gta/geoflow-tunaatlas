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
#'   `R/core/rawdata_<tRFMO>_<type>_<dataset>.R`.
#' @param raw_file Name of the raw file, in the `data/` folder of the dataset.
#' @param output_file Path of the `.Rmd` to write; by default
#'   `report_<tRFMO>_<type>_<dataset>.Rmd` in the folder of the dataset.
#' @return The path of the report, invisibly.
gta_rawdata_report <- function(dataset, raw_file, output_file = NULL) {
  root <- "R/tunaatlas_scripts/pre-harmonization"
  parts <- strsplit(dataset, "/", fixed = TRUE)[[1]]
  slug <- paste(parts, collapse = "_")
  type <- parts[2]
  fun_name <- paste0("gta_rawdata_", slug)
  core_file <- file.path("R/core", paste0("rawdata_", slug, ".R"))
  if (is.null(output_file)) {
    output_file <- here::here(root, dataset, paste0("report_", slug, ".Rmd"))
  }
  lines <- readLines(here::here(core_file), warn = FALSE, encoding = "UTF-8")
  doc <- sub("^#' ?", "", lines[grepl("^#'", lines)])
  code <- lines[!grepl("^#'", lines)]
  title <- doc[1]
  description <- doc[-1]

  fact <- if (type == "effort") "effort" else "catch"
  base <- tools::file_path_sans_ext(raw_file)
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
    "This document transforms a dataset that is not in the CWP format into the CWP format:",
    "it first changes the structure of the data, then maps its codes to the CWP code lists.",
    sprintf("It is generated from the function `%s()` (`%s`), the code that the Global Tuna Atlas workflow runs.", fun_name, core_file),
    "",
    "The raw dataset is read from the `data/` folder next to this document, which holds the first",
    "lines of each file under the name given by the tRFMO at export (this name may change).",
    "A summary of the mapping is given at the end; the results are saved in the same folder.",
    "",
    "*Knit the document from the repository (here::here() locates its root), or run it chunk by chunk.*",
    "",
    "# The function",
    "",
    description,
    "",
    chunk(
      "source(here::here('R/core/gta_data.R'))",
      code
    ),
    "# Harmonisation",
    "",
    chunk(
      sprintf("path_to_raw_dataset <- %s", data_path(raw_file)),
      sprintf("georef_dataset <- %s(path_to_raw_dataset)", fun_name),
      sprintf("write.csv(georef_dataset, %s, row.names = FALSE)", data_path(paste0(base, "_harmonized.csv")))
    ),
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
      sprintf("data.table::fwrite(mapping_codelist$recap_mapping, %s)", data_path(paste0(base, "_recap_mapping.csv"))),
      sprintf("data.table::fwrite(mapping_codelist$not_mapped_total, %s)", data_path(paste0(base, "_not_mapped_total.csv"))),
      sprintf("data.table::fwrite(georef_dataset, %s)", data_path(paste0(base, "_CWP_dataset.csv"))),
      "head(mapping_codelist$recap_mapping)"
    )
  )
  while (length(rmd) > 0 && rmd[length(rmd)] == "") rmd <- rmd[-length(rmd)]
  writeLines(rmd, output_file, useBytes = TRUE)
  invisible(output_file)
}
