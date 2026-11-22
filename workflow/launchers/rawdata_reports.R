# =============================================================================
# workflow/launchers/rawdata_reports.R
#
# update_rawdata_reports(): rewrites the harmonisation reports of the raw
# datasets in the repository (R/tunaatlas_scripts/pre-harmonization/<tRFMO>/
# <type>/<dataset>/report_*.Rmd), from their core functions, and optionally
# refreshes the sample outputs and renders the HTML. Used by the qa_rmd step.
# =============================================================================

#' Rewrite the harmonisation reports of the raw datasets
#'
#' @param job_dirs Job folders of the rawdata workflows (optional): the first
#'   rows of their `*_mapped.csv` outputs are copied to the `data/` folder of
#'   each dataset, as samples.
#' @param render Also render each report to HTML, next to it.
#' @param n_rows Number of rows kept in the sample outputs.
#' @return The paths of the reports, invisibly.
update_rawdata_reports <- function(job_dirs = NULL, render = TRUE, n_rows = 5) {
  source(here::here("R/core/gta_rawdata_report.R"))
  root <- "R/tunaatlas_scripts/pre-harmonization"

  # Raw dataset entities: dataset folder (from the action path) and raw files
  rows <- do.call(rbind, lapply(Sys.glob(here::here("config/entities/entities_raw_*.csv")), function(f) {
    utils::read.csv(f, stringsAsFactors = FALSE, check.names = FALSE, encoding = "UTF-8")[, "Data", drop = FALSE]
  }))
  field <- function(data, key) {
    m <- regmatches(data, regexpr(sprintf("%s:[^\n]*", key), data))
    sub("_$", "", sub(sprintf("^%s:", key), "", m))
  }
  datasets <- unique(do.call(rbind, lapply(rows$Data, function(d) {
    script <- sub("^[^@]*@\\./", "", field(d, "action"))
    sources <- basename(strsplit(field(d, "source"), ",", fixed = TRUE)[[1]])
    raw_files <- sources[!grepl("code_lists", sources)]
    data.frame(dataset = gta_rawdata_report_dataset(script, raw_files),
               raw_files = paste(raw_files, collapse = ","),
               stringsAsFactors = FALSE)
  })))
  all_datasets <- datasets   # every entity, for the sample outputs
  # A dataset used by several entities: one report, from the first entity
  datasets <- datasets[!duplicated(datasets$dataset), ]

  # Sample outputs from the jobs: <raw file>_mapped.csv goes with its dataset
  if (!is.null(job_dirs)) {
    mapped <- unlist(lapply(job_dirs, list.files, pattern = "_mapped\\.csv$",
                            recursive = TRUE, full.names = TRUE))
    for (f in mapped) {
      base <- sub("_mapped\\.csv$", "", basename(f))
      hit <- all_datasets$dataset[vapply(strsplit(all_datasets$raw_files, ","), function(r) {
        base %in% tools::file_path_sans_ext(r)
      }, logical(1))]
      for (d in unique(hit)) {
        x <- readr::read_csv(f, show_col_types = FALSE)
        readr::write_csv(utils::head(x, n_rows), here::here(root, d, "data", basename(f)))
      }
    }
  }

  reports <- character(0)
  for (i in seq_len(nrow(datasets))) {
    report <- gta_rawdata_report(datasets$dataset[i], strsplit(datasets$raw_files[i], ",")[[1]])
    reports <- c(reports, report)
    if (render) {
      tryCatch(
        rmarkdown::render(report, output_dir = dirname(report), quiet = TRUE,
                          intermediates_dir = tempdir(), envir = new.env()),
        error = function(e) message("Rendering failed for ", report, ": ", conditionMessage(e))
      )
    }
  }
  invisible(reports)
}
