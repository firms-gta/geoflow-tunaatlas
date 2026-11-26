#' Aggregated summary of the final datasets of a run
#'
#' Summarises the final datasets of a workflow run (nominal catch, level 0,
#' level 1, level 2, effort) into one small table that can be kept for a long
#' time and compared with the summary of another run
#' (gta_compare_summaries()). Each dataset is aggregated with
#' CWP.dataset::cwp_aggregate(): values summed by year, by 5 degree square
#' (the 1 degree squares are grouped into their 5 degree square) and by all
#' the other dimensions of the dataset.
#'
#' @param datasets Paths of the final dataset files (CSV), or a named list of
#'   data frames. Files whose name contains "_harmonized" are skipped (same
#'   data as the public file).
#' @return A data.table with one row per stratum: `dataset` (name of the file
#'   without its years, e.g. "global_catch_ird_level0"), then the columns of
#'   CWP.dataset::cwp_aggregate(): `time_start` (first day of the year), the
#'   dimensions, `measurement_value` (sum) and `n_rows` (number of rows
#'   summed).
#' @examples
#' \dontrun{
#' s <- gta_run_summary(list.files("data/GTA_2026/dataoutputGTA", "\\.csv$", full.names = TRUE))
#' data.table::fwrite(s, "summary.csv.gz")
#' }
#' @export
gta_run_summary <- function(datasets) {
  if (is.character(datasets)) {
    datasets <- datasets[!grepl("_harmonized", basename(datasets))]
    names(datasets) <- gta_dataset_key(basename(datasets))
    datasets <- as.list(datasets)
  }
  if (is.null(names(datasets)) || any(!nzchar(names(datasets)))) {
    stop("datasets must be file paths or a named list of data frames")
  }
  out <- lapply(names(datasets), function(key) {
    d <- datasets[[key]]
    if (is.character(d)) d <- gta_read_csv(d)
    s <- CWP.dataset::cwp_aggregate(d)
    s[, dataset := key]
    data.table::setcolorder(s, "dataset")
    s
  })
  data.table::rbindlist(out, use.names = TRUE, fill = TRUE)
}

#' Read a CSV file, compressed (.gz) or not, all columns as strings
#'
#' Codes stay strings (gear "09.39" is not the number 9.39). A .gz file is
#' decompressed by gzip: data.table::fread() would need the R.utils package
#' for it, which the workflow image does not have.
#' @noRd
gta_read_csv <- function(file) {
  if (grepl("\\.gz$", file)) {
    data.table::fread(cmd = paste("gzip -dc", shQuote(file)), colClasses = "character", showProgress = FALSE)
  } else {
    data.table::fread(file, colClasses = "character", showProgress = FALSE)
  }
}

#' Name of a dataset without its years and its version suffix
#'
#' "global_catch_ird_level0_1950_2024.csv" -> "global_catch_ird_level0", so
#' that the same dataset of two releases has the same name.
#' @noRd
gta_dataset_key <- function(file) {
  key <- sub("\\.[A-Za-z0-9]+$", "", file)
  key <- sub("_(harmonized|public)$", "", key)
  sub("(_[0-9]{4})+$", "", key)
}

#' Compare the summaries of two runs
#'
#' Compares two run summaries (gta_run_summary()), dataset by dataset, with
#' the functions of CWP.dataset:
#'
#' * CWP.dataset::cwp_split_common_periods() keeps apart, for each tRFMO, the
#'   years present in both runs, which are comparable, and the years present
#'   in one run only (typically the new year of a release), listed with their
#'   totals;
#' * on the common years, CWP.dataset::fonction_groupement() gives the totals
#'   by tRFMO and by tRFMO and year with their differences, and
#'   CWP.dataset::comprehensive_cwp_dataframe_analysis() the detailed
#'   comparison (differences by dimension, strata present in one run only,
#'   temporal differences).
#'
#' @param previous,new Run summaries (data frames, or paths of CSV files
#'   written with data.table::fwrite()).
#' @param labels Names of the two runs in the tables.
#' @param analyse TRUE to add the detailed comparison of CWP.dataset (the
#'   longest part).
#' @return A list: `datasets` (which run has which dataset), `labels`, and
#'   `comparison`, one element per dataset present in both runs, with `years`
#'   (years of each tRFMO), `uncommon` (totals of the years present in one run
#'   only), `by_source` and `annual` (totals of the common years and their
#'   differences: `difference`, `difference_pct`) and `analysis` (result of
#'   CWP.dataset::comprehensive_cwp_dataframe_analysis(), or the error message
#'   when it failed).
#' @export
gta_compare_summaries <- function(previous, new, labels = c("previous", "new"), analyse = TRUE) {
  read <- function(x) {
    if (is.character(x)) x <- gta_read_csv(x)
    x <- data.table::as.data.table(x)
    x[, measurement_value := as.numeric(measurement_value)]
    x[, n_rows := as.integer(n_rows)]
    x
  }
  a <- read(previous)
  b <- read(new)
  keys <- sort(union(unique(a$dataset), unique(b$dataset)))
  datasets <- data.frame(dataset = keys, previous = keys %in% a$dataset, new = keys %in% b$dataset)
  names(datasets)[2:3] <- labels
  common <- keys[keys %in% a$dataset & keys %in% b$dataset]
  comparison <- lapply(stats::setNames(common, common), function(key) {
    gta_compare_dataset(a[dataset == key], b[dataset == key], labels, analyse, key)
  })
  list(datasets = datasets, comparison = comparison, labels = labels)
}

#' Comparison of one dataset (see gta_compare_summaries())
#' @noRd
gta_compare_dataset <- function(a, b, labels, analyse, key) {
  drop_empty <- function(x) x[, names(x)[vapply(x, function(v) !all(is.na(v) | v == ""), logical(1))], with = FALSE]
  a <- drop_empty(a[, !"dataset"])
  b <- drop_empty(b[, !"dataset"])

  split <- CWP.dataset::cwp_split_common_periods(a, b, titre_1 = labels[1], titre_2 = labels[2])
  common_a <- split$init
  common_b <- split$final
  common_a[, year := substr(time_start, 1, 4)]
  common_b[, year := substr(time_start, 1, 4)]

  # Totals of the common years and their differences (CWP.dataset), with
  # readable column names
  totals <- function(by) {
    g <- CWP.dataset::fonction_groupement(by, common_a, common_b)
    data.table::setnames(g, "Precision", by[1])
    g[, difference := round(value_sum_2 - value_sum_1, 6)]   # no floating-point noise
    g[, difference_pct := ifelse(value_sum_1 == 0, ifelse(value_sum_2 == 0, 0, Inf), 100 * difference / value_sum_1)]
    g <- g[, c(by, "measurement_unit", "value_sum_1", "value_sum_2", "number_lines1", "number_lines2",
               "difference", "difference_pct"), with = FALSE]
    data.table::setnames(g, c("value_sum_1", "value_sum_2", "number_lines1", "number_lines2"),
                         c(labels, paste("strata", labels)))
    data.table::setorderv(g, c(by, "measurement_unit"))
    g[]
  }
  by_source <- totals("source_authority")
  annual <- totals(c("source_authority", "year"))

  analysis <- NULL
  if (analyse && nrow(common_a) && nrow(common_b)) {
    analysis <- tryCatch(
      CWP.dataset::comprehensive_cwp_dataframe_analysis(
        as.data.frame(common_a[, !c("year", "n_rows")]), as.data.frame(common_b[, !c("year", "n_rows")]),
        fig.path = tempfile("gta_comparison_"),
        parameter_fact = if (grepl("effort", key)) "effort" else "catch",
        parameter_time_dimension = "time_start",
        parameter_geographical_dimension = "geographic_identifier",
        parameter_geographical_dimension_groupping = "gridtype",
        print_map = FALSE, coverage = TRUE, plotting_type = "view",
        parameter_titre_dataset_1 = labels[1], parameter_titre_dataset_2 = labels[2]),
      error = function(e) conditionMessage(e))
  }
  list(years = split$periods, uncommon = split$uncommon, by_source = by_source, annual = annual,
       analysis = analysis)
}
