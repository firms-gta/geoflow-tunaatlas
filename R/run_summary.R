#' Aggregated summary of the final datasets of a run
#'
#' Summarises the final datasets of a workflow run (nominal catch, level 0,
#' level 1, level 2, effort) into one small table that can be kept for a long
#' time and compared with the summary of another run
#' (gta_compare_summaries()). The values are summed by year (the month is
#' dropped), by 5 degree square (the 1 degree squares are grouped into their 5
#' degree square) and by all the other dimensions of the dataset.
#'
#' @param datasets Paths of the final dataset files (CSV), or a named list of
#'   data frames. Files whose name contains "_harmonized" are skipped (same
#'   data as the public file).
#' @return A data.table with one row per stratum: `dataset` (name of the file
#'   without its years, e.g. "global_catch_ird_level0"), `time_start` (first
#'   day of the year, as a string), the dimensions of the dataset
#'   (source_authority, species, gear_type, fishing_fleet, fishing_mode,
#'   geographic_identifier, measurement_type, measurement_unit, when present),
#'   `measurement_value` (sum) and `n_rows` (number of rows summed).
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
    if (is.character(d)) d <- data.table::fread(d, colClasses = "character", showProgress = FALSE)
    s <- gta_summarise_dataset(data.table::as.data.table(d))
    s[, dataset := key]
    data.table::setcolorder(s, "dataset")
    s
  })
  data.table::rbindlist(out, use.names = TRUE, fill = TRUE)
}

#' Dimensions kept in a run summary, in this order
#' @noRd
gta_summary_dimensions <- c("source_authority", "species", "gear_type", "fishing_fleet",
                            "fishing_mode", "geographic_identifier", "measurement_type",
                            "measurement_unit")

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

#' 5 degree square of a CWP 1 degree square code (other codes unchanged)
#' @noRd
gta_cwp_to_5deg <- function(code) {
  code <- as.character(code)
  one <- !is.na(code) & nchar(code) == 7 & substr(code, 1, 1) == "5"
  if (any(one)) {
    lat <- as.integer(substr(code[one], 3, 4))
    lon <- as.integer(substr(code[one], 5, 7))
    code[one] <- paste0("6", substr(code[one], 2, 2),
                        sprintf("%02d", lat - lat %% 5), sprintf("%03d", lon - lon %% 5))
  }
  code
}

#' Summary of one dataset (see gta_run_summary())
#' @noRd
gta_summarise_dataset <- function(d) {
  d <- data.table::copy(d)
  year <- if ("year" %in% names(d)) d$year else substr(d$time_start, 1, 4)
  d[, time_start := paste0(year, "-01-01")]
  d[, measurement_value := as.numeric(measurement_value)]
  if ("geographic_identifier" %in% names(d)) {
    d[, geographic_identifier := gta_cwp_to_5deg(geographic_identifier)]
  }
  dims <- intersect(gta_summary_dimensions, names(d))
  d[, list(measurement_value = sum(measurement_value, na.rm = TRUE), n_rows = .N),
    by = c("time_start", dims)]
}

#' Compare the summaries of two runs
#'
#' Compares two run summaries (gta_run_summary()), dataset by dataset and
#' tRFMO by tRFMO, keeping apart:
#'
#' * the years present in both runs for a tRFMO, which are comparable: totals
#'   and their differences, and the detailed comparison of CWP.dataset
#'   (CWP.dataset::comprehensive_cwp_dataframe_analysis(): differences by
#'   dimension, strata present in one run only, temporal differences);
#' * the years present in one run only (typically the new year of a release):
#'   listed with their totals, without difference.
#'
#' @param previous,new Run summaries (data frames, or paths of CSV files
#'   written with data.table::fwrite()).
#' @param labels Names of the two runs in the tables.
#' @param analyse TRUE to add the detailed comparison of CWP.dataset (the
#'   longest part).
#' @return A list: `datasets` (which run has which dataset) and `comparison`,
#'   one element per dataset present in both runs, with `years` (years of each
#'   tRFMO), `uncommon` (totals of the years present in one run only),
#'   `by_source` and `annual` (totals of the common years and their
#'   differences) and `analysis` (result of CWP.dataset, or the error message
#'   when it failed).
#' @export
gta_compare_summaries <- function(previous, new, labels = c("previous", "new"), analyse = TRUE) {
  read <- function(x) {
    # Codes stay strings (gear "09.39" is not the number 9.39)
    if (is.character(x)) x <- data.table::fread(x, colClasses = "character", showProgress = FALSE)
    x <- data.table::as.data.table(x)
    x[, measurement_value := as.numeric(measurement_value)]
    x[, n_rows := as.integer(n_rows)]
    x[, year := as.integer(substr(time_start, 1, 4))]
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
  drop_empty <- function(x) x[, names(x)[vapply(x, function(v) !all(is.na(v)), logical(1))], with = FALSE]
  a <- drop_empty(a[, !"dataset"])
  b <- drop_empty(b[, !"dataset"])

  # Years of each tRFMO, and the (tRFMO, year) pairs present in both runs
  ya <- unique(a[, list(source_authority, year)])
  yb <- unique(b[, list(source_authority, year)])
  both <- merge(ya, yb, by = c("source_authority", "year"))
  sources <- sort(union(ya$source_authority, yb$source_authority))
  years <- data.table::rbindlist(lapply(sources, function(s) {
    y1 <- ya[source_authority == s, year]
    y2 <- yb[source_authority == s, year]
    data.table::data.table(
      source_authority = s,
      v1 = gta_year_ranges(y1), v2 = gta_year_ranges(y2),
      common = gta_year_ranges(intersect(y1, y2)),
      only_previous = gta_year_ranges(setdiff(y1, y2)),
      only_new = gta_year_ranges(setdiff(y2, y1)))
  }))
  data.table::setnames(years, c("v1", "v2", "only_previous", "only_new"),
                       c(paste("years", labels), paste("only in", labels)))

  # Totals of the years present in one run only
  total <- function(x, by) x[, list(value = sum(measurement_value), n_rows = sum(n_rows)), by = by]
  uncommon <- data.table::rbindlist(list(
    total(a[!both, on = c("source_authority", "year")], c("source_authority", "year", "measurement_unit"))[, run := labels[1]],
    total(b[!both, on = c("source_authority", "year")], c("source_authority", "year", "measurement_unit"))[, run := labels[2]]
  ))
  if (nrow(uncommon)) data.table::setorderv(uncommon, c("run", "source_authority", "year"))

  # Common years: totals and differences
  ac <- a[both, on = c("source_authority", "year")]
  bc <- b[both, on = c("source_authority", "year")]
  diff_table <- function(by) {
    m <- merge(total(ac, by), total(bc, by), by = by, all = TRUE, suffixes = c("_1", "_2"))
    for (v in c("value_1", "value_2", "n_rows_1", "n_rows_2")) data.table::set(m, which(is.na(m[[v]])), v, 0)
    m[, difference := round(value_2 - value_1, 6)]   # no floating-point noise
    m[, difference_pct := ifelse(value_1 == 0, ifelse(value_2 == 0, 0, Inf), 100 * difference / value_1)]
    data.table::setnames(m, c("value_1", "value_2", "n_rows_1", "n_rows_2"),
                         c(labels, paste("rows", labels)))
    m[]
  }
  by_source <- diff_table(c("source_authority", "measurement_unit"))
  annual <- diff_table(c("source_authority", "year", "measurement_unit"))

  analysis <- NULL
  if (analyse && nrow(ac) && nrow(bc)) {
    analysis <- tryCatch(
      CWP.dataset::comprehensive_cwp_dataframe_analysis(
        as.data.frame(ac[, !c("year", "n_rows")]), as.data.frame(bc[, !c("year", "n_rows")]),
        fig.path = tempfile("gta_comparison_"),
        parameter_fact = if (grepl("effort", key)) "effort" else "catch",
        parameter_time_dimension = "time_start",
        parameter_geographical_dimension = "geographic_identifier",
        parameter_geographical_dimension_groupping = "gridtype",
        print_map = FALSE, coverage = TRUE, plotting_type = "view",
        parameter_titre_dataset_1 = labels[1], parameter_titre_dataset_2 = labels[2]),
      error = function(e) conditionMessage(e))
  }
  list(years = years[], uncommon = uncommon, by_source = by_source, annual = annual, analysis = analysis)
}

#' Years as compact ranges: c(1950:1953, 1960) -> "1950-1953, 1960"
#' @noRd
gta_year_ranges <- function(y) {
  y <- sort(unique(y))
  if (!length(y)) return("")
  groups <- cumsum(c(1, diff(y) != 1))
  paste(vapply(split(y, groups), function(g) {
    if (length(g) == 1) as.character(g) else paste0(g[1], "-", g[length(g)])
  }, character(1)), collapse = ", ")
}
