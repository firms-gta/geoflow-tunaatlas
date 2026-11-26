#' Columns of the public version of a Global Tuna Atlas dataset
#'
#' @return Character vector; the public dataset keeps those of its columns
#'   that are in this list, in the order of the dataset.
#' @export
gta_public_columns <- function() {
  c("source_authority", "species", "gear_type", "fishing_fleet", "fishing_mode",
    "time_start", "time_end", "year", "month", "quarter", "geographic_identifier",
    "measurement_unit", "measurement_value", "measurement", "measurement_type",
    "measurement_processing_level")
}

#' Harmonised and public versions of a Global Tuna Atlas dataset
#'
#' Final formatting of a dataset (nominal, level 0, 1 or 2, effort), as
#' written by the workflow: the harmonised version (unit "TRUE" read as "t",
#' dates as Date, geographic identifier as character) and the public version
#' (plus year, month and quarter of time_end, restricted to the public
#' columns). The workflow writes them as <id>_harmonized.csv and
#' <id>_public.csv; the public version is the one the next levels read.
#'
#' @param dataset The dataset (data frame).
#' @param columns_to_keep Columns of the public version (see gta_public_columns()).
#' @return A list: `harmonized`, `public` (data frames) and `temporal_extent`
#'   ("start/end").
#' @export
gta_dataset_outputs <- function(dataset, columns_to_keep = gta_public_columns()) {
  # Correct measurement unit if necessary
  idx <- which(dataset$measurement_unit == "TRUE")
  if (length(idx) > 0) {
    dataset$measurement_unit[idx] <- "t"
  }
  dataset$time_start <- as.Date(dataset$time_start)
  dataset$time_end <- as.Date(dataset$time_end)
  temporal_extent <- paste(as.character(min(dataset$time_start)), as.character(max(dataset$time_end)), sep = "/")
  dataset$geographic_identifier <- as.character(dataset$geographic_identifier)
  harmonized <- dataset

  dataset$year <- as.integer(format(dataset$time_end, "%Y"))
  dataset$month <- as.integer(format(dataset$time_end, "%m"))
  dataset$quarter <- as.integer(substr(quarters(dataset$time_end), 2, 2))
  columns_to_keep <- intersect(colnames(dataset), columns_to_keep)
  public <- dataset[, columns_to_keep]

  list(harmonized = harmonized, public = public, temporal_extent = temporal_extent)
}
