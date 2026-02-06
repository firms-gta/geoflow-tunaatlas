#' Harmonize IOTC Effort Dataset (standardized formatting)
#'
#' This geoflow step reads an IOTC effort raw dataset and formats it into the
#' harmonized Tuna Atlas structure (one record per stratum with standard fields),
#' while keeping the usual geoflow resource mapping (source / harmonized / codelists).
#'
#' @param action The action context from geoflow, used for controlling workflow processes.
#' @param entity The entity context from geoflow, which manages dataset-specific details.
#' @param config The configuration context from geoflow, used for managing global settings.
#'
#' @return None; the function outputs files directly and registers them as geoflow resources.
#'
#' @details
#' The formatting logic is implemented inline:
#' \itemize{
#'   \item Builds `time_start` and `time_end` from YEAR/MONTH_START/MONTH_END.
#'   \item Maps `FISHING_GROUND_CODE` to `geographic_identifier` and infers `gridtype`.
#'   \item Standardizes key dimensions and effort value/unit fields.
#'   \item Enriches entity temporal extent and exports harmonized CSV.
#' }
#'
#' @importFrom dplyr mutate select case_when
#' @export
#' @keywords IOTC, tuna, fisheries, data harmonization, effort data
#' @author Bastien Grasset, IRD \email{bastien.grasset@ird.fr}
function(action, entity, config){
  
  if(!requireNamespace("dplyr", quietly = TRUE)){
    install.packages("dplyr")
  }
  library(dplyr)
  
  filename1 <- entity$data$source[[1]] # data
  filename2 <- entity$data$source[[2]] # structure / codelists
  path_to_raw_dataset <- entity$getJobDataResource(config, filename1)
  
  config$logger.info(sprintf("Pre-harmonization of dataset '%s'", entity$identifiers[["id"]]))
  
  opts <- options()
  options(encoding = "UTF-8")
  on.exit(options(opts), add = TRUE)
  
  # ---- read raw ----
  effort_raw <- read.csv(path_to_raw_dataset, stringsAsFactors = FALSE)
  config$logger.info(paste0("EF_RAW columns: ", paste(colnames(effort_raw), collapse = ", ")))
  config$logger.info(sprintf("Raw effort dimensions: %s rows", nrow(effort_raw)))
  
  # ---- format inline ----
  effort <- effort_raw %>%
    dplyr::mutate(
      # dates
      time_start = paste0(YEAR, "-", sprintf("%02d", MONTH_START), "-01"),
      time_end   = paste0(YEAR, "-", sprintf("%02d", MONTH_END), "-01"),
      
      # geography + gridtype
      geographic_identifier = as.character(FISHING_GROUND_CODE),
      gridtype = case_when(
        substr(as.character(FISHING_GROUND_CODE), 1, 1) == "5" ~ "1deg_x_1deg",
        substr(as.character(FISHING_GROUND_CODE), 1, 1) == "6" ~ "5deg_x_5deg",
        TRUE ~ "unknown"
      ),
      
      # core dimensions
      source_authority = "IOTC",
      gear_type        = as.character(GEAR_CODE),
      fishing_fleet    = as.character(FLEET_CODE),
      fishing_mode     = as.character(SCHOOL_TYPE_CODE),
      
      # measure
      measurement_value = EFFORT,
      measurement_unit  = as.character(EFFORT_UNIT_CODE),
      
      # optional label carried along
      fishing_fleet_label = as.character(FLEET)
    ) %>%
    dplyr::select(
      source_authority, gear_type, fishing_fleet, fishing_mode, fishing_fleet_label,
      time_start, time_end, geographic_identifier, gridtype,
      measurement_value, measurement_unit
    ) %>%
    # types normalization (inline)
    dplyr::mutate(
      geographic_identifier = as.character(geographic_identifier),
      time_start = as.character(time_start),
      time_end = as.character(time_end),
      gridtype = as.character(gridtype),
      source_authority = as.character(source_authority),
      gear_type = as.character(gear_type),
      fishing_fleet = as.character(fishing_fleet),
      fishing_mode = as.character(fishing_mode),
      measurement_unit = as.character(measurement_unit)
    ) %>%
    # minimal mode normalization (inline) — si tu as déjà norm_mode() ailleurs, remplace par ça
    dplyr::mutate(
      fishing_mode = trimws(fishing_mode),
      fishing_mode = ifelse(fishing_mode == "", NA_character_, fishing_mode)
    )
  
  # ---- temporal extent for geoflow entity ----
  effort$time_start <- as.Date(effort$time_start)
  effort$time_end   <- as.Date(effort$time_end)
  effort$measurement_processing_level <- "unknown"
effort$measurement <- "effort" 
  dataset_temporal_extent <- paste(
    paste0(format(min(effort$time_start, na.rm = TRUE), "%Y"), "-01-01"),
    paste0(format(max(effort$time_end, na.rm = TRUE), "%Y"), "-12-31"),
    sep = "/"
  )
  entity$setTemporalExtent(dataset_temporal_extent)
  
  base1 <- tools::file_path_sans_ext(basename(filename1))
  #@geoflow -> export as csv
  # sorties same folder as path_to_raw_dataset 
  output_name_dataset   <- file.path(dirname(path_to_raw_dataset), paste0(base1, "_harmonized.csv"))
  output_name_codelists <- file.path(dirname(path_to_raw_dataset), paste0(base1, "_codelists.csv"))
  
  write.csv(effort, output_name_dataset, row.names = FALSE)
  
  file.rename(  from = entity$getJobDataResource(config, filename2),  to   = output_name_codelists)
  
  entity$addResource("source", path_to_raw_dataset)
  entity$addResource("harmonized", output_name_dataset)
  entity$addResource("codelists", output_name_codelists)
  
}
