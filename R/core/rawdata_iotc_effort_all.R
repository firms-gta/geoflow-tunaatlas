#' Harmonize IOTC Effort Dataset (standardized formatting)
#'
#' This geoflow step reads an IOTC effort raw dataset and formats it into the
#' harmonized Tuna Atlas structure (one record per stratum with standard fields),
#' while keeping the usual geoflow resource mapping (source / harmonized / codelists).
#'
#' @details
#' The formatting logic is implemented inline:
#' \itemize{
#'   \item Builds `time_start` and `time_end` from YEAR/MONTH_START/MONTH_END.
#'   \item Maps `FISHING_GROUND_CODE` to `geographic_identifier` and infers `gridtype`.
#'   \item Standardizes key dimensions and effort value/unit fields.
#' }
#'
#' @author Bastien Grasset, IRD \email{bastien.grasset@ird.fr}
#'
#' @param data The raw dataset: a data frame, or the path to the file.
#' @return The harmonized dataset (data frame), as written by the geoflow action
#'   (iotc/effort/all/action_iotc_effort_all.R).
gta_rawdata_iotc_effort_all <- function(data) {
  
  
  
  
  
  # ---- read raw ----
  effort_raw <- gta_data(data, reader = function(path_to_raw_dataset) gta_utf8(read.csv(path_to_raw_dataset, stringsAsFactors = FALSE)))
  
  effort_raw <- effort_raw %>% dplyr::mutate(GEAR_CODE = ifelse(is.na(GEAR_CODE), "UNCL", GEAR_CODE)) #some NAs replaced by UNCL which is coding for Other/Unk
  
  log_info(paste0("EF_RAW columns: ", paste(colnames(effort_raw), collapse = ", ")))
  log_info(sprintf("Raw effort dimensions: %s rows", nrow(effort_raw)))
  
  # ---- format inline ----
  effort <- effort_raw %>%
    dplyr::mutate(
      time_start = as.Date(sprintf("%d-%02d-01", YEAR, MONTH_START)),
      
      time_end = as.Date(
        sprintf("%d-%02d-01", YEAR, MONTH_END)
      ) |>
        (\(x) as.Date(format(x + 32, "%Y-%m-01")) - 1)()
    ,
      
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
    dplyr::mutate(
      fishing_mode = trimws(fishing_mode),
      fishing_mode = ifelse(fishing_mode == "", NA_character_, fishing_mode)
    )
  
  effort$time_start <- as.Date(effort$time_start)
  effort$time_end   <- as.Date(effort$time_end)
  effort$measurement_processing_level <- "unknown"
effort$measurement <- "effort" 
  effort
}
