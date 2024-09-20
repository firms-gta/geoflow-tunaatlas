#' Convert Fish Capture Numbers to Tons Using Nominal Data
#'
#' This function processes a georeferenced dataset of fish capture data, filtering and converting entries
#' based on measurement units ("tons" or "numbers"). It attempts to match data in "numbers" with
#' existing entries in "tons" and adjusts the dataset accordingly using a raising factor from nominal data.
#' 
#' @param georef_dataset The georeferenced dataset containing fish capture data.
#' @param global_nominal_catch_firms_level0 The dataset of nominal fish capture data for comparison.
#' @return A list with georeferenced dataset with "numbers" converted to "tons" and unmatched records logged.
#' @examples
#' result <- convert_number_to_nominal(georef_dataset, global_nominal_catch_firms_level0)
#' @export
convert_number_to_nominal <- function(georef_dataset, global_nominal_catch_firms_level0) {
  
  global_nominal_catch_firms_level0 <- global_nominal_catch_firms_level0 %>%
    dplyr::mutate(year = lubridate::year(time_start)) %>% 
    ungroup()
  # Log the start of the function
  flog.info("Starting conversion of fish capture numbers to tons.")
  
  # Filter dataset for records with "tons" as the measurement unit
  georef_dataset_tons <- georef_dataset %>%
    dplyr::filter(measurement_unit == "t") %>%
    dplyr::mutate(year = lubridate::year(time_start)) %>%
    ungroup()
  
  flog.info("Filtered georeferenced dataset to include only entries measured in tons.")
  
  # Select distinct strata in the "tons" dataset
  strata_tons <- georef_dataset_tons %>%
    dplyr::select(c("source_authority", "species", "gear_type", "fishing_fleet", "year")) %>%
    distinct()
  
  # Filter dataset for records with "numbers" as the measurement unit
  georef_dataset_number <- georef_dataset %>%
    dplyr::filter(measurement_unit == "no") %>%
    dplyr::mutate(year = lubridate::year(time_start)) %>%
    ungroup()
  
  flog.info("Filtered georeferenced dataset to include only entries measured in numbers.")
  
  # Select distinct strata in the "numbers" dataset
  strata_number <- georef_dataset_number %>%
    dplyr::select(c("source_authority", "species", "gear_type", "fishing_fleet", "year")) %>%
    distinct()
  
  # Find matching strata between tons and numbers datasets
  strates_existantes_en_tonnes <- strata_number %>%
    dplyr::inner_join(strata_tons, by = c("source_authority", "species", "gear_type", "fishing_fleet", "year"))
  
  if(nrow(strates_existantes_en_tonnes) != 0) {
    
    flog.info("Found existing matching strata between numbers and tons.")
    
    # Join matching strata back with the original number dataset
    georef_dataset_tonnes_and_number <- georef_dataset_number %>%
      dplyr::inner_join(strates_existantes_en_tonnes, by = c("source_authority", "species", "gear_type", "fishing_fleet", "year"))
    
    # Append converted data to tons dataset
    georef_dataset_tons <- rbind(georef_dataset_tons, georef_dataset_tonnes_and_number)
    
    # Remove matching strata from the numbers dataset
    georef_dataset_number <- georef_dataset_number %>%
      dplyr::anti_join(strates_existantes_en_tonnes, by = c("source_authority", "species", "gear_type", "fishing_fleet", "year"))
    
    flog.info("Removed matching strata from the numbers dataset to prevent double counting.")
  }
  
  # Now handle unmatched strata
  flog.info("Processing unmatched strata using the nominal dataset.")
  
  georef_and_nominal_augmentation <- georef_dataset_number %>%
    dplyr::left_join(global_nominal_catch_firms_level0, by = c("source_authority", "species", "gear_type", "fishing_fleet", "year")) %>%
    dplyr::group_by(source_authority, species, gear_type, fishing_fleet, year) %>%
    dplyr::mutate(sum = sum(measurement_value.x, na.rm = TRUE)) %>%
    dplyr::mutate(raising_factor = measurement_value.y / sum) %>%
    dplyr::mutate(percentageoftotalstrata = 100 * (measurement_value.x / sum)) %>%
    dplyr::mutate(new_measurement = (percentageoftotalstrata * measurement_value.y) / 100)
  
  saveRDS(georef_and_nominal_augmentation, "data/raisingfactorfromnumber_to_nominal.rds")
  flog.info("Saved raising factors from numbers to nominal.")
  
  # Create new strata for converted measurements
  nouvelles_strates <- georef_and_nominal_augmentation %>%
    dplyr::filter(!is.na(new_measurement)) %>%
    dplyr::rename(measurement_value = new_measurement) %>%
    dplyr::ungroup() %>%
    dplyr::select(time_start = time_start.x, time_end = time_end.x, gear_type, species, source_authority, fishing_fleet, 
                  geographic_identifier = geographic_identifier.x, measurement_value, fishing_mode = fishing_mode.x) %>%
    dplyr::mutate(measurement_unit = "t")
  
  # Handle hopeless cases (entries without conversion)
  not_converted_number <- georef_and_nominal_augmentation %>%
    dplyr::filter(is.na(new_measurement)) %>%
    dplyr::ungroup() %>%
    dplyr::select(time_start = time_start.x, time_end = time_end.x, gear_type, species, source_authority, fishing_fleet, 
                  geographic_identifier = geographic_identifier.x, measurement_value = measurement_value.x, fishing_mode = fishing_mode.x) %>%
    dplyr::mutate(measurement_unit = "no")
  
  # Combine all datasets
  georef_dataset <- rbind(
    georef_dataset_tons %>% dplyr::select(-year), 
    nouvelles_strates, 
    not_converted_number
  )
  
  flog.info("Completed the conversion of numbers to tons and combined datasets.")
  
  # Return the final dataset
  return(list(georef_dataset = georef_dataset, not_converted_number = not_converted_number))
}
