#' Convert fish capture data from numbers to tons and combine datasets
#'
#' This function processes georeferenced fish capture datasets by converting measurements
#' from numbers ("no") to tons ("t") where possible, using a global nominal catch dataset 
#' for augmenting unmatched strata. It handles missing or unmatched strata and ensures that
#' the resulting dataset is consistent and ready for further analysis. It also allows control
#' over whether to raise only unmatched strata or all strata.
#'
#' @param georef_dataset A georeferenced dataset containing fish capture data with measurements 
#'        in numbers and tons.
#' @param global_nominal_catch_firms_level0 A global nominal catch dataset for augmenting unmatched strata.
#' @param strata A character vector of columns used to define strata, with the default being 
#'        c("source_authority", "species", "gear_type", "fishing_fleet", "year").
#' @param raise_only_unmatched Logical, if TRUE, the function will raise only the strata that
#'        do not have equivalent strata in tons. If FALSE, it will raise all strata, including 
#'        those that have an equivalent in tons. Default is TRUE.
#' @param raise_only_on_unk Logical, if TRUE, the function will raise only the strata containing UNK, NEI or 99.9, this 
#'        is to make sure no data for instance of a certain gear_type is allocated to another gear_type if the gear_type dimension is not used
#'
#' @return A list containing two datasets:
#' \item{georef_dataset}{The combined dataset with measurements in tons and numbers.}
#' \item{not_converted_number}{The entries that could not be converted from numbers to tons.}
#'
#' @examples
#' result <- convert_fish_capture_data(georef_dataset, global_nominal_catch_firms_level0, raise_only_unmatched = TRUE)
#'
convert_number_to_nominal <- function(georef_dataset, global_nominal_catch_firms_level0, 
                                      strata = c("source_authority", "species", "gear_type", "fishing_fleet", "year"),
                                      raise_only_unmatched = TRUE, 
                                      raise_only_on_unk = TRUE) {
  
  dim_perfectly_compatible_data <- c("species", "fishing_mode", "gear_type", "fishing_fleet", "year", "geographic_identifier_nom", "source_authority")
  
  if(raise_only_on_unk){
    unk_dim <- setdiff(dim_perfectly_compatible_data, strata)
    conds <- list()
    if ("fishing_mode"  %in% (unk_dim)) conds <- append(conds, expr(fishing_mode  == "UNK"))
    if ("fishing_fleet" %in% (unk_dim)) conds <- append(conds, expr(fishing_fleet == "NEI"))
    if ("gear_type"     %in% (unk_dim)) conds <- append(conds, expr(gear_type     == "99.9"))
    
    if (length(conds)) {
      # réduire la liste en un unique OR
      cond_or <- reduce(conds, ~ expr( (!!.x) | (!!.y) ))
      
      global_nominal_catch_firms_level0 <- global_nominal_catch_firms_level0 %>%
        dplyr::filter(!!cond_or)
    }
    
  }
  columns <- colnames(georef_dataset)
  
  global_nominal_catch_firms_level0 <- global_nominal_catch_firms_level0 %>%
    dplyr::mutate(year = lubridate::year(time_start)) %>% 
    ungroup() %>% dplyr::group_by(across(strata)) %>% dplyr::summarise(measurement_value = sum(measurement_value))
  
  flog.info("Starting conversion of fish capture numbers to tons.")
  
  # Filter dataset for records with "tons" as the measurement unit
  georef_dataset_tons <- georef_dataset %>%
    dplyr::filter(measurement_unit == "t") %>%
    dplyr::mutate(year = lubridate::year(time_start)) %>%
    ungroup()
  
  flog.info("Filtered georeferenced dataset to include only entries measured in tons.")
  
  # Select distinct strata in the "tons" dataset
  strata_tons <- georef_dataset_tons %>%
    dplyr::select(all_of(strata)) %>%
    distinct()
  
  # Filter dataset for records with "numbers" as the measurement unit
  georef_dataset_number <- georef_dataset %>%
    dplyr::filter(measurement_unit == "no") %>%
    dplyr::mutate(year = lubridate::year(time_start)) %>%
    ungroup()
  
  flog.info("Filtered georeferenced dataset to include only entries measured in numbers.")
  
  # Select distinct strata in the "numbers" dataset
  strata_number <- georef_dataset_number %>%
    dplyr::select(all_of(strata)) %>%
    distinct()
  
  # Find matching strata between tons and numbers datasets
  strates_existantes_en_tonnes <- strata_number %>%
    dplyr::inner_join(strata_tons, by = strata)
  
  if (nrow(strates_existantes_en_tonnes) != 0) {
    
    flog.info("Found existing matching strata between numbers and tons.")
    
    # Join matching strata back with the original number dataset
    georef_dataset_tonnes_and_number <- georef_dataset_number %>%
      dplyr::inner_join(strates_existantes_en_tonnes, by = strata)

    # Remove matching strata from the numbers dataset if raising only unmatched strata
    if (raise_only_unmatched) {
      georef_dataset_number <- georef_dataset_number %>%
        dplyr::anti_join(strates_existantes_en_tonnes, by = strata)
      
      # Append converted data to tons dataset
      georef_dataset_tons <- rbind(georef_dataset_tons, georef_dataset_tonnes_and_number)
      
      flog.info("Removed matching strata from the numbers dataset to prevent double counting.")
    } else{
      
      georef_dataset_tons_groupped <- georef_dataset_tons %>% dplyr::group_by(across(all_of(strata))) %>% dplyr::summarise(measurement_value = sum(measurement_value))
      
      global_nominal_catch_firms_level0 <- global_nominal_catch_firms_level0 %>% inner_join(georef_dataset_tons_groupped, by =strata) %>% 
        dplyr::mutate(measurement_value_decreased_with_tons = measurement_value.x - measurement_value.y) %>% 
        dplyr::filter(measurement_value_decreased_with_tons > 0 ) %>% 
        dplyr::mutate(measurement_value = measurement_value_decreased_with_tons) %>% 
        dplyr::select(-c(measurement_value.x, measurement_value.y))
      
    }
  }
  
  # Now handle strata for raising (either unmatched or all depending on the parameter)
  if (raise_only_unmatched) {
    flog.info("Processing only unmatched strata using the nominal dataset.")
  } else {
    flog.info("Processing all strata using the nominal dataset.")
  }
  
  global_nominal_catch_firms_level0 <- global_nominal_catch_firms_level0 %>% dplyr::select(c(strata, "measurement_value")) %>% 
    dplyr::group_by(across(-measurement_value)) %>% dplyr::summarise(measurement_value = sum(measurement_value))
  
  georef_and_nominal_augmentation <- georef_dataset_number %>%
    dplyr::left_join(global_nominal_catch_firms_level0, by = strata) %>%
    dplyr::mutate(id___temp = dplyr::row_number()) %>%  # identifiant temporaire pour chaque ligne
    dplyr::group_by(across(all_of(strata))) %>%
    dplyr::mutate(sum = sum(measurement_value.x, na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      raising_factor = measurement_value.y / sum,
      percentageoftotalstrata = 100 * (measurement_value.x / sum),
      new_measurement = (percentageoftotalstrata * measurement_value.y) / 100
    ) %>%
    dplyr::select(-sum)
  
  
  saveRDS(georef_and_nominal_augmentation, "data/raisingfactorfromnumber_to_nominal.rds")
  flog.info("Saved raising factors from numbers to nominal.")
  
  # Create new strata for converted measurements
  nouvelles_strates <- georef_and_nominal_augmentation %>%
    dplyr::filter(!is.na(new_measurement)) %>%
    dplyr::rename(measurement_value = new_measurement) %>%
    dplyr::ungroup() %>%
    dplyr::select(all_of(columns)) %>%
    dplyr::mutate(measurement_unit = "t")%>% 
    dplyr::mutate(measurement_processing_level = "raised")
  
  # Handle hopeless cases (entries without conversion)
  not_converted_number <- georef_and_nominal_augmentation %>%
    dplyr::filter(is.na(new_measurement)) %>%
    dplyr::ungroup() %>%
    dplyr::rename(measurement_value = measurement_value.x) %>% 
    dplyr::select(all_of(columns)) %>%
    dplyr::mutate(measurement_unit = "no")
  # Combine all datasets
  georef_dataset <- rbind(
    georef_dataset_tons %>% dplyr::select(-year), 
    nouvelles_strates, 
    not_converted_number
  )
  
  georef_dataset <- georef_dataset %>% dplyr::group_by(across(-measurement_value)) %>% dplyr::summarise(measurement_value = sum(measurement_value)) %>% 
    dplyr::ungroup()
  
  flog.info("Completed the conversion of numbers to tons and combined datasets.")
  
  # Return the final dataset
  return(list(georef_dataset = georef_dataset, not_converted_number = not_converted_number, nouvelles_strates = nouvelles_strates))
}
