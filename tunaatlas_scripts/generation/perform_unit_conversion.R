#' Perform Unit Conversion
#'
#' This function performs a unit conversion on a geographic dataset using a specified conversion factor CSV file.
#'
#' @param conversion_factor_csv A string. Path to the CSV file containing conversion factors.
#' @param unit_conversion_codelist_geoidentifiers_conversion_factors A string. The name of the column in the conversion factors dataset that corresponds to the geographic identifiers.
#' @param georef_dataset A data frame. The geographic dataset that needs unit conversion.
#' @param entity An object of class `geoflow::geoflow_entity`. The entity for which the unit conversion is to be performed.
#' @param config An object of class `geoflow::geoflow_config`. Configuration settings for the conversion.
#' @param opts An object of class `geoflow::geoflow_options`. Additional options including factors and mappings for code lists.
#' @param step_description A string. Description of the step being performed.
#' @param addeddescription A string (optional). Additional description to be logged.
#'
#' @return A data frame. The geographic dataset with the converted units.
#'
#' @details
#' The function checks if the conversion factor CSV file exists. If it does, the conversion factors are loaded and used to perform the unit conversion on the dataset.
#' If the file does not exist, an error is thrown.
#'
#' @examples
#' \dontrun{
#' converted_dataset <- perform_unit_conversion(
#'   conversion_factor_csv = "path/to/conversion_factors.csv",
#'   unit_conversion_codelist_geoidentifiers_conversion_factors = "geo_id_column",
#'   georef_dataset = my_georef_dataset,
#'   entity = my_entity, # an object of class geoflow_entity
#'   config = my_config, # an object of class geoflow_config
#'   opts = my_opts, # an object of class geoflow_options
#'   step_description = "Converting fish numbers to tons",
#'   addeddescription = "This step includes specific region adjustments."
#' )
#' }
#'
#' @export
perform_unit_conversion <- function(conversion_factor_csv, unit_conversion_codelist_geoidentifiers_conversion_factors, georef_dataset, entity, config, opts, step_description, addeddescription = NULL) {
  if (file.exists(conversion_factor_csv)) {
    # Load the conversion factor dataset
    conversion_factors <- read_csv(conversion_factor_csv,
                                   col_types = cols(
                                     gear_type = col_character(),
                                     geographic_identifier = col_character(),
                                     time_start = col_character(),
                                     time_end = col_character()
                                   )) 
    # if(unit_conversion_codelist_geoidentifiers_conversion_factors %in% colnames(conversion_factors)){
    # conversion_factors <- conversion_factors %>% 
    #   dplyr::mutate(!!sym(unit_conversion_codelist_geoidentifiers_conversion_factors) := as.character(!!sym(unit_conversion_codelist_geoidentifiers_conversion_factors)))   
    # }
    # Perform unit conversion
    converted_data <- do_unit_conversion(
      entity = entity,
      config = config,
      fact = opts$fact,
      unit_conversion_csv_conversion_factor_url = conversion_factors,
      unit_conversion_codelist_geoidentifiers_conversion_factors = unit_conversion_codelist_geoidentifiers_conversion_factors,
      mapping_map_code_lists = opts$mapping_map_code_lists,
      georef_dataset = georef_dataset,
      removing_numberfish_final = FALSE
    )
    
    # Combine converted data with the rest of the dataset
    georef_dataset <- rbind(georef_dataset %>% filter(source_authority != source_authority), converted_data)
    # Log the step
    function_recap_each_step(
      step_description,
      georef_dataset,
      paste0(sprintf("The number of fish are converted to tons if there is a conversion factors in the dataset %s.",
                     conversion_factor_csv), addeddescription),
      "do_unit_conversion",
      list(conversion_factor_csv)
    )
  } else {
    warning(sprintf("Conversion factor file %s does not exist.", conversion_factor_csv))
  }
  
  return(georef_dataset)
}
