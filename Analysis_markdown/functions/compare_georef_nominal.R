#' Compare Georef and Nominal Data
#'
#' This function takes georef and nominal data as inputs, performs a comparison, and returns the results.
#'
#' @param georef Data frame containing georeferenced data.
#' @param nominal Data frame containing nominal data.
#' @param connectionDB Database connection object.
#' @return A list containing the comparison results.
#' @import dplyr
#' @importFrom lubridate year
#' @importFrom sf st_read
#' @importFrom janitor clean_names
#' @importFrom readr read_csv
#' @importFrom rmarkdown render
#' @export
compare_georef_nominal <- function(georef, nominal, connectionDB) {
  # Preprocess georef data
  georef_mapped <- georef %>%
    dplyr::mutate(year = lubridate::year(time_start)) %>%
    dplyr::select("fishing_fleet", "geographic_identifier", "species", "measurement_unit", "gear_type", "source_authority", "year", "measurement_value") %>%
    dplyr::filter(measurement_unit %in% c("t", "Tons")) %>%
    dplyr::mutate(year = as.character(year)) %>%
    dplyr::mutate(year = paste0(year, "-01-01")) %>%
    dplyr::mutate(gear_type = as.character(gear_type))
  
  # Preprocess nominal data
  nominal <- nominal %>%
    dplyr::mutate(year = lubridate::year(time_start)) %>%
    dplyr::select("fishing_fleet", "species", "measurement_unit", "gear_type", "source_authority", "year", "measurement_value") %>%
    dplyr::mutate(year = as.character(year)) %>%
    dplyr::mutate(year = paste0(year, "-01-01"))
  
  # Read additional data
  species_group <- st_read(connectionDB, query = "SELECT taxa_order, code from species.species_asfis") %>%
    janitor::clean_names() %>%
    dplyr::select(species_group = taxa_order, species = code)
  
  cl_cwp_gear_level2 <- st_read(connectionDB, query = "SELECT * FROM gear_type.isscfg_revision_1") %>%
    select(Code = code, Gear = label)
  
  shapefile.fix <- st_read(connectionDB, query = "SELECT * from area.cwp_grid") %>%
    dplyr::rename(GRIDTYPE = gridtype)
  
  shape_without_geom <- shapefile.fix %>%
    as_tibble() %>%
    dplyr::select(-geom)
  
  # Load external function
  source("https://raw.githubusercontent.com/firms-gta/geoflow-tunaatlas/master/Analysis_markdown/functions/tidying_GTA_data_for_comparison.R")
  
  # Filter georef data to keep only years present in nominal data
  nominal_year <- unique(nominal$year)
  georef_mapped <- georef_mapped %>%
    filter(year %in% nominal_year) %>%
    dplyr::group_by(across(setdiff(everything(), "measurement_value"))) %>%
    mutate(measurement_value = sum(measurement_value))
  georef_year <- unique(georef_year$year)
  nominal <- nominal %>%
    filter(year %in% georef_year) 
  
  # Define strata
  list_strata <- list(c("species", "year", "source_authority"), c("species", "year", "source_authority", "gear_type"),  c("species", "year", "source_authority", "gear_type", "fishing_fleet"))
  
  # Initialize result list
  results <- list()
  
  for (strata in list_strata) {
    name <- paste0(toString(strata))
    
    georef_no_nominal <- anti_join(georef_mapped, nominal, by = strata)
    georef_no_nominal <- georef_no_nominal %>%
      mutate(year = as.character(year)) %>%
      dplyr::group_by(across(setdiff(everything(), "measurement_value"))) %>%
      mutate(measurement_value = sum(measurement_value))
    
    georef_mapped_groupped <- georef_mapped %>%
      group_by_at(strata) %>%
      summarise(measurement_value = sum(measurement_value))
    
    nominal_groupped <- nominal %>%
      group_by_at(strata) %>%
      summarise(measurement_value = sum(measurement_value))
    
    georef_sup_nominal <- inner_join(nominal_groupped, georef_mapped_groupped, by = strata) %>%
      dplyr::rename(nominal_data = measurement_value.x, georefed_data = measurement_value.y) %>%
      mutate(nominal_data = round(nominal_data, 3)) %>%
      mutate(georefed_data = round(georefed_data, 3)) %>%
      filter(georefed_data > nominal_data) %>%
      mutate(Difference = georefed_data - nominal_data) %>%
      filter(Difference >= 1) %>%
      dplyr::mutate(measurement_unit = "Tons") %>%
      dplyr::mutate(measurement_value = Difference)
    
    georef_sup_to_nom_all <- tidying_GTA_data_for_comparison(
      dataframe = georef_sup_nominal,
      shape = shape_without_geom,
      species_group_dataframe = species_group,
      cl_cwp_gear_level2_dataframe = cl_cwp_gear_level2
    )
    
    georef_no_nominal_groupped_all <- tidying_GTA_data_for_comparison(
      dataframe = georef_no_nominal,
      shape = shape_without_geom,
      species_group_dataframe = species_group,
      cl_cwp_gear_level2_dataframe = cl_cwp_gear_level2
    )
    # Store results
    results[[name]] <- list(
      georef_sup_to_nom_all = georef_sup_to_nom_all,
      georef_no_nominal_groupped_all = georef_no_nominal_groupped_all
    )
  }
  
  return(results)
}
