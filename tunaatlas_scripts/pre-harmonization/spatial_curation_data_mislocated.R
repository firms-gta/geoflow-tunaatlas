#' Spatially Curate Data Based on Mislocation
#'
#' This function handles spatial data by either removing or reallocating entries
#' based on their mislocation status determined by whether they fall on land
#' according to a specified CWP grid. It handles database connectivity issues
#' by attempting to fetch the necessary CWP grid from a local database or,
#' failing that, from a remote server via a zip file.
#'
#' @param config Configuration list containing database connection details.
#' @param df Data frame containing the geographic data to be curated.
#' @param spatial_curation_data_mislocated Character string, either "remove" or "reallocate",
#'        specifying how to handle mislocated data.
#'
#' @return A list containing:
#'         - `dataset`: Data frame of curated geographic data.
#'         - `areas_in_land`: Data frame of geographic data located on land.
#'         - `dataset_not_cwp_grid`: Data frame of geographic data not covered by the CWP grid.
#'
#' @importFrom dplyr filter inner_join anti_join select
#' @importFrom DBI dbGetQuery
#' @import readr
#' @import httr
#' @import here
#' @examples
#' config <- list(software = list(output = list(dbi = your_db_connection)))
#' df <- read.csv("path/to/your/data.csv")
#' result <- spatial_curation_data_mislocated(config, df, "remove")
#' print(result)

spatial_curation_data_mislocated<-function(config = NULL,df, action_on_mislocated= "remove"){
  con <- config$software$output$dbi
  
  tryCatch({
    # Votre requête DBI::dbGetQuery
    cwp_grid <- DBI::dbGetQuery(con, "SELECT ON_LAND_P,cwp_code from area.cwp_grid") %>%
      dplyr::rename(geographic_identifier = cwp_code)
  }, error = function(e) {
    message("no connexion to DB trying to unzip file")
    csv_file <- here::here("data/cl_areal_grid.csv")
    if(!file.exists(csv_file)){
      message("no connexion to DB nor zip, downloading cwp_grid")
      
      # Téléchargement du fichier ZIP depuis l'URL
      zip_url <- "https://github.com/fdiwg/fdi-codelists/raw/main/global/cwp/cl_areal_grid.zip"
      local_file <- here::here("data", "cwp_grid.zip")
      download.file(zip_url, local_file, mode = "wb")
      
      # Extraction du contenu du fichier ZIP
      unzip(local_file, exdir = here::here("data"))
      
      # Lecture du fichier CSV extrait
      
    } 
    cwp_grid <- read.csv(csv_file)
    
    # Renommage de colonnes
    cwp_grid <- cwp_grid %>% dplyr::select(ON_LAND_P, CWP_CODE) %>% 
      dplyr::rename(geographic_identifier = CWP_CODE, on_land_p = ON_LAND_P)
  })
  
  source("https://raw.githubusercontent.com/firms-gta/geoflow-tunaatlas/master/tunaatlas_scripts/generation/identification_data_on_land_cwp.R")
  cat("Reallocating data that are in land areas")
  
  cat("Executing spatial_curation_intersect_areas")
  georef_dataset <- df
  
  areas_in_land<-identification_data_on_land_cwp(con , georef_dataset)
  dataset_in_land <- georef_dataset %>% dplyr::filter(geographic_identifier %in%areas_in_land)
  
  df_input_cwp_grid <- georef_dataset %>% dplyr::inner_join(cwp_grid) %>% dplyr::select(-c(on_land_p))
  
  not_cwp_grid <- georef_dataset %>% dplyr::anti_join(cwp_grid)
  
  # if (!is.null(not_cwp_grid)) {
  #   sum_fact_to_reallocate <- not_cwp_grid %>% 
  #     dplyr::group_by(measurement_unit) %>% dplyr::summarise(value_reallocate = sum(measurement_value))
  #   sum_whole_df_input <- df_input %>% dplyr::group_by(measurement_unit) %>% 
  #     dplyr::summarise(measurement_value = sum(measurement_value))
  #   stats_reallocated_data <- dplyr::left_join(sum_whole_df_input, 
  #                                              sum_fact_to_reallocate)
  #   stats_reallocated_data$percentage_reallocated <- stats_reallocated_data$value_reallocate/stats_reallocated_data$measurement_value * 
  #     100
  # }
  # else {
  #   stats_reallocated_data = NULL
  # }
  
  georef_dataset <- df_input_cwp_grid
  
  if (action_on_mislocated=="remove"){ # We remove data that is mislocated
    cat("Removing data that are in land areas...\n")
    # remove rows with areas in land
    georef_dataset<-georef_dataset[ which(!(georef_dataset$geographic_identifier %in% c(areas_in_land))), ]
    
    # fill metadata elements
    lineage<-paste0("Some data might be mislocated: either located on land areas or without any area information. These data were not kept.	Information regarding the reallocation of mislocated data for this dataset: The data that were mislocated represented percentage_of_total_catches_reallocated_weight % of the whole catches expressed in weight in the dataset and percentage_of_total_catches_reallocated_number % of the catches expressed in number. percentage_catches_on_land_reallocated % of the catches that were removed.")
    description<-"- Data located at land or without any spatial information were removed.\n"
    
    cat("Removing data that are in land areas OK\n")
  }
  
  if (action_on_mislocated=="reallocate"){   # We reallocate data that is mislocated (they will be equally distributed on areas with same reallocation_dimensions (month|year|gear|flag|species|schooltype).
    cat("Reallocating data that are in land areas...\n")
    source("https://raw.githubusercontent.com/eblondel/geoflow-tunaatlas/master/tunaatlas_scripts/generation/spatial_curation_function_reallocate_data.R")
    
    catch_curate_data_mislocated<-spatial_curation_function_reallocate_data(df_input = georef_dataset,
                                                                            dimension_reallocation = "geographic_identifier",
                                                                            vector_to_reallocate = c(areas_in_land, not_cwp_grid$geographic_identifier),
                                                                            reallocation_dimensions = setdiff(colnames(georef_dataset),c("measurement_value","geographic_identifier")))
    georef_dataset<-catch_curate_data_mislocated$df
    
    # fill metadata elements
    
    lineage<-paste0("Some data might be mislocated: either located on land areas or without any area information. These data were equally redistributed on data at sea on areas with same characteristics (same year, month, gear, flag, species, type of school).	Information regarding the reallocation of mislocated data for this dataset: The data that were mislocated represented percentage_of_total_catches_reallocated_weight % of the whole catches expressed in weight in the dataset and percentage_of_total_catches_reallocated_number % of the catches expressed in number. percentage_catches_on_land_reallocated % of the catches that were mislocated were reallocated on areas at sea.")
    description<-"- Data located at land or without any spatial information were equally redistributed on data at sea in areas described by the same stratification factors, i.e. year, month, gear, flag, species, and type of school.\n"
    
    cat("Reallocating data that are in land areas OK\n")
  }
  
  return(list(dataset=georef_dataset, areas_in_land = dataset_in_land, dataset_not_cwp_grid = not_cwp_grid))
  
  
}

