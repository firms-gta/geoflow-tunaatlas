#' Tidy and Map Data for Harmonization and Mapping Processes
#'
#' This function prepares and maps data for harmonization and mapping processes based on predefined configurations. It involves a series of steps including setting options and configurations, sourcing scripts for pre-harmonization, handling negative or null values, standardizing units of measure, dealing with irregular areas, and mapping to CWP standard codelists. The function is designed to work within a larger framework that utilizes external configuration, action, and entity objects to manage the harmonization and mapping workflow.
#'
#' @param action A geoflow object containing options and configurations for actions.
#' @param entity An geoflow object containing the dataset to be processed, including resources like harmonized data paths.
#' @param config A geoflow object that includes software configurations and a logger for information logging.
#' @return Updates the entity object with the processed dataset and logs various steps of the process.
#' @importFrom readr read_csv
#' @importFrom dplyr mutate filter inner_join group_by summarise
#' @importFrom stringr str_detect
#' @export
#' @examples
#' Tidying_and_mapping_data(action, entity, config)

Tidying_and_mapping_data = function(action, entity, config) {
  # Set options and configurations
  harmonized <- entity$resources$harmonized
  output_name_dataset_mapped <- gsub("harmonized", "mapped", harmonized)
  
  opts <- action$options
  con <- config$software$output$dbi
  options(encoding = "UTF-8")
  opts$fact <- ifelse(grepl("effort", harmonized), "effort", "catch")
  recap_each_step <- TRUE
  
  # Define step logger function
  stepLogger <- function(level, step, msg) {
    config$logger.info(sprintf("LEVEL %s => STEP %s: %s", level, step, msg))
  }
  
  # Define the base URL for scripts
  base_url <- "https://raw.githubusercontent.com/firms-gta/geoflow-tunaatlas/Developpement/"
  
  # Source scripts from URLs
  source(file.path(base_url, "tunaatlas_scripts/pre-harmonization/spatial_curation_data_mislocated.R"))
  source(file.path(base_url, "tunaatlas_scripts/pre-harmonization/curation_absurd_converted_data.R"))
  source(file.path(base_url, "tunaatlas_scripts/pre-harmonization/outside_juridiction.R"))
  source(file.path(base_url, "tunaatlas_scripts/pre-harmonization/spatial_curation.R"))
  source(file.path(base_url, "tunaatlas_scripts/pre-harmonization/map_codelists_no_DB.R"))
  source(file.path(base_url, "tunaatlas_scripts/pre-harmonization/map_codelists.R"))
  
  # Additional scripts for reporting and functions
  reporting_functions <- c(
    "functions/write_options_to_csv.R",
    "functions/function_recap_each_step.R",
    "functions/copyrmd.R"
  )
  
  lapply(reporting_functions, function(func) {
    source(file.path(base_url, "Analysis_markdown/", func))})
  
  # Save options in a CSV file
  write_options_to_csv(opts)
  
  
  stepnumber <- 1
  
  df_to_load <- as.data.frame(readr::read_csv(harmonized, guess_max=0)) %>%
    mutate(measurement_value = as.numeric(measurement_value))
  
  `%notin%` <- Negate(`%in%`)
  
  if("species" %notin% colnames(df_to_load)){
    fact = "effort"
    opts$fact <- "effort"
  }
  
  if(recap_each_step){
    function_recap_each_step(
      "rawdata",
      df_to_load)
  }
  
  georef_dataset <- df_to_load
  georef_dataset$measurement_unit <- as.character(georef_dataset$measurement_unit)
  if(!is.na(any(georef_dataset$measurement_unit) == "TRUE")) if(any(georef_dataset$measurement_unit) == "TRUE") georef_dataset[(georef_dataset$measurement_unit) == "TRUE",]$measurement_unit <- "t"
  
  #--------Negative or null values ------------------------------------------------------------------------------------------------------------------------------------
  # Negative or null values 
  #-----------------------------------------------------------------------------------------------------------------------------------------------------------
  
  negative_values <- georef_dataset %>% dplyr::filter(measurement_value <= 0)
  georef_dataset <- georef_dataset %>% dplyr::filter(measurement_value > 0)
  if(nrow(negative_values)!=0){
    if(recap_each_step){
      function_recap_each_step("negative_values",georef_dataset,paste0("In this step,handle negative values in the measurement_values of the data"))
      
      saveRDS(negative_values,"data/negative_values.rds")
      
    }
  }
  

  
  if (!grepl("nominal", harmonized)){
  
  # Curation absurd converted data ------------------------------------------
  stepLogger(level = 0, step = stepnumber, msg = "Curation absurd converted data")
  stepnumber = stepnumber+1
  
  curation_absurd_converted_data_list <-
    curation_absurd_converted_data(georef_dataset = georef_dataset,
                                   max_conversion_factor = "https://raw.githubusercontent.com/firms-gta/geoflow-tunaatlas/Developpement/data/max_conversion_factor.csv")
  
  georef_dataset <- curation_absurd_converted_data_list$georef_dataset
  
  if(exists("not_conform_conversion_factors")){
    rm(not_conform_conversion_factors)
  }
  
  not_conform_conversion_factors <- curation_absurd_converted_data_list$conversion_factor_not_to_keep
  
  if(nrow(not_conform_conversion_factors) != 0){
    
    function_recap_each_step(
      "Removing_absurd_nomt",
      georef_dataset,
      "In this step, we target implausible data. We check data having declaration both in NOMT and MTNO and if the conversion factor is implausible.
					   We either remove NOMT strata which corresponds to MTNO declaration implausible or me remove the corresponding MTNO data. More details are available in the pdf file attached.",
      "curation_absurd_converted_data"
    )
    saveRDS(not_conform_conversion_factors, "data/not_conform_conversion_factors.rds")
    
  }
  
  #----------Standardizing unit of measures---------------------------------------------------------------------------------------------------------------------------
  stepLogger(level = 0, step = stepnumber, msg = "Standardizing unit of measures")
  stepnumber = stepnumber+1
  #-------------------------------------------------------------------------------------------------------------------------------------
  
  unit_weight_to_remap = c("MT", "MTNO")
  unit_number_to_remap = c("NO", "NOMT")
  georef_dataset <- georef_dataset %>%
    dplyr::mutate(
      measurement_unit = case_when(
        measurement_unit %in% unit_weight_to_remap ~ "t",
        measurement_unit %in% unit_number_to_remap ~ "no",
        TRUE ~ measurement_unit
      )
    )
  
  # -----------spatial_curation_data_mislocated------------------------------------------------------
  
  
  stepLogger(level = 0, step = stepnumber, msg = sprintf("Reallocation of mislocated data  (i.e. on land areas or without any spatial information) (data with no spatial information have the dimension 'geographic_identifier' set to 'UNK/IND' or 'NA'). Option is: [%s] ", opts$action_on_mislocated))
  stepnumber = stepnumber+1
  
  
  spatial_curation_data_mislocated_list <- spatial_curation_data_mislocated(config = config,df = georef_dataset,action_on_mislocated = "remove")
  
  if(exists("areas_in_land")){
    rm(areas_in_land)
  }
  
  georef_dataset <- spatial_curation_data_mislocated_list$dataset
  areas_in_land <- spatial_curation_data_mislocated_list$areas_in_land
  dataset_not_cwp_grid <- spatial_curation_data_mislocated_list$dataset_not_cwp_grid
  
  if (!is.null(areas_in_land) && is.data.frame(areas_in_land) && nrow(areas_in_land) != 0) {
    
    if(recap_each_step){
      function_recap_each_step(
        "Realocating_removing_mislocated_data",
        georef_dataset,
        "In this step, the mislocated data is hanlded. Either removed, reallocated or let alone, the data on continent and the data outside the competent rfmo area are targeted. ",
        "spatial_curation_data_mislocated"
      )
      saveRDS(areas_in_land,"data/areas_in_land.rds")
    }
    gc()
    
    
  }
  if (!is.null(dataset_not_cwp_grid) && is.data.frame(dataset_not_cwp_grid) && nrow(dataset_not_cwp_grid) != 0) {
    
    if(recap_each_step){
      function_recap_each_step(
        "Realocating_removing_mislocated_data",
        georef_dataset,
        "In this step, the mislocated data is hanlded. Either removed, reallocated or let alone, the data on continent and the data outside the competent rfmo area are targeted. ",
        "spatial_curation_data_mislocated"
      )
      saveRDS(dataset_not_cwp_grid,"data/removed_irregular_areas.rds")
      # names_list_irregular_areas <-
      #   c("dataset_not_cwp_grid") #file we want to save
      # 
      # try(lapply(names_list_irregular_areas, function_write_RDS))
      
    }
    gc()
    
    
  }
  
  
  if(!is.null(NULL)){
    
    # Outside juridiction -----------------------------------------------------
    
    function_outside_juridiction <- function_outside_juridiction(georef_dataset,con)
    if(exists("outside_juridiction")){
      rm(outside_juridiction)
    }
    
    georef_dataset <- function_outside_juridiction$georef_dataset
    outside_juridiction <- function_outside_juridiction$outside_juridiction
    
    if(!is.null(outside_juridiction) && is.data.frame(outside_juridiction) && nrow(outside_juridiction) != 0) {
      
      if(recap_each_step){
        function_recap_each_step(
          "outside_juridiction",
          georef_dataset,
          paste0(
            "In this step, we handle areas that does not match with the shape of the source_authority declaring"
          ) ,
          "function_outside_juridiction",
        )
        saveRDS(outside_juridiction,"data/outside_juridiction.rds")
        
      }
      
    }
    
  }
  

  
  
  files_to_check <- c("data/not_conform_conversion_factors.rds",
                      "data/removed_irregular_areas.rds",
                      "data/areas_in_land.rds",
                      "data/outside_juridiction.rds",
                      "data/not_mapped_total.rds")
  
  if(any(file.exists(files_to_check))) {
    parameter_directory <- getwd()
    base::options(knitr.duplicate.label = "allow")
  }
  
  
  }
  
  
  #----------Map code lists -------------------------------------------------------------------------------------------------------------------------------------------------
  #Map to CWP standard codelists (if not provided by tRFMO according to the CWP RH standard data exchange format)
  #--------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  options("OutDec" = ".")
  source_authority_to_map = if(!is.null(opts$source_authority_to_map)) opts$source_authority_to_map else c("CCSBT", "IATTC", "WCPFC")
  
  if(any(unique(georef_dataset$source_authority)%in%source_authority_to_map)){
    stepLogger(level = 0, step = stepnumber, msg = "Map to CWP standard codelists (if not provided by tRFMO according to the CWP RH standard data exchange format)")
    stepnumber = stepnumber+1
    config$logger.info("Reading the CSV containing the dimensions to map + the names of the code list mapping datasets. Code list mapping datasets must be available in the database.")
    # mapping_csv_mapping_datasets_url <- "https://raw.githubusercontent.com/fdiwg/fdi-mappings/main/global/firms/gta/codelist_mapping_rfmos_to_global.csv"
    # mapping_dataset <-read.csv(mapping_csv_mapping_datasets_url,stringsAsFactors = F,colClasses = "character")
    config$logger.info("Mapping code lists of georeferenced datasets...")
    # mapping_codelist <-map_codelists(con, opts$fact, mapping_dataset = mapping_dataset,dataset_to_map = georef_dataset, mapping_keep_src_code,summary_mapping = TRUE,source_authority_to_map = source_authority_to_map) #this map condelist function is to retrieve the mapping dataset used
    mapping_codelist <-map_codelists_no_DB(opts$fact, mapping_dataset = "https://raw.githubusercontent.com/fdiwg/fdi-mappings/main/global/firms/gta/codelist_mapping_rfmos_to_global.csv", 
                                           dataset_to_map = georef_dataset, 
                                           mapping_keep_src_code = FALSE, summary_mapping = TRUE, source_authority_to_map = c("IATTC", "CCSBT", "WCPFC")) 
    
    
    georef_dataset <- mapping_codelist$dataset_mapped
    
    
    if(fact == "catch"){
      georef_dataset <- georef_dataset %>% dplyr::mutate(fishing_fleet = ifelse(fishing_fleet == "UNK", "NEI", fishing_fleet),
                                                         species = ifelse(species == "UNK", "MZZ", species),
                                                         gear_type = ifelse(gear_type == "UNK", "99.9", gear_type))
    } else {
      georef_dataset <- georef_dataset %>% dplyr::mutate(fishing_fleet = ifelse(fishing_fleet == "UNK", "NEI", fishing_fleet),
                                                         gear_type = ifelse(gear_type == "UNK", "99.9", gear_type))
      
    }
    
    config$logger.info("Mapping code lists of georeferenced datasets OK")
    
    
    if(recap_each_step){
      
      recap_mapping <- mapping_codelist$recap_mapping
      stats_total <- mapping_codelist$stats_total
      not_mapped_total <- mapping_codelist$not_mapped_total
      
      saveRDS(not_mapped_total,"data/not_mapped_total.rds")
      saveRDS(recap_mapping,"data/recap_mapping.rds")
      
      config$logger.info("Saving recap of mapping ok")
      
      function_recap_each_step(
        "mapping_codelist",
        georef_dataset,
        "This step is to map all the data with the same codes for gears, species, and fishingfleet,
	that is to say, to put all the data coming from different RFMOs in the same langage with the same
	codes. Coding systems and nomenclatures used to describe the data may differ according to tRFMOs.
	Codes used by the tuna RFMOs in their respective datasets were mapped with global code lists for
	gear (ISSCFG), flag (ISO3 countries codes), and species (ASFIS). Some codes could not be mapped
	to standard code lists, for some tRFMOs own-defined codes that usually are aggregation of existing
	codes (e.g. flag ’IDPH’ standing for Indonesia and Philippines within WCPFC or the species “Otun”
	standing for other tuna within for ICCAT). In those cases, the code was set to UNK (Unknown). For
	species and gears, these codes were mapped with more aggregated code lists, i.e. resp. group of species
	and groups of gears.",
        "map_codelists"
      )
      
      df_mapping_final_this_dimension <- recap_mapping %>%
        dplyr::filter(source_authority %in% unique(georef_dataset$source_authority))
      gc()
      
    }
    
  }
  
  #Filter on species under mandate for FIRMS level 0
  #-------------------------------------------------------------------------------------------------
  stepLogger(level = 0, step = stepnumber, msg = "Filter on species under mandate for FIRMS level 0")
  stepnumber = stepnumber+1
  
  # Temporary patch for ASFIS RMJ --> RMM
  if("species" %in% colnames(georef_dataset)){
    georef_dataset <- georef_dataset %>% dplyr::mutate(species = case_when(species == "RMJ" ~ "RMM", TRUE ~ species))
    opts$fact <- "effort"
    fact <- "effort"
    
    # Filtering on species under mandate --------------------------------------
    # done base on mapping between source_authority (tRFMO) and species
    stepLogger(level = 0, step = stepnumber, msg = "Filtering on species under mandate, targeted by GTA")
    stepnumber = stepnumber +1
    #url_asfis_list <- "https://raw.githubusercontent.com/fdiwg/fdi-codelists/main/global/firms/gta/cl_species_level0.csv"
    
    url_mapping_asfis_rfmo = "https://raw.githubusercontent.com/fdiwg/fdi-mappings/main/cross-term/codelist_mapping_source_authority_species.csv"
    species_to_be_kept_by_rfmo_in_level0 <- readr::read_csv(url_mapping_asfis_rfmo) %>% dplyr::distinct()
    georef_dataset <- georef_dataset %>% dplyr::inner_join(species_to_be_kept_by_rfmo_in_level0, 
                                                           by = c("species" = "species", "source_authority" = "source_authority"))
    
    if(recap_each_step){
      function_recap_each_step(
        "Filtering species",
        georef_dataset,
        paste0(
          "Filtering species on the base of the file ",
          url_mapping_asfis_rfmo,
          " to keep only the species under mandate of tRFMOs. This file contains " ,
          as.character(length(nrow(
            species_to_be_kept_by_rfmo_in_level0
          ))),
          " species."
        ),
        "inner_join"  ,
        NULL
      )
    }
  }
  

  
  #we do an aggregation by dimensions
  dataset <-
    georef_dataset %>% group_by(.dots = setdiff(colnames(georef_dataset), "measurement_value")) %>% dplyr::summarise(measurement_value =
                                                                                                                       sum(measurement_value))
  
  config$logger.info("LEVEL %s => STEP aggregated")
  
  dataset <- data.frame(dataset)
  
  #@eblondel additional formatting for next time support
  dataset$time_start <- as.Date(dataset$time_start)
  dataset$time_end <- as.Date(dataset$time_end)
  #we enrich the entity with temporal coverage
  dataset_temporal_extent <- paste(
    paste0(format(min(dataset$time_start), "%Y"), "-01-01"),
    paste0(format(max(dataset$time_end), "%Y"), "-12-31"),
    sep = "/"
  )
  config$logger.info("temporal extent")
  
  entity$setTemporalExtent(dataset_temporal_extent)
  
  config$logger.info("seting temporal extend")
  
  #@geoflow -> export as csv
  
  
  config$logger.info(output_name_dataset_mapped)
  
  write.csv(dataset, output_name_dataset_mapped, row.names = FALSE)
  
  config$logger.info("writted")
  
  entity$addResource("mapped", output_name_dataset_mapped)
  
  config$logger.info("addingressource")
  gc()
  
}
