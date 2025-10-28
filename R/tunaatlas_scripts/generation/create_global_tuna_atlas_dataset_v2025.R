#' Create Your Own Tuna Atlas Dataset
#'
#' This function allows users to create their own georeferenced Tuna Atlas dataset of catch or effort.
#' It takes as input the public domain datasets of the five Tuna Regional Fisheries Management Organizations (tRFMOs)
#' (IOTC, ICCAT, WCPFC, IATTC, CCSBT) stored within the Tuna atlas database. Users can customize the computation
#' of the tuna atlas using a set of parameters.
#'
#' @param action An action object containing options for dataset processing.
#' @param entity A geoflow entity object with dataset identifiers and metadata.
#' @param config A geoflow configuration object with software settings.
#' @return The status of the dataset creation process.
#' @examples
#' \dontrun{
#' # Example usage:
#' action <- list(options = list(fact = "catch", dataset_level = 1))
#' entity <- list(identifiers = list(id = "example_id"))
#' config <- list(software = list(output = list(dbi = "some_dbi_connection")),
#'                logger = list(info = function(msg) cat(msg, "\n")))
#' create_own_tuna_atlas_catch_effort(action, entity, config)
#' }
#' @export
#' 
create_global_tuna_atlas_dataset_v2025 <- function(action, entity, config) {
  
  # Initialisation ----------------------------------------------------------
  
  opts <- action$options
  con <- config$software$output$dbi
  options(encoding = "UTF-8")
  source("https://raw.githubusercontent.com/firms-gta/geoflow-tunaatlas/master/Analysis_markdown/functions/write_options_to_csv.R")
  write_options_to_csv(opts)
  # List of required packages
  packages <- c("dplyr","zen4R" ,"sf", "stringr", "R3port", "reshape2", "readr", "tools", "RPostgreSQL", "DBI", "googledrive")
  
  # Function to check and install missing packages
  install_and_load <- function(package) {
    if (!require(package, character.only = TRUE)) {
      install.packages(package)
      require(package, character.only = TRUE)
    }
  }
  # Apply the function to each package
  sapply(packages, install_and_load)
  
  stepnumber <- 1
  #scripts
  url_scripts_create_own_tuna_atlas <- "https://raw.githubusercontent.com/firms-gta/geoflow-tunaatlas/master/R/tunaatlas_scripts/generation"
  
  # Retrieve data from zenodo 
  source(file.path(url_scripts_create_own_tuna_atlas,"download_zenodo_csv.R"))
  
  #for level 0 - FIRMS
  # source(file.path(url_scripts_create_own_tuna_atlas, "get_rfmos_datasets_level0.R")) #modified for geoflow
  source(here::here("./R/tunaatlas_scripts/generation/get_rfmos_datasets_level0.R")) #modified for geoflow
  source(file.path(url_scripts_create_own_tuna_atlas, "retrieve_nominal_catch.R")) #modified for geoflow
  source("https://raw.githubusercontent.com/firms-gta/geoflow-tunaatlas/master/R/tunaatlas_scripts/pre-harmonization/map_codelists.R") #modified for geoflow
  # source(file.path(url_scripts_create_own_tuna_atlas, "function_overlapped.R")) # adding this function as overlapping is now a recurent procedures for several overlapping 
  source(here::here("tunaatlas_scripts/generation/function_overlapped.R")) #modified for geoflow
  #for filtering if needed
  source(file.path(url_scripts_create_own_tuna_atlas, "dimension_filtering_function.R")) # adding this function as overlapping is now a recurent procedures for several overlapping 
  
  # Process and aggregate final data 
  source(file.path(url_scripts_create_own_tuna_atlas, "process_and_aggregate_dataset.R"))
  
  
  #for level 1 - FIRMS (candidate)
  source(file.path(url_scripts_create_own_tuna_atlas, "double_unit_data_handling.R")) # new function for double unit
  source(file.path(url_scripts_create_own_tuna_atlas, "do_unit_conversion.R"))
  source(file.path(url_scripts_create_own_tuna_atlas, "perform_unit_conversion.R"))
  
  
  #for level 2 - IRD
  source(file.path(url_scripts_create_own_tuna_atlas, "disaggregate_on_resdeg_data_with_resolution_superior_to_resdeg.R"))
  source(file.path(url_scripts_create_own_tuna_atlas, "function_raising_georef_to_nominal.R")) #modified for geoflow
  source(here::here("tunaatlas_scripts/generation/convert_number_to_nominal.R")) #modified for geoflow
  source(here::here("tunaatlas_scripts/generation/convert_number_to_nominal.R")) #modified for geoflow
  source(here::here("tunaatlas_scripts/generation/function_raise_data.R")) #modified for geoflow
  
  
  # For filtering/aggregating
  source("https://raw.githubusercontent.com/firms-gta/geoflow-tunaatlas/master/R/sardara_functions/transform_cwp_code_from_1deg_to_5deg.R")
  
  #stepLog
  stepLogger = function(level, step, msg){
    config$logger.info(sprintf("LEVEL %s => STEP %s: %s", level, step, msg))
  }
  
  #for reporting
  source("https://raw.githubusercontent.com/firms-gta/geoflow-tunaatlas/master/Analysis_markdown/functions/function_recap_each_step.R") # new function to create rds for each treatment
  source("https://raw.githubusercontent.com/firms-gta/geoflow-tunaatlas/master/Analysis_markdown/functions/copyrmd.R")
  # Saving options in a csv file and creating a new variable for each options
  
  #action options
  filtering_on_minimum_year_declared = if(!is.null(opts$filtering_on_minimum_year_declared)) opts$filtering_on_minimum_year_declared else TRUE
  recap_each_step = if(!is.null(opts$recap_each_step)) opts$recap_each_step else TRUE
  from_rawdata <- if(!is.null(opts$from_rawdata)){opts$from_rawdata} else if(opts$fact == "effort"){TRUE}  else FALSE
  
  opts$doilevel0 = if(!is.null(opts$doilevel0)) opts$doilevel0 else "10.5281/zenodo.11460074"
  opts$keylevel0 =  if(!is.null(opts$keylevel0)) opts$keylevel0 else "global_catch_firms_level0_harmonized.csv"
  opts$forceuseofdoi =  if(!is.null(opts$forceuseofdoi)) opts$forceuseofdoi else FALSE
  opts$doinominal = if(!is.null(opts$doinominal)) opts$doinominal else "10.5281/zenodo.11410529"
  opts$keynominal =  if(!is.null(opts$keynominal)) opts$keynominal else "global_nominal_catch_firms_level0_harmonized.csv"
  opts$include_IOTC = if(!is.null(opts$include_IOTC)) opts$include_IOTC else TRUE
  opts$include_IATTC = if(!is.null(opts$include_IATTC)) opts$include_IATTC else TRUE
  opts$include_WCPFC = if(!is.null(opts$include_WCPFC)) opts$include_WCPFC else TRUE
  opts$include_CCSBT = if(!is.null(opts$include_CCSBT)) opts$include_CCSBT else TRUE
  opts$include_ICCAT = if(!is.null(opts$include_ICCAT)) opts$include_ICCAT else TRUE
  opts$fact = if(!is.null(opts$fact)) opts$fact else "catch"
  opts$dataset_level = if(!is.null(opts$dataset_level)) opts$dataset_level else 0
  opts$recap_each_step = if(!is.null(opts$recap_each_step)) opts$recap_each_step else TRUE
  opts$source_authority_to_map = if(!is.null(opts$source_authority_to_map)) opts$source_authority_to_map else c("IATTC", "CCSBT", "WCPFC")
  opts$iccat_ps_include_type_of_school = if(!is.null(opts$iccat_ps_include_type_of_school)) opts$iccat_ps_include_type_of_school else TRUE
  opts$iattc_ps_raise_flags_to_schooltype = if(!is.null(opts$iattc_ps_raise_flags_to_schooltype)) opts$iattc_ps_raise_flags_to_schooltype else FALSE
  opts$iattc_ps_catch_billfish_shark_raise_to_effort = if(!is.null(opts$iattc_ps_catch_billfish_shark_raise_to_effort)) opts$iattc_ps_catch_billfish_shark_raise_to_effort else FALSE
  opts$mapping_map_code_lists = if(!is.null(opts$mapping_map_code_lists)) opts$mapping_map_code_lists else TRUE
  opts$mapping_keep_src_code = if(!is.null(opts$mapping_keep_src_code)) opts$mapping_keep_src_code else FALSE
  opts$aggregate_on_5deg_data_with_resolution_inferior_to_5deg = if(!is.null(opts$aggregate_on_5deg_data_with_resolution_inferior_to_5deg)) opts$aggregate_on_5deg_data_with_resolution_inferior_to_5deg else FALSE
  opts$overlapping_zone_iattc_wcpfc_data_to_keep = if(!is.null(opts$overlapping_zone_iattc_wcpfc_data_to_keep)) opts$overlapping_zone_iattc_wcpfc_data_to_keep else "IATTC"
  opts$overlapping_zone_iotc_wcpfc_data_to_keep = if(!is.null(opts$overlapping_zone_iotc_wcpfc_data_to_keep)) opts$overlapping_zone_iotc_wcpfc_data_to_keep else "IOTC"
  opts$spatial_curation_data_mislocated = if(!is.null(opts$spatial_curation_data_mislocated)) opts$spatial_curation_data_mislocated else "remove"
  opts$unit_conversion_convert = if(!is.null(opts$unit_conversion_convert)) opts$unit_conversion_convert else FALSE
  opts$unit_conversion_csv_conversion_factor_url = if(!is.null(opts$unit_conversion_csv_conversion_factor_url)) opts$unit_conversion_csv_conversion_factor_url else "https://data.d4science.org/shub/4276080e-ab63-4e9a-bfe2-9213ab659a84"
  opts$unit_conversion_codelist_geoidentifiers_conversion_factors = if(!is.null(opts$unit_conversion_codelist_geoidentifiers_conversion_factors)) opts$unit_conversion_codelist_geoidentifiers_conversion_factors else "areas_conversion_factors_numtoweight_ird"
  opts$raising_georef_to_nominal = if(!is.null(opts$raising_georef_to_nominal)) opts$raising_georef_to_nominal else FALSE
  opts$raising_do_not_raise_wcfpc_data = if(!is.null(opts$raising_do_not_raise_wcfpc_data)) opts$raising_do_not_raise_wcfpc_data else FALSE
  opts$raising_raise_only_for_PS_LL = if(!is.null(opts$raising_raise_only_for_PS_LL)) opts$raising_raise_only_for_PS_LL else FALSE
  opts$disaggregate_on_5deg_data_with_resolution_superior_to_5deg = if(!is.null(opts$disaggregate_on_5deg_data_with_resolution_superior_to_5deg)) opts$disaggregate_on_5deg_data_with_resolution_superior_to_5deg else "none"
  opts$disaggregate_on_1deg_data_with_resolution_superior_to_1deg = if(!is.null(opts$disaggregate_on_1deg_data_with_resolution_superior_to_1deg)) opts$disaggregate_on_1deg_data_with_resolution_superior_to_1deg else "none"
  opts$cache = if(!is.null(opts$cache)) opts$cache else FALSE
  opts$level2RF2 = if(!is.null(opts$level2RF2)) opts$level2RF2 else FALSE
  opts$upgradeIOTC_fact_conv = if(!is.null(opts$upgradeIOTC_fact_conv)) opts$upgradeIOTC_fact_conv else FALSE
  opts$upgradeIRD_fact_conv = if(!is.null(opts$upgradeIRD_fact_conv)) opts$upgradeIRD_fact_conv else FALSE
  #Identify expected Level of processing
  opts$fact = if(!is.null(opts$fact)) opts$fact else {futile.logger::flog.error("Please provide a fact, i.e. effort or catch")}
  DATASET_LEVEL = if(!is.null(opts$dataset_level)) opts$dataset_level else {futile.logger::flog.error("Please provide a fact, i.e. effort or catch")}
  
  opts$level2RF2number = if(!is.null(opts$level2RF2number)) opts$level2RF2number else FALSE
  opts$decrease_when_rf_inferior_to_one = if(!is.null(opts$decrease_when_rf_inferior_to_one)) opts$decrease_when_rf_inferior_to_one else FALSE
  # LEVEL 0 FIRMS PRODUCT ---------------------------------------------------
  
  if(DATASET_LEVEL == 0 | from_rawdata){
    ## INITIALISATION OF MULTIPLES DATASET---------------------------------------------------
    config$logger.info("Begin: Retrieving primary datasets from Tuna atlas DB... ")
    
    stepLogger(level = 0, step = stepnumber, msg = "Retrieve georeferenced catch or effort (+ processings for IATTC) AND NOMINAL CATCH if asked")
    stepnumber = stepnumber+1
    
    rawdata <- opts
    #by default, at this step we will skip specific processings applied to IATTC data. These processings are eventually done later in the script (if options are activated)
    rawdata$iattc_ps_raise_flags_to_schooltype <- FALSE
    rawdata$iattc_ps_catch_billfish_shark_raise_to_effort <- FALSE
    rawdata$iattc_ps_dimension_to_use_if_no_raising_flags_to_schooltype <- "fishing_fleet"
    
    dataset <-
      do.call("rbind",
              lapply(
                c("IOTC", "WCPFC", "CCSBT", "ICCAT", "IATTC"),
                get_rfmos_datasets_level0,
                entity,
                config,
                rawdata
              ))
    
    dataset$time_start <-
      substr(as.character(dataset$time_start), 1, 10)
    dataset$time_end <- substr(as.character(dataset$time_end), 1, 10)
    georef_dataset <- dataset
    class(georef_dataset$measurement_value) <- "numeric"
    rm(dataset)
    
    species_to_force <- c("BSH", "FAL", "MAK", "OCS", "RSK", "SKH", "SMA", "SPN", "THR")
    georef_dataset <- georef_dataset %>%
      dplyr::mutate(measurement_processing_level = dplyr::case_when(
        source_authority == "IATTC" & species %in% species_to_force ~ "original_sample",
        TRUE ~ measurement_processing_level
      ))
    
    georef_dataset <- georef_dataset %>% dplyr::filter(substr(geographic_identifier, 1, 1) != "7") # removing 10 degrees
    
    if(recap_each_step){
      function_recap_each_step(
        "rawdata",
        georef_dataset,
        "The georeferenced data of the included tRFMOS, are binded.",
        "get_rfmos_datasets_level0"  ,
        list(), entity
      )
    }
    
    ### filtering on minimum time_start (TRUE  by default)
    if (filtering_on_minimum_year_declared) {
      stepLogger(level = 0, step = stepnumber, msg = "Filtering on minimum time_start")
      stepnumber = stepnumber+1
      
      # extract the maximum year of declaration for each source_authority
      max_years <- georef_dataset %>%
        dplyr::group_by(source_authority) %>%
        dplyr::summarise(max_time_start = max(time_start))
      
      # check if not all the source_authority columns have the same maximum year of declaration
      if (length(unique(max_years$max_time_start)) > 1) {
        config$logger.info("Careful, not all the source_authority has the same maximum year of declaration")
        
        # get the minimum time_start of all the maximum time_start of each source_authority
        min_time_start <- min(max_years$max_time_start)
        
        # filter the georef_dataset based on the minimum time_start of all the maximum time_start of each source_authority
        georef_dataset <- georef_dataset %>%
          dplyr::filter(time_start <= min_time_start)
        
        if (recap_each_step) {
          function_recap_each_step(
            "rawdata_time_harmonized",
            georef_dataset,
            "Years provided by not every tRFMOs are removed",
            "filter",
            list(), entity
          )
          saveRDS(georef_dataset, "data/rawdata.rds")
        }
      }
    }
    
    # PROCESSINGS FOR IATTC data
    if(opts$fact == "catch"){
      
      if (opts$iattc_ps_raise_flags_to_schooltype) {
        rawdata$iattc_ps_raise_flags_to_schooltype <-
          opts$iattc_ps_raise_flags_to_schooltype
        
        
        stepLogger(level = 0, step = stepnumber, msg = "Enriching data with schootype for IATTC if needed")
        stepnumber = stepnumber+1
        iattc <-
          get_rfmos_datasets_level0("IATTC", entity, config, rawdata)
        iattc$time_start <- substr(as.character(iattc$time_start), 1, 10)
        iattc$time_end <- substr(as.character(iattc$time_end), 1, 10)
        class(iattc$measurement_value) <- "numeric"
        
        georef_dataset <-
          base::rbind(georef_dataset %>% filter(source_authority != "IATTC"),
                      iattc)
        rm(iattc)
        if (filtering_on_minimum_year_declared) {
          stepLogger(level = 0, step = stepnumber, msg = "Filtering on minimum time_start")
          stepnumber = stepnumber+1
          
          # extract the maximum year of declaration for each source_authority
          max_years <- georef_dataset %>%
            dplyr::group_by(source_authority) %>%
            dplyr::summarise(max_time_start = max(time_start))
          
          # check if not all the source_authority columns have the same maximum year of declaration
          if (length(unique(max_years$max_time_start)) > 1) {
            config$logger.info("Careful, not all the source_authority has the same maximum year of declaration")
            
            # get the minimum time_start of all the maximum time_start of each source_authority
            min_time_start <- min(max_years$max_time_start)
            
            # filter the georef_dataset based on the minimum time_start of all the maximum time_start of each source_authority
            georef_dataset <- georef_dataset %>%
              filter(time_start <= min_time_start)
          }
        }
        
        if(recap_each_step){
          function_recap_each_step(
            "iattc enriched",
            georef_dataset,
            "This step is using the different datasets provided by IATTC (6 datasets) and creating a binded version of all of them.
		     IATTC PS catch-and-effort are stratified as following:
                        # - 1 dataset for tunas, stratified by type of school (but not fishingfleet)
                        # - 1 dataset for tunas, stratified by fishingfleet (but not type of school)
                        # - 1 dataset for billfishes, stratified by type of school (but not fishingfleet)
                        # - 1 dataset for billfishes, stratified by fishingfleet (but not type of school)
                        # - 1 dataset for sharks, stratified by type of school (but not fishingfleet)
                        # - 1 dataset for sharks, stratified by fishingfleet (but not type of school)
                        ## So in total there are 6 datasets.  
      This step is merging the different datasets provided and extracting type of school and adding it to the fishing_fleet datasets.",
            "raise_datasets_by_dimension"  ,entity = entity
          )
        }
      }
      
      if (opts$iattc_ps_catch_billfish_shark_raise_to_effort) {
        rawdata$iattc_ps_catch_billfish_shark_raise_to_effort <-
          opts$iattc_ps_catch_billfish_shark_raise_to_effort
        
        stepLogger(level = 0, step = stepnumber, msg = "Raising catch data to effort for IATTC")
        stepnumber = stepnumber+1
        iattc <-
          get_rfmos_datasets_level0("IATTC", entity, config, rawdata)
        iattc$time_start <- substr(as.character(iattc$time_start), 1, 10)
        iattc$time_end <- substr(as.character(iattc$time_end), 1, 10)
        class(iattc$measurement_value) <- "numeric"
        
        georef_dataset <-
          base::rbind(georef_dataset %>% filter(source_authority != "IATTC"),
                      iattc)
        rm(iattc)
        if (filtering_on_minimum_year_declared) {
          stepLogger(level = 0, step = stepnumber, msg = "Filtering on minimum time_start")
          stepnumber = stepnumber+1
          
          # extract the maximum year of declaration for each source_authority
          max_years <- georef_dataset %>%
            dplyr::group_by(source_authority) %>%
            dplyr::summarise(max_time_start = max(time_start))
          
          # check if not all the source_authority columns have the same maximum year of declaration
          if (length(unique(max_years$max_time_start)) > 1) {
            config$logger.info("Careful, not all the source_authority has the same maximum year of declaration")
            
            # get the minimum time_start of all the maximum time_start of each source_authority
            min_time_start <- min(max_years$max_time_start)
            
            # filter the georef_dataset based on the minimum time_start of all the maximum time_start of each source_authority
            georef_dataset <- georef_dataset %>%
              filter(time_start <= min_time_start)
          }
        }
        
        if(recap_each_step){
          function_recap_each_step(
            "iattc raised for billfish and shark",
            georef_dataset,
            "The effort is expressed here in terms of number of sets. This means in the case of the EPO 
		  that the effort given in some datasets may correspond to a part of the total effort allocated to a stratum since it is the observed effort, 
		  i.e. for which there was an observer on board the purse seine vessel. 
		  (1) The unique and homogeneous effort would be that of tropical tunas 
		  (2) to standardize the set of catches per stratum, 
		  it is necessary to calculate a ratio of shark catches per set (observed) and swordfish catches per set (observed) and
		  then multiply them by the effort carried over for tropical tunas since this is considered to be the effort of the fishery 
		  (which targets tunas). The raising factor is tuna effort/billfish effort and tuna effort/shark effort.",
            "get_rfmos_datasets_level0"  , entity = entity
          )
        }
      }
      
      #   if(opts$fact == "catch"){
      #   georef_dataset <-
      #     double_unit_data_handling( #to add handling of strata having multiples unit (tonnes and number which happen)
      #       con = con,
      #       entity = entity,
      #       config = config,
      #       fact = fact,
      #       unit_conversion_csv_conversion_factor_url =
      #         NULL,
      #       unit_conversion_codelist_geoidentifiers_conversion_factors =
      #         opts$unit_conversion_codelist_geoidentifiers_conversion_factors,
      #       mapping_map_code_lists = opts$mapping_map_code_lists,
      #       georef_dataset = georef_dataset
      #     )
      #   function_recap_each_step(
      #     "Removing NOMT and converting MTNO in MT",
      #     georef_dataset,
      #     "The data initially in MTNO is converted in MT and the data in NTMO is removed",
      #     "double_unit_data_handling"
      #   )
      #   
      # }
    }
    
    # Minor mapping for efforts -----------------------------------------------
    
    
    ## OVERLAPPPING ZONES---------------------------------------------------
    # This function handles the processing of overlapping zones.
    handle_overlap <- function(zone_key, rfmo_main, default_strata, recap_step = TRUE) {
      # Construct the names of options based on the zone key
      opts_key <- paste0("overlapping_zone_", zone_key, "_data_to_keep")
      strata_key <- paste0("strata_overlap_", zone_key)
      
      # Check if options for the zone are provided
      if (is.null(opts[[opts_key]])) {
        if(rfmo_main[[1]]=="CCSBT"){
          opts[[opts_key]] <- "CCSBT"
        } else {
          config$logger.info(paste0("Please provide a source authority to keep for overlapping zone ", opts_key))
          return()
        }
      }
      
      # Log the current processing step
      stepLogger(level = 0, step = stepnumber, msg = paste0("Overlapping zone (", zone_key, "): keep data from ", opts[[opts_key]]))
      stepnumber <<- stepnumber + 1
      
      # Determine which strata options to use, default or provided
      if (!exists(paste0("opts$", strata_key))) {
        options_strata <- default_strata
      } else {
        options_strata <- unlist(strsplit(opts[[strata_key]], split = ","))
      }
      
      # Determine which RFMO data not to keep
      rfmo_not_to_keep <- ifelse(opts[[opts_key]] == rfmo_main[[1]], names(rfmo_main)[[1]], rfmo_main[[1]])
      # Call the overlapping function to process the dataset
      georef_dataset <<- function_overlapped(
        dataset = georef_dataset,
        con = con,
        rfmo_to_keep = opts[[opts_key]],
        rfmo_not_to_keep = rfmo_not_to_keep,
        strata = options_strata,
        opts = opts
      )
      
      # If required, recap the steps undertaken
      if(recap_step){
        function_recap_each_step(
          paste0("overlap_", zone_key),
          georef_dataset,
          paste0("The georeferenced data present on the overlapping zone between ", rfmo_main, " and ", names(rfmo_main)[[1]], " is handled.",
                 "The option for the strata overlapping allows handling the maximum similarities allowed between two data to keep both. ",
                 "In the case the data is identical on the stratas provided, the remaining data is from ", opts[[opts_key]]),
          "function_overlapped",
          list(
            opts[[opts_key]],
            options_strata
          ), entity
        )
      }
    }
    
    
    # Configuration for each overlapping zone checking the one having an impact later and be able to easily remove unusefull steps by commenting
    zones_config <- list(
      iattc_wcpfc = list(main = c(WCPFC = "IATTC"), default_strata = c("geographic_identifier", "species", "year", "fishing_fleet", "gear_type")),
      # wcpfc_ccsbt = list(main = c(WCPFC = "CCSBT"), default_strata = c("species")), # not usefull anymore as handled in pre harmo
      # iccat_ccsbt = list(main = c(ICCAT = "CCSBT"), default_strata = c("species")),# not usefull anymore as handled in pre harmo
      # iotc_ccsbt = list(main = c(IOTC = "CCSBT"), default_strata = c("species")),# not usefull anymore as handled in pre harmo
      iotc_wcpfc = list(main = c(WCPFC = "IOTC"), default_strata = c("geographic_identifier", "species", "year", "fishing_fleet", "gear_type"))
    )
    
    if(opts$fact == "effort"){
      zones_config <- list(
        iattc_wcpfc = list(main = c(WCPFC = "IATTC"), default_strata = c("geographic_identifier", "year", "fishing_fleet", "gear_type")),
        # wcpfc_ccsbt = list(main = c(WCPFC = "CCSBT"), default_strata = c("species")), # not usefull anymore as handled in pre harmo
        # iccat_ccsbt = list(main = c(ICCAT = "CCSBT"), default_strata = c("species")),# not usefull anymore as handled in pre harmo
        # iotc_ccsbt = list(main = c(IOTC = "CCSBT"), default_strata = c("species")),# not usefull anymore as handled in pre harmo
        iotc_wcpfc = list(main = c(WCPFC = "IOTC"), default_strata = c("geographic_identifier", "year", "fishing_fleet", "gear_type"))
      )
    }
    
    # Loop over each zone and handle overlap using the defined configuration
    for (zone_key in names(zones_config)) {
      config$logger.info(paste0("Processing zone: ", zone_key))  # Log before processing
      
      # It's a good practice to use tryCatch to understand if errors in handle_overlap are stopping the loop
      tryCatch({
        handle_overlap(zone_key, zones_config[[zone_key]]$main, zones_config[[zone_key]]$default_strata, recap_step = recap_each_step)
      }, error = function(e) {
        message(paste0("Error encountered: ", e))  # Print errors to the console
      })
      
      config$logger.info(paste0("Finished processing zone: ", zone_key))  # Log after processing
    }
  } else {
    ## RETRIEVING DATA FROM DOI ---------------------------------------------------
    # Load georeferenced dataset
    if (file.exists((file.path("data", opts$keylevel0))) && !opts$forceuseofdoi) {
      georef_dataset <- readr::read_csv((file.path("data", opts$keylevel0)), guess_max = 0)
      class(georef_dataset$measurement_value) <- "numeric" 
    } else if (!is.null(opts$doilevel0)) {
      zen4R::download_zenodo(doi = opts$doilevel0, files = opts$keylevel0, path = "data")
      georef_dataset <- readr::read_csv(here::here(file.path("data", opts$keylevel0)), guess_max = 0)
      class(georef_dataset$measurement_value) <- "numeric"
    } else {
      stop("Please provide a georeferenced catch dataset")
    }
    function_recap_each_step(
      "Level0_Firms",
      georef_dataset,
      paste0(
        "Retrieving level 0 data on the basis of the following DOI: ",
        opts$doi, " the key being: ", opts$key,". ", Description),
      "download_zenodo_csv"  ,
      list(opts$doi, opts$key), entity
    )
  }
  
  
  
  # Filtering on complete year ----------------------------------------------
  if(DATASET_LEVEL == 2){
    
    if (file.exists("data/geographic_identifier_to_nominal.csv")) {
      geographic_identifier_to_nominal <- readr::read_csv("data/geographic_identifier_to_nominal.csv")
      class(geographic_identifier_to_nominal$code) <- "character"
      
    } else {
      stop("Please provide a geographic identifier to nominal dataset")
    }
    
    Description <- paste0("As dataset is to be raised on the basis of nominal catch that are displayed with a time resolution of year, the data is filtered to keep, for each source_authority,", 
                          " only the years where catch data are provided from January to December. As well we remove stratas data displayed in number for WCPFC in 09.31 and IATTC in 09.32 for dataset in 5 degrees", 
                          "which corresponds to duplicated data to the equivalent stratas in tons.")
    
    
  } else {
    Description <- NULL
  }
  
  
  
  # ### Removing duplicated data 
  # #issue(#48)
  # if(opts$fact == "catch"){
  #   georef_dataset <- georef_dataset %>%
  #     dplyr::group_by(across(setdiff(colnames(.), c("measurement_value", "measurement_unit")))) %>%
  #     dplyr::mutate(nb_units = n_distinct(measurement_unit)) %>%
  #     dplyr::filter(!(measurement_unit == "no" & nb_units >= 2)) %>%
  #     dplyr::select(-nb_units) %>%
  #     dplyr::ungroup() %>%
  #     dplyr::mutate(fishing_mode = ifelse(fishing_mode %in% c("OTH", "DEL"), "UNK", fishing_mode))
  #   
  #   function_recap_each_step(
  #     paste0("Removing_duplicated_units"),
  #     georef_dataset,
  #     "As some data is in fact duplicated for catch, we check all the duplicated data and remove the data in number when same dimensions",
  #     ""
  #   )
  # }
  
  # LEVEL 1 IRD ---------------------------------------------------
  if(DATASET_LEVEL >= 1){
    
    config$logger.info(
      "Extract and load FIRMS Level 0 nominal catch data input (required if raising process is asked) "
    )
    if(file.exists(file.path("data", opts$keynominal)) && !opts$forceuseofdoi){
      nominal_catch <-
        readr::read_csv((file.path("data", opts$keynominal)),
                        guess_max = 0)
      class(nominal_catch$measurement_value) <- "numeric"
      #@juldebar if not provided by Google drive line below should be used if nominal catch has to be extracted from the database
      # } else if(!is.null(opts$doinominal)){
      #   zen4R::download_zenodo(doi = opts$doinominal, files = opts$keynominal, path = "data")
      #   nominal_catch <-
      #     readr::read_csv(here::here(file.path("data", opts$keynominal)),
      #                     guess_max = 0)
      #   class(nominal_catch$measurement_value) <- "numeric"
      
    } else {
      stop("Please provide a nominal catch dataset")
      # nominal_catch <- retrieve_nominal_catch(entity, config, opts)
    }
    
    class(nominal_catch$measurement_value) <- "numeric"
    mapping_keep_src_code <- FALSE
    
    class(nominal_catch$measurement_unit) <- "character"
    
    if (any(nominal_catch$measurement_unit == "t"))
      nominal_catch[nominal_catch$measurement_unit == "t",]$measurement_unit <- "t"
    if (any(nominal_catch$measurement_unit == "TRUE"))
      nominal_catch[nominal_catch$measurement_unit == "TRUE",]$measurement_unit <- "t"
    if (any(nominal_catch$measurement_unit == "no"))
      nominal_catch[nominal_catch$measurement_unit == "no",]$measurement_unit <- "no"
    class(nominal_catch$measurement_value) <- "numeric"
    
    # Based on this analysis, the following cutoff years were determined for data completeness:
    #   
    # IOTC: Keep data starting from 1953.
    # ICCAT: Keep data starting from 1957.
    # WCPFC: Keep data starting from 1952.
    # IATTC: Keep data starting from 1957.
    # CCSBT: Keep all data, although years 2017 and 2020 have missing months, likely due to no fishing activity rather than missing data.
    
    # Étape 1 : extraire les années complètes dans chaque dataset
    get_complete_years <- function(data, date_col_start = "time_start", date_col_end = "time_end") {
      data %>%
        dplyr::select(.data[[date_col_start]],.data[[date_col_end]], source_authority) %>% 
        dplyr::distinct() %>% 
        dplyr::mutate(year = year(.data[[date_col_start]])) %>%
        dplyr::group_by(source_authority, year) %>%
        dplyr::summarise(
          min_date = min(as.Date(.data[[date_col_start]]), na.rm = TRUE),
          max_date = max(as.Date(.data[[date_col_end]]), na.rm = TRUE),
          .groups = "drop"
        ) %>%
        dplyr::filter(
          format(min_date, "%m-%d") == "01-01",
          format(max_date, "%m-%d") == "12-31"
        )
    }
    
    # Appliquer aux deux datasets
    complete_years_georef <- get_complete_years(georef_dataset)
    complete_years_nominal <- get_complete_years(nominal_catch)
    
    # Intersection des années complètes
    common_complete_years <- inner_join(
      complete_years_georef,
      complete_years_nominal,
      by = c("source_authority", "year")
    )
    
    # Pour chaque source_authority, garder la plus ancienne année commune complète
    threshold_years <- common_complete_years %>%
      dplyr::group_by(source_authority) %>%
      dplyr::summarise(min_complete_year = min(year), .groups = "drop")
    
    # Filtrer les deux jeux de données à partir de cette année
    georef_dataset <- georef_dataset %>%
      dplyr::mutate(year = year(time_start)) %>%
      dplyr::left_join(threshold_years, by = "source_authority") %>%
      dplyr::filter(year >= min_complete_year) %>%
      dplyr::select(-year, -min_complete_year) %>% # et ajouter la colonne geographic_identifier_to_nominal
      dplyr::left_join(geographic_identifier_to_nominal, by = c("geographic_identifier" = "code", "source_authority"))
    
    nominal_catch <- nominal_catch %>%
      dplyr::mutate(year = year(time_start)) %>%
      dplyr::left_join(threshold_years, by = "source_authority") %>%
      dplyr::filter(year >= min_complete_year) %>%
      dplyr::select(-min_complete_year)
    
    function_recap_each_step(
      paste0("Removing_data_with_no_nominal"),
      georef_dataset,
      "Since the nominal catch dataset does not cover every year, and the georeferenced data for the first years are not complete, raising would only apply to certain years and/or raising would be not accurate for first years. To avoid mixing raised and unraised data, we prefer to remove records for years that have no equivalent in the nominal dataset.",
      ""
    )
    
    config$logger.info("Level 1 start")
    # DATASET LEVEL 2 ---------------------------------------------------
    if(DATASET_LEVEL >= 2){ #with this condition code will be run to deal with dataset level 2
      
      nominal_catch <- nominal_catch %>% dplyr::rename(geographic_identifier_nom = geographic_identifier)
      
      stepLogger(level = 2, step = stepnumber, "Extract and load IRD Level 1 gridded catch data input")
      stepnumber <<- stepnumber + 1
      
      
      
      if(is.null(opts$upgrading_nominal_on_georef)){
        opts$upgrading_nominal_on_georef <- FALSE
      }
      
      if (!is.null(opts$raising_georef_to_nominal))
        if (opts$raising_georef_to_nominal) {
          
          stepLogger(level = 2, step = stepnumber, "Raise IRD gridded Level 1 (1 or 5 deg) input with FIRMS Level O total (nominal) catch dataset")
          stepnumber <<- stepnumber + 1
          
          config$logger.info("Start raising process")
          
          if (opts$fact == "catch") {
            config$logger.info("Fact=catch !")
            dataset_to_compute_rf = georef_dataset
            # year is used as a dimension to match the conversion factors dimension 
            if (is.null(opts$x_raising_dimensions)) {
              x_raising_dimensions = c("gear_type", "species", "year", "source_authority", "fishing_fleet", "geographic_identifier_nom", "fishing_mode")
            }
            
            
          } else if (opts$fact == "effort") {
            ## If we raise the efforts, the RF is calculated using the georeferenced catch data. Hence, we need to retrieve the georeferenced catch data.
            cat(
              "Catch datasets must be retrieved and processed in order to raise efforts. \nRetrieving georeferenced catch datasets from the Tuna atlas database...\n"
            )
            dataset_catch <- NULL
            rfmo_dataset <-
              get_rfmos_datasets_level0("IOTC", entity, config, rawdata)
            dataset_catch <- rbind(dataset_catch, rfmo_dataset)
            rm(rfmo_dataset)
            
            rfmo_dataset <-
              get_rfmos_datasets_level0("WCPFC", entity, config, rawdata)
            dataset_catch <- rbind(dataset_catch, rfmo_dataset)
            rm(rfmo_dataset)
            
            rfmo_dataset <-
              get_rfmos_datasets_level0("CCSBT", entity, config, rawdata)
            dataset_catch <- rbind(dataset_catch, rfmo_dataset)
            rm(rfmo_dataset)
            
            rfmo_dataset <- get_rfmos_datasets_level0(
              "IATTC",entity, config, rawdata)
            dataset_catch <- rbind(dataset_catch, rfmo_dataset)
            rm(rfmo_dataset)
            
            rfmo_dataset <- get_rfmos_datasets_level0(
              "ICCAT",entity, config, rawdata)
            dataset_catch <- rbind(dataset_catch, rfmo_dataset)
            rm(rfmo_dataset)
            
            
            if (mapping_map_code_lists == "TRUE") {
              dataset_catch <-
                map_codelists(
                  "catch",
                  "https://raw.githubusercontent.com/fdiwg/fdi-mappings/main/global/firms/gta/codelist_mapping_rfmos_to_global.csv",
                  dataset_catch,
                  mapping_keep_src_code
                )$dataset_mapped
            }
            
            dataset_catch$time_start <-
              substr(as.character(dataset_catch$time_start), 1, 10)
            dataset_catch$time_end <-
              substr(as.character(dataset_catch$time_end), 1, 10)
            if (opts$unit_conversion_convert == "TRUE") {
              # We use our conversion factors (IRD). This is now an input parameter of the script
              #@juldebar URL for unit_conversion_csv_conversion_factor_url of should not be hard coded, temporary patch
              #
              dataset_catch <- do_unit_conversion(
                entity = entity,
                config = config,
                fact = "catch",
                unit_conversion_csv_conversion_factor_url =
                  opts$unit_conversion_csv_conversion_factor_url,
                unit_conversion_codelist_geoidentifiers_conversion_factors =
                  opts$unit_conversion_codelist_geoidentifiers_conversion_factors,
                mapping_map_code_lists = opts$mapping_map_code_lists,
                dataset_catch, removing_numberfish_final= FALSE
              )
            }
            
            dataset_to_compute_rf = dataset_catch
            #@juldebar insert patch below to fix error in raise_get_rf function
            
            rm(dataset_catch)
          }
          
          class(dataset_to_compute_rf$measurement_value) <- "numeric"
          
          
          config$logger.info("Executing function function_raising_georef_to_nominal")
          
          if (fact=="catch"){
            raising_dimensions=c(x_raising_dimensions,"measurement_unit")
          } else if (fact=="effort"){
            raising_dimensions=x_raising_dimensions
            df_rf$measurement_unit=NULL
          }
          # idee raise only les donnees qui sont que en tonnes
          
          if(opts$upgrading_nominal_on_georef){
            
            if(is.null(opts$strong_weak_upgrade)){
              opts$strong_weak_upgrade <- "weak"
            } 
            
            
            specify_nominal_with_georef <- function(nominal_df,
                                                    georef_df,
                                                    strata_cols   = c("species",
                                                                      "source_authority",
                                                                      "year",
                                                                      "geographic_identifier_nom",
                                                                      "measurement_unit"),
                                                    step_order    = c("fishing_mode",
                                                                      "gear_type",
                                                                      "fishing_fleet")  ,
                                                    unk_map       = list(fishing_fleet = "NEI",
                                                                         gear_type     = "99.9",
                                                                         fishing_mode  = "UNK"),
                                                    mode        = opts$strong_weak_upgrade,
                                                    logger        = message) {
              ## add helper year column if needed
              if (!"year" %in% names(nominal_df))
                nominal_df <- nominal_df %>%
                  mutate(year = lubridate::year(time_start))
              if (!"year" %in% names(georef_df))
                georef_df  <- georef_df  %>%
                  mutate(year = lubridate::year(time_start))
              
              ## loop over the three dimensions
              for (dim_col in step_order) {
                
                unk_code <- unk_map[[dim_col]]
                dim_sym  <- ensym(dim_col)
                
                # -------- proportions from GEO -----------------------------
                geo_props <- georef_df %>%
                  # keep ALL codes, including the unknown one
                  dplyr::group_by(dplyr::across(dplyr::all_of(c(strata_cols, dim_col)))) %>%
                  dplyr::summarise(val = sum(measurement_value, na.rm = TRUE), .groups = "drop") %>%
                  dplyr::group_by(dplyr::across(dplyr::all_of(strata_cols))) %>%
                  dplyr::mutate(prop = val / sum(val, na.rm = TRUE), total = sum(val)) %>%
                  dplyr::ungroup()
                
                
                # -------- unknown rows from NOMINAL ------------------------
                unknown_rows <- nominal_df %>%
                  dplyr::filter(.data[[dim_col]] == unk_code)
                
                if (nrow(unknown_rows) == 0) next  # nothing to specify
                
                # join with props – keeps only strata where geo has detail
                to_specify <- unknown_rows %>%
                  dplyr::inner_join(geo_props, by = strata_cols, suffix = c("", "_geo"))
                
                if (nrow(to_specify) == 0) next  # no geo detail for these strata
                
                sum(to_specify$measurement_value)
                if (mode == "weak"){
                  
                  prop_diff_de_un_diff <- to_specify %>% dplyr::filter(measurement_value > total)
                  prop_diff_de_un_prop <- to_specify %>% dplyr::filter(measurement_value <= total)
                  
                  prop_diff_de_un_prop_specified <- prop_diff_de_un_prop %>%
                    dplyr::mutate(measurement_value = measurement_value * prop,
                                  !!dim_sym        := !!sym(paste0(dim_col, "_geo"))) %>%
                    dplyr::select(-val, -prop, -ends_with("_geo"))
                  
                  # 1) turn it into val copy + compute leftover
                  prop_diff_part <- prop_diff_de_un_diff %>%
                    dplyr::ungroup() %>% 
                    dplyr::mutate(
                      total_nom         = measurement_value,               # original UNK
                      measurement_value = val,                             # copy geo
                      !!dim_sym         := .data[[paste0(dim_col, "_geo")]],  
                      unknown_to_add    = total_nom - total                # leftover
                    ) %>%
                    dplyr::select(-val, -prop, -ends_with("_geo"))
                  
                  group_cols <- setdiff(
                    names(prop_diff_part),
                    c("measurement_value", dim_col)      # dims_cols = vecteur de noms à exclure
                  )
                  
                  data_init <- prop_diff_part
                  
                  prop_diff_specified <- prop_diff_part%>% 
                    
                    # 2. Regrouper
                    dplyr::group_by(across(all_of(group_cols))) %>% 
                    
                    # 3. Ajouter la ligne manquante par groupe
                    dplyr::slice(1) %>%                    
                    dplyr::mutate(measurement_value = unknown_to_add) %>% 
                    dplyr::mutate(!!dim_col := unk_code) %>% 
                    
                    # 4. Sortir du group_by
                    dplyr::ungroup() 
                  prop_diff_specified_final <- rbind(prop_diff_specified, data_init)
                  
                  specified <- rbind(prop_diff_de_un_prop_specified%>%
                                       dplyr::select(all_of(names(nominal_df))),
                                     prop_diff_specified_final%>%
                                       dplyr::select(all_of(names(nominal_df))))
                  
                } else {
                  # compute new rows
                  specified <- to_specify %>%
                    dplyr::mutate(measurement_value = measurement_value * prop,
                                  !!dim_sym        := !!sym(paste0(dim_col, "_geo"))) %>%
                    dplyr::select(-val, -prop, -ends_with("_geo"))
                  
                }
                # remove the old unknown rows and add specified ones
                replaced_keys <- to_specify %>%
                  dplyr::select(all_of(names(nominal_df))) %>%
                  dplyr::distinct() 
                
                
                nominal_df <- nominal_df %>%
                  dplyr::anti_join(replaced_keys, by = setdiff(names(nominal_df),
                                                               "measurement_value")) %>%
                  dplyr::bind_rows(specified)
                
                # ------- log the volumes re-allocated ----------------------
                raised_t  <- specified %>%
                  dplyr::ungroup() %>% 
                  dplyr::filter(measurement_unit %in% c("t", "MT", "MTNO")) %>%
                  dplyr::summarise(sum_val = sum(measurement_value, na.rm = TRUE)) %>%
                  dplyr::pull(sum_val)
                raised_no <- specified %>%
                  dplyr::ungroup() %>%
                  dplyr::filter(measurement_unit %in% c("no", "NO", "NOMT")) %>%
                  dplyr::summarise(sum_val = sum(measurement_value, na.rm = TRUE)) %>%
                  dplyr::pull(sum_val)
                
                logger(sprintf("Specified %s — %.3f t, %.3f no",
                               dim_col, raised_t, raised_no))
              }
              
              nominal_df <- nominal_df %>% dplyr::ungroup() %>% dplyr::group_by(across(-measurement_value)) %>% 
                dplyr::summarise(measurement_value = sum(measurement_value),
                                 .groups = "drop")
              
              nominal_df
            } # juste definition de la fonction, qui est longe, on l'utilise après dans le iterative
            
            strata_cols_updated <- c("gear_type", "species", "year", "source_authority",
                                     "fishing_fleet", "geographic_identifier_nom", "fishing_mode")
              # on spécifie deux fois comme ça ça converge direct
            step_order_updated <-  c("fishing_mode", "gear_type", "fishing_fleet")
            
            strata_cols_updated <- setdiff(strata_cols_updated, step_order_updated)
              nominal_catch <- specify_nominal_with_georef(nominal_catch,
                                                           georef_dataset,
                                                           strata_cols   = strata_cols_updated ,
                                                           step_order    =  step_order_updated,
                                                           mode = "weak", # could be strong
                                                           logger = function(msg) {
                                                             cat("[Specify] ", msg, "\n")
                                                           })
              
              nominal_catch <- specify_nominal_with_georef(nominal_catch,
                                                           georef_dataset,
                                                           strata_cols   = strata_cols_updated ,
                                                           step_order    =  step_order_updated,
                                                           mode = "weak", # could be strong
                                                           logger = function(msg) {
                                                             cat("[Specify] ", msg, "\n")
                                                           })
            
          }
          
          if(is.null(opts$decrease_every_time)){
            opts$decrease_every_time <- FALSE
          }
          iterative_raising <- function(fact               = "catch",
                                        georef_dataset,    # geo-referenced data.frame
                                        nominal_catch,     # nominal data.frame
                                        entity,            
                                        passes            = 8L,   # 1–8
                                        decrease_on_last  = TRUE,
                                        recap_each_step   = TRUE,
                                        stepnumber        = 1, 
                                        decrease_every_time = opts$decrease_every_time) {
            
            ## ----- 0. Dimension sets in fixed order -----
            full_dims <- c("gear_type", "species", "year", "source_authority",
                           "fishing_fleet", "geographic_identifier_nom", "fishing_mode")
            
            dim_sets <- list(
              full_dims,
              setdiff(full_dims, "fishing_mode"),
              setdiff(full_dims, c("gear_type")),
              setdiff(full_dims, c("fishing_fleet")),
              setdiff(full_dims, c("fishing_mode", "gear_type")),
              setdiff(full_dims, c("fishing_mode", "fishing_fleet")),
              setdiff(full_dims, c("gear_type", "fishing_fleet")),
              setdiff(full_dims, c("fishing_mode", "gear_type", "fishing_fleet"))
            )
            
            passes <- max(0L, min(as.integer(passes), length(dim_sets)))
            if (passes == 0L) return(georef_dataset)     # nothing to do
            
            ## ----- 1. Loop over passes -----
            for (i in seq_len(passes)) {
              
              x_dims        <- dim_sets[[i]]
              decrease_flag <- isTRUE(decrease_on_last) && i == passes | isTRUE(decrease_every_time)
              
              
              strata_cols_updated <- x_dims
              
              x_dims        <- dim_sets[[i]]
              strata_cols_updated <- x_dims
              convert_number_to_nominal_output <- convert_number_to_nominal(georef_dataset, nominal_catch, strata = strata_cols_updated, 
                                                                            raise_only_unmatched = FALSE) # raise only unmatched c'est savoir si on augmente juste les donnees en nombre qui ont pas d'équivalent en tonnes ou si on augemente tout
              # on pourrait ajouter le fait que on regarde pour chaque strate celle qui a la plus grande empreinte spatiale et on choisit ça pour que ça soit plus fin
              # mais la question est donc qu'est ce que sont les données en nombre restantes? Celles sans équivalent en nominal ? 
              georef_dataset <- convert_number_to_nominal_output$georef_dataset
              
              function_recap_each_step(
                paste0("Conv_NO_nominal", i),
                georef_dataset,
                paste0("The data that remains in Number of Fish, for which the entirety of the strata with the following dimensions:", toString(strata_cols_updated), 
                       "containing catch information in tons, is converted and raised using the nominal dataset ", opts$doinominal, ". The key identifier for this operation is: ", opts$keynominal,
                       ". This process relies on the fact that for a strata reported in both number and tons, the spatial footprint of the data in number is more often containing the spatial footprint of the data in tons.", 
                       "Then, as there is need to choose one of the measurment_unit for the raising and with limited conversion factors, we choose to keep raise the data in 'Number of fish' on the basis of the equivalent nominal strata ", 
                       "downgraded by the corresponding georeferenced catch in tons."),
                ""
              )
              
              ## 1.1 – log start of pass
              stepLogger(level = 2,
                         step  = stepnumber,
                         msg   = paste0("Raising pass ", i, " on dimensions: ",
                                        paste(x_dims, collapse = ", "),
                                        if (decrease_flag) " [decrease enabled]" else ""))
              stepnumber <- stepnumber + 1
              
              tons_units    <- c("t",  "MT",   "MTNO")
              number_units  <- c("no", "NO",   "NOMT")
              
              # ----- 1·2  totals BEFORE the pass  ------------------------------
              tot_before <- georef_dataset %>%
                dplyr::mutate(unit_class = dplyr::case_when(
                  measurement_unit %in% tons_units   ~ "tons",
                  measurement_unit %in% number_units ~ "numbers",
                  TRUE                               ~ "other")
                ) %>%
                dplyr::group_by(unit_class) %>%
                dplyr::summarise(total = sum(measurement_value, na.rm = TRUE),
                                 .groups = "drop")
              
              ## 1.3 – perform the raising
              raise_out <- function_raise_data(
                fact                              = fact,
                source_authority_filter           = c("IOTC", "ICCAT", "IATTC", "CCSBT", "WCPFC"),
                dataset_to_raise                  = georef_dataset,
                dataset_to_compute_rf             = georef_dataset,
                nominal_dataset_df                = nominal_catch,
                x_raising_dimensions              = x_dims,
                decrease_when_rf_inferior_to_one  = decrease_flag,
                raise_only_on_unk = TRUE, 
                do_not_raise_perfectly_compatible_data = ifelse(identical(full_dims,x_dims), FALSE, TRUE), 
                do_not_raise_any_unk = opts$do_not_raise_any_unk
              )
              
              georef_dataset <- raise_out$data_raised %>% dplyr::distinct()
              
              # ----- 1·4  totals AFTER the pass  -------------------------------
              tot_after <- georef_dataset %>%
                dplyr::mutate(unit_class = dplyr::case_when(
                  measurement_unit %in% tons_units   ~ "tons",
                  measurement_unit %in% number_units ~ "numbers",
                  TRUE                               ~ "other")
                ) %>%
                dplyr::group_by(unit_class) %>%
                dplyr::summarise(total = sum(measurement_value, na.rm = TRUE),
                                 .groups = "drop")
              
              # ----- 1·4bis  compute deltas ------------------------------------
              get_delta <- function(cls, type = c("raised", "decreased")) {
                before <- tot_before$total[match(cls, tot_before$unit_class)]
                after  <- tot_after$total[match(cls,  tot_after$unit_class)]
                before[is.na(before)] <- 0; after[is.na(after)] <- 0
                if (type[1] == "raised")    max(after - before, 0)
                else                        max(before - after, 0)
              }
              
              raised_tons    <- get_delta("tons",    "raised")
              decr_tons      <- get_delta("tons",    "decreased")
              raised_num     <- get_delta("numbers", "raised")
              decr_num       <- get_delta("numbers", "decreased")
              
              # ----- 1·5  detailed stepLogger ----------------------------------
              stepLogger(level = 2,
                         step  = stepnumber - 1,   # same step as start log
                         msg   = sprintf(
                           paste0("Pass %d summary – tons: +%.3f / -%.3f, ",
                                  "numbers: +%.3f / -%.3f"),
                           i, raised_tons, decr_tons, raised_num, decr_num))
              
              # --- 1.6  Recap / audit trail  ----------------------------------
              if (isTRUE(recap_each_step)) {
                recap_msg <- paste0(
                  "Pass ", i, "/", passes, " on ",
                  paste(x_dims, collapse = ", "), ". ",
                  sprintf("Tons raised %.3f, decreased %.3f. ",
                          raised_tons, decr_tons),
                  sprintf("Numbers raised %.3f, decreased %.3f. ",
                          raised_num,  decr_num),
                  if (ifelse(identical(full_dims, x_dims), FALSE, TRUE)) {
                    "As one or multiple dimensions are removed in this pass for the raising, the raising is only done on the corresponding unspecified data of the stratas. The strata having a 'perfect' match in the nominal is not included in this raising."
                  } else {
                    ""
                  },
                  if (decrease_flag) {
                    "Values above the nominal total were scaled down on this pass."
                  } else {
                    "No down-scaling was applied on this pass."
                  }
                )
                
                function_recap_each_step(
                  step_name   = paste0("RF_pass_", i),
                  rds_data    = georef_dataset,
                  explanation = recap_msg,
                  functions   = "iterative_raising",
                  option_list = list(dimensions        = x_dims,
                                     decreased_on_pass = decrease_flag,
                                     raised_tons       = raised_tons,
                                     decreased_tons    = decr_tons,
                                     raised_numbers    = raised_num,
                                     decreased_numbers = decr_num),
                  entity      = entity
                )
              }
              
              
            }
            
            georef_dataset
          }
          
          
          if(is.null(opts$passes)){
            opts$passes <- 4
          }
          
          
          georef_dataset <- iterative_raising(
            fact              = "catch",
            georef_dataset    = georef_dataset,
            nominal_catch     = nominal_catch,
            entity            = entity,
            passes            = opts$passes,
            decrease_on_last  = opts$decrease_when_rf_inferior_to_one
          )
          
        }
    }
    
    if (opts$disaggregate_on_5deg_data_with_resolution_superior_to_5deg %in% c("disaggregate", "remove")) {
      config$logger.info(
        "BEGIN function_disaggregate_on_resdeg_data_with_resolution_superior_to_resdeg() function"
      )
      
      georef_dataset <-
        function_disaggregate_on_resdeg_data_with_resolution_superior_to_resdeg(
          entity,
          config,
          opts,
          georef_dataset =
            georef_dataset,
          resolution =
            5,
          action_to_do =
            opts$disaggregate_on_5deg_data_with_resolution_superior_to_5deg
        )
      config$logger.info(
        "END function_disaggregate_on_resdeg_data_with_resolution_superior_to_resdeg() function"
      )
      
      georef_dataset <- georef_dataset$dataset
      function_recap_each_step(
        "Disaggregate5deg",
        georef_dataset,
        paste0("The data is disaggregated data on resolution higher than 5o in 5o resolution."),
        "function_disaggregate_on_resdeg_data_with_resolution_superior_to_resdeg",
        list(
          options_disaggregate_on_5deg_data_with_resolution_superior_to_5deg
        ), entity
      )
    }
    
    if (opts$disaggregate_on_1deg_data_with_resolution_superior_to_1deg %in% c("disaggregate", "remove")) {
      config$logger.info(
        "STEP 5/5: BEGIN function_disaggregate_on_resdeg_data_with_resolution_superior_to_resdeg() function"
      )
      georef_dataset <-
        function_disaggregate_on_resdeg_data_with_resolution_superior_to_resdeg(
          entity,
          config,
          opts,
          georef_dataset =
            georef_dataset,
          resolution =
            1,
          action_to_do =
            opts$disaggregate_on_1deg_data_with_resolution_superior_to_1deg
        )
      config$logger.info(
        "STEP 5/5: END function_disaggregate_on_resdeg_data_with_resolution_superior_to_resdeg() function"
      )
      
      georef_dataset <- georef_dataset$dataset
      
      function_recap_each_step(
        "Disaggregate1deg",
        georef_dataset,
        "This step disaggregate data on resolution higher than 1o in 1o resolution",
        "function_disaggregate_on_resdeg_data_with_resolution_superior_to_resdeg",
        list(
          options_disaggregate_on_1deg_data_with_resolution_superior_to_1deg
        ), entity
      )
      gc()
      
    }
    gc()
    
  }
  
  # SPATIAL AGGREGATION ---------------------------------------------------
  if (!is.null(opts$aggregate_on_5deg_data_with_resolution_inferior_to_5deg)) if (opts$aggregate_on_5deg_data_with_resolution_inferior_to_5deg) {
    stepLogger(level = 0, step = stepnumber, msg = "Spatial Aggregation of data (5deg resolution datasets only: Aggregate data on 5o resolution quadrants)")
    stepnumber = stepnumber+1
    config$logger.info("Aggregating data that are defined on quadrants or areas inferior to 5o quadrant resolution to corresponding 5o quadrant...")
    
    one_degree <- georef_dataset %>% dplyr::filter(substr(geographic_identifier, 1, 1) == "5")
    five_degree <- georef_dataset %>% dplyr::filter(substr(geographic_identifier, 1, 1) == "6")
    one_degree_aggregated <- one_degree %>% rowwise() %>% 
      dplyr::mutate(geographic_identifier = transform_cwp_code_from_1deg_to_5deg(geographic_identifier))
    
    # df_input_not_aggregated <- georef_dataset %>% dplyr::filter(is.null(geographic_identifier))
    # fwrite(df_input_not_aggregated, "data/df_input_not_aggregated.csv")
    
    georef_dataset <- as.data.frame(base::rbind(one_degree_aggregated, five_degree))
    
    config$logger.info("Aggregating data that are defined on quadrants or areas inferior to 5o quadrant resolution to corresponding 5o quadrant OK")
    
    if(recap_each_step){
      names_list_aggregation <-
        c("df_input_not_aggregated", "stats_not_aggregated") #file we want to save
      
      # try(lapply(names_list_aggregation, function_write_RDS))
      
      function_recap_each_step(
        "Aggregation",
        georef_dataset,
        "The data with resolution lower to 5o is aggregated on resolution of 5o.",
        "spatial_curation_upgrade_resolution",
        list(
          options_aggregate_on_5deg_data_with_resolution_inferior_to_5deg
        ), entity
      )
    }  
  }
  
  # FILTERING SPATIAL RESOLUTION ---------------------------------------------------
  config$logger.info("Grid spatial resolution filter")
  if (!is.null(opts$resolution_filter)) {
    
    filtering_resolution_filter <- function(datatable, first_digit) {
      filtered_data <- datatable[substr(datatable$geographic_identifier, 1, 1) == first_digit, ]
      return(filtered_data)
    }
    
    georef_dataset <- filtering_resolution_filter(georef_dataset, opts$resolution_filter)
    
    if(recap_each_step){
      function_recap_each_step(
        "filtering_on_spatial_resolution",
        georef_dataset,
        "This step is to filter on the wanted resolution.",
        "",
        list(options_resolution_filter), entity
      )
    }
  }
  
  config$logger.info("Apply filters if filter needed (Filter data by groups of everything) ")
  # FILTERING OTHER RESOLUTION ---------------------------------------------------
  
  parameter_filtering = if (!is.null(opts$filtering)) opts$filtering else list(species = NULL, fishing_fleet = NULL) # if nothing provided filtering is null so we provided first dimension with no filtering
  if(!is.null(opts$filtering)){
    if (is.character(parameter_filtering)) {
      parameter_filtering <-
        eval(parse(text = toString(parameter_filtering)))
    } #if opts$filtering is provided, we need to read the filtering parameters provided in the entities table as following ex parameter_option_filtering:c(species = "YFT", gear = c("09.32", 09.39"))
    
    matchingList <-
      parameter_filtering %>% purrr::keep(~ !is.null(.)) #removing null params in case no option is provided
    
    georef_dataset <- dimension_filtering_function(georef_dataset, filtering_params = matchingList)
    
    
    if (!is.null(opts$gear_filter)){
      gear_filter<-unlist(strsplit(opts$gear_filter, split=","))
      config$logger.info(sprintf("Filtering by gear(s) [%s]", paste(gear_filter, collapse=",")))	
      georef_dataset<-georef_dataset %>% dplyr::filter(gear_type %in% gear_filter)
      config$logger.info("Filtering gears OK")
    }
    
    if(recap_each_step){
      function_recap_each_step(
        "Filtering on every other dimension",
        georef_dataset,
        paste0("Filtering on the following paremeters :", parameter_filtering),
        "dimension_filtering_function",
        list(opts$filtering), entity
      )
    }
  }
  
  # PROCESS AND AGGREGATE DATA ---------------------------------------------------
  process_and_aggregate_dataset(georef_dataset, entity, config, opts, 
                                columns_to_keep = c("source_authority", "species", "gear_type", "fishing_fleet", "fishing_mode",
                                                    "time_start", "time_end", "year", "month", "quarter", "geographic_identifier", "measurement_unit", "measurement_value", "measurement_type", "measurement_processing_level") ) 
  config$logger.info("End: Your tuna atlas dataset has been created!")
  
  # Clean up
  rm(georef_dataset)
  gc()
  
  
  
}
