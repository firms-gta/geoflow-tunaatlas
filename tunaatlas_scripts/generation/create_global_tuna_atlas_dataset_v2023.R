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
create_global_tuna_atlas_dataset_v2023 <- function(action, entity, config) {
    
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
    url_scripts_create_own_tuna_atlas <- "https://raw.githubusercontent.com/firms-gta/geoflow-tunaatlas/master/tunaatlas_scripts/generation"
    
    # Retrieve data from zenodo 
    source(file.path(url_scripts_create_own_tuna_atlas,"download_zenodo_csv.R"))
    
    #for level 0 - FIRMS
    # source(file.path(url_scripts_create_own_tuna_atlas, "get_rfmos_datasets_level0.R")) #modified for geoflow
    source(here::here("./tunaatlas_scripts/generation/get_rfmos_datasets_level0.R")) #modified for geoflow
    source(file.path(url_scripts_create_own_tuna_atlas, "retrieve_nominal_catch.R")) #modified for geoflow
    source("https://raw.githubusercontent.com/firms-gta/geoflow-tunaatlas/master/tunaatlas_scripts/pre-harmonization/map_codelists.R") #modified for geoflow
    source(file.path(url_scripts_create_own_tuna_atlas, "function_overlapped.R")) # adding this function as overlapping is now a recurent procedures for several overlapping 
    
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
    source(file.path(url_scripts_create_own_tuna_atlas, "convert_number_to_nominal.R")) #modified for geoflow
    source(here::here("tunaatlas_scripts/generation/convert_number_to_nominal.R")) #modified for geoflow
    source(file.path(url_scripts_create_own_tuna_atlas, "function_raise_data.R")) #modified for geoflow
    
    
    # For filtering/aggregating
    source("https://raw.githubusercontent.com/firms-gta/geoflow-tunaatlas/master/sardara_functions/transform_cwp_code_from_1deg_to_5deg.R")
    
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
        if(opts$fact == "catch"){
        georef_dataset <-
          double_unit_data_handling( #to add handling of strata having multiples unit (tonnes and number which arrivent)
            con = con,
            entity = entity,
            config = config,
            fact = fact,
            unit_conversion_csv_conversion_factor_url =
              NULL,
            unit_conversion_codelist_geoidentifiers_conversion_factors =
              opts$unit_conversion_codelist_geoidentifiers_conversion_factors,
            mapping_map_code_lists = opts$mapping_map_code_lists,
            georef_dataset = georef_dataset
          )
        function_recap_each_step(
          "Removing NOMT and converting MTNO in MT",
          georef_dataset,
          "The data initially in MTNO is converted in MT and the data in NTMO is removed",
          "double_unit_data_handling"
        )
        
      }
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
        iattc_wcpfc = list(main = c(WCPFC = "IATTC"), default_strata = c("geographic_identifier", "species", "year")),
        # wcpfc_ccsbt = list(main = c(WCPFC = "CCSBT"), default_strata = c("species")), # not usefull anymore as handled in pre harmo
        # iccat_ccsbt = list(main = c(ICCAT = "CCSBT"), default_strata = c("species")),# not usefull anymore as handled in pre harmo
        # iotc_ccsbt = list(main = c(IOTC = "CCSBT"), default_strata = c("species")),# not usefull anymore as handled in pre harmo
        iotc_wcpfc = list(main = c(WCPFC = "IOTC"), default_strata = c("geographic_identifier", "species", "year"))
      )
      
      if(opts$fact == "effort"){
        zones_config <- list(
          iattc_wcpfc = list(main = c(WCPFC = "IATTC"), default_strata = c("geographic_identifier", "year")),
          # wcpfc_ccsbt = list(main = c(WCPFC = "CCSBT"), default_strata = c("species")), # not usefull anymore as handled in pre harmo
          # iccat_ccsbt = list(main = c(ICCAT = "CCSBT"), default_strata = c("species")),# not usefull anymore as handled in pre harmo
          # iotc_ccsbt = list(main = c(IOTC = "CCSBT"), default_strata = c("species")),# not usefull anymore as handled in pre harmo
          iotc_wcpfc = list(main = c(WCPFC = "IOTC"), default_strata = c("geographic_identifier", "year"))
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
          
          georef_dataset <- georef_dataset %>% 
            # dplyr::select(-dplyr::any_of(c("measurement", "measurement_status", "measurement_type")))%>% 
            # dplyr::mutate(fishing_fleet = ifelse(source_authority == "CCSBT", "NEI",fishing_fleet))%>%
            dplyr::mutate(year =lubridate::year(time_start)) %>%
            dplyr::filter((source_authority == "WCPFC" & year >= 1952) |
                            (source_authority == "IOTC" & year >= 1953) |
                            (source_authority == "ICCAT" & year >= 1957) |
                            (source_authority == "IATTC" & year >= 1957) | source_authority == "CCSBT") %>%
            dplyr::select(-year) %>% dplyr::left_join(geographic_identifier_to_nominal, by = c("geographic_identifier" = "code", "source_authority"))
          
          # # Fonction de filtrage pour une source d'autorite
          # filter_problematic_units <- function(dataset, authority, species_list) {
          #   dataset %>%
          #     dplyr::filter(
          #       source_authority == authority, 
          #       gear_type %in%c("09.32", "09.31"),
          #       substr(geographic_identifier, 1, 1) == "6", 
          #       species %in% species_list
          #     ) %>%
          #     dplyr::group_by(across(setdiff(colnames(dataset), c("measurement_value", "measurement_unit")))) %>%
          #     dplyr::mutate(n = n_distinct(measurement_unit)) %>%
          #     dplyr::filter((n == 2 & measurement_unit == "no")) %>%
          #     dplyr::select(-n)
          # }
          
          # # Listes d'especes
          # species_list_wcpfc <- c("ALB", "BET", "MAK", "MLS", "SWO", "YFT", "BLM", "BUM", "POR", "OCS", "FAL", "SPN", "THR")
          # species_list_iattc <- c("BSH", "FAL", "OCS", "MAK", "THR", "RSK", "SMA", "SKH", "SPN", "BET", "MLS", "SWO", "BUM", 
          #                         "BLM", "YFT", "SFA", "ALB", "BIL", "SSP", "SKJ", "TUN", "PBF")
          
          # # Appliquer la fonction de filtrage pour chaque tRFMOs
          # wcpfc_notok <- filter_problematic_units(georef_dataset, "WCPFC", species_list_wcpfc)
          # iattc_notok <- filter_problematic_units(georef_dataset, "IATTC", species_list_iattc)
          
          # # Jeu de donnees final en supprimant les enregistrements problematiques
          # georef_dataset_filtered <- georef_dataset %>%
          #   dplyr::anti_join(wcpfc_notok, by = setdiff(colnames(georef_dataset), c("measurement_value"))) %>%
          #   dplyr::anti_join(iattc_notok, by = setdiff(colnames(georef_dataset), c("measurement_value")))
          
          Description <- paste0("As dataset is to be raised on the basis of nominal catch that are displayed with a time resolution of year, the data is filtered to keep, for each source_authority,", 
          " only the years where catch data are provided from January to December. As well we remove stratas data displayed in number for WCPFC in 09.31 and IATTC in 09.32 for dataset in 5 degrees", 
          "which corresponds to duplicated data to the equivalent stratas in tons.")
          
          
        } else {
          Description <- NULL
        }
        

      
      ### Removing duplicated data 
      #issue(#48)
      if(opts$fact == "catch"){
      georef_dataset <- georef_dataset %>%
        dplyr::group_by(across(setdiff(colnames(georef_dataset), c("measurement_value", "measurement_unit")))) %>%
        dplyr::filter(!(measurement_unit == "no" & n_distinct(measurement_unit) == 2)) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(fishing_mode = ifelse(fishing_mode %in% c("OTH", "DEL"), "UNK", fishing_mode))
      
      function_recap_each_step(
        paste0("Removing_duplicated_units"),
        georef_dataset,
        "As some data is in fact duplicated for catch, we check all the duplicated data and remove the data in number when same dimensions",
        ""
      )
      }
    
    # LEVEL 1 IRD ---------------------------------------------------
    if(DATASET_LEVEL >= 1){
      
      config$logger.info(
        "Extract and load FIRMS Level 0 nominal catch data input (required if raising process is asked) "
      )
      if(file.exists(file.path("data", opts$keynominal)) && !opts$forceuseofdoi){
        nominal_catch <-
          readr::read_csv(here::here(file.path("data", opts$keynominal)),
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
      
      nominal_catch <- nominal_catch %>% 
        dplyr::mutate(fishing_fleet = ifelse(fishing_fleet == "UNK", "NEI", fishing_fleet))%>%
        dplyr::mutate(year =lubridate::year(time_start)) %>%
        dplyr::filter((source_authority == "WCPFC" & year >= 1952) |
                        (source_authority == "IOTC" & year >= 1953) |
                        (source_authority == "ICCAT" & year >= 1957) |
                        (source_authority == "IATTC" & year >= 1957) | source_authority == "CCSBT") %>%
        dplyr::select(-year) %>% dplyr::rename(geographic_identifier_nom = geographic_identifier)
      
      nominal_catch <- nominal_catch %>% 
        dplyr::mutate(gear_type = ifelse(source_authority == "WCPFC" & gear_type == "09.31", "09.32", gear_type))
      
      nominal_catch <- nominal_catch %>% 
        # dplyr::select(-dplyr::any_of(c("measurement", "measurement_status", "measurement_type")))%>%
        dplyr::ungroup() %>%
        dplyr::group_by(across(-measurement_value)) %>% 
        dplyr::summarise(measurement_value = sum(measurement_value, na.rm = TRUE), .groups = 'drop')
      
      #   georef_sup_nom_init <- compare_nominal_georef_corrected(nominal_catch, georef_dataset,  
      # list(c("species", "year", "source_authority", "gear_type", "fishing_fleet")))$`species, year, source_authority, gear_type, fishing_fleet`$georef_sup_nominal %>%
      #   # dplyr::select(c("species", "year", "source_authority", "gear_type", "fishing_fleet")) %>%
      #     dplyr::distinct() %>% dplyr::group_by(source_authority, year, species) %>% dplyr::mutate(sum = sum(Difference))
      #   # 
      #   # georef_sup_nom_init_species_year_gear <- compare_nominal_georef_corrected(nominal_catch, georef_dataset, list(c("species", "year", "source_authority", "gear_type")))$`species, year, source_authority, gear_type`$georef_sup_nominal %>% 
      #   #   dplyr::select(c("species", "year", "source_authority", "gear_type"))%>%
      #   #   dplyr::distinct()
      #   # 
      #   georef_sup_nom_init_species_year <- compare_nominal_georef_corrected(nominal_catch, georef_dataset, list(c("species", "year", "source_authority")))$`species, year, source_authority`$georef_sup_nominal %>%
      #     # dplyr::select(c("species", "year", "source_authority"))%>%
      #     dplyr::distinct()
      # 
      # global_nominal_catch_firms_level0_nei <- nominal_catch %>% dplyr::filter(species %in% c("TUN", "TUS", "BIL")) %>% dplyr::mutate(year = as.character(year(ymd(time_start)))) %>% 
      #   dplyr::inner_join(georef_sup_nom_init_species_year, by = c("year", "source_authority")) %>% dplyr::group_by(species.x, year) %>% dplyr::mutate(sumx = sum(measurement_value))
      
      #with this condition code will be run to deal with dataset level 1 and above
      config$logger.info("Level 1 start")

      if (!is.null(opts$unit_conversion_convert)) if (opts$unit_conversion_convert) {
        
        IOTC_conv_fact_mapped <- readr::read_csv("data/IOTC_conv_fact_mapped.csv") %>% 
          dplyr::mutate(gear_type = as.character(gear_type), time_start = as.character(time_start)) %>% 
          dplyr::mutate(year = lubridate::year(time_start))
        
        iotc <- georef_dataset %>% dplyr::filter(source_authority == "IOTC")
        
        iotc <- iotc %>% 
          dplyr::group_by(time_start,time_end, species, fishing_fleet, gear_type, source_authority, fishing_mode, geographic_identifier) %>%
            dplyr::mutate(numberunit = n_distinct(measurement_unit)) 
        
        iotc_lvl0_onlynumber <- iotc %>% dplyr::filter(measurement_unit == "no" & numberunit ==1)%>% 
          dplyr::select(colnames(georef_dataset))
        
        iotc_lvl0_onlytons_and_number_tons <- iotc %>% dplyr::filter(!(measurement_unit == "no" & numberunit ==1))%>% 
          dplyr::select(colnames(georef_dataset)) %>% dplyr::filter(measurement_unit != "no")
        
        iotc_only_number_raised <- iotc_lvl0_onlynumber %>% dplyr::inner_join(IOTC_conv_fact_mapped %>% dplyr::rename(conversion_factors = measurement_value)  %>% 
                                                             dplyr::mutate(time_end = as.character(time_end))%>% 
                                                             dplyr::mutate(geographic_identifier = as.character(geographic_identifier)) %>% 
                                                             dplyr::select(-measurement_unit)) %>% 
          dplyr::mutate(measurement_value = measurement_value * conversion_factors) %>% 
          dplyr::mutate(measurement_unit = "t") %>% 
          dplyr::select(colnames(georef_dataset))
        
        iotc_lvl0_onlytons_and_number_tons_without_number <- iotc_lvl0_onlytons_and_number_tons %>%  dplyr::filter(measurement_unit != "no")
        
        iotc_new <- rbind(iotc_only_number_raised, iotc_lvl0_onlytons_and_number_tons_without_number)
        
        georef_dataset_not_iotc <- georef_dataset %>% dplyr::filter(source_authority != "IOTC")
        

        
        georef_dataset <- rbind(iotc_new, georef_dataset_not_iotc)
        
        function_recap_each_step(
          "NewIOTCstep",
          georef_dataset,
          paste0("We convert only data for IOTC that is just in number and not in number and tons for the same strata"),
          ""
        )
        
        georef_dataset <- georef_dataset %>%
          dplyr::ungroup() %>%
          dplyr::group_by(across(-measurement_value)) %>% 
          dplyr::summarise(measurement_value = sum(measurement_value, na.rm = TRUE), .groups = 'drop')
        
        # georef_dataset <- perform_unit_conversion(
        #   conversion_factor_csv = "data/IOTC_conv_fact_mapped.csv",
        #   unit_conversion_codelist_geoidentifiers_conversion_factors = "cwp_grid",
        #   georef_dataset = georef_dataset,
        #   entity = entity,
        #   config = config,
        #   opts = opts,
        #   step_description = "Harmonising units on IOTC data", 
        #   addeddescription = paste0("This conversion factors dataset contains conversion factors for strata with the following details :",
        #   "gear_type, source_authority (IOTC),", 
        #   "species (ALB, BET, SKJ, SWO, YFT), geographic_identifier, time_start-time_end. A strata is converted only if the stratas are fully matching.")
        # )
      }
          ## UPGRADE IRD CONVERSION FACTORS ---------------------------------------------------
          # IRD_data <- readr::read_csv("data/fact_conv_IRD.csv") %>% 
          #   dplyr::mutate(gear_type = as.character(gear_type), time_start = as.character(time_start)) 
          # 
          # min_conversion_data <- IRD_data %>%
          #   dplyr::group_by(gear_type, species, time_start, geographic_identifier, time_end) %>%
          #   dplyr::summarize(conversion_factor = min(conversion_factor)) %>%
          #   dplyr::mutate(source_authority = "WCPFC") %>% 
          #   dplyr::mutate(unit = "NO", unit_target = "MT")
          # 
          # IRD_data <- base::rbind(min_conversion_data, IRD_data)
          # 
          # min_conversion_data <- IRD_data %>%
          #   dplyr::group_by(unit, unit_target, gear_type, species, time_start, geographic_identifier, time_end) %>%
          #   dplyr::summarize(conversion_factor = min(conversion_factor))
          # 
          # all <- base::rbind(min_conversion_data %>% dplyr::mutate(source_authority = "IOTC"), 
          #                    min_conversion_data %>% dplyr::mutate(source_authority = "ICCAT"), 
          #                    min_conversion_data %>% dplyr::mutate(source_authority = "IATTC"))
          # 
          # join <- full_join(all %>% dplyr::rename(conversionfactorupgraded = conversion_factor), IRD_data, by = c("unit", 
          #                                                                                                         "unit_target", "gear_type", "species", "time_start", "geographic_identifier", "time_end", "source_authority")) %>% 
          #   dplyr::mutate(conversion_factor = ifelse(is.na(conversion_factor),conversionfactorupgraded, conversion_factor )) %>% 
          #   dplyr::select(-conversionfactorupgraded)
          # 
          # duplication <- join %>% filter(gear_type == "09")
          # 
          # duplication <- duplication %>% mutate(gear_type = "09.1")
          # 
          # join_updated <- bind_rows(join, duplication)
          # 
          # iotc_min <- IOTC_conv_fact_mapped %>%
          #   dplyr::group_by(gear_type, species, time_start) %>%
          #   dplyr::summarise(min_measurement_value = min(measurement_value), .groups = 'drop')
          # 
          # join_updated_iotc <- join_updated %>%
          #   dplyr::left_join(iotc_min %>%
          #                      dplyr::mutate(time_start = as.character(time_start)),
          #                    by = c("gear_type", "species", "time_start")) %>%
          #   dplyr::mutate(conversion_factor = pmin(conversion_factor, min_measurement_value, na.rm = TRUE)) %>%
          #   dplyr::select(-min_measurement_value)
          # 
          # data.table::fwrite(join_updated_iotc, "data/fact_conv_IRD_upgraded.csv")
          
          
          # IOTC Upgraded conversion factors -------------------------------------------------
          # if(opts$upgradeIOTC_fact_conv){
          #   
          #   opts$min_q1_etc = if(!is.null(opts$min_q1_etc)) opts$min_q1_etc else "min"
          #   
          #   IOTC_conv_fact_mapped_filtered <- IOTC_conv_fact_mapped %>% 
          #     dplyr::group_by(gear_type, species, year) %>% 
          #     dplyr::summarise(
          #       mean = mean(measurement_value, na.rm = TRUE), 
          #       Q1 = quantile(measurement_value, probs = 0.25, na.rm = TRUE),
          #       median = median(measurement_value, na.rm = TRUE),
          #       Q3 = quantile(measurement_value, probs = 0.75, na.rm = TRUE),
          #       min = min(measurement_value, na.rm = TRUE)
          #     ) %>% 
          #     dplyr::mutate(conversion = case_when(
          #       opts$min_q1_etc == "min" ~ min,
          #       opts$min_q1_etc == "Q1" ~ Q1,
          #       opts$min_q1_etc == "Q3" ~ Q3,
          #       opts$min_q1_etc == "mean" ~ mean,
          #       opts$min_q1_etc == "median" ~ median,
          #       TRUE ~ NA_real_  # Valeur par defaut au cas ou aucune condition n'est remplie
          #     )) %>% 
          #     dplyr::select(gear_type, species, conversion, year)
          #   
          #   
          #   georef_dataset_tons <- georef_dataset %>% dplyr::filter(measurement_unit == "t")
          #   georef_dataset_no <- georef_dataset %>% dplyr::filter(measurement_unit == "no")
          #   
          #   georef_dataset_no_l <- georef_dataset_no %>% dplyr::mutate(year = lubridate::year(time_start)) %>% dplyr::left_join(IOTC_conv_fact_mapped_filtered) 
          #   georef_dataset_no_conv_fact <- georef_dataset_no_l %>% dplyr::filter(is.na(conversion)) %>% dplyr::select(-c(conversion, year))
          #   
          #   georef_dataset_no <- georef_dataset_no_l %>% dplyr::filter(!is.na(conversion)) %>% 
          #     dplyr::mutate(measurement_value = conversion * measurement_value) %>% 
          #     dplyr::mutate(measurement_unit = "t") %>% dplyr::select(-c(conversion, year))
          #   
          #   georef_dataset <- rbind(georef_dataset_no, georef_dataset_tons, georef_dataset_no_conv_fact)
          #   
          #   function_recap_each_step(
          #     paste0("Converting NO using upgraded IOTC convfact",  (opts$min_q1_etc)),
          #     georef_dataset,
          #     paste0("We use minimal recorded IOTC conversion factors for each couple gear/species and use those to convert the Number of fish of other source_authority"),
          #     ""
          #   )
          #   
          # }
          if(opts$upgradeIRD_fact_conv){
            
            ## CONVERT USING IRD CONVERSION FACTORS ---------------------------------------------------
            
            georef_dataset <- perform_unit_conversion(
              conversion_factor_csv = "data/fact_conv_IRD_upgraded.csv",
              unit_conversion_codelist_geoidentifiers_conversion_factors = opts$unit_conversion_codelist_geoidentifiers_conversion_factors,
              georef_dataset = georef_dataset,
              entity = entity,
              config = config,
              opts = opts,
              step_description = "Harmonising units on IRD upgraded data", 
              addeddescription = paste0("This conversion factors dataset contains conversion factors for strata with the following details : ",
                                        "gear_type, source_authority, species (", unique(join_updated_iotc$species),") , time_start-time_end and the main geographical area (equatorial or tropical). A strata",
                                        "is converted only if the stratas are fully matching. They were computed from the Japanese and Taiwanese size-frequency data as well as from the Japanese total catches", 
                                        "and catch-and-effort data. For each existing strata, if the conversion factors from IRD dataset is bigger than an equivalent in IOTC, we replace the factors with one from the previous dataset." )
            )
            
            rm(join_updated)
            rm(join_updated_iotc)
            
          }
    
    # DATASET LEVEL 2 ---------------------------------------------------
    if(DATASET_LEVEL >= 2){ #with this condition code will be run to deal with dataset level 2
      
      stepLogger(level = 2, step = stepnumber, "Extract and load IRD Level 1 gridded catch data input")
      stepnumber <<- stepnumber + 1
      
      if (!is.null(opts$raising_georef_to_nominal))
        if (opts$raising_georef_to_nominal) {
          strata <- c("source_authority", "species", "gear_type", "fishing_fleet", "year", "geographic_identifier_nom")
          convert_number_to_nominal_output <- convert_number_to_nominal(georef_dataset, nominal_catch, strata = strata, 
                                                                        raise_only_unmatched = FALSE)
          
          georef_dataset <- convert_number_to_nominal_output$georef_dataset
          
          saveRDS(convert_number_to_nominal_output$not_converted_number, "data/numberwithnonominalsecond.rds")
          
          function_recap_each_step(
            paste0("Conv_NO_nominal_no_t_full"),
            georef_dataset,
            paste0("The data that remains in Number of Fish, for which the entirety of the strata with the following dimensions:", toString(strata), 
                   "containing catch information in tons, is converted and raised using the nominal dataset ", opts$doinominal, ". The key identifier for this operation is: ", opts$keynominal,
                   ". This process relies on the fact that for a strata reported in both number and tons, the spatial footprint of the data in number is more often containing the spatial footprint of the data in tons.", 
                   "Then, as there is need to choose one of the measurment_unit for the raising and with limited conversion factors, we choose to keep raise the data in 'Number of fish' on the basis of the equivalent nominal strata ", 
                   "downgraded by the corresponding georeferenced catch in tons."),
            ""
          )
          
          
          # strata <- c("source_authority", "species", "gear_type", "fishing_fleet", "year", "geographic_identifier_nom")
          # 
          # convert_number_to_nominal_output <- convert_number_to_nominal(georef_dataset, nominal_catch, strata = strata, 
          #                                                               raise_only_unmatched = FALSE)
          # 
          # georef_dataset <- convert_number_to_nominal_output$georef_dataset
          # 
          # saveRDS(convert_number_to_nominal_output$not_converted_number, "data/numberwithnonominalsecond.rds")
          # 
          # function_recap_each_step(
          #   paste0("Conv_NO_nominal_no_t_basic"),
          #   georef_dataset,
          #   paste0("The data that remains in Number of Fish, for which the entirety of the strata with the following dimensions:", toString(strata), 
          #          "containing catch information in tons, is converted and raised using the nominal dataset ", opts$doinominal, ". The key identifier for this operation is: ", opts$keynominal,
          #          ". This process relies on the fact that for a strata reported in both number and tons, the spatial footprint of the data in number is more often containing th spatial footprint of the data in tons.", 
          #          "Then, as there is need to choose one of the measurment_unit for the raising and with limited conversion factors, we choose to keep raise the data in 'Number of fish' on the basis of the equivalent nominal strata ", 
          #          "downgraded by the corresponding georeferenced catch in tons."),
          #   ""
          # )
          
          
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
          
          wcpfc_not_to_raise <- georef_dataset %>% dplyr::filter(source_authority == "WCPFC")
          georef_dataset <- georef_dataset %>% dplyr::filter(source_authority != "WCPFC")
          dataset_to_compute_rf = georef_dataset %>% dplyr::filter(source_authority != "WCPFC")
          
          function_raise_data_output<-function_raise_data(fact,
                                                          source_authority_filter = c("IOTC","ICCAT","IATTC", "CCSBT"), 
                                                          dataset_to_raise = georef_dataset,
                                                          dataset_to_compute_rf=dataset_to_compute_rf,
                                                          nominal_dataset_df = nominal_catch,
                                                          x_raising_dimensions = x_raising_dimensions, 
                                                          decrease_when_rf_inferior_to_one = TRUE)
          
          raised_data <- function_raise_data_output$data_raised 
          
          georef_dataset <- rbind(wcpfc_not_to_raise, raised_data)
          
          rm(dataset_to_compute_rf)
          
          if(recap_each_step){
            function_recap_each_step(
              paste0("Level2_RF1_full"),
              georef_dataset,
              paste0(
                "The georeferenced data is adjusted to more accurately align with the nominal dataset. ",
                "To achieve this, all strata with corresponding equivalents in the nominal data-based on the following variables: ", 
                toString(x_raising_dimensions), "-are adjusted upward to match the nominal values. Conversely, if the georeferenced data exceeds the nominal value for any given stratum, ",
                "it is adjusted downward to correspond to the nominal data. The nominal dataset used for this adjustment is accessible via the following DOI: ",
                opts$doinominal, " with the key identifier being: ", opts$keynominal, ". Similarly, georeferenced strata expressed in numbers ",
                "that correspond to nominal strata undergoing adjustment are also removed if existing."),
              "function_raising_georef_to_nominal",
              list(
                opts$raising_georef_to_nominal ,
                fact
              ), entity
            )
          }
          
          if(opts$level2RF2number){
            
            strata = c("species", "source_authority", "year", "fishing_fleet", "geographic_identifier_nom")
            
            convert_number_to_nominal_output <- convert_number_to_nominal(georef_dataset, nominal_catch, strata = strata,
                                                                          raise_only_unmatched = FALSE)
            
            
            georef_dataset <- convert_number_to_nominal_output$georef_dataset
            
            saveRDS(convert_number_to_nominal_output$not_converted_number, "data/numberwithnonominalsecond.rds")
            
            function_recap_each_step(
              paste0("Converting_NO_RF2"),
              georef_dataset,
              paste0("The data that remains in Number of Fish, for which the entirety of the strata with the following dimensions: c('source_authority', 'species', 'gear_type', 'year')", 
                     "does not contain catch information in tons, is converted and raised using the nominal dataset ", opts$doinominal, ". The key identifier for this operation is: ", opts$keynominal,
                     ". This process relies on the assumption that for a given stratum with a unique measurement unit, raising the data to match the nominal catch serves as a reasonable approximation of what would result", 
                     " from a conversion using the available conversion factors from Number of Fish to Tons. However, this step applies the same conversion factor to all catch within the same strata, which may not fully ", 
                     "reflect the variability in reality. Consequently, this step is performed for strata where solid conversion factors are not available."),
              ""
            )
          }
          
          if(opts$level2RF2){
            
            dataset_to_compute_rf = georef_dataset
            x_raising_dimensions = c("species", "source_authority", "year", "fishing_fleet", "geographic_identifier_nom")
            function_raise_data_output<-function_raise_data(fact,
                                                            source_authority_filter = c("IOTC","ICCAT","IATTC", "WCPFC", "CCSBT"),
                                                            dataset_to_raise = georef_dataset,
                                                            dataset_to_compute_rf=dataset_to_compute_rf,
                                                            nominal_dataset_df = nominal_catch,
                                                            x_raising_dimensions = x_raising_dimensions, decrease_when_rf_inferior_to_one = FALSE)
            
            rm(dataset_to_compute_rf)
            
            georef_dataset <- function_raise_data_output$data_raised 
            
            if(recap_each_step){
              function_recap_each_step(
                paste0("Level2_RF2"),
                georef_dataset,
                paste0(
                  "The georeferenced data is adjusted to more accurately align with the nominal dataset. ",
                  "To achieve this, all strata with corresponding equivalents in the nominal data-based on the following variables: ", 
                  toString(x_raising_dimensions), 
                  "-are adjusted upward to match the nominal values. Conversely, if the georeferenced data exceeds the nominal value for any given stratum, ",
                  "it is adjusted downward to correspond to the nominal data. The nominal dataset used for this adjustment is accessible via the following DOI: ",
                  opts$doinominal, " with the key identifier being ", opts$keynominal, ". Strata without complete equivalence in the nominal data-particularly for the year 1950-",
                  "have been excluded, as the year is incomplete and cannot be fully raised from the nominal dataset. Similarly, georeferenced strata expressed in numbers ",
                  "that correspond to nominal strata undergoing adjustment are also removed. In most cases, strata reported in both number and tons maintain a consistent ",
                  "spatial footprint, as the nominal data encompasses the corresponding spatial representation. Thus, the spatial coverage remains largely unaffected, aside from ",
                  "the relative proportions of captures within each spatial unit. However, this adjustment introduces the challenge of losing some spatial information. Without ",
                  "appropriate conversion factors, we opted to exclude these strata rather than risk overestimating catches, which could lead to georeferenced strata exceeding ",
                  "their nominal counterparts."
                ),
                "function_raising_georef_to_nominal",
                list(
                  opts$raising_georef_to_nominal ,
                  fact,
                  opts$raising_do_not_raise_wcfpc_data,
                  opts$raising_raise_only_for_PS_LL
                ), entity
              )
            }
            
            dataset_to_compute_rf <- georef_dataset
            
            function_raise_data_output<-function_raise_data(fact,
                                                            source_authority_filter = c("IOTC","ICCAT","IATTC", "WCPFC", "CCSBT"), 
                                                            dataset_to_raise = georef_dataset,
                                                            dataset_to_compute_rf=dataset_to_compute_rf,
                                                            nominal_dataset_df = nominal_catch,
                                                            x_raising_dimensions = x_raising_dimensions, decrease_when_rf_inferior_to_one = TRUE)
            
            georef_dataset <- function_raise_data_output$data_raised 
            
            georef_dataset <- georef_dataset %>% dplyr::distinct()
            
            if(recap_each_step){
              function_recap_each_step(
                "Level2_RF2_decreasing_rf",
                georef_dataset,
                paste0("We remove data where the georeferenced data is superior to nominal for a same strata: " ,toString(x_raising_dimensions),". Indeed,",
                       " in some cases the value of catch in the catch-and-effort data can be greater than the one in the nominal catch, however the reasons of ", 
                       "this inconsistency are not yet fully understood."),
                "function_raising_georef_to_nominal",
                list(), entity
              )
            }
          }
          
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
                                                      "time_start", "time_end", "year", "month", "quarter", "geographic_identifier", "measurement_unit", "measurement_value") ) 
    config$logger.info("End: Your tuna atlas dataset has been created!")
    
    # Clean up
    rm(georef_dataset)
    gc()
    
    
  
}
