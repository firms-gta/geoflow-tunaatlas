#' Create Your Own Tuna Atlas Dataset
#'
#' This function allows users to create their own georeferenced Tuna Atlas dataset of catch or effort.
#' It takes as input the public domain datasets of the five Tuna Regional Fisheries Management Organizations (tRFMOs)
#' (IOTC, ICCAT, WCPFC, IATTC, CCSBT) stored within the Tuna atlas database. Users can customize the computation
#' of the tuna atlas using a set of parameters.
#'
#' @param action An action object containing options for dataset processing from geoflow.
#' @param entity A geoflow entity object with dataset identifiers and metadata.
#' @param config A geoflow configuration object with software settings.
#' @return The status of the dataset creation process.
#' @examples
#' \dontrun{
#' # Example usage:
#' action <- entity$data$actions[[1]]
#' entity <- config$metadata$content$entities[[1]]
#' config <- initWorkflow(here::here("tunaatlas_qa_global_datasets_catch.json"))
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
  packages <- c("dplyr", "sf", "stringr", "R3port", "reshape2", "readr", "tools", "RPostgreSQL", "DBI", "googledrive")
  
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
  source(file.path(url_scripts_create_own_tuna_atlas, "get_rfmos_datasets_level0.R")) #modified for geoflow
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
  source("~/firms-gta/geoflow-tunaatlas/Analysis_markdown/functions/function_recap_each_step.R")
  
  #action options
  filtering_on_minimum_year_declared = if(!is.null(opts$filtering_on_minimum_year_declared)) opts$filtering_on_minimum_year_declared else TRUE
  recap_each_step = if(!is.null(opts$recap_each_step)) opts$recap_each_step else TRUE
  from_level0 <- if(!is.null(opts$from_level0)){opts$from_level0} else if(opts$fact == "effort"){FALSE}  else TRUE
  
  opts$doilevel0 = if(!is.null(opts$doilevel0)) opts$doilevel0 else "10.5281/zenodo.11460074"
  opts$keylevel0 =  if(!is.null(opts$keylevel0)) opts$keylevel0 else "global_catch_firms_level0_harmonized.csv"
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
  opts$iattc_ps_raise_flags_to_schooltype = if(!is.null(opts$iattc_ps_raise_flags_to_schooltype)) opts$iattc_ps_raise_flags_to_schooltype else TRUE
  opts$iattc_ps_catch_billfish_shark_raise_to_effort = if(!is.null(opts$iattc_ps_catch_billfish_shark_raise_to_effort)) opts$iattc_ps_catch_billfish_shark_raise_to_effort else TRUE
  opts$mapping_map_code_lists = if(!is.null(opts$mapping_map_code_lists)) opts$mapping_map_code_lists else TRUE
  opts$mapping_keep_src_code = if(!is.null(opts$mapping_keep_src_code)) opts$mapping_keep_src_code else FALSE
  opts$aggregate_on_5deg_data_with_resolution_inferior_to_5deg = if(!is.null(opts$aggregate_on_5deg_data_with_resolution_inferior_to_5deg)) opts$aggregate_on_5deg_data_with_resolution_inferior_to_5deg else FALSE
  opts$overlapping_zone_iattc_wcpfc_data_to_keep = if(!is.null(opts$overlapping_zone_iattc_wcpfc_data_to_keep)) opts$overlapping_zone_iattc_wcpfc_data_to_keep else "IATTC"
  opts$overlapping_zone_iotc_wcpfc_data_to_keep = if(!is.null(opts$overlapping_zone_iotc_wcpfc_data_to_keep)) opts$overlapping_zone_iotc_wcpfc_data_to_keep else "IOTC"
  opts$spatial_curation_data_mislocated = if(!is.null(opts$spatial_curation_data_mislocated)) opts$spatial_curation_data_mislocated else "remove"
  opts$unit_conversion_convert = if(!is.null(opts$unit_conversion_convert)) opts$unit_conversion_convert else FALSE
  opts$unit_conversion_csv_conversion_factor_url = if(!is.null(opts$unit_conversion_csv_conversion_factor_url)) opts$unit_conversion_csv_conversion_factor_url else "https://data.d4science.org/shub/4276080e-ab63-4e9a-bfe2-9213ab659a84"
  opts$unit_conversion_codelist_geoidentifiers_conversion_factors = if(!is.null(opts$unit_conversion_codelist_geoidentifiers_conversion_factors)) opts$unit_conversion_codelist_geoidentifiers_conversion_factors else "areas_conversion_factors_numtoweight_ird"
  opts$raising_georef_to_nominal = if(!is.null(opts$raising_georef_to_nominal)) opts$raising_georef_to_nominal else TRUE
  opts$raising_do_not_raise_wcfpc_data = if(!is.null(opts$raising_do_not_raise_wcfpc_data)) opts$raising_do_not_raise_wcfpc_data else FALSE
  opts$raising_raise_only_for_PS_LL = if(!is.null(opts$raising_raise_only_for_PS_LL)) opts$raising_raise_only_for_PS_LL else FALSE
  opts$disaggregate_on_5deg_data_with_resolution_superior_to_5deg = if(!is.null(opts$disaggregate_on_5deg_data_with_resolution_superior_to_5deg)) opts$disaggregate_on_5deg_data_with_resolution_superior_to_5deg else "none"
  opts$disaggregate_on_1deg_data_with_resolution_superior_to_1deg = if(!is.null(opts$disaggregate_on_1deg_data_with_resolution_superior_to_1deg)) opts$disaggregate_on_1deg_data_with_resolution_superior_to_1deg else "none"
  opts$curation_absurd_converted_data = if(!is.null(opts$curation_absurd_converted_data)) opts$curation_absurd_converted_data else TRUE
  opts$cache = if(!is.null(opts$cache)) opts$cache else FALSE
  opts$level2RF2 = if(!is.null(opts$level2RF2)) opts$level2RF2 else FALSE
  
  #Identify expected Level of processing
  opts$fact = if(!is.null(opts$fact)) opts$fact else {futile.logger::flog.error("Please provide a fact, i.e. effort or catch")}
  DATASET_LEVEL = if(!is.null(opts$dataset_level)) opts$dataset_level else {futile.logger::flog.error("Please provide a fact, i.e. effort or catch")}
  
  
  # LEVEL 0 FIRMS PRODUCT ---------------------------------------------------
  
  if(DATASET_LEVEL == 0 | from_level0){
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
          filter(time_start <= min_time_start)
        
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
      
    }
    
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
    if(!is.null(opts$doigeoref)){
      georef_dataset <- download_zenodo_csv(opts$doigeoref, opts$keygeoref) 
      
    } else if(file.exists("~/firms-gta/geoflow-tunaatlas/data/firms_level0_dataset.qs")){
      nominal_catch <-
        qs::qread("~/firms-gta/geoflow-tunaatlas/data/firms_level0_dataset.qs")
      
      function_recap_each_step(
        "Level0Firms_Dataset",
        georef_dataset,
        paste0(
          "Retrieving level 0 data on the basis of this DOI ",
          opts$doi, " the key being ", opts$key),
        "download_zenodo_csv"  ,
        list(opts$doi, opts$key), entity
      ) 
    }
  }
  
  # LEVEL 1 IRD ---------------------------------------------------
  if(DATASET_LEVEL >= 1){
    #with this condition code will be run to deal with dataset level 1 and above
    config$logger.info("Level 1 start")
    
    opts$mapping_map_code_lists <- mapping_map_code_lists
    if (!is.null(opts$unit_conversion_convert)) if (opts$unit_conversion_convert) {
      
      georef_dataset <- perform_unit_conversion(
        conversion_factor_csv = "data/IOTC_conv_fact_mapped.csv",
        unit_conversion_codelist_geoidentifiers_conversion_factors = "cwp_grid",
        georef_dataset = georef_dataset,
        entity = entity,
        config = config,
        opts = opts,
        step_description = "Harmonising units on IOTC data"
      )
      ## CONVERSION NUMBER IF LEVEL2 ---------------------------------------------------
      
      if(DATASET_LEVEL == 2){ # If the dataset is to be raise after, we can raise the strata being only in number of fish 
        
        config$logger.info(
          "Extract and load FIRMS Level 0 nominal catch data input (required if raising process is asked) "
        )
        if(!is.null(opts$doinominal)){
          nominal_catch <- download_zenodo_csv(opts$doinominal, opts$keynominal) 
          class(nominal_catch$measurement_value) <- "numeric"
          
        } else if(file.exists("data/global_nominal_catch_firms_level0.csv")){
          nominal_catch <-
            readr::read_csv("data/global_nominal_catch_firms_level0.csv",
                            guess_max = 0) 
          
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
        }
        #@juldebar if not provided by Google drive line below should be used if nominal catch has to be extracted from the database
        else {
          nominal_catch <- retrieve_nominal_catch(entity, config, opts)
        }
        
        if(!is.null(opts$convertno_before) && opts$convertno_before){ # some fishing_fleet are not mapped correctly so we remap UNK to NEI for fishing_fleet
          nominal_catch <- nominal_catch %>% 
            dplyr::mutate(fishing_fleet = ifelse(fishing_fleet == "UNK", "NEI", fishing_fleet))
          
          georef_dataset <- georef_dataset %>% 
            dplyr::mutate(fishing_fleet = ifelse(source_authority == "CCSBT", "NEI",fishing_fleet )) # To prevent any creation of data for a specific fishing_fleet we remove this information for CCSBT data
          
          
          # we convert only the first year entirely declared in georeferenced by each tRFMOs being 1951 for WCPFC,
          # 1953 for IOTC # 1954 for ICCAT and IATTC # 1965 for CCSBT
          
          # nominal_catch <- nominal_catch %>%
          #   dplyr::mutate(year =lubridate::year(time_start)) %>% 
          #   dplyr::filter((source_authority == "WCPFC" & year >= 1951) |
          #            (source_authority == "IOTC" & year >= 1953) |
          #            (source_authority == "ICCAT" & year >= 1960) |
          #            (source_authority == "IATTC" & year >= 1954) |
          #            (source_authority == "CCSBT" & year >= 1965)) %>% 
          #   dplyr::select(-year)
          
          convert_number_to_nominal <- convert_number_to_nominal(georef_dataset, nominal_catch) 
          
          # we convert/raise only the stratas being recorded only in number of fish
          
          georef_dataset <- convert_number_to_nominal$georef_dataset
          saveRDS(convert_number_to_nominal$not_converted_number, "data/numberwithnonominal.rds")
          function_recap_each_step(
            "Converting NO using nominal",
            georef_dataset,
            "The data not converted in no is converted using nominal dataset. To be noted, fishing_fleet is removed for CCSBT dataset as the global_nominal catch of CCSBT does not contain any information on 
              the fishing_fleet. Warning data prior 1960 contains raises extremely hihgh, these comes from the fact that the georeferneced data for those years are not complete.",
            ""
          )
        }
        
      }
      
      ## UPGRADE IRD CONVERSION FACTORS ---------------------------------------------------
      
      IRD_data <- readr::read_csv("data/fact_conv_IRD.csv") %>% 
        dplyr::mutate(gear_type = as.character(gear_type), time_start = as.character(time_start))
      
      min_conversion_data <- IRD_data %>%
        dplyr::group_by(gear_type, species, time_start, geographic_identifier, time_end) %>%
        dplyr::summarize(conversion_factor = min(conversion_factor)) %>%
        dplyr::mutate(source_authority = "WCPFC") %>% 
        dplyr::mutate(unit = "NO", unit_target = "MT" )
      
      IRD_data <- base::rbind(min_conversion_data, IRD_data)
      
      
      min_conversion_data <- IRD_data %>%
        dplyr::group_by(unit, unit_target, gear_type, species, time_start, geographic_identifier, time_end) %>%
        dplyr::summarize(conversion_factor = min(conversion_factor))
      
      all <- base::rbind(min_conversion_data %>% dplyr::mutate(source_authority = "IOTC"), 
                         min_conversion_data %>% dplyr::mutate(source_authority = "ICCAT"), 
                         min_conversion_data %>% dplyr::mutate(source_authority = "IATTC"))
      
      join <- full_join(all %>% dplyr::rename(conversionfactorupgraded = conversion_factor), IRD_data, by = c("unit", "unit_target", "gear_type", "species", "time_start", 
                                                                                                              "geographic_identifier", "time_end",
                                                                                                              "source_authority")) %>% 
        dplyr::mutate(conversion_factor =ifelse(is.na(conversion_factor),conversionfactorupgraded, conversion_factor )) %>% 
        dplyr::select(-conversionfactorupgraded)
      
      join <- join %>% dplyr::mutate(gear_type = ifelse(gear_type == "09", "09.1", gear_type))
      
      data.table::fwrite(join, "data/fact_conv_IRD_upgraded.csv")
      
      ## CONVERT USING IRD CONVERSION FACTORS ---------------------------------------------------
      
      georef_dataset <- perform_unit_conversion(
        conversion_factor_csv = "data/fact_conv_IRD_upgraded.csv",
        unit_conversion_codelist_geoidentifiers_conversion_factors = opts$unit_conversion_codelist_geoidentifiers_conversion_factors,
        georef_dataset = georef_dataset,
        entity = entity,
        config = config,
        opts = opts,
        step_description = "Harmonising units on IRD upgraded data"
      )
      
    }
  }
  
  # DATASET LEVEL 2 ---------------------------------------------------
  if(DATASET_LEVEL >= 2){ #with this condition code will be run to deal with dataset level 2
    
    stepLogger(level = 2, "Extract and load IRD Level 1 gridded catch data input")
    
    
    if (!is.null(opts$raising_georef_to_nominal))
      if (opts$raising_georef_to_nominal) {
        
        if(is.null(opts$convertno_before) | !(opts$convertno_before)){
          
          nominal_catch <- nominal_catch %>% 
            dplyr::mutate(fishing_fleet = ifelse(fishing_fleet == "UNK", "NEI", fishing_fleet))
          
          georef_dataset <- georef_dataset %>% 
            dplyr::mutate(fishing_fleet = ifelse(source_authority == "CCSBT", "NEI",fishing_fleet))
          
          convert_number_to_nominal <- convert_number_to_nominal(georef_dataset, nominal_catch) 
          georef_dataset <- convert_number_to_nominal$georef_dataset
          saveRDS(convert_number_to_nominal$not_converted_number, "data/numberwithnonominal.rds")
          
          function_recap_each_step(
            "Converting NO using nominal",
            georef_dataset,
            "The data not converted in no is converted using nominal dataset. This only concern the strata having declaration in number only for a year.",
            ""
          )
          
        }
        stepLogger(level = 2, "Raise IRD gridded Level 1 (1 or 5 deg) input with FIRMS Level O total (nominal) catch dataset")
        
        config$logger.info("Start raising process")
        
        if (opts$fact == "catch") {
          config$logger.info("Fact=catch !")
          dataset_to_compute_rf = georef_dataset
          # year is used as a dimension to match the conversion factors dimension 
          if (is.null(opts$x_raising_dimensions)) {
            x_raising_dimensions = c("gear_type", "species", "year", "source_authority", "fishing_fleet")
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
        # idee raise only les données qui sont que en tonnes
        
        
        function_raise_data_output<-function_raise_data(fact,
                                                        source_authority_filter = c("IOTC","ICCAT","IATTC", "WCPFC", "CCSBT"),
                                                        dataset_to_raise = georef_dataset,
                                                        dataset_to_compute_rf=dataset_to_compute_rf,
                                                        nominal_dataset_df = nominal_catch,
                                                        x_raising_dimensions = x_raising_dimensions)
        
        georef_dataset <- function_raise_data_output$data_raised 
        
        georef_dataset <- georef_dataset %>% dplyr::distinct()
        
        raising_factor_with_raising <- function_raise_data_output$df_rf %>% dplyr::filter(rf > 1)
        
        corresponding_number_to_raised_data <- georef_dataset %>% 
          dplyr::filter(measurement_unit == "no") %>% 
          dplyr::mutate(year = lubridate::year(time_start))%>% 
          dplyr::select(c("species", "gear_type", "year", "fishing_fleet", "source_authority")) %>% dplyr::distinct() %>% 
          dplyr::inner_join(raising_factor_with_raising %>% 
                              dplyr::select(c("species", "gear_type", "year", "fishing_fleet", "source_authority"))%>% dplyr::distinct(),
                            by = c("species", "gear_type", "year", "fishing_fleet", "source_authority")) %>% 
          dplyr::mutate(measurement_unit = "no")
        
        georef_dataset <- georef_dataset %>% 
          dplyr::mutate(year = lubridate::year(time_start)) %>% 
          anti_join(corresponding_number_to_raised_data) %>% 
          dplyr::select(-year)
        
        rm(dataset_to_compute_rf)
        
        if(recap_each_step){
          function_recap_each_step(
            "Level2_RF1",
            georef_dataset,
            paste0("The georeferenced data is raised to get closer of the nominal data. Aiming this, all the stratas having an equivalent (regarding the following columns", toString(x_raising_dimensions), 
                   " ) in nominal catches are raised to reach the equivalent. If the data is lower in nominal data for the strata, the data is lowered to reach the nominal amount",
                   "The nominal dataset used to raise the data is under the following DOI: ", opts$doinominal," key being ",  opts$keynominal,
                   "Aiming this, all the stratas having an equivalent (regarding the columns given in options) in nominal catches are raised to reach the equivalent.",
                   "Data from 1950 is removed as the year is not complete and cannot be raised from the entire nominal data. Data from number having an equivalent strata in nominal that is being raised is also removed.", 
                   "The stratas displayed in number and tons are majoritarely having a spatial print of tons including the one in nominal so besides the relatives proportion of captures in each square, the spatial print does not change.", 
                   "This is still an issue as we loose some spatial print of captures, however, without any conversion factors we'd rather loose some stratas that creating catpures, including creating captures making georeferenced stratas higher than nominal.",
                   "we convert only the first year entirely declared in georeferenced by each tRFMOs being 1951 for WCPFC, 1953 for IOTC, 1954 for ICCAT and IATTC, 1965 for CCSBT"),
            "function_raising_georef_to_nominal",
            list(
              opts$raising_georef_to_nominal ,
              iattc_ps_raise_flags_to_schooltype ,
              iattc_ps_dimension_to_use_if_no_raising_flags_to_schooltype ,
              iattc_ps_catch_billfish_shark_raise_to_effort ,
              iccat_ps_include_type_of_school,
              fact
            ), entity
          )
        }
        
        if(opts$level2RF2){
          dataset_to_compute_rf = georef_dataset
          x_raising_dimensions = c("gear_type", "species", "year", "source_authority")
          dataset_to_compute_rf <- georef_dataset
          georef_dataset<-function_raise_data(fact,
                                              source_authority_filter = c("IOTC","ICCAT","IATTC", "WCPFC"),
                                              dataset_to_raise = georef_dataset,
                                              dataset_to_compute_rf=dataset_to_compute_rf,
                                              nominal_dataset_df = nominal_catch,
                                              x_raising_dimensions = x_raising_dimensions, decrease_when_rf_inferior_to_one = FALSE)
          
          rm(dataset_to_compute_rf)
          if(recap_each_step){
            function_recap_each_step(
              "Level2_RF2",
              georef_dataset,
              paste0("The georeferenced data is raised to get closer of the nominal data. Aiming this, all the stratas having an equivalent (regarding the following columns", toString(x_raising_dimensions), 
                     " ) in nominal catches are raised to reach the equivalent. If the data is lower in nominal data for the strata, the data is lowered to reach the nominal amount",
                     "The nominal dataset used to raise the data is under the following DOI: ", opts$doinominal," key being ",  opts$keynominal,
                     "Aiming this, all the stratas having an equivalent (regarding the columns given in options) in nominal catches are raised to reach the equivalent."),
              "function_raising_georef_to_nominal",
              list(
                opts$raising_georef_to_nominal ,
                iattc_ps_raise_flags_to_schooltype ,
                iattc_ps_dimension_to_use_if_no_raising_flags_to_schooltype ,
                iattc_ps_catch_billfish_shark_raise_to_effort ,
                iccat_ps_include_type_of_school,
                fact,
                raising_do_not_raise_wcfpc_data,
                raising_raise_only_for_PS_LL
              ), entity
            )
          }
          
          dataset_to_compute_rf <- georef_dataset
          
          georef_dataset<-function_raise_data(fact,
                                              source_authority_filter = c("IOTC","ICCAT","IATTC", "WCPFC"),
                                              dataset_to_raise = georef_dataset,
                                              dataset_to_compute_rf=dataset_to_compute_rf,
                                              nominal_dataset_df = nominal_catch,
                                              x_raising_dimensions = x_raising_dimensions, decrease_when_rf_inferior_to_one = TRUE)
          
          if(recap_each_step){
            function_recap_each_step(
              "Level2_RF2_decreasing_rf",
              georef_dataset,
              paste0("The georeferenced data is raised to get closer of the nominal data. Aiming this, all the stratas having an equivalent (regarding the following columns", toString(x_raising_dimensions), 
                     " ) in nominal catches are raised to reach the equivalent. If the data is lower in nominal data for the strata, the data is lowered to reach the nominal amount",
                     "The nominal dataset used to raise the data is under the following DOI: ", opts$doinominal," key being ",  opts$keynominal,
                     "Aiming this, all the stratas having an equivalent (regarding the columns given in options) in nominal catches are raised to reach the equivalent."),
              "function_raising_georef_to_nominal",
              list(), entity
            )
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
        paste0("The data is disaggregated data on resolution higher than 5° in 5° resolution."),
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
        "This step disaggregate data on resolution higher than 1° in 1° resolution",
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
    stepLogger(level = 0, step = stepnumber, msg = "Spatial Aggregation of data (5deg resolution datasets only: Aggregate data on 5° resolution quadrants)")
    stepnumber = stepnumber+1
    config$logger.info("Aggregating data that are defined on quadrants or areas inferior to 5° quadrant resolution to corresponding 5° quadrant...")
    
    one_degree <- georef_dataset %>% dplyr::filter(substr(geographic_identifier, 1, 1) == "5")
    five_degree <- georef_dataset %>% dplyr::filter(substr(geographic_identifier, 1, 1) == "6")
    one_degree_aggregated <- one_degree %>% rowwise() %>% 
      dplyr::mutate(geographic_identifier = transform_cwp_code_from_1deg_to_5deg(geographic_identifier))
    
    # df_input_not_aggregated <- georef_dataset %>% dplyr::filter(is.null(geographic_identifier))
    # fwrite(df_input_not_aggregated, "data/df_input_not_aggregated.csv")
    
    georef_dataset <- as.data.frame(base::rbind(one_degree_aggregated, five_degree))
    
    config$logger.info("Aggregating data that are defined on quadrants or areas inferior to 5° quadrant resolution to corresponding 5° quadrant OK")
    
    if(recap_each_step){
      names_list_aggregation <-
        c("df_input_not_aggregated", "stats_not_aggregated") #file we want to save
      
      # try(lapply(names_list_aggregation, function_write_RDS))
      
      function_recap_each_step(
        "Aggregation",
        georef_dataset,
        "The data with resolution lower to 5° is aggregated on resolution of 5°.",
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
