#' Georeferenced catch or effort, level 0, from the tRFMO datasets
#'
#' Builds the level 0 georeferenced dataset from the datasets of the tRFMOs
#' (outputs of the raw dataset workflow): reads and binds them
#' (gta_rfmo_dataset_level0()), applies the catch-specific filters, keeps the
#' years declared by every tRFMO, applies the IATTC processings (catch) and
#' handles the overlapping zones between tRFMOs (function_overlapped(), which
#' queries the database for the areas of competence).
#'
#' This is the level 0 part of create_georef_dataset(), moved
#' as is: the options keep their names and meaning.
#'
#' @param files Names (or paths) of the dataset files of the tRFMOs.
#' @param opts List of the options, with their defaults already set (see the
#'   geoflow action workflow/actions/create_georef_dataset.R).
#' @param con Connection to the Tuna Atlas database.
#' @param entity A geoflow entity whose metadata (provenance) the step
#'   summaries complete, or `NULL`.
#' @param recap_each_step Write the summary of each step.
#' @param filtering_on_minimum_year_declared Keep only the years declared by
#'   every tRFMO.
#' @param stepnumber Number of the first step, for the log.
#' @param data_dir Folder of the files.
#' @return A list: `georef_dataset` (data frame), `rawdata` (the options used
#'   to read the tRFMO datasets, reused by the level 2) and `stepnumber`.
gta_georef_level0 <- function(files, opts, con = NULL, entity = NULL, recap_each_step = TRUE,
                              filtering_on_minimum_year_declared = TRUE, stepnumber = 1,
                              data_dir = "data") {
  source(here::here("R/core/rfmo_datasets_level0.R"))
  source(here::here("R/tunaatlas_scripts/generation/function_overlapped.R"))
  stepLogger <- function(level, step, msg) {
    message(sprintf("LEVEL %s => STEP %s: %s", level, step, msg))
  }

  ## INITIALISATION OF MULTIPLES DATASET---------------------------------------------------
  message("Begin: Retrieving primary datasets from Tuna atlas DB... ")
  
  stepLogger(level = 0, step = stepnumber, msg = "Retrieve georeferenced catch or effort (+ processings for IATTC) AND NOMINAL CATCH if asked")
  stepnumber = stepnumber+1
  
  rawdata <- opts
  #by default, at this step we will skip specific processings applied to IATTC data. These processings are eventually done later in the script (if options are activated)
  rawdata$iattc_ps_raise_flags_to_schooltype <- FALSE
  rawdata$iattc_ps_catch_billfish_shark_raise_to_effort <- FALSE
  rawdata$iattc_ps_dimension_to_use_if_no_raising_flags_to_schooltype <- "fishing_fleet"
  # Effort: no later IATTC processing, the purse seine flag x school type
  # raising is done when the data are read (get_rfmos_datasets_level0)
  if (opts$fact == "effort") {
    rawdata$iattc_ps_raise_flags_to_schooltype <- opts$iattc_ps_raise_flags_to_schooltype
  }
  # 1) Charger chaque dataset
  datasets_list <- lapply(
    c("IOTC", "WCPFC", "CCSBT", "ICCAT", "IATTC"),
    gta_rfmo_dataset_level0,
    files,
    rawdata,
    data_dir
  )
  names(datasets_list) <- c("IOTC", "WCPFC", "CCSBT", "ICCAT", "IATTC")
  
  ref_cols <- names(datasets_list[[1]])
  
  for (nm in names(datasets_list)) {
    cols <- names(datasets_list[[nm]])
    cat("\n====================\n", nm, "\n====================\n", sep = "")
    cat("ncol =", length(cols), " | nrow =", nrow(datasets_list[[nm]]), "\n", sep = "")
    cat("Columns:\n"); print(cols)
    
    missing_vs_ref <- setdiff(ref_cols, cols)
    extra_vs_ref   <- setdiff(cols, ref_cols)
    if (length(missing_vs_ref)) { cat("Missing vs ref:\n"); print(missing_vs_ref) }
    if (length(extra_vs_ref))   { cat("Extra vs ref:\n");   print(extra_vs_ref)   }
  }
  
  # 3) rbind dans un autre objet (option: fill si pas mêmes colonnes)
  dataset <- do.call(rbind, datasets_list)
  
  dataset$time_start <-
    substr(as.character(dataset$time_start), 1, 10)
  dataset$time_end <- substr(as.character(dataset$time_end), 1, 10)
  georef_dataset <- dataset
  class(georef_dataset$measurement_value) <- "numeric"
  rm(dataset)
  if(opts$fact == "catch"){
  species_to_force <- c("BSH", "FAL", "MAK", "OCS", "RSK", "SKH", "SMA", "SPN", "THR")
  georef_dataset <- georef_dataset %>%
    dplyr::mutate(measurement_processing_level = dplyr::case_when(
      source_authority == "IATTC" & species %in% species_to_force ~ "original_sample",
      TRUE ~ measurement_processing_level
    )) %>% dplyr::mutate(fishing_mode = ifelse(fishing_mode == "OTH", "UNK", fishing_mode))
  georef_dataset <- georef_dataset %>% dplyr::filter(substr(geographic_identifier, 1, 1) != "7") # removing 10 degrees
  georef_dataset <- georef_dataset %>%
    {
      if ("species" %in% names(.)) {
        dplyr::filter(
          .,
          species != "UNK",
          !(species == "SBF" & source_authority == "IOTC")
        )
      } else .
    }
  }
  if(recap_each_step){
    CWP.dataset::function_recap_each_step(
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
      message("Careful, not all the source_authority has the same maximum year of declaration")
      
      # get the minimum time_start of all the maximum time_start of each source_authority
      min_time_start <- min(max_years$max_time_start)
      
      # filter the georef_dataset based on the minimum time_start of all the maximum time_start of each source_authority
      georef_dataset <- georef_dataset %>%
        dplyr::filter(time_start <= min_time_start)
      
      if (recap_each_step) {
        CWP.dataset::function_recap_each_step(
          "rawdata_time_harmonized",
          georef_dataset,
          "Years provided by not every tRFMOs are removed",
          "filter",
          list(), entity
        )
        # saveRDS(georef_dataset, "data/rawdata.rds")
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
        gta_rfmo_dataset_level0("IATTC", files, rawdata, data_dir)
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
          message("Careful, not all the source_authority has the same maximum year of declaration")
          
          # get the minimum time_start of all the maximum time_start of each source_authority
          min_time_start <- min(max_years$max_time_start)
          
          # filter the georef_dataset based on the minimum time_start of all the maximum time_start of each source_authority
          georef_dataset <- georef_dataset %>%
            filter(time_start <= min_time_start)
        }
      }
      
      if(recap_each_step){
        CWP.dataset::function_recap_each_step(
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
        gta_rfmo_dataset_level0("IATTC", files, rawdata, data_dir)
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
          message("Careful, not all the source_authority has the same maximum year of declaration")
          
          # get the minimum time_start of all the maximum time_start of each source_authority
          min_time_start <- min(max_years$max_time_start)
          
          # filter the georef_dataset based on the minimum time_start of all the maximum time_start of each source_authority
          georef_dataset <- georef_dataset %>%
            filter(time_start <= min_time_start)
        }
      }
      
      if(recap_each_step){
        CWP.dataset::function_recap_each_step(
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
    #   CWP.dataset::function_recap_each_step(
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
        message(paste0("Please provide a source authority to keep for overlapping zone ", opts_key))
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
      CWP.dataset::function_recap_each_step(
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
    message(paste0("Processing zone: ", zone_key))  # Log before processing
    
    # It's a good practice to use tryCatch to understand if errors in handle_overlap are stopping the loop
    tryCatch({
      handle_overlap(zone_key, zones_config[[zone_key]]$main, zones_config[[zone_key]]$default_strata, recap_step = recap_each_step)
    }, error = function(e) {
      message(paste0("Error encountered: ", e))  # Print errors to the console
    })
    
    message(paste0("Finished processing zone: ", zone_key))  # Log after processing
  }

  list(georef_dataset = georef_dataset, rawdata = rawdata, stepnumber = stepnumber)
}
