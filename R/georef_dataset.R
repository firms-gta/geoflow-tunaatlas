#' Georeferenced catch or effort dataset (level 0 or level 2)
#'
#' Builds a georeferenced dataset of the Global Tuna Atlas from the datasets of
#' the tRFMOs (level 0, or effort) or from a level 0/1 file (level 2):
#' gta_georef_level0(), then the filters on complete years and duplicated
#' units, gta_georef_level2() for the level 2, then the optional spatial
#' aggregation and filters. This is the body of the geoflow action
#' workflow/actions/create_georef_dataset.R, moved as is: the options keep
#' their names, meaning and defaults.
#'
#' @param files Names of the dataset files (tRFMO datasets, nominal catch...),
#'   in `data_dir`.
#' @param opts List of options of the dataset (fact, dataset_level, include_*,
#'   raising_georef_to_nominal...); the missing ones take their defaults.
#' @param con Connection to the Tuna Atlas database, or NULL (default): not
#'   needed with the default options.
#' @param entity A geoflow entity whose metadata the steps complete, or NULL.
#' @param data_dir Folder of the files.
#' @return A list: `georef_dataset` (data frame) and `opts` (the options with
#'   their defaults).
gta_georef_dataset <- function(files, opts, con = NULL, entity = NULL, data_dir = "data") {
  stepnumber <- 1
  stepLogger <- function(level, step, msg) {
    log_info(sprintf("LEVEL %s => STEP %s: %s", level, step, msg))
  }

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
    # Level 0 from the tRFMO datasets: core function gta_georef_level0()
    # (R/level0.R), on the files of the entity (data/ folder of the job)
    level0 <- gta_georef_level0(files, opts, con = con, entity = entity,
                                recap_each_step = recap_each_step,
                                filtering_on_minimum_year_declared = filtering_on_minimum_year_declared,
                                stepnumber = stepnumber, data_dir = data_dir)
    georef_dataset <- level0$georef_dataset
    rawdata <- level0$rawdata
    stepnumber <- level0$stepnumber
    rm(level0)
  } else {
    ## RETRIEVING DATA FROM DOI ---------------------------------------------------
    # Load georeferenced dataset
    if (file.exists((file.path(data_dir, opts$keylevel0))) && !opts$forceuseofdoi) {
      georef_dataset <- readr::read_csv((file.path(data_dir, opts$keylevel0)), guess_max = 0)
      class(georef_dataset$measurement_value) <- "numeric" 
    } else if (!is.null(opts$doilevel0)) {
      rec <- sub("^.*zenodo\\.(\\d+).*$", "\\1", opts$doilevel0)
      url <- sprintf("https://zenodo.org/records/%s/files/%s?download=1", rec, opts$keylevel0)
      download.file(url, file.path(data_dir, opts$keylevel0), mode = "wb")
      georef_dataset <- readr::read_csv(here::here(file.path(data_dir, opts$keylevel0)), guess_max = 0)
      class(georef_dataset$measurement_value) <- "numeric"
    } else {
      stop("Please provide a georeferenced catch dataset")
    }
    CWP.dataset::function_recap_each_step(
      "Level0_Firms",
      georef_dataset,
      paste0(
        "Retrieving level 0 data on the basis of the following DOI: ",
        opts$doi, " the key being: ", opts$key,". "),
      "download_zenodo_csv"  ,
      list(opts$doi, opts$key), entity
    )
  }
  
  
  
  # Filtering on complete year ----------------------------------------------
  if(DATASET_LEVEL == 2){
    if (file.exists(file.path(data_dir, "geographic_identifier_to_nominal.csv")) ) { # hotfix for upgrading_nominal_on_georef see upgrading_nominal_on_georef elsewhere 500
      geographic_identifier_to_nominal <- readr::read_csv(file.path(data_dir, "geographic_identifier_to_nominal.csv"))
      class(geographic_identifier_to_nominal$code) <- "character"
      geographic_identifier_to_nominal <- geographic_identifier_to_nominal%>% dplyr::distinct() %>% 
        dplyr::filter(!(code =="5233022" & geographic_identifier_nom == "WCPFC")) %>% #some grids that are in both juridiction areas but as we keep data from 
        dplyr::filter(!(code =="5304080" & geographic_identifier_nom == "WCPFC")) # and IIATTC in the overlapping we keep only the geographic_identifier from IOTC and IATTC
      
    } else if(!opts$upgrading_nominal_on_georef){
      warning("fastmode")
    } else {
      stop("Please provide a geographic identifier to nominal dataset")
    }
    
    Description <- paste0("As dataset is to be raised on the basis of nominal catch that are displayed with a time resolution of year, the data is filtered to keep, for each source_authority,", 
                          " only the years where catch data are provided from January to December. As well we remove stratas data displayed in number for WCPFC in 09.31 and IATTC in 09.32 for dataset in 5 degrees", 
                          "which corresponds to duplicated data to the equivalent stratas in tons.")
    
    
  } else {
    Description <- NULL
  }
  
  
  
  ### Removing duplicated data
  #issue(#48)
  if(opts$fact == "catch"){
    log_info(sprintf("Begin remove duplicated units %s", Sys.time()))  # Log after processing
    
    georef_dataset <- georef_dataset %>%
      dplyr::group_by(across(setdiff(colnames(.), c("measurement_value", "measurement_unit")))) %>%
      dplyr::mutate(nb_units = n_distinct(measurement_unit)) %>%
      dplyr::filter(!(measurement_unit == "no" & nb_units >= 2)) %>%
      dplyr::select(-nb_units) 

    CWP.dataset::function_recap_each_step(
      paste0("Removing_duplicated_units"),
      georef_dataset,
      "As some data is in fact duplicated for catch, we check all the duplicated data and remove the data in number when same dimensions",
      ""
    )
    log_info(sprintf("End remove duplicated units %s", Sys.time()))  # Log after processing
  }
  
  # LEVEL 1 IRD ---------------------------------------------------
  if(DATASET_LEVEL >= 2){
    # Level 2: core function gta_georef_level2() (R/level2.R)
    level2 <- gta_georef_level2(
      georef_dataset, opts, con = con,
      files = files,
      rawdata = if (exists("rawdata", inherits = FALSE)) rawdata else opts,
      geographic_identifier_to_nominal = if (exists("geographic_identifier_to_nominal", inherits = FALSE)) geographic_identifier_to_nominal else NULL,
      DATASET_LEVEL = DATASET_LEVEL, entity = entity, recap_each_step = recap_each_step,
      stepnumber = stepnumber, data_dir = data_dir)
    georef_dataset <- level2$georef_dataset
    stepnumber <- level2$stepnumber
    rm(level2)
  }
  
  # SPATIAL AGGREGATION ---------------------------------------------------
  if (!is.null(opts$aggregate_on_5deg_data_with_resolution_inferior_to_5deg)) if (opts$aggregate_on_5deg_data_with_resolution_inferior_to_5deg) {
    stepLogger(level = 0, step = stepnumber, msg = "Spatial Aggregation of data (5deg resolution datasets only: Aggregate data on 5o resolution quadrants)")
    stepnumber = stepnumber+1
    log_info("Aggregating data that are defined on quadrants or areas inferior to 5o quadrant resolution to corresponding 5o quadrant...")
    
    one_degree <- georef_dataset %>% dplyr::filter(substr(geographic_identifier, 1, 1) == "5")
    five_degree <- georef_dataset %>% dplyr::filter(substr(geographic_identifier, 1, 1) == "6")
    one_degree_aggregated <- one_degree %>% rowwise() %>% 
      dplyr::mutate(geographic_identifier = transform_cwp_code_from_1deg_to_5deg(geographic_identifier))
    
    # df_input_not_aggregated <- georef_dataset %>% dplyr::filter(is.null(geographic_identifier))
    # fwrite(df_input_not_aggregated, file.path(data_dir, "df_input_not_aggregated.csv"))
    
    georef_dataset <- as.data.frame(base::rbind(one_degree_aggregated, five_degree))
    
    log_info("Aggregating data that are defined on quadrants or areas inferior to 5o quadrant resolution to corresponding 5o quadrant OK")
    
    if(recap_each_step){
      names_list_aggregation <-
        c("df_input_not_aggregated", "stats_not_aggregated") #file we want to save
      
      # try(lapply(names_list_aggregation, function_write_RDS))
      
      CWP.dataset::function_recap_each_step(
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
  log_info("Grid spatial resolution filter")
  if (!is.null(opts$resolution_filter)) {
    
    filtering_resolution_filter <- function(datatable, first_digit) {
      filtered_data <- datatable[substr(datatable$geographic_identifier, 1, 1) == first_digit, ]
      return(filtered_data)
    }
    
    georef_dataset <- filtering_resolution_filter(georef_dataset, opts$resolution_filter)
    
    if(recap_each_step){
      CWP.dataset::function_recap_each_step(
        "filtering_on_spatial_resolution",
        georef_dataset,
        "This step is to filter on the wanted resolution.",
        "",
        list(options_resolution_filter), entity
      )
    }
  }
  
  log_info("Apply filters if filter needed (Filter data by groups of everything) ")
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
      log_info(sprintf("Filtering by gear(s) [%s]", paste(gear_filter, collapse=",")))	
      georef_dataset<-georef_dataset %>% dplyr::filter(gear_type %in% gear_filter)
      log_info("Filtering gears OK")
    }
    
    if(recap_each_step){
      CWP.dataset::function_recap_each_step(
        "Filtering on every other dimension",
        georef_dataset,
        paste0("Filtering on the following paremeters :", parameter_filtering),
        "dimension_filtering_function",
        list(opts$filtering), entity
      )
    }
  }
  
  # PROCESS AND AGGREGATE DATA ---------------------------------------------------

  list(georef_dataset = georef_dataset, opts = opts)
}
