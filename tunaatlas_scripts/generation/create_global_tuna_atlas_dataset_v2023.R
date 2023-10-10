function(action, entity, config) {
  opts <- action$options
  con <- config$software$input$dbi
  options(encoding = "UTF-8")
  ######################################################################
  ##### 52North WPS annotations ##########
  ######################################################################
  # wps.des: id = create_own_tuna_atlas_catch_effort, title = Create your own georeferenced Tuna Atlas dataset of catch or efforts, abstract = This algorithm allows to create own regional or global tuna atlas of geo-referenced gridded catch or efforts. It takes as input the public domain datasets of the five Tuna Regional Fisheries Management Organizations (tRFMOs) (IOTC ICCAT WCPFC IATTC CCSBT) stored within the Tuna atlas database. It proposes a set of parameters to customize the computation of the tuna atlas. ;
  # wps.in: id = fact, type = string, title = Variable output of the tuna atlas (catch or effort), value = "catch|effort";
  # wps.in: id = datasets_year_release, type = string, title = Year of release of the datasets by the tRFMOs. First available year is 2017. Usually datasets released in the year Y contain the time series from the beginning of the fisheries (e.g. 1950) to year Y-2 (included). For instance 2017 will extract the datasets released in 2017 and that cover the time series from 1950 to 2015 (included), value = "2017";
  # wps.in: id = iccat_ps_include_type_of_school, type = string, title = Concerns ICCAT Purse Seine data. Use only if parameter include_ICCAT is set to TRUE. ICCAT disseminates two catch-and-efforts datasets: one that provides the detail of the type of school (Fad|Free school) for purse seine fisheries only and that starts in 1994 (called Task II catch|effort by operation mode Fad|Free school) and one that does not provide the information of the type of school and that covers all the time period (from 1950) (called Task II catch|effort). These data are redundant (i.e. the data from the dataset Task II catch|effort by operation mode are also available in the dataset Task II catch|effort but in the latter the information on the type of school is not available). Combine both datasets to produce a dataset with fishing mode information (Fad | Free school)? TRUE : both datasets will be combined to produce a dataset with fishing mode information (Fad | free school). FALSE : Only the dataset without the type of school will be used. In that case the output dataset will not have the information on the fishing mode for ICCAT., value = "TRUE|FALSE";
  # wps.in: id = iattc_ps_raise_flags_to_schooltype, type = string, title = Concerns IATTC Purse Seine data. Use only if parameter include_IATTC is set to TRUE. For confidentiality policies information on fishing country (flag) and type of school for the geo-referenced catches is available in separate files for the Eastern Pacific Ocean purse seine datasets. IATTC hence provides two public domain dataset: one with the information on the type of school and one with the information on the flag. Both datasets can be combined - using raising methods - to estimate the catches by both flag and type of school for purse seine fisheries. Combine both datasets? TRUE : both datasets (by fishing mode and by flag) will be combined - using raising methods i.e. dataset with the information on the flag will be raised to the dataset with the information on the type of school - to have the detail on both fishing mode and flag for each stratum. FALSE : Only one dataset will be used (either Flag or Type of school). The parameter dimension_to_use_if_no_raising_flags_to_schooltype allows to decide which dataset to use., value = "TRUE|FALSE";
  # wps.in: id = iattc_ps_dimension_to_use_if_no_raising_flags_to_schooltype, type = string, title = Concerns IATTC Purse Seine data. Use only if parameter include_IATTC is set to TRUE and parameter iattc_raise_flags_to_schooltype is set to FALSE. In the case IATTC purse seine datasets are not combined (see description of parameter iattc_raise_flags_to_schooltype) which dataset to use? flag : use dataset with the information on flag. Information on type of school will therefore not be available. schooltype : use dataset with the information on type of school. Information on flag will therefore not be available., value = "flag|schooltype";
  # wps.in: id = iattc_ps_catch_billfish_shark_raise_to_effort, type = boolean, title = Concerns IATTC Purse Seine data. Use only if parameter include_IATTC is set to TRUE and parameter variable is set to 'catch'. In addition to the separation flag / schooltype (see above) IATTC Purse seine catch-and-effort are available in 3 separate files according to the group of species: tuna, billfishes, sharks. This is due to the fact that PS data is collected from 2 sources: observer and fishing vessel logbooks. Observer records are used when available, and for unobserved trips logbooks are used. Both sources collect tuna data but only observers collect shark and billfish data. So as an example a strata may have observer effort and the number of sets from the observed trips would be counted for tuna and shark and billfish. But there may have also been logbook data for unobserved sets in the same strata so the tuna catch and number of sets for a cell would be added. This would make a higher total number of sets for tuna catch than shark or billfish. So efforts in the billfish and shark datasets might represent only a proportion of the total effort allocated in some strata since it is the observed effort i.e. for which there was an observer onboard. As a result catch in the billfish and shark datasets might represent only a proportion of the total catch allocated in a some strata. TRUE: Raise billfish (resp. shark) catch to the ratio  effort tuna / effort billfish (resp. shark). FALSE: Keep billfish (resp. shark) catch as they are provided in the billfish (resp. shark) catch datasets., value = "TRUE|FALSE";
  # wps.in: id = mapping_map_code_lists, type = string, title = Map code lists (gears|species|flags|schooltypes|catchtype)? When using multiple sources of data (i.e. multiple RFMOS) code lists used by the various tRFMOs might be different. They should therefore be mapped to single code lists in order to be able to compare the data. TRUE : map code lists. The url to the datasets providing the code list mappings to use must be set in the parameter mapping_source_mappings. See parameter mapping_source_mappings for more details. FALSE : do not map code lists. Output data will use input codes., value = "TRUE|FALSE" ;
  # wps.in: id = mapping_csv_mapping_datasets_url, type = string, title = Use only if parameter mapping_map_code_lists is set to TRUE. Path to the CSV file containing the dimensions that must be mapped and the name of the mapping dataset for each dimension mapped. The mapping datasets must be available in Tuna atlas database.  A template can be found here: https://goo.gl/YZmeDV . , value="http://data.d4science.org/ZWFMa3JJUHBXWk9NTXVPdFZhbU5BUFEyQnhUeWd1d3lHbWJQNStIS0N6Yz0" ;
  # wps.in: id = mapping_keep_src_code, type = string, title = Use only if parameter mapping_map_code_lists is set to TRUE. In case of code list mapping (mapping_map_code_lists is TRUE) keep source coding system column? TRUE : conserve in the output dataset both source and target coding systems columns. FALSE : conserve only target coding system. , value="FALSE|TRUE" ;
  # wps.in: id = gear_filter, type = string, title = Filter data by gear. Gear codes in this parameter must by the same as the ones used in the catch dataset (i.e. raw tRFMOs gear codes if no mapping or in case of mapping (mapping_map_code_lists is TRUE) codes used in the mapping code list). NULL : do not filter. If you want to filter you must write the codes to filter by separated by a comma in case of multiple codes.  , value = "NULL";
  # wps.in: id = unit_conversion_convert, type = string, title = Convert units of measure? if TRUE you must fill in the parameter unit_conversion_df_conversion_factor. , value = "FALSE|TRUE";
  # wps.in: id = unit_conversion_csv_conversion_factor_url, type = string, title = Use only if parameter unit_conversion_convert is set to TRUE. If units are converted path to the csv containing the conversion factors dataset. The conversion factor dataset must be properly structured. A template can be found here: https://goo.gl/i7QJYC . The coding systems used in the dimensions of the conversion factors must be the same as the ones used in the catch dataset (i.e. raw tRFMOs codes or in case of mapping (mapping_map_code_lists isTRUE) codes used in the mapping code lists) except for spatial code list. Additional information on the structure are provided here: https://ptaconet.github.io/rtunaatlas//reference/convert_units.html , value = opts$unit_conversion_csv_conversion_factor_url;
  # wps.in: id = unit_conversion_codelist_geoidentifiers_conversion_factors, type = string, title = Use only if parameter unit_conversion_convert is set to TRUE. If units are converted name of the coding system of the spatial dimension used in the conversion factor dataset (i.e. identifier of the layer in the Tuna atlas database)., value = "areas_conversion_factors_numtoweigth_ird";
  # wps.in: id = raising_georef_to_nominal, type = string, title =  Geo-referenced catch data and associated effort can represent only part of the total catches. Raise georeferenced catches to nominal catches? Depending on the availability of the flag dimension (currently not available for the geo-referenced catch-and-effort dataset from the WCPFC and CCSBT) the dimensions used for the raising are either {Flag|Species|Year|Gear} or {Species|Year|Gear}. Some catches cannot be raised because the combination {Flag|Species|Year|Gear} (resp. {Species|Year|Gear}) does exist in the geo-referenced catches but the same combination does not exist in the total catches. In this case non-raised catch data are kept. TRUE : Raise georeferenced catches to total catches. FALSE : Do not raise., value = "TRUE|FALSE";
  # wps.in: id = raising_do_not_raise_wcfpc_data, type = string, title =  WCPFC georeferenced data are already raised [terminer la description]. TRUE : Do not Raise WCPFC georeferenced catches to total catches. FALSE : Raise., value = "TRUE|FALSE";
  # wps.in: id = raising_raise_only_for_PS_LL, type = string, title =  Raise the data only for Purse Seiners and Longliners (i.e. do not raise for artisanal fisheries) [terminer la description]. TRUE : Raise only for PS and LL. FALSE : Raise for all gears., value = "TRUE|FALSE";
  # wps.in: id = aggregate_on_5deg_data_with_resolution_inferior_to_5deg, type = string, title =  Aggregate data that are defined on quadrants or areas inferior to 5° quadrant resolution to corresponding 5° quadrant? TRUE : Aggregate. Data that are provided at spatial resolutions superior to 5° x 5°  will be aggregated to the corresponding 5° quadrant. FALSE : Do not aggregate. Data that are provided at spatial resolutions superior to 5° x 5° will be will be kept as so. , value = "TRUE|FALSE";
  # wps.in: id = disaggregate_on_5deg_data_with_resolution_superior_to_5deg, type = string, title = What to do with data that are defined on quadrants or areas superior to 5° quadrant resolution to 5° quadrant? none: Do not do anything. Data that are provided at spatial resolutions inferior to 5° x 5° will be kept as so. disaggregate : data that are provided at spatial resolutions superior to 5° x 5°  will be disaggregated to the corresponding 5°  x 5°  quadrants by dividing the catch equally on the overlappings 5° x 5°  quadrants. remove : Data that are provided at spatial resolutions superior to 5° x 5°  will be removed from the dataset. , value = "none|disaggregate|remove";
  # wps.in: id = disaggregate_on_1deg_data_with_resolution_superior_to_1deg, type = string, title = Same as parameter disaggregate_on_5deg_data_with_resolution_superior_to_5deg but for 1° resolutions  , value = "none|disaggregate|remove";
  # wps.in: id = spatial_curation_data_mislocated, type = string, title = Some data might be mislocated: either located on land areas or without any area information. This parameter allows to control what to do with these data. reallocate : Reallocate the mislocated data (equally distributed on areas with same dimensions (month|gear|flag|species|schooltype). no_reallocation : do not reallocate mislocated data. The output dataset will keep these data with their original location (eg on land or with no area information). remove : remove the mislocated data., value = "remove|reallocate|no_reallocation";
  # wps.in: id = overlapping_zone_iattc_wcpfc_data_to_keep, type = string, title = Concerns IATTC and WCPFC data. IATTC and WCPFC have an overlapping area in their respective area of competence. Which data should be kept for this zone? IATTC : keep data from IATTC. WCPFC : keep data from WCPFC. NULL : Keep data from both tRFMOs. Caution: with the option NULL data in the overlapping zone are likely to be redundant., value = "IATTC|WCPFC|NULL";
  # wps.in: id = SBF_data_rfmo_to_keep, type = string, title = Concerns Southern Bluefin Tuna (SBF) data. Use only if parameter fact is set to 'catch' and parameter include_CCSBT is set to TRUE. SBF tuna data do exist in both CCSBT data and the other tuna RFMOs data. Wich data should be kept? CCSBT : CCSBT data are kept for SBF. other_trfmos : data from the other TRFMOs are kept for SBF. NULL : Keep data from all the tRFMOs. Caution: with the option NULL data in the overlapping zones are likely to be redundant., value = "CCSBT|other_trfmos|NULL";
  # wps.out: id = zip_namefile, type = text/zip, title = Outputs are 3 csv files: the dataset of georeferenced catches + a dataset of metadata (including informations on the computation, i.e. how the primary datasets were transformed by each correction) [TO DO] + a dataset providing the code lists used for each dimension (column) of the output dataset [TO DO]. All outputs and codes are compressed within a single zip file. ;
  #packages
  
  if (!require(dplyr)) {
    install.packages("dplyr")
    require(dplyr)
  }
  if (!require(sf)) {
    install.packages("sf")
    require(sf)
  }
  if (!require(stringr)) {
    install.packages("stringr")
    require(stringr)
  }
  
  if (!require(R3port)) {
    install.packages("R3port")
    require(R3port)
  }
  
  if (!require(reshape2)) {
    install.packages("reshape2")
    require(reshape2)
  }
  
  if (!require(readr)) {
    install.packages("readr")
    require(readr)
  }
  if (!require(tools)) {
    install.packages("tools")
    require(tools)
  }
  
  if (!require(RPostgreSQL)) {
    install.packages("RPostgreSQL")
    require(RPostgreSQL)
  }
  if (!require(DBI)) {
    install.packages("DBI")
    require(DBI)
  }
  
  if (!require(googledrive)) {
    install.packages("googledrive")
    require(googledrive)
  }
  
  stepnumber <- 1
  
  #scripts
  url_scripts_create_own_tuna_atlas <- "https://raw.githubusercontent.com/firms-gta/geoflow-tunaatlas/master/tunaatlas_scripts/generation"
  #for level 0 - FIRMS
  source(file.path(url_scripts_create_own_tuna_atlas, "get_rfmos_datasets_level0.R")) #modified for geoflow
  source(file.path(url_scripts_create_own_tuna_atlas, "retrieve_nominal_catch.R")) #modified for geoflow
  source(file.path(url_scripts_create_own_tuna_atlas, "map_codelists.R")) #modified for geoflow
  source(file.path(url_scripts_create_own_tuna_atlas, "function_overlapped.R")) # adding this function as overlapping is now a recurent procedures for several overlapping 
  
  #for filtering if needed
  source(file.path(url_scripts_create_own_tuna_atlas, "dimension_filtering_function.R")) # adding this function as overlapping is now a recurent procedures for several overlapping 
  
  
  #for level 1 - FIRMS (candidate)
  source(file.path(url_scripts_create_own_tuna_atlas, "spatial_curation_data_mislocated.R")) #modified for geoflow
  source(file.path(url_scripts_create_own_tuna_atlas, "double_unit_data_handling.R")) # new function for double unit
  source(file.path(url_scripts_create_own_tuna_atlas, "do_unit_conversion.R"))

  #for level 2 - IRD
  source(file.path(url_scripts_create_own_tuna_atlas, "disaggregate_on_resdeg_data_with_resolution_superior_to_resdeg.R"))
  source(file.path(url_scripts_create_own_tuna_atlas, "function_raising_georef_to_nominal.R")) #modified for geoflow

  #stepLog
  stepLogger = function(level, step, msg){
	config$logger.info(sprintf("LEVEL %s => STEP %s: %s", level, step, msg))
  }
  
  #for reporting
  source("https://raw.githubusercontent.com/firms-gta/geoflow-tunaatlas/master/Analysis_markdown/functions/write_options_to_csv.R")
  source("https://raw.githubusercontent.com/firms-gta/geoflow-tunaatlas/master/Analysis_markdown/functions/function_recap_each_step.R") # new function to create rds for each treatment
  source("https://raw.githubusercontent.com/firms-gta/geoflow-tunaatlas/master/Analysis_markdown/functions/copyrmd.R")
  # Saving options in a csv file and creating a new variable for each options
  write_options_to_csv(opts)
  
  # #############
  #action options
  filtering_on_minimum_year_declared = if(!is.null(opts$filtering_on_minimum_year_declared)) opts$filtering_on_minimum_year_declared else TRUE
  recap_each_step = if(!is.null(opts$recap_each_step)) opts$recap_each_step else FALSE
  
  #Identify expected Level of processing
  DATASET_LEVEL <- opts$dataset_level
  #===========================================================================================================================================================
  #===========================================================================================================================================================
  #>(||||*> LEVEL 0 FIRMS PRODUCTS
  #===========================================================================================================================================================
  #===========================================================================================================================================================
  
  #### 1) Retrieve tuna RFMOs data from Tuna atlas DB at level 0. Level 0 is the merging of the tRFMOs primary datasets, with the more complete possible value of georef_dataset per stratum (i.e. duplicated or splitted strata among the datasets are dealt specifically -> this is the case for ICCAT and IATTC)  ####
  config$logger.info("Begin: Retrieving primary datasets from Tuna atlas DB... ")
  
  #-------get_rfmos_datasets_level0------------------------------------------------------------------------------------------------------------------------------
  stepLogger(level = 0, step = stepnumber, msg = "Retrieve georeferenced catch or effort (+ processings for IATTC) AND NOMINAL CATCH if asked")
  stepnumber = stepnumber+1
  #-------------------------------------------------------------------------------------------------------------------------------------
  if(!exists("data/rawdata.rds")){
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
  ### next steps to correct identifier incorrect of iotc
  
  ### filtering on minimum time_start (TRUE  by default)

  if (filtering_on_minimum_year_declared) {
	stepLogger(level = 0, step = stepnumber, msg = "Filtering on minimum time_start")
	stepnumber = stepnumber+1
    
    # extract the maximum year of declaration for each source_authority
    max_years <- georef_dataset %>%
      group_by(source_authority) %>%
      summarise(max_time_start = max(time_start))
    
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
  } else {
    georef_dataset <- readRDS("data/rawdata.rds")
  }
  if(recap_each_step){
	  function_recap_each_step(
		"rawdata",
		georef_dataset,
		"Retrieve georeferenced catch or effort : In this step, the georeference data of the included (in options) rfmos, are binded.",
		"get_rfmos_datasets_level0"  ,
		list(
		)
	  )
	  saveRDS(georef_dataset, "data/rawdata.rds")
  }
  
  
  # for(files in list.files("data")){
  #   if(str_contains("catch", "effort" does not contain "codelist" "conversion factors"))
  #     
  #   i <- read_csv()
  #   i <- map_codelist(i)
  # }
  # -------------------------------------------------------------------------
  # PROCESSINGS FOR IATTC data
  
  if (opts$iattc_ps_raise_flags_to_schooltype) {
    rawdata$iattc_ps_raise_flags_to_schooltype <-
      opts$iattc_ps_raise_flags_to_schooltype
    
    
    #---------Enriching data with schootype for iattc----------------------------------------------------------------------------------------------------------------------------
    stepLogger(level = 0, step = stepnumber, msg = "Enriching data with schootype for IATTC if needed")
	stepnumber = stepnumber+1
    #-------------------------------------------------------------------------------------------------------------------------------------
    iattc <-
      get_rfmos_datasets_level0("IATTC", entity, config, rawdata)
    iattc$time_start <- substr(as.character(iattc$time_start), 1, 10)
    iattc$time_end <- substr(as.character(iattc$time_end), 1, 10)
    class(iattc$measurement_value) <- "numeric"
    
    georef_dataset <-
      rbind(georef_dataset %>% filter(source_authority != "IATTC"),
            iattc)
    rm(iattc)
    
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
		  "raise_datasets_by_dimension"  ,
		)
	}
    ################
  }
  if (opts$iattc_ps_catch_billfish_shark_raise_to_effort) {
    rawdata$iattc_ps_catch_billfish_shark_raise_to_effort <-
      opts$iattc_ps_catch_billfish_shark_raise_to_effort
    
    #----------Raising catch data to effort for iattc---------------------------------------------------------------------------------------------------------------------------
    stepLogger(level = 0, step = stepnumber, msg = "Raising catch data to effort for IATTC")
	stepnumber = stepnumber+1
    #-------------------------------------------------------------------------------------------------------------------------------------
    iattc <-
      get_rfmos_datasets_level0("IATTC", entity, config, rawdata)
    iattc$time_start <- substr(as.character(iattc$time_start), 1, 10)
    iattc$time_end <- substr(as.character(iattc$time_end), 1, 10)
    class(iattc$measurement_value) <- "numeric"
    
    georef_dataset <-
      rbind(georef_dataset %>% filter(source_authority != "IATTC"),
            iattc)
    rm(iattc)
    
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
		  "get_rfmos_datasets_level0"  ,
		)
	}
    ################
  }
  
  #----------Standardizing unit of measures---------------------------------------------------------------------------------------------------------------------------
  stepLogger(level = 0, step = stepnumber, msg = "Standardizing unit of measures")
  stepnumber = stepnumber+1
  #-------------------------------------------------------------------------------------------------------------------------------------
  unit_weight_to_remap = c("MT")
  unit_number_to_remap = c("NO")
  if(DATASET_LEVEL == 0){
	#in the case we are tackling the production of a level 0, we need to replace MT,MTNO by "t" and NO,NOMT by "no"
	#MTNO and NOMT are code artefacts created at preharmonization level (now targeting essentially IATTC for catch)
	unit_weight_to_remap = c("MT", "MTNO")
	unit_number_to_remap = c("NO", "NOMT")
  }
  georef_dataset <- georef_dataset %>% 
	dplyr::mutate( 
		measurement_unit = case_when(
			measurement_unit %in% unit_weight_to_remap ~ "t", 
			measurement_unit %in% unit_number_to_remap ~ "no", 
			TRUE ~ measurement_unit
		)
	)
  
  #----------Map code lists -------------------------------------------------------------------------------------------------------------------------------------------------
  #Map to CWP standard codelists (if not provided by tRFMO according to the CWP RH standard data exchange format)
  #--------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  options("OutDec" = ".")
  source_authority_to_map = if(!is.null(opts$source_authority_to_map)) opts$source_authority_to_map else c("CCSBT", "IATTC", "WCPFC")
  
  if (!is.null(opts$mapping_map_code_lists)){
    if (opts$mapping_map_code_lists) {
	  stepLogger(level = 0, step = stepnumber, msg = "Map to CWP standard codelists (if not provided by tRFMO according to the CWP RH standard data exchange format)")
      stepnumber = stepnumber+1
      config$logger.info(
        "Reading the CSV containing the dimensions to map + the names of the code list mapping datasets. Code list mapping datasets must be available in the database."
      )
      mapping_csv_mapping_datasets_url <- "https://raw.githubusercontent.com/fdiwg/fdi-mappings/main/global/firms/gta/codelist_mapping_rfmos_to_global.csv"
      mapping_dataset <-
        read.csv(
          mapping_csv_mapping_datasets_url,
          stringsAsFactors = F,
          colClasses = "character"
        )
      mapping_keep_src_code <- if (!is.null(opts$mapping_keep_src_code)) opts$mapping_keep_src_code else FALSE
      
      config$logger.info("Mapping code lists of georeferenced datasets...")
      mapping_codelist <-
        map_codelists(
          con,
          opts$fact,
          mapping_dataset = mapping_dataset,
          dataset_to_map = georef_dataset,
          mapping_keep_src_code,
          summary_mapping = TRUE,
          source_authority_to_map = source_authority_to_map
        ) #this map condelist function is to retrieve the mapping dataset used
      
      georef_dataset <- mapping_codelist$dataset_mapped
    
      config$logger.info("Mapping code lists of georeferenced datasets OK")

      
	  if(recap_each_step){
	  
		  recap_mapping <- mapping_codelist$recap_mapping
		  stats_total <- mapping_codelist$stats_total
		  not_mapped_total <- mapping_codelist$not_mapped_total
	  
		  names_list <-
			c("recap_mapping", "stats_total", "not_mapped_total") #file we want to save
		  
		  function_write_RDS = function(name) {
			file_name <- paste0("data/", name, ".rds")
			object_list <- mget(name, envir = globalenv())
			if (!is.null(object_list[[1]])) {
			  object_df <- object_list[[1]]
			  saveRDS(object_df, file = file_name)
			} else {
			  config$logger.info(sprintf("Skipping %s: Object is NULL\n", name))
			}
		  }
		  
		  try(lapply(names_list, function_write_RDS))
		  
		  
		  
		  
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
	"map_codelists",
			list(options_mapping_map_code_lists)
		  )
		  
		  

	  }
    }
  }
	  #Filter on species under mandate for FIRMS level 0
	  #-------------------------------------------------------------------------------------------------
	  stepLogger(level = 0, step = stepnumber, msg = "Filter on species under mandate for FIRMS level 0")
      stepnumber = stepnumber+1
	  
	  # Temporary patch for ASFIS RMJ --> RMM
	  georef_dataset <- georef_dataset %>% dplyr::mutate(species = case_when(species == "RMJ" ~ "RMM", TRUE ~ species))
	  
	  # Filtering on species under mandate --------------------------------------
	  # done base on mapping between source_authority (tRFMO) and species
	  stepLogger(level = 0, step = stepnumber, msg = "Filtering on species under mandate, targeted by GTA")
	  stepnumber = stepnumber +1
	  #url_asfis_list <- "https://raw.githubusercontent.com/fdiwg/fdi-codelists/main/global/firms/gta/cl_species_level0.csv"
	  
	  url_mapping_asfis_rfmo = "https://raw.githubusercontent.com/fdiwg/fdi-mappings/main/cross-term/codelist_mapping_source_authority_species.csv"
	  species_to_be_kept_by_rfmo_in_level0 <- readr::read_csv(url_mapping_asfis_rfmo)
	  georef_dataset <- georef_dataset %>% dplyr::inner_join(species_to_be_kept_by_rfmo_in_level0, by = c("species" = "species", "source_authority" = "source_authority"))
	  
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
	  
      
    
  
  
  #--------Overlapping zone (IATTC/WCPFC)---------------------------------------------------------------------------------------------------------------------
  #Overlapping zone (IATTC/WCPFC): keep data from IATTC or WCPFC?
  #-----------------------------------------------------------------------------------------------------------------------------------------------------------
  
  if (!is.null(opts$overlapping_zone_iattc_wcpfc_data_to_keep)) {
	stepLogger(level = 0, step = stepnumber, msg = "Overlapping zone (IATTC/WCPFC): keep data from IATTC or WCPFC?")
	stepnumber = stepnumber+1
    if (!exists("opts$strata_overlap_iattc_wcpfc")) {
      options_strata_overlap_iattc_wcpfc <-
        c("geographic_identifier",    "species", "year")
    } else {
      options_strata_overlap_iattc_wcpfc <-
        unlist(strsplit(opts$strata_overlap_iattc_wcpfc , split = ","))
    }
    config$logger.info(paste0(options_strata_overlap_iattc_wcpfc))
    
    formals(function_overlapped)$opts <- opts
    
    georef_dataset <-
      function_overlapped(
        dataset = georef_dataset ,
        con = con ,
        rfmo_to_keep = overlapping_zone_iattc_wcpfc_data_to_keep,
        rfmo_not_to_keep = (if (overlapping_zone_iattc_wcpfc_data_to_keep == "IATTC") {
          "WCPFC"
        } else {
          "IATTC"
        }),
        strata = options_strata_overlap_iattc_wcpfc
      )
	  
	if(recap_each_step){
		function_recap_each_step(
		  "overlap_iattc_wcpfc",
		  georef_dataset,
		  "In this step, the georeferenced data present on the overlapping zone between IATTC and WCPFC is handled. The option for the strata overlapping allow to handle the maximum similarities allowed between two data to keep both.",
		  "function_overlapped" ,
		  list(
			options_overlapping_zone_iattc_wcpfc_data_to_keep,
			options_strata_overlap_iattc_wcpfc
		  )
		)
    }
    
  }
  
  #--------Overlapping zone (IOTC/WCPFC)----------------------------------------------------------------------------------------------------------------------
  #Overlapping zone (IOTC/WCPFC): keep data from IOTC or WCPFC?
  #-----------------------------------------------------------------------------------------------------------------------------------------------------------
  
  if (!is.null(opts$overlapping_zone_iotc_wcpfc_data_to_keep)) {
	stepLogger(level = 0, step = stepnumber, msg = "Overlapping zone (IOTC/WCPFC): keep data from IOTC or WCPFC?")
	stepnumber = stepnumber+1
    # overlapping_zone_iotc_wcpfc_data_to_keep <- opts$overlapping_zone_iotc_wcpfc_data_to_keep
    if (!exists("opts$strata_overlap_iotc_wcpfc")) {
      options_strata_overlap_iotc_wcpfc <-
        c("geographic_identifier",
          "species",
          "year",
          "fishing_fleet")
    } else {
      options_strata_overlap_iotc_wcpfc <-
        unlist(strsplit(opts$strata_overlap_iotc_wcpfc, split = ","))
    }
    
    
    georef_dataset <-
      function_overlapped(
        georef_dataset,
        con,
        rfmo_to_keep = opts$overlapping_zone_iotc_wcpfc_data_to_keep,
        rfmo_not_to_keep = (if (opts$overlapping_zone_iotc_wcpfc_data_to_keep == "IOTC") {
          "WCPFC"
        } else {
          "IOTC"
        }) ,
        strata = options_strata_overlap_iotc_wcpfc
      )
    config$logger.info(
      paste0(
        "Keeping only data from ",
        opts$overlapping_zone_iotc_wcpfc_data_to_keep,
        " in the IOTC/WCPFC overlapping zone..."
      )
    )
    
    config$logger.info(
      paste0(
        "Keeping only data from ",
        opts$overlapping_zone_iotc_wcpfc_data_to_keep,
        " in the IOTC/WCPFC overlapping zone OK"
      )
    )
    
	if(recap_each_step){
		function_recap_each_step(
		  "overlap_iotc_wcpfc",
		  georef_dataset,
		  paste0(
			"In this step, the georeferenced data present on the overlapping zone between IOTC and WCPFC is handled.
					   The option for the strata overlapping allow to handle the maximum similarities allowed between two data to keep both.
					   In the case the data is identical on the stratas privided, the remaining data is from ",
			opts$overlapping_zone_iotc_wcpfc_data_to_keep
		  ) ,
		  "function_overlapped",
		  list(
			options_overlapping_zone_iotc_wcpfc_data_to_keep,
			options_strata_overlap_iotc_wcpfc
		  )
		)
    }
  }
  
  
  #-------------Overlap sbf-------------------------------------------------------------------------------------------------------------------------------------
  #Overlapping on SBF
  #-------------------------------------------------------------------------------------------------------------------------------------------------------------
  
	if(!exists("opts$strata_overlap_sbf")){
		options_strata_overlap_sbf <- c("species")	
	} else {  
		config$logger.info("Attention you are providing options for Southern Bluefin Tuna possible overlapping data")
		options_strata_overlap_sbf <- unlist(strsplit(opts$strata_overlap_sbf, split = ","))
	}
    
	rfmo_to_keep = "CCSBT"
	
	#-----------------------------------------------------------------------------------------------------------------------------------------------------------
	#Overlapping zone (WCPFC/CCSBT): keep data from WCPFC or CCSBT?
    #-----------------------------------------------------------------------------------------------------------------------------------------------------------
    
	  stepLogger(level = 0, step = stepnumber, msg = "Overlapping zone (WCPFC/CCSBT): keep data from WCPFC or CCSBT?")
	  stepnumber = stepnumber+1
      config$logger.info("Keeping only data from CCSBT in the WCPFC/CCSBT overlapping zone...") 
      georef_dataset <-
        function_overlapped(
          dataset = georef_dataset,
          con = con,
          rfmo_to_keep = rfmo_to_keep,
          rfmo_not_to_keep = "WCPFC",
          strata = options_strata_overlap_sbf,
		  opts = opts
        )   
      config$logger.info("Keeping only data from CCSBT in the WCPFC/CCSBT overlapping zone OK")
      
	  if(recap_each_step){
		  function_recap_each_step(
			"overlap_ccsbt_wcpfc",
			georef_dataset,
			paste0(
			  "In this step, the georeferenced data present on the overlapping zone between CCSBT and WCPFC is handled.",
			  "The option for the strata overlapping allow to handle the maximum similarities allowed between two data to keep both.",
			  "In the case the data is identical on the stratas privided, the remaining data is from ",
			  rfmo_to_keep
			),
			"function_overlapped",
			list(
			  options_strata_overlap_sbf
			)
		  )
      }
    
    
    #-----------------------------------------------------------------------------------------------------------------------------------------------------------
	#Overlapping zone (ICCAT/CCSBT): keep data from ICCAT or CCSBT?
    #-----------------------------------------------------------------------------------------------------------------------------------------------------------
    
		stepLogger(level = 0, step = stepnumber, msg = "Overlapping zone (ICCAT/CCSBT): keep data from ICCAT or CCSBT?")
		stepnumber = stepnumber+1
		config$logger.info("Keeping only data from CCSBT in the ICCAT/CCSBT overlapping zone...")
		georef_dataset <- function_overlapped(
          dataset = georef_dataset,
          con = con,
          rfmo_to_keep = "CCSBT",
          rfmo_not_to_keep = "ICCAT",
          strata = options_strata_overlap_sbf,
		  opts = opts
        )
		config$logger.info("Keeping only data from ICCAT in the ICCAT/CCSBT overlapping zone OK")
      
		if(recap_each_step){
		  function_recap_each_step(
			"overlap_iccat_ccsbt",
			georef_dataset,
			paste0(
			  "In this step, the georeferenced data present on the overlapping zone between ICCAT and CCSBT is handled.",
			  "The option for the strata overlapping allow to handle the maximum similarities allowed between two data to keep both.",
			  "In the case the data is identical on the stratas privided, the remaining data is from ",
			  rfmo_to_keep
			),
			
			"function_overlapped",
			list(
			  options_strata_overlap_sbf
			)
		  )
		}
      
    
    
    #-----------------------------------------------------------------------------------------------------------------------------------------------------------
	#Overlapping zone (IOTC/CCSBT): keep data from IOTC or CCSBT?
    #-----------------------------------------------------------------------------------------------------------------------------------------------------------
    
		stepLogger(level = 0, step = stepnumber, msg = "Overlapping zone (IOTC/CCSBT): keep data from IOTC or CCSBT?")
		stepnumber = stepnumber+1
		config$logger.info("Keeping only data from CCSBT in the IOTC/CCSBT overlapping zone...")
		georef_dataset <- function_overlapped(
          dataset = georef_dataset,
          con = con,
          rfmo_to_keep = "CCSBT",
          rfmo_not_to_keep = "IOTC",
          strata = options_strata_overlap_sbf,
		  opts = opts
        )
		config$logger.info("Keeping only data from IOTC in the IOTC/CCSBT overlapping zone OK")
      
		if(recap_each_step){
		  function_recap_each_step(
			"overlap_iotc_ccsbt",
			georef_dataset,
			paste0(
			  "In this step, the georeferenced data present on the overlapping zone between IOTC and CCSBT is handled.
								The option for the strata overlapping allow to handle the maximum similarities allowed between two data to keep both.",
			  "In the case the data is identical on the stratas privided, the remaining data is from ",
			  rfmo_to_keep
			),
			
			"function_overlapped",
			list(
			  options_strata_overlap_sbf
			)
		  )
		}
    
  
  
  #--------irregular areas------------------------------------------------------------------------------------------------------------------------------------
  #Irregular areas handling in case of area not corresponding to cwp 
  #-----------------------------------------------------------------------------------------------------------------------------------------------------------
  spatial_curation = NULL
  if(!is.null(opts$irregular_area)) if (opts$irregular_area %in% c("remove", "reallocate")) {
	stepLogger(level = 0, step = stepnumber, msg = "Irregular areas handling")
	stepnumber = stepnumber+1
    source(file.path(url_scripts_create_own_tuna_atlas, 'spatial_curation.R'))
    spatial_curation <-
      spatial_curation(con, georef_dataset, opts$irregular_area)
    georef_dataset <- spatial_curation$df
    removed_irregular_areas <-
      spatial_curation$df_input_areas_not_curated
    stats_irregular_areas <- spatial_curation$stats
    
	if(recap_each_step){
		function_recap_each_step(
		  "irregular_area_handling",
		  georef_dataset,
		  paste0(
			"In this step, we handle areas that does not match cwp grids norme"
		  ) ,
		  "function_overlapped",
		  list(options_irregular_area)
		)
		names_list_irregular_areas <-
		c("removed_irregular_areas", "stats_irregular_areas") #file we want to save
	  
		try(lapply(names_list_irregular_areas, function_write_RDS))
    }
  }

  
  #------Spatial aggregation of data------------------------------------------------------------------------------------------------------------------------
  #Spatial Aggregation of data (5deg resolution datasets only: Aggregate data on 5° resolution quadrants)
  #-----------------------------------------------------------------------------------------------------------------------------------------------------------
  if (!is.null(opts$aggregate_on_5deg_data_with_resolution_inferior_to_5deg)) if (opts$aggregate_on_5deg_data_with_resolution_inferior_to_5deg) {
	  stepLogger(level = 0, step = stepnumber, msg = "Spatial Aggregation of data (5deg resolution datasets only: Aggregate data on 5° resolution quadrants)")
	  stepnumber = stepnumber+1
      config$logger.info("Aggregating data that are defined on quadrants or areas inferior to 5° quadrant resolution to corresponding 5° quadrant...")
      source(file.path(
        url_scripts_create_own_tuna_atlas,
        "aggregate_resolution.R"
      )) #modified for geoflow
      aggregated <- aggregate_resolution(con, georef_dataset, 6)
      df_input_not_aggregated <- aggregated$df_input_not_aggregated
      stats_not_aggregated <- aggregated$stats_not_aggregated
      georef_dataset <- aggregated$df
      
   
      config$logger.info("Aggregating data that are defined on quadrants or areas inferior to 5° quadrant resolution to corresponding 5° quadrant OK")
      
	  if(recap_each_step){
		  names_list_aggregation <-
			c("df_input_not_aggregated", "stats_not_aggregated") #file we want to save
		  
		  try(lapply(names_list_aggregation, function_write_RDS))

		  function_recap_each_step(
			"Aggregation",
			georef_dataset,
			"This step is to aggregate data on resolution lower than 5° in 5°.",
			"spatial_curation_upgrade_resolution",
			list(
			  options_aggregate_on_5deg_data_with_resolution_inferior_to_5deg
			)
		  )
      }  
	}
	
	#-----------spatial_curation_data_mislocated------------------------------------------------------  
	if(is.null(opts$spatial_curation_data_mislocated)) if(DATASET_LEVEL == 0) opts$spatial_curation_data_mislocated = "remove" #we force this in case option is not set for level 0
	if(!is.null(opts$spatial_curation_data_mislocated)) if (opts$spatial_curation_data_mislocated %in% c("reallocate", "remove")) {
	
		if(DATASET_LEVEL == 0) opts$spatial_curation_data_mislocated = "remove" #we force this in case option is badly set for level 0
		
		stepLogger(level = 0, step = stepnumber, msg = sprintf("Reallocation of mislocated data  (i.e. on land areas or without any spatial information) (data with no spatial information have the dimension 'geographic_identifier' set to 'UNK/IND' or 'NA'). Option is: [%s] ", opts$spatial_curation_data_mislocated))
		stepnumber = stepnumber+1
		
		ntons_before_this_step <- round(georef_dataset %>% select(measurement_value)  %>% sum())
		config$logger.info(
		  sprintf(
			"Gridded catch dataset before Reallocation of mislocated data has [%s] lines and total catch is [%s] Tons",
			nrow(georef_dataset),
			ntons_before_this_step
		  )
		)
		
		georef_dataset <- spatial_curation_data_mislocated(
		  entity = entity,
		  config = config,
		  df = georef_dataset,
		  spatial_curation_data_mislocated = opts$spatial_curation_data_mislocated
		)
		
		georef_dataset <- georef_dataset$dataset
		ntons_after_mislocated <-
		  round(georef_dataset %>% select(measurement_value)  %>% sum())
		config$logger.info(
		  sprintf(
			"Gridded catch dataset after Reallocation of mislocated data has [%s] lines and total catch is [%s] Tons",
			nrow(georef_dataset),
			ntons_after_mislocated
		  )
		)
		config$logger.info(
		  sprintf(
			"Reallocation of mislocated data generated [%s] additionnal tons",
			ntons_after_mislocated - ntons_before_this_step
		  )
		)
		if(recap_each_step){
			function_recap_each_step(
			  "Realocating_removing_mislocated_data",
			  georef_dataset,
			  "In this step, the mislocated data is hanlded. Either removed, reallocated or let alone, the data on continent and the data outside the competent rfmo area are targeted. ",
			  "spatial_curation_data_mislocated",
			  list(options_spatial_curation_data_mislocated)
			)
		}
		gc()
		
	  } else{
		config$logger.info(
		  "-----------------------------------------------------------------------------------------------------"
		)
		config$logger.info(
		  sprintf(
			"Step for managing mislocated data not executed not executed  for file (since not selected in the workflow options, see column 'Data' of geoflow entities spreadsheet):  Reallocation of mislocated data  (i.e. on land areas or without any spatial information) (data with no spatial information have the dimension 'geographic_identifier' set to 'UNK/IND' or 'NA'). Option is: [%s] ",
			opts$spatial_curation_data_mislocated
		  )
		)
		config$logger.info(
		  "-----------------------------------------------------------------------------------------------------"
		)
	  }
 
	#===========================================================================================================================================================
    #===========================================================================================================================================================
	#>(||||*> LEVEL 1 FIRMS CANDIDATE PRODUCT
	#===========================================================================================================================================================
	#===========================================================================================================================================================
	#TODO review and clean
	if(DATASET_LEVEL >= 1){ #with this condition code will be run to deal with dataset level 1 and above

	  
	  # Curation absurd converted data ------------------------------------------
	  if (!is.null(opts$curation_absurd_converted_data)) {
		source(
		  file.path(
			url_scripts_create_own_tuna_atlas,
			"curation_absurd_converted_data.R"
		  )
		)
		
		max_conversion_factor <-
		  read.csv("data/max_conversion_factor.csv")
		
		georef_dataset <-
		  curation_absurd_converted_data(georef_dataset = georef_dataset,
										 max_conversion_factor = max_conversion_factor)
		
		function_recap_each_step(
		  "Removing_absurd_nomt",
		  georef_dataset,
		  "In this step, we target implausible data. We check data having declaration both in NOMT and MTNO and if the conversion factor is implausible.
					   We either remove NOMT strata which corresponds to MTNO declaration implausible or me remove the corresponding MTNO data. More details are available in the pdf file attached.",
		  "spatial_curation_data_mislocated",
		  list(curation_absurd_converted_data)
		)
	  }
	  
	  
	  # unit conversion already given factors -----------------------------------
	  
	  if (!is.null(opts$unit_conversion_convert)) if (opts$unit_conversion_convert) {
		  georef_dataset <-
			double_unit_data_handling(
			  con = con,
			  entity = entity,
			  config = config,
			  fact = fact,
			  unit_conversion_csv_conversion_factor_url =
				opts$unit_conversion_csv_conversion_factor_url,
			  unit_conversion_codelist_geoidentifiers_conversion_factors =
				opts$unit_conversion_codelist_geoidentifiers_conversion_factors,
			  mapping_map_code_lists = opts$mapping_map_code_lists,
			  georef_dataset = georef_dataset
			)
		  function_recap_each_step(
			"Removing NOMT and converting MTNO in MT",
			georef_dataset,
			"In this step, we target the data provided in Tons and Number of fish. The data initially in MTNO will be converted in MT and the data in NTMO will be removed.",
			"",
			list("")
		  )
		  
		  new_version_iotc_raising_available <- TRUE
		  if (file.exists("data/conversion_factors_IOTC.csv")) {
			# unit conversion IOTC given factors -----------------------------------
			iotc_conv_fact <- read_csv(
			  "data/conversion_factors_IOTC.csv",
			  col_types = cols(
				geographic_identifier = col_character(),
				time_start = col_character(),
				time_end = col_character()
			  )
			) %>% dplyr::rename(measurement_value = conversion_factor, measurement_unit = unit, 
			                    gear_type  = gear)#this map condelist function is to retrieve the mapping dataset used
			
			iotc_conv_fact_mapped <-
			  map_codelists(
				con = con,
				"catch",
				mapping_dataset = mapping_dataset,
				dataset_to_map = iotc_conv_fact,
				mapping_keep_src_code = TRUE,
				source_authority_to_map = c("IOTC")
			  )$dataset_mapped
			
			# iotc_conv_fact_mapped$time_start <-
			#   as.Date(iotc_conv_fact_mapped$time_start)
			# iotc_conv_fact_mapped$time_start <-
			#   as.character(lubridate::floor_date(iotc_conv_fact_mapped$time_start, "year"))
			# iotc_conv_fact_mapped$time_end <-
			#   as.Date(iotc_conv_fact_mapped$time_end)
			# iotc_conv_fact_mapped$time_end <-
			#   as.character(
			# 	lubridate::ceiling_date(iotc_conv_fact_mapped$time_end, "year") - lubridate::days(1)
			#   )
			# 
			iotc_conv_fact_mapped <- iotc_conv_fact_mapped %>% select(-c(species_src_code, gear_type_src_code)) %>% 
			  dplyr::group_by(across(setdiff(everything(), "measurement_value"))) %>%
			  dplyr::summarise(measurement_value = mean(measurement_value)) %>% distinct()
			
			iotc_conv_fact_mapped <- iotc_conv_fact_mapped %>% 
			  dplyr::mutate(measurement_unit = dplyr::case_when(measurement_unit %in% c("MT", "t") ~ "t", measurement_unit %in% c("NO", "no") ~ "no", TRUE ~ measurement_unit)) %>% 
			  dplyr::mutate(unit_target = dplyr::case_when(unit_target %in% c("MT", "t") ~ "t", unit_target %in% c("NO", "no")~ "no" , TRUE ~ unit_target)) 
			
			iotc_data <- georef_dataset %>% filter(source_authority == "IOTC")
			
			iotc_data_converted <- do_unit_conversion(
			  entity = entity,
			  config = config,
			  fact = fact,
			  unit_conversion_csv_conversion_factor_url =
				iotc_conv_fact_mapped,
			  unit_conversion_codelist_geoidentifiers_conversion_factors =
				"cwp_grid",
			  mapping_map_code_lists =
				opts$mapping_map_code_lists,
			  georef_dataset = iotc_data,
			  removing_numberfish_final = FALSE
			) # do not remove number of fish as they will be converted later with other conversion factor data
			
			georef_dataset <- rbind(georef_dataset %>% filter(source_authority != "IOTC"), iotc_data_converted)
			
			function_recap_each_step(
			  "Harmonising units on IOTC data",
			  georef_dataset,
			  "In this step, we target the data provided in Tons and Number of fish provided by IOTC.
					 As a new conversion factor dataset has been provided by Emmanuel Chassot",
			  "",
			  list("")
			)
			
			
		  }
		  
		  # unit conversion with factors from IRD dataset -----------------------------------
		  
		  
		  config$logger.info(
			"-----------------------------------------------------------------------------------------------------"
		  )
		  config$logger.info(
			sprintf(
			  "LEVEL 1 => STEP 2/5  for file [%s] is executed: Convert units by using A. Fonteneau file. Option is: [%s] ",
			  entity$data$source[[1]],
			  opts$unit_conversion_convert
			)
		  )
		  config$logger.info(
			"-----------------------------------------------------------------------------------------------------"
		  )
		  mapping_map_code_lists <- TRUE
		  if (!is.null(opts$mapping_map_code_lists))
			mapping_map_code_lists = opts$mapping_map_code_lists
		  if (is.null(opts$unit_conversion_csv_conversion_factor_url))
			stop(
			  "Conversion of unit requires parameter 'unit_conversion_csv_conversion_factor_url'"
			)
		  if (is.null(opts$unit_conversion_codelist_geoidentifiers_conversion_factors))
			stop(
			  "Conversion of unit requires parameter 'unit_conversion_codelist_geoidentifiers_conversion_factors'"
			)
		  
		  ntons_before_this_step <-
			round(georef_dataset %>% filter(measurement_unit == "t")  %>% select(measurement_value)  %>% sum())
		  config$logger.info(
			sprintf(
			  "STEP 2/5 : Gridded catch dataset before unit conversion has [%s] lines and total catch is [%s] Tons",
			  nrow(georef_dataset),
			  ntons_before_this_step
			)
		  )
		  
		  config$logger.info("STEP 2/5: BEGIN do_unit_conversion() function to convert units of georef_dataset")
		  
		  georef_dataset <- do_unit_conversion(
			entity = entity,
			config = config,
			fact = fact,
			unit_conversion_csv_conversion_factor_url =
			  opts$unit_conversion_csv_conversion_factor_url,
			unit_conversion_codelist_geoidentifiers_conversion_factors =
			  opts$unit_conversion_codelist_geoidentifiers_conversion_factors,
			mapping_map_code_lists = mapping_map_code_lists,
			georef_dataset = georef_dataset
		  )
		  config$logger.info("STEP 2/5: END do_unit_conversion() function")
		  
		  ntons_after_conversion <-
			round(georef_dataset %>% select(measurement_value)  %>% sum())
		  config$logger.info(
			sprintf(
			  "STEP 2/5 : Gridded catch dataset after unit conversion has [%s] lines and total catch is [%s] Tons",
			  nrow(georef_dataset),
			  ntons_after_conversion
			)
		  )
		  # config$logger.info(sprintf("STEP 2/5 : [%s] lines have been removed", nrow(georef_dataset)-nrow_before))
		  config$logger.info(
			sprintf(
			  "STEP 2/5 : Unit conversion generated [%s] additionnal tons",
			  ntons_after_conversion - ntons_before_this_step
			)
		  )
		  config$logger.info(
			sprintf(
			  "STEP 2/5 : Total number for 'NO' unit is now [%s] individuals",
			  georef_dataset %>% filter(measurement_unit == "no")  %>% select(measurement_value)  %>% sum()
			)
		  )
		  config$logger.info("END STEP 2/5")
		  function_recap_each_step(
			"raising",
			georef_dataset,
			"In this step, we harmonise the data declared in NO, converting it in Tons using the by using A. Fonteneau file. The file used for the conversion can also be a parameter.",
			fonctions =
			  "do_unit_conversion, unit_conversion_csv_conversion_factor_url, extract_dataset,
								map_codelist, convert_units",
			list(
			  options_mapping_map_code_lists ,
			  options_unit_conversion_csv_conversion_factor_url ,
			  options_unit_conversion_codelist_geoidentifiers_conversion_factors ,
			  options_unit_conversion_convert
			)
		  )
		  
		  
		  
		  
		} else{
		  config$logger.info(
			"-----------------------------------------------------------------------------------------------------"
		  )
		  config$logger.info(
			sprintf(
			  "LEVEL 1 => STEP 2/5 not executed  for file [%s] (since not selected in the workflow options, see column 'Data' of geoflow entities spreadsheet): Convert units by using A. Fonteneau file. Option is: [%s] ",
			  entity$data$source[[1]],
			  opts$unit_conversion_convert
			)
		  )
		  config$logger.info(
			"-----------------------------------------------------------------------------------------------------"
		  )
		}
	  

		# Filtering on species for level 1 --------------------------------------
		config$logger.info("Filtering on species for level 1")
		url_asfis_list_level1 <- "https://raw.githubusercontent.com/fdiwg/fdi-codelists/main/global/firms/gta/cl_species_level1.csv"
		species_to_be_kept_in_level1 <- read_csv(url_asfis_list_level1) %>% dplyr::select(code)
		georef_dataset <- georef_dataset %>% dplyr::inner_join(species_to_be_kept_in_level1, by = c("species" = "code"))

		# Removing all the fishingfleet from the georef_dataset
		georef_dataset$fishing_fleet <- "UNK"
		
		function_recap_each_step(
			"Filtering species level 1",
			georef_dataset,
			paste0(
			  "Filtering species on the base of the file ",
			  url_asfis_list_level1,
			  " to keep only the species. This file contains " ,
			  as.character(length(nrow(
				species_to_be_kept_in_level1
			  ))),
			  " species."
			),
			"inner_join"  ,
			NULL
		)
	
		
		
	}
	# -------------------------------------------------------------------------
  
	#===========================================================================================================================================================
	#===========================================================================================================================================================
	#>(||||*> LEVEL 2 IRD PRODUCTS, not yet submitted/endorsed by FIRMS
	#===========================================================================================================================================================
	#===========================================================================================================================================================
	#TODO review and clean
	if(DATASET_LEVEL >= 2){ #with this condition code will be run to deal with dataset level 2
		config$logger.info(
		"-----------------------------------------------------------------------------------------------------"
		)
		stepLogger(level = 2, step = 1, "Set parameters")
		config$logger.info(
		"-----------------------------------------------------------------------------------------------------"
		)
		# raising_georef_to_nominal <- opts$raising_georef_to_nominal
		iattc_ps_raise_flags_to_schooltype <-
		opts$iattc_ps_raise_flags_to_schooltype
		iattc_ps_dimension_to_use_if_no_raising_flags_to_schooltype <-
		opts$iattc_ps_dimension_to_use_if_no_raising_flags_to_schooltype
		iattc_ps_catch_billfish_shark_raise_to_effort <-
		opts$iattc_ps_catch_billfish_shark_raise_to_effort
		iccat_ps_include_type_of_school <-
		opts$iccat_ps_include_type_of_school

		config$logger.info(
		"-----------------------------------------------------------------------------------------------------"
		)
		stepLogger(level = 2, step = 2, "Extract and load IRD Level 1 gridded catch data input")
		config$logger.info(
		"-----------------------------------------------------------------------------------------------------"
		)


		if (!is.null(opts$raising_georef_to_nominal))
		if (opts$raising_georef_to_nominal) {
		  config$logger.info(
			"-----------------------------------------------------------------------------------------------------"
		  )
		  stepLogger(level = 2, step = 3, "Raise IRD gridded Level 1 (1 or 5 deg) input with FIRMS Level O total (nominal) catch dataset")
		  config$logger.info(
			"-----------------------------------------------------------------------------------------------------"
		  )
		  
		  config$logger.info(
			"Extract and load FIRMS Level 0 nominal catch data input (required if raising process is asked) "
		  )
		  nominal_catch <-
			readr::read_csv(entity$getJobDataResource(config, entity$data$source[[1]]),
							guess_max = 0) 
		  class(nominal_catch$measurement_value) <- "numeric"
		  mapping_dataset <-
			read.csv(
			  mapping_csv_mapping_datasets_url,
			  stringsAsFactors = F,
			  colClasses = "character"
			)
		  mapping_keep_src_code <- FALSE
		  
		  #no need to map it is already mapped
		  # nominal_catch2 <- map_codelists(con, "catch", mapping_dataset, nominal_catch, mapping_keep_src_code)
		  # nominal_catch <- read.csv2("entities/global_catch_1deg_1m_ps_bb_firms_Bastien_with_step_rds__level2/data/nominal_catch_mapped.csv", sep = ";")
		  #         #@juldebar keep same units for all datatets
		  class(nominal_catch$measurement_unit) <- "character"
		  
		  if (any(nominal_catch$measurement_unit == "t"))
		    nominal_catch[nominal_catch$measurement_unit == "t",]$measurement_unit <- "t"
		  if (any(nominal_catch$measurement_unit == "TRUE"))
		    nominal_catch[nominal_catch$measurement_unit == "TRUE",]$measurement_unit <- "t"
		  if (any(nominal_catch$measurement_unit == "no"))
			nominal_catch[nominal_catch$measurement_unit == "no",]$measurement_unit <- "no"
		  class(nominal_catch$measurement_value) <- "numeric"
		  #@juldebar if not provided by Google drive line below should be used if nominal catch has to be extracted from the database
		  if (nrow(nominal_catch) == 0) {
			nominal_catch <- retrieve_nominal_catch(entity, config, opts)
		  }
		  config$logger.info(sprintf("Nominal catch dataset has [%s] lines", nrow(nominal_catch)))
		  config$logger.info(paste0(
			"Total of  nominal catch for file ",
			entity$data$source[[1]],
			"is : ",
			sum(nominal_catch$measurement_value),
			"  \n"
		  ))
		  
		  config$logger.info("Start raising process")
		  
		  if (fact == "catch") {
			config$logger.info("Fact=catch !")
			dataset_to_compute_rf = georef_dataset
			# year is used as a dimension to match the conversion factors dimension 
			if (is.null(opts$x_raising_dimensions)) {
			  x_raising_dimensions = c("gear_type", "species", "year", "source_authority")
			}
			
			
		  } else if (fact == "effort") {
			## If we raise the efforts, the RF is calculated using the georeferenced catch data. Hence, we need to retrieve the georeferenced catch data.
			cat(
			  "Catch datasets must be retrieved and processed in order to raise efforts. \nRetrieving georeferenced catch datasets from the Tuna atlas database...\n"
			)
			dataset_catch <- NULL
			  rfmo_dataset <-
				get_rfmos_datasets_level0("IOTC", "catch", datasets_year_release)
			  dataset_catch <- rbind(dataset_catch, rfmo_dataset)
			  rm(rfmo_dataset)
			
			  rfmo_dataset <-
				get_rfmos_datasets_level0("WCPFC", "catch", datasets_year_release)
			  dataset_catch <- rbind(dataset_catch, rfmo_dataset)
			  rm(rfmo_dataset)
			
			  rfmo_dataset <-
				get_rfmos_datasets_level0("CCSBT", "catch", datasets_year_release)
			  dataset_catch <- rbind(dataset_catch, rfmo_dataset)
			  rm(rfmo_dataset)

			  			  rfmo_dataset <- get_rfmos_datasets_level0(
				"IATTC",
				"catch",
				datasets_year_release,
				iattc_ps_raise_flags_to_schooltype =
				  iattc_ps_raise_flags_to_schooltype,
				iattc_ps_dimension_to_use_if_no_raising_flags_to_schooltype =
				  iattc_ps_dimension_to_use_if_no_raising_flags_to_schooltype,
				iattc_ps_catch_billfish_shark_raise_to_effort =
				  TRUE
			  )
			  dataset_catch <- rbind(dataset_catch, rfmo_dataset)
			  rm(rfmo_dataset)

			  			  rfmo_dataset <- get_rfmos_datasets_level0("ICCAT",
														"catch",
														datasets_year_release,
														iccat_ps_include_type_of_school =
														  iccat_ps_include_type_of_school)
			  dataset_catch <- rbind(dataset_catch, rfmo_dataset)
			  rm(rfmo_dataset)
			
			
			if (mapping_map_code_lists == "TRUE") {
			  dataset_catch <-
				map_codelists(
				  "catch",
				  mapping_csv_mapping_datasets_url,
				  dataset_catch,
				  mapping_keep_src_code
				)$dataset_mapped
			}
			
			dataset_catch$time_start <-
			  substr(as.character(dataset_catch$time_start), 1, 10)
			dataset_catch$time_end <-
			  substr(as.character(dataset_catch$time_end), 1, 10)
			if (unit_conversion_convert == "TRUE") {
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
				dataset_catch
			  )
			}
			
			dataset_to_compute_rf = dataset_catch
			#@juldebar insert patch below to fix error in raise_get_rf function
			
			rm(dataset_catch)
			#@juldebar : update with the new name of "flag" dimension (now "fishingfleet")
			# x_raising_dimensions=c("fishingfleet","gear","year","source_authority")
		  }
		  
		  class(dataset_to_compute_rf$measurement_value) <- "numeric"
		  
		  
		  config$logger.info("Executing function function_raising_georef_to_nominal")
		  config$logger.info(paste0(
			"Total ",
			fact,
			" before raising is : ",
			sum(georef_dataset$measurement_value),
			"\n"
		  ))
		  config$logger.info(paste0(
			"Total ",
			fact,
			" in nominal data is : ",
			sum(nominal_catch$measurement_value),
			"\n"
		  ))
		  
		  nominal_catch$fishing_fleet <- "UNK"
		  
		  georef_dataset <-
			function_raising_georef_to_nominal(
			  con = con,
			  opts = opts ,
			  entity = entity,
			  config = config,
			  dataset_to_raise =
				georef_dataset,
			  nominal_dataset_df = nominal_catch,
			  # nominal_catch,
			  # dataset_to_compute_rf=nominal_catch,
			  dataset_to_compute_rf =
				dataset_to_compute_rf,
			  x_raising_dimensions =
				x_raising_dimensions
			)
		  
		  rm(dataset_to_compute_rf)
		  
		  georef_dataset <- georef_dataset$dataset
		  config$logger.info(paste0(
			"Total ",
			fact,
			" after raising is now: ",
			sum(georef_dataset$measurement_value),
			"\n"
		  ))
		  config$logger.info(sprintf("Gridded catch dataset has [%s] lines", nrow(georef_dataset)))
		  config$logger.info(paste0(
			"Total catch for data after raising is ",
			sum(georef_dataset$measurement_value),
			"  \n"
		  ))
		  
		  if(recap_each_step){
			  function_recap_each_step(
				"Level2_RF1",
				georef_dataset,
				"In this step, the georeferenced data is raised to get closer of the nominal data. Aiming this, all the stratas having an equivalent (regarding the columns given in options) in nominal catches are raised to reach the equivalent. If the data is lower in nominal data for the strata, the data is lowed to reach the nominal amount ",
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
				)
			  )
			}
		  
		  
		}


		if (opts$disaggregate_on_5deg_data_with_resolution_superior_to_5deg %in% c("disaggregate", "remove")) {
		config$logger.info(
		  "-----------------------------------------------------------------------------------------------------"
		)
		config$logger.info(
		  sprintf(
			"LEVEL 1 => STEP 4/5  for file [%s] is executed: Disaggregate data on 5° resolution quadrants (for 5deg resolution datasets only). Option is: [%s] ",
			entity$data$source[[1]],
			opts$disaggregate_on_5deg_data_with_resolution_superior_to_5deg
		  )
		)
		config$logger.info(
		  "-----------------------------------------------------------------------------------------------------"
		)


		ntons_before_this_step <-
		  round(georef_dataset %>% select(measurement_value)  %>% sum())
		config$logger.info(
		  sprintf(
			"STEP 4/5 : Gridded catch dataset before Disaggregate data on 5° resolution has [%s] lines and total catch is [%s] Tons",
			nrow(georef_dataset),
			ntons_before_this_step
		  )
		)

		config$logger.info(
		  "STEP 4/5: BEGIN function_disaggregate_on_resdeg_data_with_resolution_superior_to_resdeg() function"
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
		  "STEP 4/5: END function_disaggregate_on_resdeg_data_with_resolution_superior_to_resdeg() function"
		)


		georef_dataset <- georef_dataset$dataset
		ntons_after_disaggregation_5deg <-
		  round(georef_dataset %>% select(measurement_value)  %>% sum())
		config$logger.info(
		  sprintf(
			"STEP 4/5 : Gridded catch dataset after Disaggregate data on 5° resolution has [%s] lines and total catch is [%s] Tons",
			nrow(georef_dataset),
			ntons_after_disaggregation_5deg
		  )
		)
		config$logger.info(
		  sprintf(
			"STEP 4/5 : Disaggregate data on 5° generated [%s] additionnal tons",
			ntons_after_disaggregation_5deg - ntons_before_this_step
		  )
		)
		config$logger.info("END STEP 4/5")
		function_recap_each_step(
		  "Disaggregate5deg",
		  georef_dataset,
		  "This step disaggregate data on resolution higher than 5° in 5° resolution. iGridded catch dataset before Disaggregate data on 5° resolution has [%s] lines and total catch is [%s] Tons",
		  "function_disaggregate_on_resdeg_data_with_resolution_superior_to_resdeg",
		  list(
			options_disaggregate_on_5deg_data_with_resolution_superior_to_5deg
		  )
		)
		} else{
		config$logger.info(
		  "-----------------------------------------------------------------------------------------------------"
		)
		config$logger.info(
		  sprintf(
			"LEVEL 1 => STEP 4/5 not executed  for file [%s] (since not selected in the workflow options, see column 'Data' of geoflow entities spreadsheet):  Disaggregate data on 5° resolution quadrants (for 5deg resolution datasets only). Option is: [%s] ",
			entity$data$source[[1]],
			opts$disaggregate_on_5deg_data_with_resolution_superior_to_5deg
		  )
		)
		config$logger.info(
		  "-----------------------------------------------------------------------------------------------------"
		)
		}



		if (opts$disaggregate_on_1deg_data_with_resolution_superior_to_1deg %in% c("disaggregate", "remove")) {
		config$logger.info(
		  "-----------------------------------------------------------------------------------------------------"
		)
		config$logger.info(
		  sprintf(
			"LEVEL 1 => STEP 5/5 for file [%s] is executed: Disaggregate data on 1° resolution quadrants (for 1deg resolution datasets only). Option is: [%s] ",
			entity$data$source[[1]],
			opts$disaggregate_on_1deg_data_with_resolution_superior_to_1deg
		  )
		)
		config$logger.info(
		  "-----------------------------------------------------------------------------------------------------"
		)

		ntons_before_this_step <-
		  round(georef_dataset %>% select(measurement_value)  %>% sum())
		config$logger.info(
		  sprintf(
			"STEP 5/5 : Gridded catch dataset before Disaggregate data on 1° has [%s] lines and total catch is [%s] Tons",
			nrow(georef_dataset),
			ntons_before_this_step
		  )
		)

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
		ntons_after_disaggregation_1deg <-
		  round(georef_dataset %>% select(measurement_value)  %>% sum())
		config$logger.info(
		  sprintf(
			"STEP 5/5 : Gridded catch dataset after Disaggregate data on 1° has [%s] lines and total catch is now [%s] Tons",
			nrow(georef_dataset),
			ntons_after_disaggregation_1deg
		  )
		)
		config$logger.info(
		  sprintf(
			"STEP 5/5 : Disaggregate data on 1° generated [%s] additionnal tons",
			ntons_after_disaggregation_1deg - ntons_before_this_step
		  )
		)
		config$logger.info("END STEP 5/5")
		function_recap_each_step(
		  "Disaggregate1deg",
		  georef_dataset,
		  "This step disaggregate data on resolution higher than 1° in 1° resolution. iGridded catch dataset before Disaggregate data on 1° resolution has [%s] lines and total catch is [%s] Tons",
		  "function_disaggregate_on_resdeg_data_with_resolution_superior_to_resdeg",
		  list(
			options_disaggregate_on_1deg_data_with_resolution_superior_to_1deg
		  )
		)
		gc()

		} else{
		config$logger.info(
		  "-----------------------------------------------------------------------------------------------------"
		)
		config$logger.info(
		  sprintf(
			"LEVEL 1 => STEP 5/5 not executed  for file [%s] (since not selected in the workflow options, see column 'Data' of geoflow entities spreadsheet): Disaggregate data on 1° resolution quadrants (for 1deg resolution datasets only). Option is: [%s] ",
			entity$data$source[[1]],
			opts$disaggregate_on_1deg_data_with_resolution_superior_to_1deg
		  )
		)
		config$logger.info(
		  "-----------------------------------------------------------------------------------------------------"
		)
		}
		gc()
	}
  
	#===========================================================================================================================================================
	#===========================================================================================================================================================
	#>(||||*> FILTERS (MAY APPLY TO LEVEL 0, 1 or 2)
	#===========================================================================================================================================================
	#===========================================================================================================================================================
	#-----------------------------------------------------------------------------------------------------------------------------------------------------------
	config$logger.info("LEVEL 0 => STEP 3/8: Grid spatial resolution filter")
	#-----------------------------------------------------------------------------------------------------------------------------------------------------------
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
			  list(options_resolution_filter)
			)
		}
	}

	#-----------------------------------------------------------------------------------------------------------------------------------------------------------
	config$logger.info("Apply filters if filter needed (Filter data by groups of everything) ")
	#-----------------------------------------------------------------------------------------------------------------------------------------------------------
	# Filtering data on multiple dimension if needed for a particular final data
	
	parameter_filtering = if (!is.null(opts$filtering)) opts$filtering else list(species = NULL, fishing_fleet = NULL) # if nothing provided filtering is null so we provided first dimension with no filtering
	if (is.character(parameter_filtering)) {
	parameter_filtering <-
	  eval(parse(text = toString(parameter_filtering)))
	} #if opts$filtering is provided, we need to read the filtering parameters provided in the entities table as following ex parameter_option_filtering:c(species = "YFT", gear = c("09.32", 09.39"))

	matchingList <-
	parameter_filtering %>% purrr::keep(~ !is.null(.)) #removing null params in case no option is provided

	georef_dataset <- dimension_filtering_function(georef_dataset, filtering_params = matchingList)

	#----------------------------------------------------------------------------------------------------------------------------

	#we do an aggregation by dimensions
	dataset <- 
	  georef_dataset %>% group_by(.dots = setdiff(colnames(georef_dataset), "measurement_value")) %>% dplyr::summarise(measurement_value =
																										   sum(measurement_value))
	dataset <- data.frame(dataset)
	if(!is.na(any(dataset$measurement_unit) == "TRUE")) if(any(dataset$measurement_unit) == "TRUE") dataset[(dataset$measurement_unit) == "TRUE",]$measurement_unit <- "t" #patch because of https://github.com/firms-gta/geoflow-tunaatlas/issues/41
  
	#----------------------------------------------------------------------------------------------------------------------------
	#@eblondel additional formatting for next time support
	dataset$time_start <- as.Date(dataset$time_start)
	dataset$time_end <- as.Date(dataset$time_end)
	#we enrich the entity with temporal coverage
	dataset_temporal_extent <-
	paste(as.character(min(dataset$time_start)), as.character(max(dataset$time_end)), sep = "/")
	entity$setTemporalExtent(dataset_temporal_extent)

	#if there is any entity relation with name 'codelists' we read the file
	df_codelists <- NULL
	cl_relations <-
	entity$relations[sapply(entity$relations, function(x) {
	  x$name == "codelists"
	})]
	if (length(cl_relations) > 0) {
		config$logger.info("Appending codelists to global dataset generation action output")
		googledrive_baseurl <- "https://drive.google.com/open?id="
		if (startsWith(cl_relations[[1]]$link, googledrive_baseurl)) {
		  #managing download through google drive
		  config$logger.info("Downloading file using Google Drive R interface")
		  drive_id <- unlist(strsplit(cl_relations[[1]]$link, "id="))[2]
		  drive_id <-
			unlist(strsplit(drive_id, "&export"))[1] #control in case export param is appended
		  googledrive::drive_download(file = googledrive::as_id(drive_id),
									  path = file.path("data", paste0(
										entity$identifiers[["id"]], "_codelists.csv"
									  )), overwrite = TRUE)
		  df_codelists <-
			read.csv(file.path("data", paste0(
			  entity$identifiers[["id"]], "_codelists.csv"
			)))
		} else{
		  df_codelists <- read.csv(cl_relations[[1]]$link)
		}
	}


	#@geoflow -> output structure as initially used by https://raw.githubusercontent.com/ptaconet/rtunaatlas_scripts/master/workflow_etl/scripts/generate_dataset.R
	dataset <- list(
		dataset = dataset,
		additional_metadata = NULL,
		#nothing here
		codelists = df_codelists #in case the entity was provided with a link to codelists
	)


	# if (fact=="effort" & DATA_LEVEL %in% c("1", "2")){
	#   # Levels 1 and 2 of non-global datasets should be expressed with tRFMOs code lists. However, for the effort unit code list and in those cases, we take the tuna atlas effort unit codes although this is not perfect. but going back to tRFMOs codes is too complicated
	#   df_codelists$code_list_identifier[which(df_codelists$dimension=="unit")]<-"effortunit_rfmos"
	# }
	# ltx_combine(combine = wd, out = "alltex.tex", clean = 0)

	#@geoflow -> export as csv
	#-------------------------------------------------------
	output_name_dataset <- file.path("data", paste0(entity$identifiers[["id"]], "_harmonized.csv"))
	readr::write_csv(dataset$dataset, output_name_dataset)
	#-------------------------------------------------------
	
	output_name_dataset_public <- file.path("data", paste0(entity$identifiers[["id"]], "_public.csv"))
	dataset_enriched = dataset$dataset
	dataset_enriched$year = as.integer(format(dataset_enriched$time_end, "%Y"))
	dataset_enriched$month = as.integer(format(dataset_enriched$time_end, "%m"))
	dataset_enriched$quarter = as.integer(substr(quarters(dataset_enriched$time_end), 2, 2))
	dataset_enriched = dataset_enriched[,c("source_authority", "species", "gear_type", "fishing_fleet", "fishing_mode", "time_start", "time_end", "year", "month", "quarter", "geographic_identifier", "measurement_unit", "measurement_value")]
	readr::write_csv(dataset_enriched, output_name_dataset_public)
	
	#-------------------------------------------------------
	output_name_codelists <- file.path("data", paste0(entity$identifiers[["id"]], "_codelists.csv"))
	write.csv(dataset$codelists, output_name_codelists, row.names = FALSE)
	# ---------------------------------------------------------------------------------------------------------------------------
	entity$addResource("harmonized", output_name_dataset)
	entity$addResource("public", output_name_dataset_public)
	entity$addResource("codelists", output_name_codelists)
	entity$addResource("geom_table", opts$geom_table)
	#### END
	config$logger.info(
	"-----------------------------------------------------------------------------------------------------"
	)
	config$logger.info("End: Your tuna atlas dataset has been created!")
	config$logger.info(
	"-----------------------------------------------------------------------------------------------------"
	)
	# write.csv(options)

	rm(georef_dataset)

	gc()
}
