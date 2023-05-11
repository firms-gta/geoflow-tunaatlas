function(action, entity, config){
  opts <- action$options
  con <- config$software$input$dbi
  options(encoding = "UTF-8")
  ######################################################################
  ##### 52North WPS annotations ##########
  ######################################################################
  # wps.des: id = create_own_tuna_atlas_catch_effort, title = Create your own georeferenced Tuna Atlas dataset of catch or efforts, abstract = This algorithm allows to create own regional or global tuna atlas of geo-referenced gridded catch or efforts. It takes as input the public domain datasets of the five Tuna Regional Fisheries Management Organizations (tRFMOs) (IOTC ICCAT WCPFC IATTC CCSBT) stored within the Tuna atlas database. It proposes a set of parameters to customize the computation of the tuna atlas. ;
  # wps.in: id = fact, type = string, title = Variable output of the tuna atlas (catch or effort), value = "catch|effort";
  # wps.in: id = include_IOTC, type = string, title = Include IOTC data (Indian Ocean) in the atlas (TRUE or FALSE), value = "TRUE|FALSE";
  # wps.in: id = include_ICCAT, type = string, title = Include ICCAT data (Atlantic Ocean) in the tuna atlas?, value = "TRUE|FALSE";
  # wps.in: id = include_IATTC, type = string, title = Include IATTC data (Eastern Pacific Ocean) in the tuna atlas?, value = "TRUE|FALSE";
  # wps.in: id = include_WCPFC, type = string, title = Include WCPFC data (Western Pacific Ocean) in the tuna atlas?, value = "TRUE|FALSE";
  # wps.in: id = include_CCSBT, type = string, title = Include CCSBT data db(Southern hemisphere Oceans - only Southern Bluefin Tuna) in the atlas?, value = "TRUE|FALSE";
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

  if(!require(dplyr)){
    install.packages("dplyr")
    require(dplyr)
  }
  if(!require(sf)){
    install.packages("sf")
    require(sf)
  }
  if(!require(stringr)){
    install.packages("stringr")
    require(stringr)
  }
  
  if(!require(R3port)){
    install.packages("R3port")
    require(R3port)
  }
  
  
  if(!require(reshape2)){
    install.packages("reshape2")
    require(reshape2)
  }
  
  
  
  if(!require(readr)){
    install.packages("readr")
    require(readr)
  }
  if(!require(tools)){
    install.packages("tools")
    require(tools)
  }
  
  if(!require(RPostgreSQL)){
    install.packages("RPostgreSQL")
    require(RPostgreSQL)
  } 
  if(!require(DBI)){
    install.packages("DBI")
    require(DBI)
  } 
  
  if(!require(googledrive)){
    install.packages("googledrive")
    require(googledrive)
  }
  
  
  counting <- 1
  
  
  
  
  
  
  
  #scripts
  url_scripts_create_own_tuna_atlas <- "https://raw.githubusercontent.com/eblondel/geoflow-tunaatlas/master/tunaatlas_scripts/generation"
  source(file.path(url_scripts_create_own_tuna_atlas, "get_rfmos_datasets_level0.R")) #modified for geoflow
  source(file.path(url_scripts_create_own_tuna_atlas, "retrieve_nominal_catch.R")) #modified for geoflow
  source(file.path(url_scripts_create_own_tuna_atlas, "map_codelists.R")) #modified for geoflow
  source(file.path(url_scripts_create_own_tuna_atlas, "do_unit_conversion.R") )
  source(file.path(url_scripts_create_own_tuna_atlas, "function_overlapped.R") ) # adding this function as overlapping is now a recurent procedures for several overlapping
  source(file.path(url_scripts_create_own_tuna_atlas, "disaggregate_on_resdeg_data_with_resolution_superior_to_resdeg.R"))
  source(file.path(url_scripts_create_own_tuna_atlas, "function_raising_georef_to_nominal.R")) #modified for geoflow
  source(file.path(url_scripts_create_own_tuna_atlas, "spatial_curation_data_mislocated.R")) #modified for geoflow
  source(file.path(url_scripts_create_own_tuna_atlas, "double_unit_data_handling.R")) # new function for double unit 
  source(file.path(url_scripts_create_own_tuna_atlas, "function_recap_each_step.R"))  # new function to create rds for each treatment
  
  
  
  j <- 1
  
  list_options <-assign("list_options", data.frame(matrix(ncol =2 , nrow = 1)), envir= .GlobalEnv)
  colnames(list_options) <- c("Options", "Position")
  
  
  for (i in names(opts)){
    if (i != ""){
      
      assign(paste0("options_",i), paste(opts[[j]], collapse = ' ; '), envir= .GlobalEnv)
      if(!exists(i)){
        assign(i, paste0(opts[[j]]), envir= .GlobalEnv)
      }
    }
    if (opts[[j]][1] == TRUE){
      assign(i, opts[[j]], envir= .GlobalEnv)
    } else if (opts[[j]][1] == FALSE){
      assign(i, opts[[j]], envir= .GlobalEnv)
    } 
    assign("data_i",  data.frame(i, paste(opts[[j]], collapse = ' ; ')))
    names(data_i) <- colnames(list_options)
    assign("data_i",data_i, envir= .GlobalEnv)
    list_options <- rbind(list_options, data_i)
    
    j <-  j+1 
  }
  list_options = list_options[-1,]
  
  write_csv(list_options, "list_options.csv")
  #Identify expected Level of processing
  DATA_LEVEL <- unlist(strsplit(entity$identifiers[["id"]], "_level"))[2]
  
  
  
  # #############
  
  #Identify expected Level of processing
  #-----------------------------------------------------------------------------------------------------------------------------------------------------------
  #LEVEL 0 FIRMS PRODUCTS
  #-----------------------------------------------------------------------------------------------------------------------------------------------------------
  
  #### 1) Retrieve tuna RFMOs data from Tuna atlas DB at level 0. Level 0 is the merging of the tRFMOs primary datasets, with the more complete possible value of georef_dataset per stratum (i.e. duplicated or splitted strata among the datasets are dealt specifically -> this is the case for ICCAT and IATTC)  ####
  config$logger.info("Begin: Retrieving primary datasets from Tuna atlas DB... ")
  
  #-------get_rfmos_datasets_level0------------------------------------------------------------------------------------------------------------------------------
  config$logger.info("LEVEL 0 => STEP 1/8: Retrieve georeferenced catch or effort (+ processings for ICCAT and IATTC) AND NOMINAL CATCH if asked")
  #-------------------------------------------------------------------------------------------------------------------------------------
  

  rawdata <- opts
  rawdata$iattc_ps_raise_flags_to_schooltype<- FALSE
  rawdata$iccat_ps_include_type_of_school<- FALSE
  rawdata$iattc_ps_catch_billfish_shark_raise_to_effort <- FALSE
  rawdata$iattc_ps_dimension_to_use_if_no_raising_flags_to_schooltype <- "fishingfleet"
  
  dataset <- do.call("rbind", lapply(c("IOTC", "WCPFC", "CCSBT", "ICCAT", "IATTC"), get_rfmos_datasets_level0, entity, config, rawdata))
  dataset$time_start<-substr(as.character(dataset$time_start), 1, 10)
  dataset$time_end<-substr(as.character(dataset$time_end), 1, 10)
  georef_dataset<-dataset
  class(georef_dataset$value) <- "numeric"
  rm(dataset)
  ### next steps to correct identifier incorrect of iotc
  
  
  ### filtering on minimum time_start 
  
  if(is.null(opts$filtering_on_minimum_year_declared)){
    filtering_on_minimum_year_declared <- TRUE
    
    } else {filtering_on_minimum_year_declared <- opts$filtering_on_minimum_year_declared}
  
  
  
  if(filtering_on_minimum_year_declared){
  config$logger.info("filtering on minimum time_start ")
  
 
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
  
  
  function_recap_each_step("rawdata",
                           georef_dataset,
                           "Retrieve georeferenced catch or effort : In this step, the georeference data of the included (in options) rfmos, are binded.",
                           "get_rfmos_datasets_level0"  , list(options_include_IOTC,options_include_ICCAT,
                                                               options_include_IATTC,options_include_WCPFC,
                                                               options_include_CCSBT))
  saveRDS(georef_dataset, "data/rawdata.rds")
  
  
  # unlink("Markdown")
  
  if(opts$iccat_ps_include_type_of_school){
    rawdata$iccat_ps_include_type_of_school<- opts$iccat_ps_include_type_of_school
    
    
    #------------Enriching data with schootype for iccat-------------------------------------------------------------------------------------------------------------------------
    config$logger.info("LEVEL 0 => STEP Enriching data with schootype for iccat if needed")
    #-------------------------------------------------------------------------------------------------------------------------------------
    iccat <- get_rfmos_datasets_level0("ICCAT", entity, config, rawdata)
    iccat$time_start<-substr(as.character(iccat$time_start), 1, 10)
    iccat$time_end<-substr(as.character(iccat$time_end), 1, 10)
    class(iccat$value) <- "numeric"
    
    georef_dataset<-rbind(georef_dataset %>% filter(source_authority != "ICCAT"),iccat)
    rm(iccat)
    
    function_recap_each_step("iccat enriched",
                             georef_dataset,
                             "Retrieve georeferenced catch or effort : In this step, the georeference data of the included (in options) rfmos, are binded.",
                             "get_rfmos_datasets_level0"  , list(options_include_ICCAT))
    ################
  }
  
  if(opts$iattc_ps_raise_flags_to_schooltype){
    rawdata$iattc_ps_raise_flags_to_schooltype<- opts$iattc_ps_raise_flags_to_schooltype
    
    
    #---------Enriching data with schootype for iattc----------------------------------------------------------------------------------------------------------------------------
    config$logger.info("LEVEL 0 => STEP Enriching data with schootype for iattc if needed")
    #-------------------------------------------------------------------------------------------------------------------------------------
    iattc <- get_rfmos_datasets_level0("IATTC", entity, config, opts)
    iattc$time_start<-substr(as.character(iattc$time_start), 1, 10)
    iattc$time_end<-substr(as.character(iattc$time_end), 1, 10)
    class(iattc$value) <- "numeric"
    
    georef_dataset<-rbind(georef_dataset %>% filter(source_authority != "IATTC"),iattc)
    rm(iattc)
    
    function_recap_each_step("iattc enriched",
                             georef_dataset,
                             "Retrieve georeferenced catch or effort : In this step, the georeference data of the included (in options) rfmos, are binded.",
                             "get_rfmos_datasets_level0"  , list(options_include_IATTC))
    ################
  }
  if(opts$iattc_ps_catch_billfish_shark_raise_to_effort){
    rawdata$iattc_ps_catch_billfish_shark_raise_to_effort<- opts$iattc_ps_catch_billfish_shark_raise_to_effort
    
    #----------Raising catch data to effort for iattc---------------------------------------------------------------------------------------------------------------------------
    config$logger.info("LEVEL 0 => Raising catch data to effort for iattc")
    #-------------------------------------------------------------------------------------------------------------------------------------
    iattc <- get_rfmos_datasets_level0("IATTC", entity, config, rawdata)
    iattc$time_start<-substr(as.character(iattc$time_start), 1, 10)
    iattc$time_end<-substr(as.character(iattc$time_end), 1, 10)
    class(iattc$value) <- "numeric"
    
    georef_dataset<-rbind(georef_dataset %>% filter(source_authority != "IATTC"),iattc)
    rm(iattc)
    
    function_recap_each_step("iattc raised for billfish and shark",
                             georef_dataset,
                             "Retrieve georeferenced catch or effort : In this step, the georeference data of the included (in options) rfmos, are binded.",
                             "get_rfmos_datasets_level0"  , list(options_include_IATTC))
    ################
  }
  
  
  
  
  # iotc_data <- georef_dataset %>% dplyr::filter(source_authority == "IOTC")
  # iotc_data <- iotc_data %>% dplyr::mutate(geographic_identifier = case_when(geographic_identifier == "1100030" ~ "9100030",
  #                                                                            geographic_identifier =="2120060"~"8120060",
  #                                                                            geographic_identifier == "3200050"~"7200050", 
  #                                                                            geographic_identifier == "4220040"~"8220040", 
  #                                                                            geographic_identifier == "6130045\n"~"6130045", TRUE~geographic_identifier))
  # 
  # georef_dataset <- rbind(georef_dataset %>% filter(source_authority != "IOTC"), iotc_data)

  georef_dataset <- georef_dataset %>% 
    dplyr::mutate(unit = case_when(unit %in% c("t") ~ "t", unit %in% c("no") ~ "no", TRUE ~ unit))
  
  
  
  
  #----------Map code lists -------------------------------------------------------------------------------------------------------------------------------------------------
  config$logger.info("LEVEL 0 => STEP 2/8: Map code lists ")
  #-----------------------------------------------------------------------------------------------------------------------------------------------------------
  
  if (!is.null(opts$mapping_map_code_lists)) if(opts$mapping_map_code_lists){
    
    config$logger.info("Reading the CSV containing the dimensions to map + the names of the code list mapping datasets. Code list mapping datasets must be available in the database.")
    mapping_csv_mapping_datasets_url <- entity$getJobDataResource(config, entity$data$source[[2]])
    mapping_dataset<- read.csv(mapping_csv_mapping_datasets_url, stringsAsFactors = F,colClasses = "character")
    mapping_keep_src_code <- FALSE
    if(!is.null(opts$mapping_keep_src_code)) mapping_keep_src_code = opts$mapping_keep_src_code
    
    config$logger.info("Mapping code lists of georeferenced datasets...")
    mapping_codelist <- map_codelists(con, opts$fact, mapping_dataset = mapping_dataset, dataset_to_map=georef_dataset, mapping_keep_src_code, summary_mapping = TRUE,source_authority_to_map = opts$source_authority_to_map) #this map condelist function is to retrieve the mapping dataset used
    
    georef_dataset <- mapping_codelist$dataset_mapped
    
    recap_mapping <- mapping_codelist$recap_mapping
    stats_total <- mapping_codelist$stats_total
    not_mapped_total <- mapping_codelist$not_mapped_total
    
    config$logger.info("Mapping code lists of georeferenced datasets OK")
    
    
    names_list <- c("recap_mapping", "stats_total", "not_mapped_total") #file we want to save
    
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
    
    function_recap_each_step("mapping_codelist",
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
and groups of gears.", "map_codelists", list(options_mapping_map_code_lists))
    
    
    
  }
  

  
  
  
  #--------Overlapping zone (IATTC/WCPFC)--------------------------------------------------------------------------------------------------------------------------------------------------
  config$logger.info("LEVEL 0 => STEP 6/8: Overlapping zone (IATTC/WCPFC): keep data from IATTC or WCPFC?")
  #-----------------------------------------------------------------------------------------------------------------------------------------------------------
  
  if (opts$include_IATTC && opts$include_WCPFC && !is.null(opts$overlapping_zone_iattc_wcpfc_data_to_keep)) {
    
    if(!exists("opts$strata_overlap_iattc_wcpfc")){options_strata_overlap_iattc_wcpfc <- c("geographic_identifier",    "species","year")
    } else {options_strata_overlap_iattc_wcpfc<-unlist(strsplit(opts$strata_overlap_iattc_wcpfc , split=","))}
    config$logger.info(paste0(options_strata_overlap_iattc_wcpfc))
    
    formals(function_overlapped)$opts <- opts
    
    georef_dataset <- function_overlapped(dataset = georef_dataset , con =con , rfmo_to_keep = overlapping_zone_iattc_wcpfc_data_to_keep,
                                          rfmo_not_to_keep = (if (overlapping_zone_iattc_wcpfc_data_to_keep == "IATTC"){"WCPFC"} else {"IATTC"}),
                                          strata = options_strata_overlap_iattc_wcpfc)
    function_recap_each_step("overlap_iattc_wcpfc",
                             georef_dataset,
                             "In this step, the georeferenced data present on the overlapping zone between IATTC and WCPFC is handled. The option for the strata overlapping allow to handle the maximum similarities allowed between two data to keep both.",
                             "function_overlapped" , list(options_include_IATTC,
                                                          options_include_WCPFC , options_overlapping_zone_iattc_wcpfc_data_to_keep, options_strata_overlap_iattc_wcpfc))
    
    
  }
  
  
  #--------Overlapping zone (IOTC/WCPFC)--------------------------------------------------------------------------------------------------------------------------------------------------
  config$logger.info("LEVEL 0 => STEP 6/8: Overlapping zone (IOTC/WCPFC): keep data from IOTC or WCPFC?")
  #-----------------------------------------------------------------------------------------------------------------------------------------------------------
  
  if (opts$include_IOTC && opts$include_WCPFC && !is.null(opts$overlapping_zone_iotc_wcpfc_data_to_keep)) {
    # overlapping_zone_iotc_wcpfc_data_to_keep <- opts$overlapping_zone_iotc_wcpfc_data_to_keep
    if(!exists("opts$strata_overlap_iotc_wcpfc")){options_strata_overlap_iotc_wcpfc <- c("geographic_identifier",    "species","year",
                                                                                         "fishingfleet")
    } else {options_strata_overlap_iotc_wcpfc<-unlist(strsplit(opts$strata_overlap_iotc_wcpfc, split=","))}
    
    
    georef_dataset <- function_overlapped(georef_dataset, con, rfmo_to_keep = overlapping_zone_iotc_wcpfc_data_to_keep,
                                          rfmo_not_to_keep = (if (overlapping_zone_iotc_wcpfc_data_to_keep == "IOTC"){"WCPFC"} else {"IOTC"}) ,
                                          strata = options_strata_overlap_iotc_wcpfc)
    config$logger.info(paste0("Keeping only data from ",overlapping_zone_iotc_wcpfc_data_to_keep," in the IOTC/WCPFC overlapping zone..."))
    # georef_dataset_level0_step10 <- georef_dataset
    # georef_dataset_level0_step10_ancient<- overlapping_ancient_method
    # georef_dataset_level0_step10_reverse <- reverse_overlapping
    
    config$logger.info(paste0("Keeping only data from ",overlapping_zone_iotc_wcpfc_data_to_keep," in the IOTC/WCPFC overlapping zone OK"))
    
    function_recap_each_step("overlap_iotc_wcpfc",
                             georef_dataset,
                             paste0("In this step, the georeferenced data present on the overlapping zone between IOTC and WCPFC is handled.
                   The option for the strata overlapping allow to handle the maximum similarities allowed between two data to keep both.
                   In the case the data is identical on the stratas privided, the remaining data is from ",options_overlapping_zone_iotc_wcpfc_data_to_keep) ,
                             "function_overlapped",
                             list(options_include_WCPFC,
                                  options_include_IOTC, options_overlapping_zone_iotc_wcpfc_data_to_keep, options_strata_overlap_iotc_wcpfc))
    
  }
  
  
  
  
  
  
  
  
  #-----------------------------------------------------------------------------------------------------------------------------------------------------------
  config$logger.info("LEVEL 0 => STEP 7/: Overlapping zone (WCPFC/CCSBT): keep data from WCPFC or CCSBT?")
  if(!exists("opts$strata_overlap_sbf")){options_strata_overlap_sbf <- c("species", "year")
  } else {options_strata_overlap_sbf<-unlist(strsplit(opts$strata_overlap_sbf, split=","))}
  
  #-----------------------------------------------------------------------------------------------------------------------------------------------------------
  if (opts$include_WCPFC && opts$include_CCSBT && !is.null(opts$overlapping_zone_wcpfc_ccsbt_data_to_keep)) {
    
    georef_dataset <- function_overlapped(dataset = georef_dataset, con =con, rfmo_to_keep = overlapping_zone_wcpfc_ccsbt_data_to_keep,
                                          rfmo_not_to_keep = (if (overlapping_zone_wcpfc_ccsbt_data_to_keep == "WCPFC"){"CCSBT"} else {"WCPFC"}),
                                          strata =options_strata_overlap_sbf)
    config$logger.info(paste0("Keeping only data from ",overlapping_zone_wcpfc_ccsbt_data_to_keep," in the WCPFC/CCSBT overlapping zone..."))
    
    config$logger.info(paste0("Keeping only data from ",overlapping_zone_wcpfc_ccsbt_data_to_keep," in the WCPFC/CCSBT overlapping zone OK"))
    
    function_recap_each_step("overlap_ccsbt_wcpfc",
                             georef_dataset,
                             paste0("In this step, the georeferenced data present on the overlapping zone between CCSBT and WCPFC is handled.
                            The option for the strata overlapping allow to handle the maximum similarities allowed between two data to keep both.",
                                    "In the case the data is identical on the stratas privided, the remaining data is from ",options_overlapping_zone_wcpfc_ccsbt_data_to_keep),
                             "function_overlapped",
                             list( options_include_CCSBT  ,
                                   options_include_WCPFC ,
                                   options_overlapping_zone_wcpfc_ccsbt_data_to_keep, options_strata_overlap_sbf ))
    
  }
  
  
  
  #-----------------------------------------------------------------------------------------------------------------------------------------------------------
  config$logger.info("LEVEL 0 => STEP 8/8: Overlapping zone (ICCAT/CCSBT): keep data from ICCAT or CCSBT?")
  #-----------------------------------------------------------------------------------------------------------------------------------------------------------
  if (opts$include_ICCAT && opts$include_CCSBT && !is.null(opts$overlapping_zone_iccat_ccsbt_data_to_keep)) {
    
    georef_dataset <- function_overlapped(dataset = georef_dataset, con =con, rfmo_to_keep = overlapping_zone_iccat_ccsbt_data_to_keep,
                                          rfmo_not_to_keep = (if (overlapping_zone_iccat_ccsbt_data_to_keep == "ICCAT"){"CCSBT"} else {"ICCAT"}),
                                          strata = options_strata_overlap_sbf)
    config$logger.info(paste0("Keeping only data from ",overlapping_zone_iccat_ccsbt_data_to_keep," in the ICCAT/CCSBT overlapping zone..."))
    
    # georef_dataset_level0_step8_ancient<- overlapping_ancient_method
    # georef_dataset_level0_step8 <- georef_dataset
    # georef_dataset_level0_step8_reverse <- reverse_overlapping
    
    config$logger.info(paste0("Keeping only data from ",overlapping_zone_iccat_ccsbt_data_to_keep," in the ICCAT/CCSBT overlapping zone OK"))
    
    function_recap_each_step("overlap_iccat_ccsbt",
                             georef_dataset,
                             paste0("In this step, the georeferenced data present on the overlapping zone between ICCAT and CCSBT is handled.
                            The option for the strata overlapping allow to handle the maximum similarities allowed between two data to keep both.",
                                    "In the case the data is identical on the stratas privided, the remaining data is from ",options_overlapping_zone_iccat_ccsbt_data_to_keep),
                             
                             "function_overlapped",
                             list(options_include_CCSBT,
                                  options_include_ICCAT, options_overlapping_zone_iccat_ccsbt_data_to_keep, options_strata_overlap_sbf))
    
    
  }
  
  
  
  #-----------------------------------------------------------------------------------------------------------------------------------------------------------
  config$logger.info("LEVEL 0 => STEPs Overlapping zone (IOTC/CCSBT): keep data from IOTC or CCSBT?")
  #-----------------------------------------------------------------------------------------------------------------------------------------------------------
  if (opts$include_IOTC && opts$include_CCSBT && !is.null(opts$overlapping_zone_iotc_ccsbt_data_to_keep)) {
    
    georef_dataset <- function_overlapped(dataset = georef_dataset, con = con, rfmo_to_keep = overlapping_zone_iotc_ccsbt_data_to_keep,
                                          rfmo_not_to_keep = (if (overlapping_zone_iotc_ccsbt_data_to_keep == "IOTC"){"CCSBT"} else {"IOTC"}),
                                          strata = options_strata_overlap_sbf)
    config$logger.info(paste0("Keeping only data from ",overlapping_zone_iotc_ccsbt_data_to_keep," in the IOTC/CCSBT overlapping zone..."))
    # georef_dataset_level0_step9 <- georef_dataset
    # georef_dataset_level0_step9_ancient<- overlapping_ancient_method
    # georef_dataset_level0_step9_reverse <- reverse_overlapping
    
    config$logger.info(paste0("Keeping only data from ",overlapping_zone_iotc_ccsbt_data_to_keep," in the IOTC/CCSBT overlapping zone OK"))
    
    function_recap_each_step("overlap_iotc_ccsbt",
                             georef_dataset,
                             paste0("In this step, the georeferenced data present on the overlapping zone between IOTC and CCSBT is handled.
                            The option for the strata overlapping allow to handle the maximum similarities allowed between two data to keep both.",
                                    "In the case the data is identical on the stratas privided, the remaining data is from ",options_overlapping_zone_iccat_ccsbt_data_to_keep),
                             
                             "function_overlapped",
                             list( options_include_CCSBT  ,
                                   options_include_IOTC ,options_overlapping_zone_wcpfc_ccsbt_data_to_keep, options_overlapping_zone_iccat_ccsbt_data_to_keep,options_overlapping_zone_iotc_ccsbt_data_to_keep, options_strata_overlap_sbf ))
    
    
  }
  
  #--------irregular areas--------------------------------------------------------------------------------------------------------------------------------------------------
  config$logger.info("LEVEL 0 => irregular areas hanlding")
  #-----------------------------------------------------------------------------------------------------------------------------------------------------------
  
  if (opts$irregular_area %in% c("remove", "reallocate")) {
    source(file.path(url_scripts_create_own_tuna_atlas,'spatial_curation.R'))
    spatial_curation <- spatial_curation(con,georef_dataset, opts$irregular_area)
    georef_dataset <- spatial_curation$df
    removed_irregular_areas <- spatial_curation$df_input_areas_not_curated
    stats_irregular_areas <- spatial_curation$stats

    function_recap_each_step("irregular_area_handling",
                             georef_dataset,
                             paste0("In this step, we handle areas that does not match cwp grids norme") ,
                             "function_overlapped",
                             list(options_irregular_area))
    
  }
  
  names_list_irregular_areas <- c("removed_irregular_areas", "stats_irregular_areas") #file we want to save
  
  try(lapply(names_list_irregular_areas, function_write_RDS))
  
  
  
  
  
  
  #-----------------------------------------------------------------------------------------------------------------------------------------------------------
  config$logger.info("LEVEL 0 => Spatial Aggregation of data (5deg resolution datasets only: Aggregate data on 5° resolution quadrants)")
  #-----------------------------------------------------------------------------------------------------------------------------------------------------------
  if(!is.null(opts$aggregate_on_5deg_data_with_resolution_inferior_to_5deg)) if (opts$aggregate_on_5deg_data_with_resolution_inferior_to_5deg) {
    
    config$logger.info("Aggregating data that are defined on quadrants or areas inferior to 5° quadrant resolution to corresponding 5° quadrant...")
    source(file.path(url_scripts_create_own_tuna_atlas, "aggregate_resolution.R")) #modified for geoflow
    georef_dataset<-aggregate_resolution(con, georef_dataset, 6)
    georef_dataset<-georef_dataset$df
    
    
    
    config$logger.info("Aggregating data that are defined on quadrants or areas inferior to 5° quadrant resolution to corresponding 5° quadrant OK")
    function_recap_each_step("Aggregation",
                             georef_dataset,
                             "This step is to aggregate data on resolution lower than 5° in 5°.",
                             "spatial_curation_upgrade_resolution",
                             list(options_aggregate_on_5deg_data_with_resolution_inferior_to_5deg))
  }
  #-----------------------------------------------------------------
  
  
  if (opts$spatial_curation_data_mislocated %in% c("reallocate","remove")){
    
    config$logger.info("---------------------------------------spatial_curation_intersect_areas--------------------------------------------------------------")
    config$logger.info(sprintf("LEVEL 1 => STEP 3/5  for file [%s] is executed: Reallocation of mislocated data  (i.e. on land areas or without any spatial information) (data with no spatial information have the dimension 'geographic_identifier' set to 'UNK/IND' or 'NA'). Option is: [%s] ",entity$data$source[[1]], opts$spatial_curation_data_mislocated))
    config$logger.info("-----------------------------------------------------------------------------------------------------")
    
    ntons_before_this_step <- round(georef_dataset %>% select(value)  %>% sum())
    config$logger.info(sprintf("STEP 3/5 : Gridded catch dataset before Reallocation of mislocated data has [%s] lines and total catch is [%s] Tons", nrow(georef_dataset),ntons_before_this_step))
    
    config$logger.info("STEP 3/5: BEGIN function_spatial_curation_data_mislocated() function")
    georef_dataset<-spatial_curation_data_mislocated(entity=entity,
                                                     config=config,
                                                     df=georef_dataset,
                                                     spatial_curation_data_mislocated=opts$spatial_curation_data_mislocated)
    config$logger.info("STEP 3/5: END function_spatial_curation_data_mislocated() function")
    
    #@juldebar: pending => metadata elements below to be managed (commented for now)
    # metadata$description<-paste0(metadata$description,georef_dataset$description)
    # metadata$lineage<-c(metadata$lineage,georef_dataset$lineage)
    
    georef_dataset<-georef_dataset$dataset
    ntons_after_mislocated <- round(georef_dataset %>% select(value)  %>% sum())
    config$logger.info(sprintf("STEP 3/5 : Gridded catch dataset after Reallocation of mislocated data has [%s] lines and total catch is [%s] Tons", nrow(georef_dataset),ntons_after_mislocated))
    config$logger.info(sprintf("STEP 3/5 : Reallocation of mislocated data generated [%s] additionnal tons", ntons_after_mislocated-ntons_before_this_step))
    config$logger.info("END STEP 3/5")
    function_recap_each_step("Realocating_removing_mislocated_data",
                             georef_dataset,
                             "In this step, the mislocated data is hanlded. Either removed, reallocated or let alone, the data on continent and the data outside the competent rfmo area are targeted. ",
                             "spatial_curation_data_mislocated",
                             list(options_spatial_curation_data_mislocated))
    
    gc()
    
  }else{
    config$logger.info("-----------------------------------------------------------------------------------------------------")
    config$logger.info(sprintf("LEVEL 1 => STEP 3/5 not executed  for file [%s] (since not selected in the workflow options, see column 'Data' of geoflow entities spreadsheet):  Reallocation of mislocated data  (i.e. on land areas or without any spatial information) (data with no spatial information have the dimension 'geographic_identifier' set to 'UNK/IND' or 'NA'). Option is: [%s] ",entity$data$source[[1]], opts$spatial_curation_data_mislocated))
    config$logger.info("-----------------------------------------------------------------------------------------------------")
  }
  
  if (!is.null(opts$curation_absurd_converted_data)){
    
    source(file.path(url_scripts_create_own_tuna_atlas,"curation_absurd_converted_data.R"))
    
    max_conversion_factor <- read.csv("data/max_conversion_factor.csv")
    
    georef_dataset <- curation_absurd_converted_data(georef_dataset = georef_dataset, 
    max_conversion_factor = max_conversion_factor)
    
    function_recap_each_step("Removing_absurd_nomt",
                             georef_dataset,
                             "In this step, we target implausible data. We check data having declaration both in NOMT and MTNO and if the conversion factor is implausible.
                   We either remove NOMT strata which corresponds to MTNO declaration implausible or me remove the corresponding MTNO data. More details are available in the pdf file attached.",
                             "spatial_curation_data_mislocated",
                             list(curation_absurd_converted_data))
  }
  
  
  # unit conversion already given factors -----------------------------------
  
  if(!is.null(opts$unit_conversion_convert)) if (opts$unit_conversion_convert){
      
  georef_dataset <- double_unit_data_handling(con = con, entity=entity,
                                              config=config,
                                              fact=fact,
                                              unit_conversion_csv_conversion_factor_url=opts$unit_conversion_csv_conversion_factor_url,
                                              unit_conversion_codelist_geoidentifiers_conversion_factors=opts$unit_conversion_codelist_geoidentifiers_conversion_factors,
                                              mapping_map_code_lists=opts$mapping_map_code_lists,
                                              georef_dataset=georef_dataset)
  function_recap_each_step("Removing NOMT and converting MTNO in MT",
                           georef_dataset,
                           "In this step, we target the data provided in Tons and Number of fish. The data initially in MTNO will be converted in MT and the data in NTMO will be removed.",
                           "",
                           list(""))
  
  new_version_iotc_raising_available <- TRUE
  if(file.exists("data/conversion_factors_IOTC.csv")){
  # unit conversion IOTC given factors -----------------------------------
  iotc_conv_fact <- read_csv("data/conversion_factors_IOTC.csv", 
                             col_types = cols(geographic_identifier = col_character(), 
                                              time_start = col_character(), time_end = col_character()))
  iotc_conv_fact_mapped <- map_codelists(con, opts$fact, mapping_dataset = mapping_dataset, dataset_to_map = iotc_conv_fact, mapping_keep_src_code,  source_authority_to_map = c("IOTC"))$dataset_mapped #this map condelist function is to retieve the mapping dataset used
    
  georef_dataset <- do_unit_conversion( entity=entity,
                                                    config=config,
                                                    fact=fact,
                                                    unit_conversion_csv_conversion_factor_url=iotc_conv_fact_mapped,
                                                    unit_conversion_codelist_geoidentifiers_conversion_factors="areas_tuna_rfmos_task2",
                                                    mapping_map_code_lists=opts$mapping_map_code_lists,
                                                    georef_dataset=georef_dataset, 
                                                    removing_numberfish_final = FALSE) # do not remove number of fish as they will be converted later with other conversion factor data
  
  
  function_recap_each_step("Harmonising units on IOTC data",
                           georef_dataset,
                           "In this step, we target the data provided in Tons and Number of fish provided by IOTC.
                 As a new conversion factor dataset has been provided by Emmanuel Chassot",
                           "",
                           list(""))
  
  
  }
  
  # unit conversion with factors -----------------------------------
  
  
    config$logger.info("-----------------------------------------------------------------------------------------------------")
    config$logger.info(sprintf("LEVEL 1 => STEP 2/5  for file [%s] is executed: Convert units by using A. Fonteneau file. Option is: [%s] ",entity$data$source[[1]], opts$unit_conversion_convert))
    config$logger.info("-----------------------------------------------------------------------------------------------------")
    mapping_map_code_lists <- TRUE
    if(!is.null(opts$mapping_map_code_lists)) mapping_map_code_lists = opts$mapping_map_code_lists
    if(is.null(opts$unit_conversion_csv_conversion_factor_url)) stop("Conversion of unit requires parameter 'unit_conversion_csv_conversion_factor_url'")
    if(is.null(opts$unit_conversion_codelist_geoidentifiers_conversion_factors)) stop("Conversion of unit requires parameter 'unit_conversion_codelist_geoidentifiers_conversion_factors'")
    
    ntons_before_this_step <- round(georef_dataset %>% filter(unit=="t")  %>% select(value)  %>% sum())
    config$logger.info(sprintf("STEP 2/5 : Gridded catch dataset before unit conversion has [%s] lines and total catch is [%s] Tons", nrow(georef_dataset),ntons_before_this_step))
    
    config$logger.info("STEP 2/5: BEGIN do_unit_conversion() function to convert units of georef_dataset")
    
    georef_dataset <- do_unit_conversion( entity=entity,
                                          config=config,
                                          fact=fact,
                                          unit_conversion_csv_conversion_factor_url=opts$unit_conversion_csv_conversion_factor_url,
                                          unit_conversion_codelist_geoidentifiers_conversion_factors=opts$unit_conversion_codelist_geoidentifiers_conversion_factors,
                                          mapping_map_code_lists=opts$mapping_map_code_lists,
                                          georef_dataset=georef_dataset)
    config$logger.info("STEP 2/5: END do_unit_conversion() function")
    
    ntons_after_conversion <- round(georef_dataset %>% select(value)  %>% sum())
    config$logger.info(sprintf("STEP 2/5 : Gridded catch dataset after unit conversion has [%s] lines and total catch is [%s] Tons", nrow(georef_dataset),ntons_after_conversion))
    # config$logger.info(sprintf("STEP 2/5 : [%s] lines have been removed", nrow(georef_dataset)-nrow_before))
    config$logger.info(sprintf("STEP 2/5 : Unit conversion generated [%s] additionnal tons", ntons_after_conversion-ntons_before_this_step))
    config$logger.info(sprintf("STEP 2/5 : Total number for 'NO' unit is now [%s] individuals", georef_dataset %>% filter(unit=="no")  %>% select(value)  %>% sum()))
    config$logger.info("END STEP 2/5")
    function_recap_each_step("raising",
                             georef_dataset,
                             "In this step, we harmonise the data declared in NO, converting it in Tons using the by using A. Fonteneau file. The file used for the conversion can also be a parameter.", fonctions =
                               "do_unit_conversion, unit_conversion_csv_conversion_factor_url, extract_dataset,
                            map_codelist, convert_units",
                             list( options_mapping_map_code_lists ,
                                   options_unit_conversion_csv_conversion_factor_url ,
                                   options_unit_conversion_codelist_geoidentifiers_conversion_factors ,
                                   options_unit_conversion_convert))
    
    
    
    
  }else{
    config$logger.info("-----------------------------------------------------------------------------------------------------")
    config$logger.info(sprintf("LEVEL 1 => STEP 2/5 not executed  for file [%s] (since not selected in the workflow options, see column 'Data' of geoflow entities spreadsheet): Convert units by using A. Fonteneau file. Option is: [%s] ",entity$data$source[[1]], opts$unit_conversion_convert))
    config$logger.info("-----------------------------------------------------------------------------------------------------")
  }
  

  
  
  
  
  
  
  
  
  
  
  
  #end switch LEVEL 1
  
  
  
  # #-----------------------------------------------------------------------------------------------------------------------------------------------------------
  # #LEVEL 2 IRD PRODUCTS
  # #-----------------------------------------------------------------------------------------------------------------------------------------------------------
  
  config$logger.info("-----------------------------------------------------------------------------------------------------")
  config$logger.info("LEVEL 2 => STEP 1/3: Set parameters")
  config$logger.info("-----------------------------------------------------------------------------------------------------")
  # raising_georef_to_nominal <- opts$raising_georef_to_nominal
  iattc_ps_raise_flags_to_schooltype <- opts$iattc_ps_raise_flags_to_schooltype
  iattc_ps_dimension_to_use_if_no_raising_flags_to_schooltype <- opts$iattc_ps_dimension_to_use_if_no_raising_flags_to_schooltype
  iattc_ps_catch_billfish_shark_raise_to_effort <- opts$iattc_ps_catch_billfish_shark_raise_to_effort
  iccat_ps_include_type_of_school <- opts$iccat_ps_include_type_of_school
  
  config$logger.info("-----------------------------------------------------------------------------------------------------")
  config$logger.info("LEVEL 2 => STEP 2/3: Extract and load IRD Level 1 gridded catch data input")
  config$logger.info("-----------------------------------------------------------------------------------------------------")
  
  
  if(!is.null(opts$raising_georef_to_nominal)) if (opts$raising_georef_to_nominal){
    config$logger.info("-----------------------------------------------------------------------------------------------------")
    config$logger.info("LEVEL 2 => STEP 3/3: Raise IRD gridded Level 1 (1 or 5 deg) input with FIRMS Level O total (nominal) catch dataset")
    config$logger.info("-----------------------------------------------------------------------------------------------------")
    
    config$logger.info("Extract and load FIRMS Level 0 nominal catch data input (required if raising process is asked) ")
    nominal_catch <- readr::read_csv(entity$getJobDataResource(config, entity$data$source[[1]]), guess_max = 0)
    class(nominal_catch$value) <- "numeric"
    mapping_dataset<- read.csv(mapping_csv_mapping_datasets_url, stringsAsFactors = F,colClasses = "character")
    mapping_keep_src_code <- FALSE
    
    #no need to map it is already mapped
    # nominal_catch2 <- map_codelists(con, "catch", mapping_dataset, nominal_catch, mapping_keep_src_code)
    # nominal_catch <- read.csv2("entities/global_catch_1deg_1m_ps_bb_firms_Bastien_with_step_rds__level2/data/nominal_catch_mapped.csv", sep = ";")
    #         #@juldebar keep same units for all datatets
    if(any(nominal_catch$unit == "t")) nominal_catch[nominal_catch$unit == "t", ]$unit <- "t"
    if(any(nominal_catch$unit == "no")) nominal_catch[nominal_catch$unit == "no", ]$unit <- "no"
    class(nominal_catch$value) <- "numeric"
    #@juldebar if not provided by Google drive line below should be used if nominal catch has to be extracted from the database
    if(nrow(nominal_catch)==0){nominal_catch <-retrieve_nominal_catch(entity, config, opts)}
    config$logger.info(sprintf("Nominal catch dataset has [%s] lines", nrow(nominal_catch)))
    config$logger.info(paste0("Total of  nominal catch for file ",entity$data$source[[1]], "is : ",sum(nominal_catch$value),"  \n"))
    
    config$logger.info("Start raising process")
    
    if (fact=="catch"){
      config$logger.info("Fact=catch !")
      dataset_to_compute_rf=georef_dataset
      #@juldebar why do we use "year' as time dimension here ?
      if (is.null(opts$x_raising_dimensions)){
        x_raising_dimensions=c("fishingfleet","gear", "species","year","source_authority")}
      
      
    } else if (fact=="effort"){    ## If we raise the efforts, the RF is calculated using the georeferenced catch data. Hence, we need to retrieve the georeferenced catch data.
      cat("Catch datasets must be retrieved and processed in order to raise efforts. \nRetrieving georeferenced catch datasets from the Tuna atlas database...\n")
      dataset_catch<-NULL
      if (include_IOTC=="TRUE"){
        rfmo_dataset<-get_rfmos_datasets_level0("IOTC","catch",datasets_year_release)
        dataset_catch<-rbind(dataset_catch,rfmo_dataset)
        rm(rfmo_dataset)
      }
      if (include_WCPFC=="TRUE"){
        rfmo_dataset<-get_rfmos_datasets_level0("WCPFC","catch",datasets_year_release)
        dataset_catch<-rbind(dataset_catch,rfmo_dataset)
        rm(rfmo_dataset)
      }
      if (include_CCSBT=="TRUE"){
        rfmo_dataset<-get_rfmos_datasets_level0("CCSBT","catch",datasets_year_release)
        dataset_catch<-rbind(dataset_catch,rfmo_dataset)
        rm(rfmo_dataset)
      }
      if (include_IATTC=="TRUE"){
        rfmo_dataset<-get_rfmos_datasets_level0("IATTC",
                                                "catch",
                                                datasets_year_release,
                                                iattc_ps_raise_flags_to_schooltype=iattc_ps_raise_flags_to_schooltype,
                                                iattc_ps_dimension_to_use_if_no_raising_flags_to_schooltype=iattc_ps_dimension_to_use_if_no_raising_flags_to_schooltype,
                                                iattc_ps_catch_billfish_shark_raise_to_effort=TRUE)
        dataset_catch<-rbind(dataset_catch,rfmo_dataset)
        rm(rfmo_dataset)
      }
      if (include_ICCAT=="TRUE"){
        rfmo_dataset<-get_rfmos_datasets_level0("ICCAT",
                                                "catch",
                                                datasets_year_release,
                                                iccat_ps_include_type_of_school=iccat_ps_include_type_of_school)
        dataset_catch<-rbind(dataset_catch,rfmo_dataset)
        rm(rfmo_dataset)
      }
      
      
      if (mapping_map_code_lists=="TRUE"){
        dataset_catch<-map_codelists("catch",mapping_csv_mapping_datasets_url,dataset_catch,mapping_keep_src_code)$dataset_mapped
      }
      
      dataset_catch$time_start<-substr(as.character(dataset_catch$time_start), 1, 10)
      dataset_catch$time_end<-substr(as.character(dataset_catch$time_end), 1, 10)
      if (unit_conversion_convert=="TRUE"){
        # We use our conversion factors (IRD). This is now an input parameter of the script
        #@juldebar URL for unit_conversion_csv_conversion_factor_url of should not be hard coded, temporary patch
        #
        dataset_catch <- do_unit_conversion( entity=entity,
                                             config=config,
                                             fact="catch",
                                             unit_conversion_csv_conversion_factor_url=opts$unit_conversion_csv_conversion_factor_url,
                                             unit_conversion_codelist_geoidentifiers_conversion_factors=opts$unit_conversion_codelist_geoidentifiers_conversion_factors,
                                             mapping_map_code_lists=opts$mapping_map_code_lists,
                                             dataset_catch)
      }
      
      dataset_to_compute_rf=dataset_catch
      #@juldebar insert patch below to fix error in raise_get_rf function
      
      rm(dataset_catch)
      #@juldebar : update with the new name of "flag" dimension (now "fishingfleet")
      # x_raising_dimensions=c("fishingfleet","gear","year","source_authority")
    }
    
    class(dataset_to_compute_rf$value) <- "numeric"
    
    
    config$logger.info("Executing function function_raising_georef_to_nominal")
    config$logger.info(paste0("Total ",fact," before raising is : ",sum(georef_dataset$value),"\n"))
    config$logger.info(paste0("Total ",fact," in nominal data is : ",sum(nominal_catch$value),"\n"))
    
    georef_dataset<-function_raising_georef_to_nominal(con = con, opts = opts ,entity=entity,
                                                       config=config,
                                                       dataset_to_raise=georef_dataset,
                                                       nominal_dataset_df= nominal_catch,
                                                       # nominal_catch,
                                                       # dataset_to_compute_rf=nominal_catch,
                                                       dataset_to_compute_rf=dataset_to_compute_rf,
                                                       x_raising_dimensions=x_raising_dimensions)
    
    rm(dataset_to_compute_rf)
    
    #@juldebar: pending => metadata elements below to be managed (commented for now)
    # metadata$description<-paste0(metadata$description,georef_dataset$description)
    # metadata$lineage<-c(metadata$lineage,georef_dataset$lineage)
    # metadata$supplemental_information<-paste0(metadata$supplemental_information,georef_dataset$supplemental_information)
    
    georef_dataset<-georef_dataset$dataset
    config$logger.info(paste0("Total ",fact," after raising is now: ",sum(georef_dataset$value),"\n"))
    config$logger.info(sprintf("Gridded catch dataset has [%s] lines", nrow(georef_dataset)))
    config$logger.info(paste0("Total catch for data after raising is ",sum(georef_dataset$value),"  \n"))
    
    function_recap_each_step("Level2_RF1",
                             georef_dataset,
                             "In this step, the georeferenced data is raised to get closer of the nominal data. Aiming this, all the stratas having an equivalent (regarding the columns given in options) in nominal catches are raised to reach the equivalent. If the data is lower in nominal data for the strata, the data is lowed to reach the nominal amount ",
                             "function_raising_georef_to_nominal", list(opts$raising_georef_to_nominal ,
                                                                        iattc_ps_raise_flags_to_schooltype ,
                                                                        iattc_ps_dimension_to_use_if_no_raising_flags_to_schooltype ,
                                                                        iattc_ps_catch_billfish_shark_raise_to_effort ,
                                                                        iccat_ps_include_type_of_school, include_IATTC, include_IOTC,
                                                                        include_ICCAT, include_CCSBT, include_WCPFC,
                                                                        fact, raising_do_not_raise_wcfpc_data, raising_raise_only_for_PS_LL
                             ))
    
    
    
    if (fact=="catch"){
      config$logger.info("Fact=catch !")
      dataset_to_compute_rf=georef_dataset
      #@juldebar why do we use "year' as time dimension here ?
      if (is.null(opts$x_raising_dimensions)){
        x_raising_dimensions=c("gear", "species","year","source_authority")}
      
      
    } else if (fact=="effort"){    ## If we raise the efforts, the RF is calculated using the georeferenced catch data. Hence, we need to retrieve the georeferenced catch data.
      cat("Catch datasets must be retrieved and processed in order to raise efforts. \nRetrieving georeferenced catch datasets from the Tuna atlas database...\n")
      dataset_catch<-NULL
      if (include_IOTC=="TRUE"){
        rfmo_dataset<-get_rfmos_datasets_level0("IOTC","catch",datasets_year_release)
        dataset_catch<-rbind(dataset_catch,rfmo_dataset)
        rm(rfmo_dataset)
      }
      if (include_WCPFC=="TRUE"){
        rfmo_dataset<-get_rfmos_datasets_level0("WCPFC","catch",datasets_year_release)
        dataset_catch<-rbind(dataset_catch,rfmo_dataset)
        rm(rfmo_dataset)
      }
      if (include_CCSBT=="TRUE"){
        rfmo_dataset<-get_rfmos_datasets_level0("CCSBT","catch",datasets_year_release)
        dataset_catch<-rbind(dataset_catch,rfmo_dataset)
        rm(rfmo_dataset)
      }
      if (include_IATTC=="TRUE"){
        rfmo_dataset<-get_rfmos_datasets_level0("IATTC",
                                                "catch",
                                                datasets_year_release,
                                                iattc_ps_raise_flags_to_schooltype=iattc_ps_raise_flags_to_schooltype,
                                                iattc_ps_dimension_to_use_if_no_raising_flags_to_schooltype=iattc_ps_dimension_to_use_if_no_raising_flags_to_schooltype,
                                                iattc_ps_catch_billfish_shark_raise_to_effort=TRUE)
        dataset_catch<-rbind(dataset_catch,rfmo_dataset)
        rm(rfmo_dataset)
      }
      if (include_ICCAT=="TRUE"){
        rfmo_dataset<-get_rfmos_datasets_level0("ICCAT",
                                                "catch",
                                                datasets_year_release,
                                                iccat_ps_include_type_of_school=iccat_ps_include_type_of_school)
        dataset_catch<-rbind(dataset_catch,rfmo_dataset)
        rm(rfmo_dataset)
      }
      
      
      if (mapping_map_code_lists=="TRUE"){
        dataset_catch<-map_codelists("catch",mapping_csv_mapping_datasets_url,dataset_catch,mapping_keep_src_code)$dataset_mapped
      }
      
      dataset_catch$time_start<-substr(as.character(dataset_catch$time_start), 1, 10)
      dataset_catch$time_end<-substr(as.character(dataset_catch$time_end), 1, 10)
      if (unit_conversion_convert=="TRUE"){
        # We use our conversion factors (IRD). This should be an input parameter of the script
        #@juldebar URL for unit_conversion_csv_conversion_factor_url of should not be hard coded, temporary patch
        dataset_catch <- do_unit_conversion( entity=entity,
                                             config=config,
                                             fact="catch",
                                             unit_conversion_csv_conversion_factor_url=opts$unit_conversion_csv_conversion_factor_url,
                                             unit_conversion_codelist_geoidentifiers_conversion_factors=opts$unit_conversion_codelist_geoidentifiers_conversion_factors,
                                             mapping_map_code_lists=opts$mapping_map_code_lists,
                                             dataset_catch)
      }
      
      dataset_to_compute_rf=dataset_catch
      #@juldebar insert patch below to fix error in raise_get_rf function
      
      rm(dataset_catch)
      #@juldebar : update with the new name of "flag" dimension (now "fishingfleet")
      # x_raising_dimensions=c("species","gear","year","source_authority")
    }
    
    class(dataset_to_compute_rf$value) <- "numeric"
    
    georef_dataset<-function_raising_georef_to_nominal(con = con, opts = opts ,entity=entity,
                                                       config=config,
                                                       dataset_to_raise=georef_dataset,
                                                       nominal_dataset_df=nominal_catch,
                                                       # nominal_catch,
                                                       # dataset_to_compute_rf=nominal_catch,
                                                       dataset_to_compute_rf=dataset_to_compute_rf,
                                                       x_raising_dimensions=x_raising_dimensions)
    georef_dataset<-georef_dataset$dataset
    
    rm(dataset_to_compute_rf)
    
    #@juldebar: pending => metadata elements below to be managed (commented for now)
    # metadata$description<-paste0(metadata$description,georef_dataset$description)
    # metadata$lineage<-c(metadata$lineage,georef_dataset$lineage)
    # metadata$supplemental_information<-paste0(metadata$supplemental_information,georef_dataset$supplemental_information)
    
    config$logger.info(paste0("Total ",fact," after raising is now: ",sum(georef_dataset$value),"\n"))
    config$logger.info(sprintf("Gridded catch dataset has [%s] lines", nrow(georef_dataset)))
    config$logger.info(paste0("Total catch for data after raising is ",sum(georef_dataset$value),"  \n"))
    
    function_recap_each_step("Level2_RF2",
                             georef_dataset,
                             "In this step, the georeferenced data is raised to get closer of the nominal data. Aiming this, all the stratas having an equivalent (regarding the columns given in options) in nominal catches are raised to reach the equivalent. If the data is lower in nominal data for the strata, the data is lowed to reach the nominal amount ",
                             fonctions = "function_raising_georef_to_nominal",
                             list(raising_georef_to_nominal ,
                                  iattc_ps_raise_flags_to_schooltype ,
                                  iattc_ps_dimension_to_use_if_no_raising_flags_to_schooltype ,
                                  iattc_ps_catch_billfish_shark_raise_to_effort ,
                                  iccat_ps_include_type_of_school, include_IATTC, include_IOTC,
                                  include_ICCAT, include_CCSBT, include_WCPFC,
                                  fact, raising_do_not_raise_wcfpc_data, raising_raise_only_for_PS_LL
                             ))
    
    
    
    
    
    
    if (fact=="catch"){
      config$logger.info("Fact=catch !")
      dataset_to_compute_rf=georef_dataset
      #@juldebar why do we use "year' as time dimension here ?
      if(!is.null(opts$x_raising_dimensions)){
        x_raising_dimensions=c("species","year","source_authority")}
    } else if (fact=="effort"){    ## If we raise the efforts, the RF is calculated using the georeferenced catch data. Hence, we need to retrieve the georeferenced catch data.
      cat("Catch datasets must be retrieved and processed in order to raise efforts. \nRetrieving georeferenced catch datasets from the Tuna atlas database...\n")
      dataset_catch<-NULL
      if (include_IOTC=="TRUE"){
        rfmo_dataset<-get_rfmos_datasets_level0("IOTC","catch",datasets_year_release)
        dataset_catch<-rbind(dataset_catch,rfmo_dataset)
        rm(rfmo_dataset)
      }
      if (include_WCPFC=="TRUE"){
        rfmo_dataset<-get_rfmos_datasets_level0("WCPFC","catch",datasets_year_release)
        dataset_catch<-rbind(dataset_catch,rfmo_dataset)
        rm(rfmo_dataset)
      }
      if (include_CCSBT=="TRUE"){
        rfmo_dataset<-get_rfmos_datasets_level0("CCSBT","catch",datasets_year_release)
        dataset_catch<-rbind(dataset_catch,rfmo_dataset)
        rm(rfmo_dataset)
      }
      if (include_IATTC=="TRUE"){
        rfmo_dataset<-get_rfmos_datasets_level0("IATTC",
                                                "catch",
                                                datasets_year_release,
                                                iattc_ps_raise_flags_to_schooltype=iattc_ps_raise_flags_to_schooltype,
                                                iattc_ps_dimension_to_use_if_no_raising_flags_to_schooltype=iattc_ps_dimension_to_use_if_no_raising_flags_to_schooltype,
                                                iattc_ps_catch_billfish_shark_raise_to_effort=TRUE)
        dataset_catch<-rbind(dataset_catch,rfmo_dataset)
        rm(rfmo_dataset)
      }
      if (include_ICCAT=="TRUE"){
        rfmo_dataset<-get_rfmos_datasets_level0("ICCAT",
                                                "catch",
                                                datasets_year_release,
                                                iccat_ps_include_type_of_school=iccat_ps_include_type_of_school)
        dataset_catch<-rbind(dataset_catch,rfmo_dataset)
        rm(rfmo_dataset)
      }
      
      
      if (mapping_map_code_lists=="TRUE"){
        dataset_catch<-map_codelists("catch",mapping_csv_mapping_datasets_url,dataset_catch,mapping_keep_src_code)$dataset_mapped
      }
      
      dataset_catch$time_start<-substr(as.character(dataset_catch$time_start), 1, 10)
      dataset_catch$time_end<-substr(as.character(dataset_catch$time_end), 1, 10)
      if (unit_conversion_convert=="TRUE"){
        # We use our conversion factors (IRD). This should be an input parameter of the script
        #@juldebar URL for unit_conversion_csv_conversion_factor_url of should not be hard coded, temporary patch
        dataset_catch <- do_unit_conversion( entity=entity,
                                             config=config,
                                             fact="catch",
                                             unit_conversion_csv_conversion_factor_url=opts$unit_conversion_csv_conversion_factor_url,
                                             unit_conversion_codelist_geoidentifiers_conversion_factors=opts$unit_conversion_codelist_geoidentifiers_conversion_factors,
                                             mapping_map_code_lists=opts$mapping_map_code_lists,
                                             dataset_catch)
      }
      
      dataset_to_compute_rf=dataset_catch
      #@juldebar insert patch below to fix error in raise_get_rf function
      
      rm(dataset_catch)
      #@juldebar : update with the new name of "flag" dimension (now "fishingfleet")
      # x_raising_dimensions=c("species","gear","year","source_authority")
    }
    
    
    
    georef_dataset<-function_raising_georef_to_nominal(con = con, opts = opts, entity=entity,
                                                       config=config,
                                                       dataset_to_raise=georef_dataset,
                                                       nominal_dataset_df=nominal_catch,
                                                       # nominal_catch,
                                                       # dataset_to_compute_rf=nominal_catch,
                                                       dataset_to_compute_rf=dataset_to_compute_rf,
                                                       x_raising_dimensions=x_raising_dimensions)
    
    rm(dataset_to_compute_rf)
    
    georef_dataset<-georef_dataset$dataset
    
    function_recap_each_step("Level2_RF3without_gears",
                             georef_dataset,
                             "In this step, the georeferenced data is raised to get closer of the nominal data. Aiming this, all the stratas having an equivalent (regarding the columns given in options) in nominal catches are raised to reach the equivalent. If the data is lower in nominal data for the strata, the data is lowed to reach the nominal amount ",
                             "function_raising_georef_to_nominal, \n",
                             list(opts$raising_georef_to_nominal ,
                                  iattc_ps_raise_flags_to_schooltype ,
                                  iattc_ps_dimension_to_use_if_no_raising_flags_to_schooltype ,
                                  iattc_ps_catch_billfish_shark_raise_to_effort ,
                                  iccat_ps_include_type_of_school, include_IATTC, include_IOTC,
                                  include_ICCAT, include_CCSBT, include_WCPFC,
                                  fact, raising_do_not_raise_wcfpc_data, raising_raise_only_for_PS_LL
                             ))}else{
                               config$logger.info("LEVEL 2 => STEP 3/3 not executed (since not selected in the workflow options (see column 'Data' of geoflow entities spreadsheet)")
                             }
  
  
  
  
  
  if (opts$disaggregate_on_5deg_data_with_resolution_superior_to_5deg %in% c("disaggregate","remove")) {
    
    config$logger.info("-----------------------------------------------------------------------------------------------------")
    config$logger.info(sprintf("LEVEL 1 => STEP 4/5  for file [%s] is executed: Disaggregate data on 5° resolution quadrants (for 5deg resolution datasets only). Option is: [%s] ",entity$data$source[[1]], opts$disaggregate_on_5deg_data_with_resolution_superior_to_5deg))
    config$logger.info("-----------------------------------------------------------------------------------------------------")
    
    
    ntons_before_this_step <- round(georef_dataset %>% select(value)  %>% sum())
    config$logger.info(sprintf("STEP 4/5 : Gridded catch dataset before Disaggregate data on 5° resolution has [%s] lines and total catch is [%s] Tons", nrow(georef_dataset),ntons_before_this_step))
    
    config$logger.info("STEP 4/5: BEGIN function_disaggregate_on_resdeg_data_with_resolution_superior_to_resdeg() function")
    
    georef_dataset<-function_disaggregate_on_resdeg_data_with_resolution_superior_to_resdeg(entity,config,opts,
                                                                                            georef_dataset=georef_dataset,
                                                                                            resolution=5,
                                                                                            action_to_do=opts$disaggregate_on_5deg_data_with_resolution_superior_to_5deg)
    config$logger.info("STEP 4/5: END function_disaggregate_on_resdeg_data_with_resolution_superior_to_resdeg() function")
    
    
    georef_dataset<-georef_dataset$dataset
    ntons_after_disaggregation_5deg <- round(georef_dataset %>% select(value)  %>% sum())
    config$logger.info(sprintf("STEP 4/5 : Gridded catch dataset after Disaggregate data on 5° resolution has [%s] lines and total catch is [%s] Tons", nrow(georef_dataset),ntons_after_disaggregation_5deg))
    config$logger.info(sprintf("STEP 4/5 : Disaggregate data on 5° generated [%s] additionnal tons", ntons_after_disaggregation_5deg-ntons_before_this_step))
    config$logger.info("END STEP 4/5")
    function_recap_each_step("Disaggregate5deg",
                             georef_dataset,
                             "This step disaggregate data on resolution higher than 5° in 5° resolution. iGridded catch dataset before Disaggregate data on 5° resolution has [%s] lines and total catch is [%s] Tons",
                             "function_disaggregate_on_resdeg_data_with_resolution_superior_to_resdeg",
                             list(options_disaggregate_on_5deg_data_with_resolution_superior_to_5deg))
  }else{
    config$logger.info("-----------------------------------------------------------------------------------------------------")
    config$logger.info(sprintf("LEVEL 1 => STEP 4/5 not executed  for file [%s] (since not selected in the workflow options, see column 'Data' of geoflow entities spreadsheet):  Disaggregate data on 5° resolution quadrants (for 5deg resolution datasets only). Option is: [%s] ",entity$data$source[[1]], opts$disaggregate_on_5deg_data_with_resolution_superior_to_5deg))
    config$logger.info("-----------------------------------------------------------------------------------------------------")
  }
  
  
  
  if (opts$disaggregate_on_1deg_data_with_resolution_superior_to_1deg %in% c("disaggregate","remove")) {
    
    config$logger.info("-----------------------------------------------------------------------------------------------------")
    config$logger.info(sprintf("LEVEL 1 => STEP 5/5 for file [%s] is executed: Disaggregate data on 1° resolution quadrants (for 1deg resolution datasets only). Option is: [%s] ",entity$data$source[[1]], opts$disaggregate_on_1deg_data_with_resolution_superior_to_1deg))
    config$logger.info("-----------------------------------------------------------------------------------------------------")
    
    ntons_before_this_step <- round(georef_dataset %>% select(value)  %>% sum())
    config$logger.info(sprintf("STEP 5/5 : Gridded catch dataset before Disaggregate data on 1° has [%s] lines and total catch is [%s] Tons", nrow(georef_dataset),ntons_before_this_step))
    
    config$logger.info("STEP 5/5: BEGIN function_disaggregate_on_resdeg_data_with_resolution_superior_to_resdeg() function")
    georef_dataset<-function_disaggregate_on_resdeg_data_with_resolution_superior_to_resdeg(entity,config,opts,
                                                                                            georef_dataset=georef_dataset,
                                                                                            resolution=1,
                                                                                            action_to_do=opts$disaggregate_on_1deg_data_with_resolution_superior_to_1deg)
    config$logger.info("STEP 5/5: END function_disaggregate_on_resdeg_data_with_resolution_superior_to_resdeg() function")
    
    
    georef_dataset<-georef_dataset$dataset
    ntons_after_disaggregation_1deg <- round(georef_dataset %>% select(value)  %>% sum())
    config$logger.info(sprintf("STEP 5/5 : Gridded catch dataset after Disaggregate data on 1° has [%s] lines and total catch is now [%s] Tons", nrow(georef_dataset),ntons_after_disaggregation_1deg))
    config$logger.info(sprintf("STEP 5/5 : Disaggregate data on 1° generated [%s] additionnal tons", ntons_after_disaggregation_1deg-ntons_before_this_step))
    config$logger.info("END STEP 5/5")
    function_recap_each_step("Disaggregate1deg",
                             georef_dataset,
                             "This step disaggregate data on resolution higher than 1° in 1° resolution. iGridded catch dataset before Disaggregate data on 1° resolution has [%s] lines and total catch is [%s] Tons",
                             "function_disaggregate_on_resdeg_data_with_resolution_superior_to_resdeg",
                             list(options_disaggregate_on_1deg_data_with_resolution_superior_to_1deg))
    gc()
    
  } else{
    config$logger.info("-----------------------------------------------------------------------------------------------------")
    config$logger.info(sprintf("LEVEL 1 => STEP 5/5 not executed  for file [%s] (since not selected in the workflow options, see column 'Data' of geoflow entities spreadsheet): Disaggregate data on 1° resolution quadrants (for 1deg resolution datasets only). Option is: [%s] ",entity$data$source[[1]], opts$disaggregate_on_1deg_data_with_resolution_superior_to_1deg))
    config$logger.info("-----------------------------------------------------------------------------------------------------")
  }
  gc()
  
  #-----------------------------------------------------------------------------------------------------------------------------------------------------------
  config$logger.info("LEVEL 0 => STEP 3/8: WCPFC at the end")
  #-----------------------------------------------------------------------------------------------------------------------------------------------------------
  if (!is.null(opts$filter_WCPFC_at_the_end)){
    config$logger.info(("Filtering WCPFC_at_the_end [%s]"))
    georef_dataset<-georef_dataset %>% dplyr::filter(source_authority != "WCPFC")
    config$logger.info("Filtering WCPFC OK")
    config$logger.info(sprintf("Gridded catch dataset has [%s] lines", nrow(georef_dataset)))
    
    function_recap_each_step("Filtering_on_WCPFC_at_the_end",
                             georef_dataset,
                             "This step is to remove data provided by WCPFC as it is not relevant for 1° resolution data.",
                             "", list(options_filter_WCPFC_at_the_end))
    
    
    
    
  }
  
  
  
  
    
    
    
  
  
  #-----------------------------------------------------------------------------------------------------------------------------------------------------------
  config$logger.info("LEVEL 0 => STEP 3/8: Grid spatial resolution filter")
  #-----------------------------------------------------------------------------------------------------------------------------------------------------------
  if (!is.null(opts$resolution_filter)){
    geograph_identifier <- georef_dataset
    geograph_identifier$geograph_identifier_num <- as.numeric(geograph_identifier$geographic_identifier)
    geograph_identifier <- geograph_identifier %>% filter(geograph_identifier_num == geographic_identifier)
    if(nrow(geograph_identifier) != nrow(georef_dataset)){
      
      query <- "SELECT code, st_area(geom), geom from area.cwp_grid"
      world_sf <- st_read(con, query = query)
      query <- "SELECT  code,st_area(geom), geom from area.irregular_areas_task2_iotc"
      irregular_iotc <- st_read(con, query = query)%>% filter(!st_is_empty(.))
      
      world_sf <- rbind(world_sf, irregular_iotc)
      
      shapefile.fix <- st_make_valid(world_sf)%>% filter(!st_is_empty(.))
      area <-  case_when(opts$resolution_filter == "5" ~ "1", opts$resolution_filter == "6" ~ "25")
      shape_without_geom  <- shapefile.fix %>% as_tibble() %>% select(-geom) %>% filter(st_area == as.numeric(area))
      georef_dataset <- georef_dataset %>% semi_join(shape_without_geom, by =c("geographic_identifier"= "code"))} else{
      georef_dataset <- georef_dataset[startsWith(georef_dataset$geographic_identifier, opts$resolution_filter),]
        
        
        
        
      }
    # georef_dataset <- georef_dataset[startsWith(georef_dataset$geographic_identifier, opts$resolution_filter),]
    function_recap_each_step("filtering_on_spatial_resolution",
                             georef_dataset,
                             "This step is to filter on the wanted resolution.",
                             "", list(options_resolution_filter))
    
    
    
  }
  
  
  #-----------------------------------------------------------------------------------------------------------------------------------------------------------
  config$logger.info("LEVEL 0 => STEP 3/8: Apply filters on fishing gears if needed (Filter data by groups of gears) ")
  #-----------------------------------------------------------------------------------------------------------------------------------------------------------
  if (!is.null(opts$gear_filter)){
    gear_filter<-unlist(strsplit(opts$gear_filter, split=","))
    config$logger.info(sprintf("Filtering by gear(s) [%s]", paste(gear_filter, collapse=",")))
    georef_dataset<-georef_dataset %>% dplyr::filter(gear %in% gear_filter)
    config$logger.info("Filtering gears OK")
    config$logger.info(sprintf("Gridded catch dataset has [%s] lines", nrow(georef_dataset)))
    
    function_recap_each_step("filtering_on_gear",
                             georef_dataset,
                             "This step is to filter on gears if needed (for 1 deg resolution only few gears should be kept, other are irrelevant).",
                             "", list(options_gear_filter))
    
    
    
    
  }
  

  #-----------------------------------------------------------------------------------------------------------------------------------------------------------
  config$logger.info("Last step/8: Apply filters if filter needed (Filter data by groups of everything) ")
  #-----------------------------------------------------------------------------------------------------------------------------------------------------------
  
  
  if(!is.null(opts$filtering)){parameter_filtering <-opts$filtering} else{ parameter_filtering <-list(species = NULL, fishingfleet = NULL)}
  if(is.character(parameter_filtering)){
    parameter_filtering <- eval(parse(text=toString(parameter_filtering)))
  }
  
  matchingList <- parameter_filtering %>% purrr::keep( ~ !is.null(.) )
  
  filtering_function = function(dataframe_to_filter, filtering_params = matchingList){
    colnames_to_filter <- colnames(dataframe_to_filter %>% select(names(filtering_params)))
    names(filtering_params) <- colnames_to_filter
    
    filtering_params <- lapply(filtering_params, function(x){ #handling single filter
      if(length(x) == 1){
        x <- c(x, x) }else {
          x
        }
    }
    )
    
    if(length(matchingList)!= 0){  dataframe_to_filter <- dataframe_to_filter%>% filter(!! rlang::parse_expr(str_c(colnames_to_filter, matchingList, sep = '%in%', collapse="&")))} else{dataframe_to_filter}
    
  }
  
  georef_dataset <- filtering_function(georef_dataset)
  
  
  
  
  dataset<-georef_dataset %>% group_by(.dots = setdiff(colnames(georef_dataset),"value")) %>% dplyr::summarise(value=sum(value))
  dataset<-data.frame(dataset)
  
  #----------------------------------------------------------------------------------------------------------------------------
  #@eblondel additional formatting for next time support
  dataset$time_start <- as.Date(dataset$time_start)
  dataset$time_end <- as.Date(dataset$time_end)
  #we enrich the entity with temporal coverage
  dataset_temporal_extent <- paste(as.character(min(dataset$time_start)), as.character(max(dataset$time_end)), sep = "/")
  entity$setTemporalExtent(dataset_temporal_extent)
  
  #if there is any entity relation with name 'codelists' we read the file
  df_codelists <- NULL
  cl_relations <- entity$relations[sapply(entity$relations, function(x){x$name=="codelists"})]
  if(length(cl_relations)>0){
    config$logger.info("Appending codelists to global dataset generation action output")
    googledrive_baseurl <- "https://drive.google.com/open?id="
    if(startsWith(cl_relations[[1]]$link, googledrive_baseurl)){
      #managing download through google drive
      config$logger.info("Downloading file using Google Drive R interface")
      drive_id <- unlist(strsplit(cl_relations[[1]]$link, "id="))[2]
      drive_id <- unlist(strsplit(drive_id, "&export"))[1] #control in case export param is appended
      googledrive::drive_download(file = googledrive::as_id(drive_id), path = file.path("data", paste0(entity$identifiers[["id"]], "_codelists.csv")))
      df_codelists <- read.csv(file.path("data", paste0(entity$identifiers[["id"]], "_codelists.csv")))
    }else{
      df_codelists <- read.csv(cl_relations[[1]]$link)
    }
  }
  
  
  #@geoflow -> output structure as initially used by https://raw.githubusercontent.com/ptaconet/rtunaatlas_scripts/master/workflow_etl/scripts/generate_dataset.R
  dataset <- list(
    dataset = dataset,
    additional_metadata = NULL, #nothing here
    codelists = df_codelists #in case the entity was provided with a link to codelists
  )
  
  
  if (fact=="effort" & DATA_LEVEL %in% c("1", "2")){
    # Levels 1 and 2 of non-global datasets should be expressed with tRFMOs code lists. However, for the effort unit code list and in those cases, we take the tuna atlas effort unit codes although this is not perfect. but going back to tRFMOs codes is too complicated
    df_codelists$code_list_identifier[which(df_codelists$dimension=="unit")]<-"effortunit_rfmos"
  }
  # ltx_combine(combine = wd, out = "alltex.tex", clean = 0)

  #@geoflow -> export as csv
  output_name_dataset <- file.path("data", paste0(entity$identifiers[["id"]], "_harmonized.csv"))
  readr::write_csv(dataset$dataset, output_name_dataset)#export with fwrite which simplifies the data having too many decimals
  output_name_codelists <- file.path("data", paste0(entity$identifiers[["id"]], "_codelists.csv"))
  write.csv(dataset$codelists, output_name_codelists, row.names = FALSE)
  # ---------------------------------------------------------------------------------------------------------------------------
  
  entity$addResource("harmonized", output_name_dataset)
  
  entity$addResource("codelists", output_name_codelists)
  
  entity$addResource("geom_table", opts$geom_table)
  #### END
  config$logger.info("-----------------------------------------------------------------------------------------------------")
  config$logger.info("End: Your tuna atlas dataset has been created!")
  config$logger.info("-----------------------------------------------------------------------------------------------------")
  # write.csv(options)
  
  rm(georef_dataset)
  
  gc()
}



