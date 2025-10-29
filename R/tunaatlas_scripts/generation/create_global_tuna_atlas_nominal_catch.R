######################################################################
##### 52North WPS annotations ##########
######################################################################
# wps.des: id = create_own_tuna_atlas_nominal_catch, title = Create your own georeferenced nominal catch Tuna Atlas dataset, abstract = This algorithm allows to create own regional or global tuna altas. It takes as input the public domain datasets of the five Tuna Regional Fisheries Management Organizations (tRFMOs) (IOTC|ICCAT|WCPFC|IATTC|CCSBT) stored within the Sardara database. It proposes a set of parameters to customize the computation of the tuna atlas. ;
# wps.in: id = include_IOTC, type = string, title = Include IOTC data (Indian Ocean) in the atlas (TRUE or FALSE), value = TRUE;
# wps.in: id = include_ICCAT, type = string, title = Include ICCAT data (Atlantic Ocean) in the tuna atlas?, value = TRUE;
# wps.in: id = include_IATTC, type = string, title = Include IATTC data (Eastern Pacific Ocean) in the tuna atlas?, value = TRUE;
# wps.in: id = include_WCPFC, type = string, title = Include WCPFC data (Western Pacific Ocean) in the tuna atlas?, value = TRUE;
# wps.in: id = include_CCSBT, type = string, title = Include CCSBT data (Southern hemisphere Oceans - only Southern Bluefin Tuna) in the atlas?, value = TRUE;
# wps.in: id = datasets_year_release, type = string, title = Year of release of the datasets by the tRFMOs. First available year is 2017. Usually, datasets released in the year Y contain the time series from the beginning of the fisheries (e.g. 1950) to year Y-2 (included). For instance 2017 will extract the datasets released in 2017 and that cover the time series from 1950 to 2015 (included), value = 2017;
# wps.in: id = iccat_nominal_catch_spatial_stratification, type = string, title = Concerns ICCAT Nominal catch data. Use only if parameter include_ICCAT is set to TRUE. ICCAT nominal catch datasets can be spatially stratified either by sampling areas or by stock areas. Which spatial stratification to select for the output dataset? sampling_area or stock_area. ,value = "sampling_area|stock_area"
# wps.in: id = mapping_map_code_lists, type = string, title = Map code lists (gears, species, flags, schooltypes, catchtype)? When using multiple sources of data (i.e. multiple RFMOS), code lists used by the various tRFMOs might be different. They should therefore be mapped to single code lists in order to be able to compare the data. TRUE : map code lists. The url to the datasets providing the code list mappings to use must be set in the parameter mapping_source_mappings. See parameter mapping_source_mappings for more details. FALSE : do not map code lists. Output data will use input codes., value = "TRUE" ;
# wps.in: id = mapping_csv_mapping_datasets_url, type = string, title = Use only if parameter mapping_map_code_lists is set to TRUE. Path to the CSV file containing the dimensions that must be mapped and the name of the mapping dataset for each dimension mapped. The mapping datasets must be available in Sardara database. An example of this CSV can be found here: https://goo.gl/2hA1sq. , value="http://data.d4science.org/ZWFMa3JJUHBXWk9NTXVPdFZhbU5BUFEyQnhUeWd1d3lHbWJQNStIS0N6Yz0" ;
# wps.in: id = mapping_keep_src_code, type = string, title = Use only if parameter mapping_map_code_lists is set to TRUE. In case of code list mapping (mapping_map_code_lists==TRUE) keep source coding system column? TRUE : conserve in the output dataset both source and target coding systems columns. FALSE : conserve only target coding system. , value= "FALSE" ;
# wps.in: id = SBF_data_rfmo_to_keep, type = string, title = Concerns Southern Bluefin Tuna (SBF) data. Use only if parameter include_CCSBT is set to TRUE. SBF tuna data do exist in both CCSBT data and the other tuna RFMOs data. Wich data should be kept? CCSBT : CCSBT data are kept for SBF. other_trfmos : data from the other TRFMOs are kept for SBF. NULL : Keep data from all the tRFMOs. Caution: with the option NULL, data in the overlapping zones are likely to be redundant., value = "CCSBT|other_trfmos|NULL";
# wps.out: id = zip_namefile, type = text/zip, title = Outputs are 3 csv files: the dataset of georeferenced catches + a dataset of metadata (including informations on the computation, i.e. how the primary datasets were transformed by each correction) [TO DO] + a dataset providing the code lists used for each dimension (column) of the output dataset [TO DO]. All outputs and codes are compressed within a single zip file. ; 

function(action, entity, config){
  opts <- action$options
  # #############
  #action options
  recap_each_step = if(!is.null(opts$recap_each_step)) opts$recap_each_step else TRUE #default FALSE
  SBF_data_rfmo_to_keep = if(!is.null(opts$SBF_data_rfmo_to_keep)) opts$SBF_data_rfmo_to_keep else "CCSBT"
  from_repo = if(!is.null(opts$from_repo)) opts$from_repo else FALSE
  
  
  #packages
  if(!require(dplyr)){
    install.packages("dplyr")
    require(dplyr)
  }  
  if(!require(readr)){
    install.packages("readr")
    require(readr)
  }
  
  if(from_repo){
    nominal_catch <-
      readr::read_csv("~/firms-gta/geoflow-tunaatlas/data/global_nominal_catch_firms_level0_harmonized.csv",
                      guess_max = 0) %>% dplyr::mutate(measurement_value = as.numeric(measurement_value))
  } else {
  
  # connect to Tuna atlas database
  con <- config$software$output$dbi
  
  #scripts
  url_scripts_create_own_tuna_atlas <- "https://raw.githubusercontent.com/eblondel/geoflow-tunaatlas/master/R/tunaatlas_scripts/generation"
  source(file.path(url_scripts_create_own_tuna_atlas, "retrieve_nominal_catch.R"))
  
  # For final processing
  source(file.path(url_scripts_create_own_tuna_atlas, "process_and_aggregate_dataset.R"))
  
  
  #for reporting
  CWP.dataset::write_options_to_csv(opts)
  
  
  #### 1) Retrieve tuna RFMOs data from Sardara DB at level 0. 
  config$logger.info("Retrieving RFMOs nominal catch...")
  nominal_catch <-retrieve_nominal_catch(entity, config, opts)
  config$logger.info("Retrieving RFMOs nominal catch OK")
  
  if(recap_each_step){
    CWP.dataset::function_recap_each_step(
      "rawdata",
      nominal_catch, "rawdata","retrieve_nominal_catch",NULL)
  }
  
  #### 2) Southern Bluefin Tuna (SBF): SBF data: keep data from CCSBT or data from the other tuna RFMOs?
  
  if (!is.null(SBF_data_rfmo_to_keep)){
    
    config$logger.info(paste0("Keeping only data from ",SBF_data_rfmo_to_keep," for the Southern Bluefin Tuna..."))
    if (SBF_data_rfmo_to_keep=="CCSBT"){
      nominal_catch <- nominal_catch[ which(!(nominal_catch$species %in% "SBF" & nominal_catch$source_authority %in% c("ICCAT","IOTC","IATTC","WCPFC"))), ]
    } else {
      nominal_catch <- nominal_catch[ which(!(nominal_catch$species %in% "SBF" & nominal_catch$source_authority == "CCSBT")), ]
    }
    config$logger.info(paste0("Keeping only data from ",SBF_data_rfmo_to_keep," for the Southern Bluefin Tuna OK")) 
    
    if(recap_each_step){
      CWP.dataset::function_recap_each_step(
        "Filtering_SBF_data",
        nominal_catch,
        paste0(
          "Filtering SBF data to keep only data from",SBF_data_rfmo_to_keep),
        "inner_join"  ,
        NULL
      )
    }
    
  }
  }
  #final step
  source(file.path(url_scripts_create_own_tuna_atlas, "process_and_aggregate_dataset.R"))
  process_and_aggregate_dataset(nominal_catch, entity, config, opts, 
                                columns_to_keep = c("source_authority", "species", "gear_type", "fishing_fleet", "fishing_mode", "time_start", "time_end", "year", "month", "quarter", "geographic_identifier", "measurement_unit", "measurement_value", 
                                                    "measurement", "measurement_type", "measurement_processing_level"))
  
  #### END
  config$logger.info("End: Your tuna atlas dataset has been created!")
}
