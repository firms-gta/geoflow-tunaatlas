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

  #packages
  if(!require(dplyr)){
    install.packages("dplyr")
    require(dplyr)
  }  
  if(!require(readr)){
    install.packages("readr")
    require(readr)
  }
  
  # #############
  #action options
  recap_each_step = if(!is.null(opts$recap_each_step)) opts$recap_each_step else FALSE #default FALSE
  mapping_map_code_lists = if(!is.null(opts$mapping_map_code_lists)) opts$mapping_map_code_lists else TRUE #default is TRUE
  mapping_keep_src_code = if(!is.null(opts$mapping_keep_src_code)) opts$mapping_keep_src_code else FALSE #default is FALSE
  source_authority_to_map = if(!is.null(opts$source_authority_to_map)) opts$source_authority_to_map else c("IATTC", "CCSBT", "WCPFC")
  SBF_data_rfmo_to_keep = if(!is.null(opts$SBF_data_rfmo_to_keep)) opts$SBF_data_rfmo_to_keep else "CCSBT"
  
  # connect to Tuna atlas database
  con <- config$software$output$dbi
  
  #scripts
  url_scripts_create_own_tuna_atlas <- "https://raw.githubusercontent.com/eblondel/geoflow-tunaatlas/master/tunaatlas_scripts/generation"
  source(file.path(url_scripts_create_own_tuna_atlas, "map_codelists.R")) #modified for geoflow
  source(file.path(url_scripts_create_own_tuna_atlas, "retrieve_nominal_catch.R"))
  
  #for reporting
  source("https://raw.githubusercontent.com/firms-gta/geoflow-tunaatlas/master/Analysis_markdown/functions/write_options_to_csv.R")
  source("https://raw.githubusercontent.com/firms-gta/geoflow-tunaatlas/master/Analysis_markdown/functions/function_recap_each_step.R") # new function to create rds for each treatment
  source("https://raw.githubusercontent.com/firms-gta/geoflow-tunaatlas/master/Analysis_markdown/functions/copyrmd.R")
  # Saving options in a csv file and creating a new variable for each options
  write_options_to_csv(opts)
  
  
  #### 1) Retrieve tuna RFMOs data from Sardara DB at level 0. 
  config$logger.info("Retrieving RFMOs nominal catch...")
  nominal_catch <-retrieve_nominal_catch(entity, config, opts)
  config$logger.info("Retrieving RFMOs nominal catch OK")
  
  #### 2) Map code lists 
  
  if (mapping_map_code_lists){
    
    config$logger.info("Reading the CSV containing the dimensions to map + the names of the code list mapping datasets. Code list mapping datasets must be available in the database.")
    mapping_csv_mapping_datasets_url <- "https://raw.githubusercontent.com/fdiwg/fdi-mappings/main/global/firms/gta/codelist_mapping_rfmos_to_global.csv"
    mapping_dataset <- read.csv(mapping_csv_mapping_datasets_url, stringsAsFactors = F,colClasses = "character")
    
    config$logger.info("Mapping code lists of georeferenced datasets...")
    mapping_codelist <- map_codelists(con, "catch", mapping_dataset, nominal_catch, mapping_keep_src_code, summary_mapping = TRUE, source_authority_to_map = source_authority_to_map)
    config$logger.info("Mapping code lists of georeferenced datasets OK")
    nominal_catch = mapping_codelist$dataset_mapped
    
    recap_mapping <- mapping_codelist$recap_mapping
    stats_total <- mapping_codelist$stats_total
    not_mapped_total <- mapping_codelist$not_mapped_total
    
    config$logger.info("Mapping code lists of georeferenced datasets OK")
    
    if(recap_each_step){
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
	}
	
	# Temporary patch  --------------------------------------------------------
	## Species RMJ -------------
	nominal_catch <- nominal_catch %>% dplyr::mutate(species = case_when(species == "RMJ" ~ "RMM", TRUE ~ species))
	
  ## Measurement type for ICCAT and IOTC -------------
  nominal_catch <- nominal_catch %>% 
      dplyr::mutate(measurement_type = case_when(measurement_type == "landings" ~ "Landings",
                                                 measurement_type == "discards" ~ "Discards",
      TRUE ~ measurement_type))
    
	# Filtering on species under mandate --------------------------------------
	# done base on mapping between source_authority (tRFMO) and species 
	url_mapping_asfis_rfmo = "https://raw.githubusercontent.com/fdiwg/fdi-mappings/main/cross-term/codelist_mapping_source_authority_species.csv"
	species_to_be_kept_by_rfmo_in_level0 <- readr::read_csv(url_mapping_asfis_rfmo)
	
	removed <- nominal_catch %>% dplyr::anti_join(species_to_be_kept_by_rfmo_in_level0, by = c("species" = "species",
	                                                                                           "source_authority" = "source_authority"))
	nominal_catch <- nominal_catch %>% dplyr::inner_join(species_to_be_kept_by_rfmo_in_level0, by = c("species" = "species",
  "source_authority" = "source_authority"))
	
  
	if(recap_each_step){
	  function_recap_each_step(
		"Filtering species",
		nominal_catch,
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
  
  
  #### 9) Southern Bluefin Tuna (SBF): SBF data: keep data from CCSBT or data from the other tuna RFMOs?
  
  if (!is.null(SBF_data_rfmo_to_keep)){
    
    config$logger.info(paste0("Keeping only data from ",SBF_data_rfmo_to_keep," for the Southern Bluefin Tuna..."))
    if (SBF_data_rfmo_to_keep=="CCSBT"){
      nominal_catch <- nominal_catch[ which(!(nominal_catch$species %in% "SBF" & nominal_catch$source_authority %in% c("ICCAT","IOTC","IATTC","WCPFC"))), ]
    } else {
      nominal_catch <- nominal_catch[ which(!(nominal_catch$species %in% "SBF" & nominal_catch$source_authority == "CCSBT")), ]
    }
    config$logger.info(paste0("Keeping only data from ",SBF_data_rfmo_to_keep," for the Southern Bluefin Tuna OK")) 
    
    if(recap_each_step){
      function_recap_each_step(
        "Filtering_SBF_data",
        nominal_catch,
        paste0(
          "Filtering SBF data to keep only data from",SBF_data_rfmo_to_keep),
        "inner_join"  ,
        NULL
      )
    }
    
  }
  
  #final step
  dataset<-nominal_catch %>% group_by_(.dots = setdiff(colnames(nominal_catch),"masurement_value")) %>% dplyr::summarise(measurement_value=sum(measurement_value))
  dataset<-data.frame(dataset)
  if(!is.na(any(dataset$measurement_unit) == "TRUE")) if(any(dataset$measurement_unit) == "TRUE") dataset[(dataset$measurement_unit) == "TRUE",]$measurement_unit <- "t"  #patch because of https://github.com/firms-gta/geoflow-tunaatlas/issues/41
  
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
      googledrive::drive_download(file = googledrive::as_id(drive_id), path = file.path("data", paste0(entity$identifiers[["id"]], "_codelists.csv")), overwrite = TRUE)
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
  
  #@geoflow -> export as csv
  output_name_dataset <- file.path("data", paste0(entity$identifiers[["id"]], "_harmonized.csv"))
  write.csv(dataset$dataset, output_name_dataset, row.names = FALSE)
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
  #----------------------------------------------------------------------------------------------------------------------------  
  entity$addResource("harmonized", output_name_dataset)
  entity$addResource("public", output_name_dataset_public)
  entity$addResource("codelists", output_name_codelists)
  entity$addResource("geom_table", opts$geom_table)
  
  #### END
  config$logger.info("End: Your tuna atlas dataset has been created!")
}
