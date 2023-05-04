retrieve_nominal_catch <- function(entity, config, options){
  source("https://raw.githubusercontent.com/firms-gta/geoflow-tunaatlas/master/map_codelist.R")
  
	con <- config$software$output$dbi
	
	#list of dataset files (from entity data sources)
	dataset_files <- sapply(entity$data$source[3:length(entity$data$source)], function(x){ entity$getJobDataResource(config, x) })
	names(dataset_files) <- entity$data$source[3:length(entity$data$source)]
	dataset_files_nominal_catch <- dataset_files[regexpr("nominal", names(dataset_files)) > 0]
	
	# There are 2 ICCAT datasets for nominal catch: one that provides the stratification by Sampling areas, and one that provides the stratification by Stock areas. For nominal catch, the user decides as input parameter which one he wants to keep.
	iccat_nominal_catch_spatial_stratification <- options$iccat_nominal_catch_spatial_stratification
	if(is.null(iccat_nominal_catch_spatial_stratification)) iccat_nominal_catch_spatial_stratification <- "sampling_area"
	if (iccat_nominal_catch_spatial_stratification=="sampling_area"){
		dataset_files_nominal_catch <- dataset_files_nominal_catch[names(dataset_files_nominal_catch)!= "atlantic_nominal_catch_iccat_level0__bystockarea.csv"]
	} else if (iccat_nominal_catch_spatial_stratification=="stock_area"){
		dataset_files_nominal_catch <- dataset_files_nominal_catch[names(dataset_files_nominal_catch)!= "atlantic_nominal_catch_iccat_level0__bysamplingarea.csv"]
	}
	if(!options$include_IOTC) dataset_files_nominal_catch <- dataset_files_nominal_catch[!regexpr("indian",names(dataset_files_nominal_catch))>0]
	if(!options$include_ICCAT) dataset_files_nominal_catch <- dataset_files_nominal_catch[!regexpr("atlantic",names(dataset_files_nominal_catch))>0]
	if(!options$include_WCPFC) dataset_files_nominal_catch <- dataset_files_nominal_catch[!regexpr("west_pacific",names(dataset_files_nominal_catch))>0]
	if(!options$include_CCSBT) dataset_files_nominal_catch <- dataset_files_nominal_catch[!regexpr("southern_hemisphere",names(dataset_files_nominal_catch))>0]
	if(!options$include_IATTC) dataset_files_nominal_catch <- dataset_files_nominal_catch[!regexpr("east_pacific",names(dataset_files_nominal_catch))>0]
	
	columns_to_keep=c("source_authority","species","gear","fishingfleet","time_start","time_end","geographic_identifier","unit","value")
	library(dplyr)
	
	# create a function that renames columns
	library(dplyr)
	
	rename_columns <- function(x) {
	  nominal <- readr::read_csv(x)
	  
	  # rename columns only if they exist
	  if ("gear_type" %in% colnames(nominal)) {
	    nominal <- nominal %>% rename(gear = gear_type)
	  }
	  if ("fishing_mode" %in% colnames(nominal)) {
	    nominal <- nominal %>% rename(schooltype = fishing_mode)
	  }
	  if ("measurement_value" %in% colnames(nominal)) {
	    nominal <- nominal %>% rename(value = measurement_value)
	  }
	  if ("measurement_unit" %in% colnames(nominal)) {
	    nominal <- nominal %>% rename(unit = measurement_unit)
	  }
	  nominal_catch <- nominal[,columns_to_keep]
	  class(nominal_catch$value) <- "numeric"
	  
	  
	  return(nominal_catch)
	}
	
	nominal_catch <- as.data.frame(do.call("rbind", lapply(dataset_files_nominal_catch,rename_columns)))

	# # For ICCAT Nominal catch, we need to map flag code list, because flag code list used in nominal catch dataset is different from flag code list used in ICCAT task2; however we have to use the same flag code list for data raising. In other words, we express all ICCAT datasets following ICCAT task2 flag code list.
	# if (options$include_ICCAT){
	# 	# extract mapping
	# 	df_mapping_source <- entity$data$source[[2]]
	# 	df_mapping_file <- entity$getJobDataResource(config, df_mapping_source)
	# 	df_mapping <- as.data.frame(readr::read_csv(df_mapping_file, guess_max = 0))
	# 	df_mapping$source_authority <- "ICCAT"
	#   
	# 	nominal_catch_other_rfmos <- nominal_catch %>% filter (source_authority != "ICCAT")
	# 	nominal_catch_iccat <- nominal_catch %>% filter (source_authority == "ICCAT")
	# 	nominal_catch_iccat <- map_codelist(nominal_catch_iccat, df_mapping, "fishingfleet")$df 
	#  
	# 	nominal_catch<-rbind(nominal_catch_other_rfmos,nominal_catch_iccat)
	# }

	nominal_catch$time_start<-substr(as.character(nominal_catch$time_start), 1, 10)
	nominal_catch$time_end<-substr(as.character(nominal_catch$time_end), 1, 10)
	return(nominal_catch)
}	
