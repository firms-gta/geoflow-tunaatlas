retrieve_nominal_catch <- function(entity, config, options){
									   
	
	con <- config$software$output$dbi
	
	#list of dataset files (from entity data sources)
	dataset_files <- sapply(entity$data$source[2:length(entity$data$source)], function(x){ entity$getJobDataResource(config, x) })
	names(dataset_files) <- entity$data$source[2:length(entity$data$source)]
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
	
	columns_to_keep=c("source_authority","species","gear","flag","time_start","time_end","geographic_identifier","unit","value")
	nominal_catch <- as.data.frame(do.call("rbind", lapply(dataset_files_nominal_catch, readr::read_csv, guess_max = 0)))
	nominal_catch <- nominal_catch[,columns_to_keep]
	class(nominal_catch$value) <- "numeric"

	# For ICCAT Nominal catch, we need to map flag code list, because flag code list used in nominal catch dataset is different from flag code list used in ICCAT task2; however we have to use the same flag code list for data raising. In other words, we express all ICCAT datasets following ICCAT task2 flag code list.
	if (options$include_ICCAT){
		# extract mapping
		df_mapping <- as.data.frame(readr::read_csv("global_nominal_catch_ird_level0_other_codelist_mapping_flag_iccat_from_ncandcas_flag_iccat.csv", guess_max = 0))
		df_mapping$source_authority <- "ICCAT"
	  
		nominal_catch_other_rfmos <- nominal_catch %>% filter (source_authority != "ICCAT")
		nominal_catch_iccat <- nominal_catch %>% filter (source_authority == "ICCAT")
		nominal_catch_iccat <- rtunaatlas::map_codelist(nominal_catch_iccat, df_mapping, "flag")$df 
	 
		nominal_catch<-rbind(nominal_catch_other_rfmos,nominal_catch_iccat)
	}

	nominal_catch$time_start<-substr(as.character(nominal_catch$time_start), 1, 10)
	nominal_catch$time_end<-substr(as.character(nominal_catch$time_end), 1, 10)
	return(nominal_catch)
}	