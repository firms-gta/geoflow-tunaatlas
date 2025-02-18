retrieve_nominal_catch <- function(entity, config, options){

	con <- config$software$output$dbi
	
	#list of dataset files (from entity data sources)
	dataset_files <- sapply(entity$data$source, function(x){ entity$getJobDataResource(config, x) })
	dataset_files_nominal_catch <- dataset_files[regexpr("nominal", basename(dataset_files)) > 0]
	
	if(!options$include_IOTC) dataset_files_nominal_catch <- dataset_files_nominal_catch[!regexpr("iotc",names(dataset_files_nominal_catch))>0]
	if(!options$include_ICCAT) dataset_files_nominal_catch <- dataset_files_nominal_catch[!regexpr("iccat",names(dataset_files_nominal_catch))>0]
	if(!options$include_WCPFC) dataset_files_nominal_catch <- dataset_files_nominal_catch[!regexpr("wcpfc",names(dataset_files_nominal_catch))>0]
	if(!options$include_CCSBT) dataset_files_nominal_catch <- dataset_files_nominal_catch[!regexpr("ccsbt",names(dataset_files_nominal_catch))>0]
	if(!options$include_IATTC) dataset_files_nominal_catch <- dataset_files_nominal_catch[!regexpr("iattc",names(dataset_files_nominal_catch))>0]
	
	columns_to_keep=c("source_authority","species","gear_type", "fishing_mode","fishing_fleet","time_start","time_end","geographic_identifier","measurement_unit","measurement_value")
	
	nominal_catch <- as.data.frame(do.call("rbind", lapply(dataset_files_nominal_catch, function(x){ readr::read_csv(x, guess_max = 0) %>% dplyr::select(all_of(columns_to_keep))})))
	nominal_catch$measurement_value = as(nominal_catch$measurement_value, "numeric")

	nominal_catch$time_start<-substr(as.character(nominal_catch$time_start), 1, 10)
	nominal_catch$time_end<-substr(as.character(nominal_catch$time_end), 1, 10)
	return(nominal_catch)
}	
