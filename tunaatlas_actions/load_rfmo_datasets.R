load_rfmo_datasets <- function(entity, config, options){

	if(dir.exists("errors_mappings")){
		errMsg <- "Hum, It seems they are still missing codelist mappings! Cannot proceed with loading datasets. Aborting workflow..."
		config$logger.error(errMsg)
		stop(errMsg)
	}

}