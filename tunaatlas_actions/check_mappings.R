check_mappings <- function(entity, config, options){

	if(dir.exists("errors_codelists")){
		errMsg <- "Hum, It seems they are still missing codelists! Cannot proceed with loading datasets. Aborting workflow..."
		config$logger.error(errMsg)
		stop(errMsg)
	}

}