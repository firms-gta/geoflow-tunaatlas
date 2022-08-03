check_mappings <- function(action,entity, config, options){
	
	#control to check that everything is ok on codelists side, if not we stop the workflow until codelists are fixed/updated
	if(dir.exists("errors_codelists")){
		errMsg <- "Hum, It seems they are still missing codelists! Cannot proceed with loading datasets. Aborting workflow..."
		config$logger.error(errMsg)
		stop(errMsg)
	}

}