load_dataset <- function(entity, config, options){

	#control to check that everything is ok on mappings side, if not we stop the workflow until mappings are fixed/updated
	if(dir.exists("errors_mappings")){
		errMsg <- "Hum, It seems they are still missing codelist mappings! Cannot proceed with loading datasets. Aborting workflow..."
		config$logger.error(errMsg)
		stop(errMsg)
	}
	
	#get entity as data.frame to make easier mapping with DBMS metadata table
	entity_df <- entity$asDataFrame()

	
	
}