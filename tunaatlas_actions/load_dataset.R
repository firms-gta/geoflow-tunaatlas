load_dataset <- function(entity, config, options){

	#control to check that everything is ok on mappings side, if not we stop the workflow until mappings are fixed/updated
	if(dir.exists("errors_mappings")){
		errMsg <- "Hum, It seems they are still missing codelist mappings! Cannot proceed with loading datasets. Aborting workflow..."
		config$logger.error(errMsg)
		stop(errMsg)
	}
	
	#enrich entity with id_version
	id_parts <- unlist(strsplit(entity$identifiers[["id"]], "_tuna"))
	id_version <- paste0(id_parts[1], "_", gsub("-","_", format(entity$temporal_extent$start, "%Y-%m-%d")),"_", gsub("-","_", format(entity$temporal_extent$end, "%Y-%m-%d")), "_tuna", id_parts[2], "_", format(Sys.Date(),"%Y"))
	entity$setIdentifier("id_version", id_version)
	print(entity$identifiers[["id_version"]])
	entity$enrichWithMetadata()
	
	#get entity as data.frame to make easier mapping with DBMS metadata table
	entity_df <- entity$asDataFrame()
	print(entity_df)

	
	
}