load_metadata <- function(action,entity, config, options){
  opts <- action$options
	#create and load metadata table with entities as dataframe
	if(dir.exists("errors_mappings")){
		errMsg <- "create and load metadata table with entities as dataframe"
		config$logger.error(errMsg)
		stop(errMsg)
	}
	
  con_database <- config$software$output$dbi
  user_database <- config$software$output$dbi_config$parameters$user
  
  #get entity as data.frame to make easier mapping with DBMS metadata table
  entity_df <- entity$asDataFrame()
  # temporary patch to fit with DCMI data structure
  entity_df$Format <- "resource:application/x-sql"
  entity_df$Source <- "RFMOs"
  
  if (!is.null(opts$create_table)) if(opts$create_table){
    query_create_table_metadata <- paste(readLines("https://raw.githubusercontent.com/eblondel/geoflow-tunaatlas/master/tunaatlas_sql/create_Dublin_Core_metadata.sql"), collapse=" ")
    query_create_table_metadata <- gsub("%db_admin%",user_database,query_create_table_metadata)
    create_table_metadata <- dbSendQuery(con_database,query_create_table_metadata)
  }
  
  #check if there is a better way to specify the name of the schema the table is part of
  table_id <- c("metadata","metadata_dcmi")
  load_table_metadata <- dbWriteTable(conn = con_database,
                                      name =  table_id,
                                      value = entity_df[,c(1,3,2,4,5,6,7,9,10,8,11,12,16,13,15,14)],
                                      row.names=FALSE,
                                      overwrite=FALSE,
                                      append=TRUE)
	# update_geometry <- dbSendQuery(con_database, paste0('UPDATE "metadata"."metadata_DCMI" SET geometry = ST_GeomFromText("SpatialCoverage",4326);'))
}
