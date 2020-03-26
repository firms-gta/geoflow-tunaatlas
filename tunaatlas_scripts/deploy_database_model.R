#deploy_database_model
deploy_database_model <- function(config, software, software_config){

	db_read <- software_config$parameters$user #TO BE REPLACE BY ACTUAL READ-ONLY USER
	repository_sql_scripts_database_deployement <- "https://raw.githubusercontent.com/eblondel/geoflow-tunaatlas/master/tunaatlas_sql"
	db_dimensions <- "area,catchtype,unit,flag,gear,schooltype,sex,sizeclass,species,time,source"
	db_variables_and_associated_dimensions <- "catch=schooltype,species,time,area,gear,flag,catchtype,unit,source@effort=schooltype,time,area,gear,flag,unit,source@catch_at_size=schooltype,species,time,area,gear,flag,catchtype,sex,unit,sizeclass,source"
 
	#out sql
	outsql <- sprintf("-- SQL script to deploy database '%s'", db_read)
	outsql <- paste(outsql, sprintf("-- Generated on '%s'", format(Sys.time(), "%Y-%m-%dT%H:%M:%S")), sep = "\n")

	#Preliminary step: grant select on all objects of the DB to the user with select privileges
	outsql <- paste(outsql, '-- PREREQUISITES', sep = "\n")
	outsql <- paste(outsql, '-- Preliminary step: grant select on all objects of the DB to the user with select privileges', sep = "\n")
	outsql <- paste(outsql, paste0("alter default privileges grant select on tables to \"",db_read,"\";"), sep = "\n")
	outsql <- paste0(outsql, "\n");

	## 1) Deploy schema metadata and associated tables
	outsql <- paste(outsql, '-- 1/ METADATA', sep = "\n")
	# Read Sql files
	# METADATA legacy table
	outsql <- paste(outsql, '-- 1A/ METADATA Legacy table', sep = "\n")
	fileName <- paste(repository_sql_scripts_database_deployement,"create_schema_metadata.sql",sep="/")
	sql_deploy_metadata <- paste(readLines(fileName), collapse="\n")
	outsql <- paste(outsql, sql_deploy_metadata, sep="\n")
	# METADATA DCMI table (NEW)
	outsql <- paste(outsql, '-- 1A/ METADATA DCMI table', sep = "\n")
	fileName <- paste(repository_sql_scripts_database_deployement,"create_Dublin_Core_metadata.sql",sep="/")
	sql_deploy_metadata <- paste(readLines(fileName), collapse="\n")
	outsql <- paste(outsql, sql_deploy_metadata, sep="\n")
	
	## 2) Deploy dimensions
	outsql <- paste(outsql, '-- 2/ DIMENSIONS', sep = "\n")
	# Create vector of dimensions to deploy
	dimensions<-strsplit(db_dimensions, ",")[[1]]
	# One by one, create the dimensions
	for (i in 1:length(dimensions)){
	  
	  dimension <- dimensions[i]
	  outsql <- paste(outsql, sprintf("-- DIMENSION '%s'",dimension), sep = "\n")
	  
	  scriptName <- switch(dimension,
		"time" = "create_schema_dimension_time.sql",
		"sizeclass" = "create_schema_dimension_sizeclass.sql",
		"create_schema_dimension.sql"
	  )
	  fileName <- paste(repository_sql_scripts_database_deployement, scriptName,sep="/")
	  sql_deploy_dimension <- paste(readLines(fileName), collapse="\n")
	  sql_deploy_dimension <- gsub("%dimension_name%", dimension, sql_deploy_dimension)
	  outsql <- paste(outsql, sql_deploy_dimension, sep = "\n")
	  
	  if (dimensions[i]=="area"){
		# Create table area.area_wkt
		sql_deploy_table_area_wkt <- paste(readLines(paste(repository_sql_scripts_database_deployement,"create_table_area_wkt.sql",sep="/")), collapse="\n")
		outsql <- paste(outsql, sql_deploy_table_area_wkt, sep = "\n")
		outsql <- paste(outsql, "COMMENT ON SCHEMA area IS 'Schema containing the spatial code lists (i.e. reference data) (including the PostGIS geometries) used in the datasets';", sep = "\n")
		
		# Update view area.area_labels
		sql_deploy_view_area_labels <- paste(readLines(paste(repository_sql_scripts_database_deployement,"create_view_area_labels.sql",sep="/")), collapse="\n")
		outsql <- paste(outsql, sql_deploy_view_area_labels, sep = "\n")
	  }
	 
	  outsql <- paste0(outsql, "\n")
	  
	}
	
	## 3) Deploy variable / fact tables of the data warehouse
	outsql <- paste(outsql, '-- 3/ VARIABLES', sep = "\n")
	#Create of vector of variables to deploy
	facts <- strsplit(db_variables_and_associated_dimensions, "@")[[1]]

	for (i in 1:length(facts)){
	  
	  fact_name <- sub('=.*', '', facts[i])
	  dimensions_for_fact <- strsplit(sub('.*=', '', facts[i]),",")[[1]]
	  
	  outsql <- paste(outsql, sprintf("-- VARIABLE (FACT) '%s'", fact_name), sep = "\n")
	  
	  sql_deploy_fact_table<-paste0("CREATE TABLE fact_tables.",fact_name,"(
								   id_",fact_name," SERIAL PRIMARY KEY,
								   id_metadata INTEGER REFERENCES metadata.metadata(id_metadata),")
	  for (j in 1:length(dimensions_for_fact)){
		sql_deploy_fact_table<-paste0(sql_deploy_fact_table,"id_",dimensions_for_fact[j], " INTEGER REFERENCES ",dimensions_for_fact[j],".",dimensions_for_fact[j],"(id_",dimensions_for_fact[j],"),")
	  }
	  sql_deploy_fact_table<-paste0(sql_deploy_fact_table,"value numeric(12,2) NOT NULL);ALTER TABLE metadata.metadata
	  OWNER TO \"",software_config$parameters$user,"\";
	  GRANT ALL ON TABLE fact_tables.",fact_name," TO \"",software_config$parameters$user,"\";

	  CREATE INDEX id_metadata_",fact_name,"_idx
	  ON fact_tables.",fact_name,"
	  USING btree
	  (id_metadata);")
	  outsql <- paste(outsql, sql_deploy_fact_table, sep = "\n")
	}


	#fill with db variables
	outsql <- gsub("%db_admin%", software_config$parameters$user, outsql)
	outsql <- gsub("%db_read%", db_read, outsql)
	
	return(outsql)
}



  