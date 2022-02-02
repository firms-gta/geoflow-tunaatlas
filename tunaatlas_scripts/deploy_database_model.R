#deploy_database_model
deploy_database_model <- function(config, software, software_config){

	db_read <- software_config$properties$user_readonly
	repository_sql_scripts_database_deployement <- "https://raw.githubusercontent.com/eblondel/geoflow-tunaatlas/master/tunaatlas_sql"
	db_dimensions <- "area,catchtype,unit,fishingfleet,gear,schooltype,sex,sizeclass,species,time,source"
	db_variables_and_associated_dimensions <- "catch=schooltype,species,time,area,gear,fishingfleet,catchtype,unit,source@effort=schooltype,time,area,gear,fishingfleet,unit,source@catch_at_size=schooltype,species,time,area,gear,fishingfleet,catchtype,sex,unit,sizeclass,source"
 
	#out sql
	outsql <- sprintf("-- SQL script to deploy database '%s'", db_read)
	outsql <- paste(outsql, sprintf("-- Generated on '%s'", format(Sys.time(), "%Y-%m-%dT%H:%M:%S")), sep = "\n")

	#Preliminary step: grant select on all objects of the DB to the user with select privileges
	outsql <- paste(outsql, '-- PREREQUISITES', sep = "\n")
	outsql <- paste(outsql, '-- Preliminary step: grant select on all objects of the DB to the user with select privileges', sep = "\n")
	outsql <- paste(outsql, '-- create extension postgis', sep = "\n")
	outsql <- paste(outsql, paste0("create extension postgis ;"), sep = "\n")
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
	outsql <- paste(outsql, '-- 1B/ METADATA DCMI table', sep = "\n")
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
	  
	  #Julien => merge with other comments 
	  dimensionTableComment <- switch(dimension,
	                                  "catch" = paste0("COMMENT ON TABLE ",dimension,".",dimension," IS 'Quantity of fish  (in number or weight / biomass) harvested in a given stratum. This table is a dimension of the data warehouse: a list of codes which gives the context of the values stored in the fact table.';"),
	                                  "effort" = paste0("COMMENT ON TABLE ",dimension,".",dimension," IS 'Quantity of effort (expressed in a given unit such as number of sets, number of fishing hours, number of hooks, etc.) exerted in a given stratum. This table is a dimension of the data warehouse: a list of codes which gives the context of the values stored in the fact table.';"),
	                                  "catch-at-size" = paste0("COMMENT ON TABLE ",dimension,".",dimension," IS 'Quantity of fish at a given size (or size class) harvested in a given stratum. This table is a dimension of the data warehouse: a list of codes which gives the context of the values stored in the fact table.';"),
	                                  "area" = paste0("COMMENT ON TABLE ",dimension,".",dimension," IS 'Spatial area (zone) where the fact has taken place. The data in Sardara are mainly defined on the following areas: 1) Nominal catch are mostly defined on the areas of competence of the RFMOs. For some RFMOs, the spatial stratification can be thinner: IOTC gives nominal catch at the FAO areas scale and ICCAT gives it at the sampling area scale. 2) Georeferenced catch and effort and catch-at-size are mostly defined on 1 or 5 degree square resolution. In some cases irregular areas are also used (e.g. in IOTC). This may happen when the reporting country/institution does not provide the data at 1 or 5 degree resolution. This table is a dimension of the data warehouse: a list of codes which gives the context of the values stored in the fact table.';"),
	                                  "catchtype" = paste0("COMMENT ON TABLE ",dimension,".",dimension," IS 'Fate of the catch, i.e. landed, discarded, unknown. Given the nature of the data, only landing data are currently available in SARDARA, with a very few exceptions of discarded fishes for ICCAT. This table is a dimension of the data warehouse: a list of codes which gives the context of the values stored in the fact table.';"),
	                                  "catchunit" = paste0("COMMENT ON TABLE ",dimension,".",dimension," IS 'Unit of catch (metric tons or number of fish). Catches are mainly given in weight (metric tons), but some historical catch (e.g. japanese longliners) are reported in number of fishes captured. Some catch are also reported in both weight and number. This table is a dimension of the data warehouse: a list of codes which gives the context of the values stored in the fact table.';"),
	                                  "effortunit" = paste0("COMMENT ON TABLE ",dimension,".",dimension," IS 'Unit of effort. The unit of effort mainly depends on the gear and the country that reports the data. Several units of efforts are available, even for a given gear (e.g. day at sea and searching days for purse seine). This table is a dimension of the data warehouse: a list of codes which gives the context of the values stored in the fact table.';"),
	                                  "fishingfleet" = paste0("COMMENT ON TABLE ",dimension,".",dimension," IS 'Fishing fleet. A group of fishing vessels authorized to operate in a t-RFMO convention area / area of competence, and whose fishing operations and catches of tuna and tuna-like species are responsibility of, and accounted for by a political entity or sub-entity recognized by the corresponding t-RFMO. To be noted that the actual occurrences of the Fishing fleet concept do not necessarily refer or correspond to a recognized country (e.g.: EUR - European Union, FRAT – French territories), nor to a distinct member / contracting party / cooperating, non-contracting party of a t-RFMO (e.g.: EU,ESP - EU (Spain), TWN – Chinese Taipei / Taiwan province of China – for some t-RFMOs). The proposed list of fishing fleet codes also includes a generic reference that applies to fishing operations and catches from unidentified sources (e.g.: NEI - not elsewhere identified).';"),
	                                  "gear" = paste0("COMMENT ON TABLE ",dimension,".",dimension," IS 'Fishing gear used. The number of gears varies a lot depending on the RFMOs. ICCAT, for instance, has around 60 gears while IATTC has 10 gears. This table is a dimension of the data warehouse: a list of codes which gives the context of the values stored in the fact table.';"),
	                                  "schooltype" = paste0("COMMENT ON TABLE ",dimension,".",dimension," IS 'A school is a group of fishes evolving together. The type of school indicates the nature of the school on which the catch has been made: free school, log school, unknown, dolphin.';"),
	                                  "sex" = paste0("COMMENT ON TABLE ",dimension,".",dimension," IS 'Sex of the fish captured. Only partially available in the catch-at-size datasets.';"),
	                                  "species" = paste0("COMMENT ON TABLE ",dimension,".",dimension," IS 'Species captured. The main tuna species are available in all the RFMOs Depending on the RFMO, some non-target species (e.g. some sharks or turtles) are also reported. This table is a dimension of the data warehouse: a list of codes which gives the context of the values stored in the fact table.';"),
	                                  "sizeclass" = paste0("COMMENT ON TABLE ",dimension,".",dimension," IS 'Class of size of a distribution. This dimension is used only in the catch-at-size dataset. A class is defined by its minimum size (e.g. 30 cm) and the size bin (e.g. 2 cm). This table is a dimension of the data warehouse: a list of codes which gives the context of the values stored in the fact table.';"),
	                                  "time" = paste0("COMMENT ON TABLE ",dimension,".",dimension," IS 'Dating of the fact. In Sardara, the dating is provided as two columns: time_start gives the first date of availability of the measure (included) and time_end gives the last date of availability of the measure (not included). The data in Sardara are mainly defined over the following time steps: 1) Nominal catch are mostly defined on 1 year resolution. 2) Georeferenced catch-and-effort and catch-at-size are mostly defined on 1 month resolution.  This table is a dimension of the data warehouse: a list of codes which gives the context of the values stored in the fact table.';")
	  )

	  outsql <- paste0(outsql,"\n", dimensionTableComment,"\n")
	  
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
	  column_comment <-paste0("COMMENT ON COLUMN fact_tables.",fact_name,".id_",fact_name," IS 'identifier (primary key)  of this measure';
	                          COMMENT ON COLUMN fact_tables.",fact_name,".id_metadata IS 'identifier (foreign key) of related dataset described in metadata table';")
	  
	  for (j in 1:length(dimensions_for_fact)){
		sql_deploy_fact_table<-paste0(sql_deploy_fact_table,"id_",dimensions_for_fact[j], " INTEGER REFERENCES ",dimensions_for_fact[j],".",dimensions_for_fact[j],"(id_",dimensions_for_fact[j],"),")
	  column_comment <-paste0(column_comment,"COMMENT ON COLUMN fact_tables.",fact_name,".id_",dimensions_for_fact[j]," IS 'identifier (foreign key) of related ",dimensions_for_fact[j], " dimension';")
	  }
	  column_comment <-paste0(column_comment,"COMMENT ON COLUMN fact_tables.",fact_name,".value IS 'this column gives the measure of the fact';")
	  
	  sql_deploy_fact_table<-paste0(sql_deploy_fact_table,"value numeric(12,2) NOT NULL);ALTER TABLE metadata.metadata


	  OWNER TO \"",software_config$parameters$user,"\";
	  GRANT ALL ON TABLE fact_tables.",fact_name," TO \"",software_config$parameters$user,"\";

	  CREATE INDEX id_metadata_",fact_name,"_idx
	  ON fact_tables.",fact_name,"
	  USING btree
	  (id_metadata); COMMENT ON TABLE fact_tables.",fact_name," IS 'one of the fact table of the data ware house';")
	  outsql <- paste(outsql, sql_deploy_fact_table, column_comment, sep = "\n")
	}


	#fill with db variables
	outsql <- gsub("%db_admin%", software_config$parameters$user, outsql)
	outsql <- gsub("%db_read%", db_read, outsql)
	
	return(outsql)
}



  