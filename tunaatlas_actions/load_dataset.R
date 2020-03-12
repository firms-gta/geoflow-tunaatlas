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
	
	#################################################################
	# Julien for test only
	require(geoflow)
	setwd("~/Bureau/CODES/geoflow-tunaatlas")
	config=initWorkflow("tunaatlas_julien_datasets.json")
	jobdir <- initWorkflowJob(config)
	config$job <- jobdir
	entities = config$getEntities()
	entity = entities[[15]]
	entity$copyDataToJobDir(config, jobdir)
	# Julien for test only
	#################################################################
	
	con = config$software$input$dbi
	user_postgres <- config$software$input$dbi_config$parameters$user
	
	#----------------------------------------------------------------------------------------------------------------------------
	#@geoflow --> with this script 2 objects are pre-loaded
	#config --> the global config of the workflow
	#entity --> the entity you are managing
	#get data from geoflow current job dir
	dataset_pid <- entity$identifiers[["id"]]
	filename <- entity$data$source[[1]]
	path_to_dataset <- entity$getJobDataResource(config, filename)
	path_to_raw_dataset	<- path_to_dataset
	# path_to_effort_dataset	<- "to be done"
	# spatial_stratification	<- "to be done"
	geoflow_local_action <-entity$data$actions[[1]]$script
	
	# system(paste0("mv ",filename," ",paste0(dataset_pid,".csv")))
	#----------------------------------------------------------------------------------------------------------------------------
	
	table_name <- entity$data$uploadSource[[1]]
	dimension_name <- sub('\\..*', '', table_name)
	# begin patch julien
	schema_name_for_view <- dimension_name
	database_view_name <- dataset_pid
	# end patch julien
	
	config$logger.info(sprintf("Dataset '%s' will be loaded in table '%s'",dataset_pid, table_name))
	config$logger.info(sprintf("Load dataset from jobdir file '%s'", path_to_dataset))
	df_to_load <- read.csv(path_to_raw_dataset)
	#set parameters to run geoflow local action: R script set in the "Data" column
	# see details in https://github.com/ptaconet/rtunaatlas_scripts/blob/72c03473910b111bf8e78812cf761a7f493d6924/workflow_etl/scripts/generate_dataset.R
	source(geoflow_local_action)
	df_to_load <- dataset
	
	if(!(exists("additional_metadata"))){
	  additional_metadata<-NULL
	}
	if(!(exists("df_codelists"))){
	  df_codelists<-NULL
	}
	
	
	### METADATA => replace / set df_metadata with geoflow current entity
	#------------------------------------------------------------------------------------------------------------------------
	#get geoflow entity as data.frame representation to make easier mapping with DBMS metadata table
	geoflow_df <- entity$asDataFrame()
	
	#build legacy metadata Tuna atlas metadata data.frame representation
	config$logger.info("Preparing legacy Tuna atlas metadata entry")
	InputMetadataset <- data.frame(
	  identifier = entity$identifiers[["id"]],
	  persistent_identifier = entity$identifiers[["id"]],
	  title = entity$title,
	  contacts_and_roles = geoflow_df$Contact,
	  subject = geoflow_df$Subject,
	  description = geoflow_df$Description,
	  date = geoflow_df$Date,
	  format = NA, #TODO
	  language = entity$language,
	  relation = geoflow_df$Relation,
	  spatial_coverage = geoflow_df$SpatialCoverage,
	  temporal_coverage = geoflow_df$TemporalCoverage,
	  rights = geoflow_df$Rights,
	  source = NA, #TODO ?
	  lineage = geoflow_df$Provenance,
	  supplemental_information = NA, #this is now managed within description
	  dataset_type = "codelist",
	  sql_query_dataset_extraction = NA, #filled below with R code
	  database_table_name = table_name,
	  database_view_name = NA, #not applicable for codelits
	  stringsAsFactors = FALSE
	)
	InputMetadataset[is.na(InputMetadataset)] <- "NA"
	
	df_metadata <- InputMetadataset
	
	metadata_and_parameterization <- df_metadata
	
	
	
	# julien => should be taken from data dictionnary embedded in the database and used for 19110 ?
	db_dimensions_parameters<-read.csv(system.file("extdata", "db_dimensions_parameters.csv",package="rtunaatlas"),stringsAsFactors = F,strip.white=TRUE)
	
	variable_name<-gsub("fact_tables.","",df_metadata$database_table_name)
	
	dimensions<-list_variable_available_dimensions(con,variable_name)
	
	# temporary patch julien for test purpose
	df_codelists_input <- read.csv("http://data.d4science.org/b1VSdHp1YUJrd0dZdFBua3lVbFNXRUdpRkpWSDlBd25HbWJQNStIS0N6Yz0")
	# Set df_inputs to use
	df_codelists_input<-df_codelists_input[which(df_codelists_input$dimension %in% dimensions),]
	
	# convert columns that are not character to character and ensure that the column "value" is a numeric
	cols<-setdiff(colnames(df_to_load),"value")
	for (i in 1:length(cols)){
	  if (typeof(df_to_load[,cols[i]])!="character"){
	    df_to_load[,cols[i]]<-as.character(df_to_load[,cols[i]])
	  }
	}
	df_to_load$value<-as.numeric(df_to_load$value)
	
	#### First we deal with all the dimensions that are "real" code lists: area,catchtype,catchunit,effortunit,flag,gear,schooltype,species,sex,ocean
	# Dimensions time and sizeclass are not "real" code lists. They are dealt in a second step
	
	db_dimensions_parameters<-db_dimensions_parameters[which(db_dimensions_parameters$dimension %in% dimensions),]
	
	#Keep only dimensions that are code list (i.e. dimensions time and sizeclass will be dealt separately,as non-code list dimension)
	db_df_inputlike_dimensions_parameters<-db_dimensions_parameters[ which(db_dimensions_parameters$codelist_table==TRUE), ]
	
	area_df_inputtouse<-df_codelists_input[which(df_codelists_input$dimension=="area"),]$code_list_identifier
	# if the area code list to use is wkt, remove from the dimensions that are code lists. It will be treated later
	if (area_df_inputtouse == "area_wkt"){
	  db_df_inputlike_dimensions_parameters<-db_df_inputlike_dimensions_parameters[-which(db_df_inputlike_dimensions_parameters$dimension=="area"),]
	}
	
	
	# Create columns of the dimensions that do not exist. These dimensions will be set to NULL in Sardara.
	for (i in 1:nrow(db_df_inputlike_dimensions_parameters)){
	  if(!(db_df_inputlike_dimensions_parameters$csv_formatted_dimension_colname[i] %in% colnames(df_to_load))){
	    df_to_load[db_df_inputlike_dimensions_parameters$csv_formatted_dimension_colname[i]]<-"ALL"
	  }
	  # We make sure that all the columns (except the value column) are characters
	  #if (sapply(df_to_load[db_df_inputlike_dimensions_parameters$csv_formatted_dimension_colname[i]], class)[1] != "character"){
	  #df_to_load[db_df_inputlike_dimensions_parameters$csv_formatted_dimension_colname[i]]<-as.character(df_to_load[db_df_inputlike_dimensions_parameters$csv_formatted_dimension_colname[i]])
	  #}
	}
	
	
	
	
	# One by one, merge the dimensions
	
	#Initialize TablesToUpdateInDB and missingCodesInTableInDB. These vectors will be used to advise the user on the codes that are missing inside the DB
	TablesToUpdateInDB<-c()
	missingCodesInTableInDB<-c()
	
	for (i in 1:nrow(db_df_inputlike_dimensions_parameters)){
	  
	  if (length(unique(df_to_load[,db_df_inputlike_dimensions_parameters$csv_formatted_dimension_colname[i]]))==1 & df_to_load[1,db_df_inputlike_dimensions_parameters$csv_formatted_dimension_colname[i]]=="ALL"){
	    df_to_load[,db_df_inputlike_dimensions_parameters$db_pkattribute_colname[i]]=0
	  } else {
	    #Retrieve the name of the code list to use
	    index<-which(df_codelists_input$dimension==db_df_inputlike_dimensions_parameters$dimension[i])
	    db_df_inputstouse<-df_codelists_input$code_list_identifier[index]
	    
	    # Merge the dimension 
	    df_to_load<-FUNMergeDimensions_CodeListLike(
	      con,
	      db_df_inputlike_dimensions_parameters$db_tablename[i],
	      db_df_inputlike_dimensions_parameters$db_pkattribute_colname[i],
	      db_df_inputlike_dimensions_parameters$db_codesource_colname[i],
	      db_df_inputlike_dimensions_parameters$db_tablesource_colname[i],
	      df_to_load,
	      db_df_inputlike_dimensions_parameters$csv_formatted_dimension_colname[i],
	      db_df_inputstouse
	    )
	    
	    # For the considered dimension, the code lists might have been updated. Checks if some codes are present in the dataset to upload but absent in the corresponding dimension table of the DB. 
	    index.na<-which(is.na(df_to_load[,db_df_inputlike_dimensions_parameters$db_pkattribute_colname[i]]))
	    
	    
	    if (length(index.na)>0){
	      missingCodesInDB<-unique(df_to_load[index.na,db_df_inputlike_dimensions_parameters$csv_formatted_dimension_colname[i]])
	    } else {
	      missingCodesInDB<-NULL
	    }
	    
	    
	    if (!is.null(missingCodesInDB)){
	      for (k in 1:length(missingCodesInDB)){
	        TablesToUpdateInDB<-c(TablesToUpdateInDB,as.character(db_df_inputstouse))
	      }
	      missingCodesInTableInDB<-c(missingCodesInTableInDB,missingCodesInDB)
	      
	      #warning(paste("Some code(s) exist in the dataset to upload but do not exist in the corresponding code list in the  do not exist in the table ",db_df_inputstouse," of the database. You should update the table ",db_df_inputstouse," of Sardara with these code before uploading the dataset in Sardara.",sep=""))
	    }
	  }
	  #remove from the dataset to upload the column that has just been merged
	  varsToDelete <- names(df_to_load) %in% db_df_inputlike_dimensions_parameters$csv_formatted_dimension_colname[i]
	  df_to_load <- df_to_load[!varsToDelete]
	  
	}
	
	
	# These are the codes that are missing in the tables within the DB. The tables of the DB have to be filled with these values before the dataset is uploaded in the DB.
	missingCodes_dataframe<- data.frame(TablesToUpdateInDB,missingCodesInTableInDB)
	missingCodes_dataframe<-missingCodes_dataframe[! (missingCodes_dataframe$missingCodesInTableInDB %in% c("ALL","UNK")),]
	
	if (nrow(missingCodes_dataframe)>0){
	  missingCodes_dataframe<-missingCodes_dataframe[!is.na(missingCodes_dataframe$missingCodesInTableInDB),]
	  print(missingCodes_dataframe)
	  stop("Some code(s) exist in the dataset to upload but do not exist in the corresponding code list. You should update the tables of Sardara with these code(s) (and the mapping if relevant) before uploading the dataset in Sardara.")
	} else {
	  
	  #replace NA by 'NA'. These values will be set to NULL in the DB. These values are the ones that were set to "ALL" in the dataset to upload
	  #df_to_load<-replace(df_to_load, is.na(df_to_load), "NA") 
	  
	  ## Now deal with non-code list like dimensions (time,sizeclass). For these dimensions, the column names on the dataset to upload MUST BE the sames as the DBs ones. i.e. for time, the dataset to upload must have a "time_start" and a "time_end" column , and the DB table "time" must also have the same "time_start" and "time_end" columns
	  
	  #Keep only the dimensions that are non-code list
	  db_nondf_inputlike_dimensions_parameters<-db_dimensions_parameters[ which(db_dimensions_parameters$codelist_table==FALSE), ]
	  
	  # One by one, retrieve the numeric codes
	  for (dim in 1:nrow(db_nondf_inputlike_dimensions_parameters)){
	    
	    if (db_nondf_inputlike_dimensions_parameters$dimension[dim]=="sizeclass"){
	      df_to_load$size_min<-as.numeric(df_to_load$size_min)
	      df_to_load$size_step<-as.numeric(df_to_load$size_step)
	    }
	    
	    # Merge to get back the ID from the DB
	    df_to_load <- FUNMergeDimensions_NonCodeListLike(
	      con,
	      df_to_load,
	      db_nondf_inputlike_dimensions_parameters$db_pkattribute_colname[dim],
	      db_nondf_inputlike_dimensions_parameters$csv_formatted_dimension_colname[dim],
	      db_nondf_inputlike_dimensions_parameters$db_tablename[dim]
	    )
	    
	    #check if some codes are missing after the matching. If so, upload the new records in Sardara
	    index.na<-which(is.na(df_to_load[, db_nondf_inputlike_dimensions_parameters$db_pkattribute_colname[dim]]))
	    
	    if (length(index.na)>0){
	      rs<-FUNuploadNewRecordsToDB(con,
	                                  df_to_load,
	                                  db_nondf_inputlike_dimensions_parameters$db_tablename[dim],
	                                  db_nondf_inputlike_dimensions_parameters$db_pkattribute_colname[dim],
	                                  db_nondf_inputlike_dimensions_parameters$csv_formatted_dimension_colname[dim])
	      
	      #if codes were missing in db, re-run the function that does the merging
	      
	      df_to_load[,db_nondf_inputlike_dimensions_parameters$db_pkattribute_colname[dim]]<-NULL
	      
	      df_to_load <- FUNMergeDimensions_NonCodeListLike(
	        con,
	        df_to_load,
	        db_nondf_inputlike_dimensions_parameters$db_pkattribute_colname[dim],
	        db_nondf_inputlike_dimensions_parameters$csv_formatted_dimension_colname[dim],
	        db_nondf_inputlike_dimensions_parameters$db_tablename[dim]
	      )
	    }
	    
	    #remove from the dataset to upload the column that has just been merged
	    #colnames_to_merge_vectorformat<-strsplit(db_nondf_inputlike_dimensions_parameters$csv_formatted_dimension_colname[dim],",")[[1]]
	    
	    #varsToDelete <- names(df_to_load) %in% colnames_to_merge_vectorformat
	    #df_to_load <- df_to_load[!varsToDelete]
	    
	  }
	  
	  
	  ### Deal with area in wkt format
	  if (area_df_inputtouse=="area_wkt"){
	    
	    df_to_load<-FUNMergeDimensions_CodeListLike(
	      con,
	      "area.area",
	      "id_area",
	      "codesource_area",
	      "tablesource_area",
	      df_to_load,
	      "geographic_identifier",
	      "area_wkt"
	    )
	    
	    #check if some codes are missing after the matching
	    index.na<-which(is.na(df_to_load[, "id_area"]))
	    
	    if (length(index.na)>0){
	      
	      sql<-paste("SELECT code,code as code_wkt_merge from area.area_wkt")
	      df_inputFromDB<-dbGetQuery(con, sql)
	      
	      df_to_load<-data.table(df_to_load)
	      df_to_load<-merge(df_to_load,df_inputFromDB,by.x="geographic_identifier",by.y="code",all.x=TRUE)
	      df_to_load<-as.data.frame(df_to_load)
	      
	      # upload the new wkt to the db
	      CodesToLoad<-unique(df_to_load[index.na,"geographic_identifier"])
	      
	      sql4 <- paste0("COPY  area.area_wkt (code) FROM STDIN NULL 'NA' ")
	      postgresqlpqExec(con, sql4)
	      postgresqlCopyInDataframe(con, data.frame(CodesToLoad))
	      rs <- postgresqlgetResult(con)
	      
	      #if codes were missing in db, re-run the function that does the merging
	      
	      df_to_load[,"id_area"]<-NULL
	      
	      df_to_load<-FUNMergeDimensions_CodeListLike(
	        con,
	        "area.area",
	        "id_area",
	        "codesource_area",
	        "tablesource_area",
	        df_to_load,
	        "geographic_identifier",
	        "area_wkt"
	      )
	    }
	    
	    ## Refresh materialized view area.area_labels to take into account the new WKT codes just inserted
	    dbSendQuery(con,"REFRESH MATERIALIZED VIEW area.area_labels")
	    
	  }
	  
	  cat("Data merged with code lists of database\n")
	  
	  
	  # Load metadata
	  rs<-FUNUploadDatasetToTableInDB(con,df_metadata,"metadata.metadata")
	  cat("Metadata loaded\n")
	  
	  # Retrieve the PK of the metadata for the line just inserted
	  sql<- "SELECT max(id_metadata) FROM metadata.metadata"
	  PK_metadata <- dbGetQuery(con, sql)
	  PK_metadata<-as.integer(PK_metadata$max[1])
	  
	  df_to_load$id_metadata<-PK_metadata
	  
	  # For the dataset to upload, keep only rows that are in the database table (i.e. remove all other columns)  (is it really necessary? TO CHECK)
	  # This first line is to change the metric column name
	  # colnames(df_to_load)[which(colnames(df_to_load) == metric_colname_dftoupload)] <- metric_colname_db
	  
	  query_get_colnames<-paste("select column_name from information_schema.columns where table_name='",variable_name,"' and table_schema='fact_tables' and column_default is null",sep="")
	  column_names<-dbGetQuery(con, query_get_colnames)
	  column_names_to_keep<-column_names$column_name
	  
	  df_to_load<-df_to_load[column_names_to_keep]
	  
	  # Replace NA by O. In the db, 0 = unknown
	  df_to_load<-replace(df_to_load, is.na(df_to_load), 0)
	  
	  cat("Loading the dataset in the database...\n")
	  
	  # Upload file to database
	  
	  rs<-FUNUploadDatasetToTableInDB(con,df_to_load,df_metadata$database_table_name)
	  
	  cat("Data loaded in the DB\n")
	  
	  
	  ## Update some metadata elements
	  # first drop materialized view
	  # if(!is.null(database_view_name)){
	    dbSendQuery(con,paste0("DROP MATERIALIZED VIEW IF EXISTS ",paste0(schema_name_for_view,".",database_view_name,";")))
	  # }
	  
	  # sql_query_dataset_extraction
	  df_metadata$id_metadata<-PK_metadata
	  sql_query_dataset_extraction<-getSQLSardaraQueries(con,df_metadata)
	  dbSendQuery(con,paste0("UPDATE metadata.metadata SET sql_query_dataset_extraction='",gsub("'","''",sql_query_dataset_extraction$query_CSV_with_labels),"' WHERE identifier='",df_metadata$identifier,"'"))
	  
	  # spatial coverage
	  sp_extent_sql<-paste0("SELECT st_astext(ST_Extent(geom)) FROM ",df_metadata$database_table_name," c LEFT JOIN area.area_labels USING (id_area) WHERE id_metadata='",PK_metadata,"'")
	  sp_extent<-dbGetQuery(con,sp_extent_sql)$st_astext
	  dbSendQuery(con,paste0("UPDATE metadata.metadata SET spatial_coverage='",sp_extent,"' WHERE identifier='",df_metadata$identifier,"'"))
	  
	  
	  # metadata_mapping
	  # 1) dataset-codelists (le dataset x utilise les codelist x,y,z) 
	  for (i in 1:nrow(df_codelists_input)){
	    id_metadata_code_list<-dbGetQuery(con,paste0("SELECT id_metadata from metadata.metadata where identifier='",df_codelists_input$code_list_identifier[i],"'"))
	    if(nrow(id_metadata_code_list)){
	      dbSendQuery(con,paste0("INSERT INTO metadata.metadata_mapping(metadata_mapping_id_from,metadata_mapping_id_to) VALUES (",PK_metadata,",",id_metadata_code_list,")"))
	    }
	  }
	  # 2) dataset-mappings (le dataset x utilise les mappings x,y,z) -> PAS ENCORE GERE
	  # 3) dataset-dataset  (le dataset x utilise les dataset x,y,z) -> PAS ENCORE GERE
	  
	  
	  # Create the materialized view if set in the metadata
	  if(!is.na(database_view_name)){
	    cat(paste0("Creating materialized view ",paste0(schema_name_for_view,".",database_view_name)," (with codes and labels)\n"))
	    # Check if schema exists
	    list_of_schemas<-dbGetQuery(con,"select schema_name from information_schema.schemata")$schema_name
	    # Get schema name where to store the materialized view
	    # schema_name_for_view<-sub('\\..*', '', database_view_name)
	    # Create the schema if it does not exist
	    
	    if (!(schema_name_for_view %in% list_of_schemas)){
	      dbSendQuery(con,paste0("CREATE SCHEMA ",schema_name_for_view,"; GRANT USAGE ON SCHEMA ",schema_name_for_view," TO ",user_postgres,";ALTER DEFAULT PRIVILEGES IN SCHEMA ",schema_name_for_view," GRANT SELECT ON TABLES TO ",user_postgres,";"))
	    }
	    # Create the materialized view without the labels (to get the labels, replace sql_query_dataset_extraction$query_CSV by sql_query_dataset_extraction$query_CSV_with_labels)
	    dbSendQuery(con,paste0("DROP MATERIALIZED VIEW IF EXISTS ",paste0(schema_name_for_view,".",database_view_name),";
                             CREATE MATERIALIZED VIEW ",paste0(schema_name_for_view,".",database_view_name)," AS ",sql_query_dataset_extraction$query_CSV_with_labels,";
                             COMMENT ON MATERIALIZED VIEW ",paste0(schema_name_for_view,".",database_view_name)," IS '",df_metadata$title,"'"))
	  }
	  
	}
	
	
	
	
}