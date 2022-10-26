load_dataset <- function(action,entity, config, options){
  source("https://raw.githubusercontent.com/firms-gta/geoflow-tunaatlas/master/sardara_functions/getSQLSardaraQueries.R")
  
  opts <- action$options
  
    remotes::install_github("eblondel/rtunaatlas")
  
  }
  
  if(!require(readr)){
    install.packages("readr")
    require(readr)
  }
  
  if(!require(googledrive)){
    install.packages("googledrive")
    require(googledrive)
  }
  
  #control to check that everything is ok on mappings side, if not we stop the workflow until mappings are fixed/updated
  if(dir.exists("errors_mappings")){
    errMsg <- "Hum, It seems they are still missing codelist mappings! Cannot proceed with loading datasets. Aborting workflow..."
    config$logger.error(errMsg)
    stop(errMsg)
  }
  
  #opts
  upload_to_db <- if(!is.null(opts$upload_to_db)) opts$upload_to_db else TRUE
  create_materialized_view <- if(!is.null(opts$create_materialized_view)) opts$create_materialized_view else TRUE
  create_indexes <- if(!is.null(opts$create_indexes)) opts$create_indexes else TRUE
  add_sql_comments <- if(!is.null(opts$add_sql_comments)) opts$add_sql_comments else TRUE
  upload_to_googledrive <- if(!is.null(opts$upload_to_googledrive)) opts$upload_to_googledrive else TRUE
  
  #db
  con = config$software$output$dbi
  user_postgres <- config$software$output$dbi_config$parameters$user
  
  #identifiers
  dataset_pid <- entity$identifiers[["id"]]
  
  #enrich entity with id_version
  # @juldebar temporary patch to fix errors with the line below
  #id_version <- paste0(dataset_pid, "_", gsub("-","_", format(entity$temporal_extent$start, "%Y-%m-%d")),"_", gsub("-","_", format(entity$temporal_extent$end, "%Y-%m-%d")), "_", format(Sys.Date(),"%Y"))
  id_version <- dataset_pid
  entity$setIdentifier("id_version", id_version)
  entity$enrichWithMetadata()
  
  #----------------------------------------------------------------------------------------------------------------------------
    
  #resources
  path_to_dataset <- entity$resources$harmonized
  path_to_codelists <- entity$resources$codelists
  	  
  #names
  table_name <- entity$data$uploadSource[[1]]
  dimension_name <- sub('\\..*', '', table_name)
  schema_name_for_view <- dimension_name
  database_view_name <- dataset_pid
  
  if(upload_to_db){
	  
	  #read sources
	  df_to_load <- as.data.frame(readr::read_csv(path_to_dataset, guess_max=0))
	  df_codelists <- as.data.frame(readr::read_csv(path_to_codelists, guess_max=0))
	  
	  config$logger.info(sprintf("Dataset '%s' will be loaded in table '%s'",dataset_pid, table_name))
	  config$logger.info(sprintf("Load dataset from jobdir file '%s'", path_to_dataset))
	  
	  ### METADATA => replace / set InputMetadataset with geoflow current entity
	  #------------------------------------------------------------------------------------------------------------------------
	  #get geoflow entity as data.frame representation to make easier mapping with DBMS metadata table
	  geoflow_df <- entity$asDataFrame()
	  
	  #build legacy metadata Tuna atlas metadata data.frame representation
	  config$logger.info("Preparing legacy Tuna atlas metadata entry")
	  InputMetadataset <- data.frame(
	    identifier = entity$identifiers[["id"]],		
	    persistent_identifier = entity$identifiers[["id"]],
	    title = entity$titles[["title"]],
	    contacts_and_roles = geoflow_df$Creator,
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
	    dataset_type = "raw_dataset",
	    sql_query_dataset_extraction = NA, #filled below with R code
	    database_table_name = table_name,
	    database_view_name = dataset_pid,
	    stringsAsFactors = FALSE
	  )
	  
	  InputMetadataset[is.na(InputMetadataset)] <- "NA"
	  
	  # julien => should be taken from data dictionnary embedded in the database and used for 19110 ?
	  db_dimensions_parameters<-read.csv("https://raw.githubusercontent.com/eblondel/rtunaatlas/master/inst/extdata/db_dimensions_parameters.csv",stringsAsFactors = F,strip.white=TRUE)
	  # db_dimensions_parameters<-read.csv(system.file("extdata", "db_dimensions_parameters.csv",package="rtunaatlas"),stringsAsFactors = F,strip.white=TRUE)
	  
	  variable_name<-gsub("fact_tables.","",InputMetadataset$database_table_name)
	  
	  dimensions<-list_variable_available_dimensions(con,variable_name)
	  
	  df_codelists_input<-df_codelists[which(df_codelists$dimension %in% dimensions),]
	  
	  # convert columns that are not character to character and ensure that the column "value" is a numeric
	  cols<-setdiff(colnames(df_to_load),"value")
	  for (i in 1:length(cols)){
		if (typeof(df_to_load[,cols[i]])!="character"){
		  df_to_load[,cols[i]]<-as.character(df_to_load[,cols[i]])
		}
	  }
	  df_to_load$value<-as.numeric(df_to_load$value)
	  
	  #### First we deal with all the dimensions that are "real" code lists: area,catchtype,catchunit,effortunit,fishingfleet,gear,schooltype,species,sex,ocean
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
	  #missingCodes_dataframe<- data.frame(TablesToUpdateInDB,missingCodesInTableInDB)
	  #missingCodes_dataframe<-missingCodes_dataframe[! (missingCodes_dataframe$missingCodesInTableInDB %in% c("ALL","UNK")),]
	  
	  #if (nrow(missingCodes_dataframe)>0){
	  #  missingCodes_dataframe<-missingCodes_dataframe[!is.na(missingCodes_dataframe$missingCodesInTableInDB),]
	  #  print(missingCodes_dataframe)
	  #  stop("Some code(s) exist in the dataset to upload but do not exist in the corresponding code list. You should update the tables of Sardara with these code(s) (and the mapping if relevant) before uploading the dataset in Sardara.")
	  #} else {
	  
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
	  # Retrieve the PK of the metadata for the line just inserted
	  sql<- sprintf("SELECT * FROM metadata.metadata where identifier = '%s'", dataset_pid)
	  dataset_metadata <- dbGetQuery(con, sql)
	  if(nrow(dataset_metadata)==0){
		config$logger.info(sprintf("Loading metadata for dataset '%s'", dataset_pid))
		rs<-FUNUploadDatasetToTableInDB(con,InputMetadataset,"metadata.metadata")
		config$logger.info(sprintf("Metadata loaded for dataset '%s'", dataset_pid))
	  }else{
		config$logger.info(sprintf("Metadata already existing in DB for dataset '%s'. Skipping metadata insert...", dataset_pid))
		#TODO update
	  }
	  dataset_metadata <- dbGetQuery(con, sql)
	  PK_metadata<-as.integer(dataset_metadata$id_metadata)
	  InputMetadataset$id_metadata <- PK_metadata
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
	  
	  rs<-FUNUploadDatasetToTableInDB(con,df_to_load,InputMetadataset$database_table_name)
	  
	  cat("Data loaded in the DB\n")
	  
	  
	  ## Update some metadata elements
	  # first drop materialized view
	  # if(!is.null(database_view_name)){
	  config$logger.info(sprintf("Droping materialized view '%s'", dataset_pid))
	  dataset_drop_view_sql <- paste0("DROP MATERIALIZED VIEW IF EXISTS ",paste0(schema_name_for_view,".",database_view_name,";"))
	  config$logger.info(sprintf("SQL: %s", dataset_drop_view_sql))
	  dbSendQuery(con, dataset_drop_view_sql)
	  # }
	  
	  # sql_query_dataset_extraction
	  sql_query_dataset_extraction<-getSQLSardaraQueries(con,InputMetadataset)
	  config$logger.info(sprintf("Update metadata sql_query_dataset_extraction' field for '%s'",dataset_pid))
	  dataset_update_meta_sql <- paste0("UPDATE metadata.metadata SET sql_query_dataset_extraction='",gsub("'","''",sql_query_dataset_extraction$query_CSV_with_labels),"' WHERE identifier='",InputMetadataset$identifier,"'")
	  config$logger.info(sprintf("SQL: %s", dataset_update_meta_sql))
	  dbSendQuery(con, dataset_update_meta_sql)
	  
	  # spatial coverage
	  sp_extent_sql<-paste0("SELECT st_astext(ST_Extent(geom)) FROM ",InputMetadataset$database_table_name," c LEFT JOIN area.area_labels USING (id_area) WHERE id_metadata='",PK_metadata,"'")
	  sp_extent<-dbGetQuery(con,sp_extent_sql)$st_astext
	  #TODO --> update geoflow entity?
	  #TODO --> update new metadataDCMI table?
	  dbSendQuery(con,paste0("UPDATE metadata.metadata SET spatial_coverage='",sp_extent,"' WHERE identifier='",InputMetadataset$identifier,"'"))
	  
	  
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
	  
  
  }
  
  # Create the materialized view if set in the metadata
  if(!is.na(database_view_name)){
    
    # Create the materialized view without the labels (to get the labels, replace sql_query_dataset_extraction$query_CSV by sql_query_dataset_extraction$query_CSV_with_labels)
    if(upload_to_db && create_materialized_view){
	
		config$logger.info(sprintf("Creating materialized view '%s' (with codes and labels)",paste0(schema_name_for_view,".",database_view_name)))
		# Check if schema exists
		list_of_schemas <- dbGetQuery(con,"select schema_name from information_schema.schemata")$schema_name
		# Get schema name where to store the materialized view
		# schema_name_for_view<-sub('\\..*', '', database_view_name)
		# Create the schema if it does not exist
		#TODO --> IF fact_tables is used now, then this piece of of code is probably useless and we can remove it
		if (!(schema_name_for_view %in% list_of_schemas)){
		  config$logger.info(sprintf("Schema '%s' doesn't exist. Creating it...", schema_name_for_view))
		  schema_create_sql <- paste0("CREATE SCHEMA ",schema_name_for_view,"; GRANT USAGE ON SCHEMA ",schema_name_for_view," TO ",user_postgres,";ALTER DEFAULT PRIVILEGES IN SCHEMA ",schema_name_for_view," GRANT SELECT ON TABLES TO ",user_postgres,";")
		  config$logger.info(sprintf("SQL: %s", schema_create_sql))
		  dbSendQuery(con,schema_create_sql)
		}
	
		config$logger.info(sprintf("Dropping materialized view '%s'", paste0(schema_name_for_view,".",database_view_name)))
		sql_drop_materialized_view <- paste0("DROP MATERIALIZED VIEW IF EXISTS ",paste0(schema_name_for_view,".",database_view_name),";")
		config$logger.info(sprintf("SQL: %s", sql_drop_materialized_view))
		dbSendQuery(con, sql_drop_materialized_view)
		
		config$logger.info(sprintf("Creating materialized view '%s'", paste0(schema_name_for_view,".",database_view_name)))
		sql_create_materialized_view <- paste0("CREATE MATERIALIZED VIEW ",paste0(schema_name_for_view,".",database_view_name)," AS ",sql_query_dataset_extraction$query_CSV,";")
		config$logger.info(sprintf("SQL: %s", sql_create_materialized_view))
		dbSendQuery(con, sql_create_materialized_view)
	
		#create indexes for main columns
		if(create_indexes){
			config$logger.info(sprintf("Creating indexes for view '%s'", paste0(schema_name_for_view,".",database_view_name)))
			this_view <- dbGetQuery(con,paste0("SELECT * FROM ",paste0(schema_name_for_view,".",database_view_name)," LIMIT 1;"))
			column_names <- colnames(this_view)
			time_dimensions <- c("time_start", "time_end", "year", "quarter", "month")
			columns_to_index <- c(column_names[column_names %in% dimensions], time_dimensions)
			for(column_name in columns_to_index){
				create_index_sql <- sprintf("CREATE INDEX %s_%s_idx  ON %s.%s (%s);", database_view_name, column_name, schema_name_for_view, database_view_name, column_name)
				config$logger.info(sprintf("SQL: %s", create_index_sql))
				dbSendQuery(con, create_index_sql)
			}
		}
		
		#comment DB materialized view columns
		if(add_sql_comments){
			dbSendQuery(con,paste0("COMMENT ON MATERIALIZED VIEW ",paste0(schema_name_for_view,".",database_view_name)," IS '",InputMetadataset$title,"';"))
			this_view <- dbGetQuery(con,paste0("SELECT * FROM ",paste0(schema_name_for_view,".",database_view_name)," LIMIT 1;"))
			column_names <- colnames(this_view)
			column_comments <-NULL
			
			dictionary <- config$getDictionary()
			if(!is.null(dictionary)){
			  ft <- dictionary$getFeatureTypeById(variable_name)
			  for(i in 1:length(column_names)){
				member <- ft$getMemberById(column_names[i])
				if(!is.null(member)){
				  config$logger.info(sprintf("Adding column definition from dictionary for column '%s'", column_names[i]))
				  new_comment <- paste0("COMMENT ON COLUMN ",paste0(schema_name_for_view,".",database_view_name,".", column_names[i]),"  IS '",member$def,"';")
				  column_comments <- paste0(column_comments,new_comment)
				}else{
				  config$logger.warn(sprintf("No dictionary definition for column '%s'. Skip adding comment to materialized view column", column_names[i]))
				}
			  }
			}else{
				for(i in 1:length(column_names)){
					new_comment <- switch(column_names[i],
								  "source_authority" = paste0("COMMENT ON COLUMN ",paste0(schema_name_for_view,".",database_view_name,".source_authority"),"  IS 'Source authority in charge of producing the source statistics collated and harmonized.';"),
								  "source_authority_label" = paste0("COMMENT ON COLUMN ",paste0(schema_name_for_view,".",database_view_name,".source_authority_label")," IS 'source_authority_label.';"),
								  "fishingfleet" = paste0("COMMENT ON COLUMN ",paste0(schema_name_for_view,".",database_view_name,".fishingfleet")," IS 'Fishing fleet. A group of fishing vessels authorized to operate in a t-RFMO convention area / area of competence, and whose fishing operations and catches of tuna and tuna-like species are responsibility of, and accounted for by a political entity or sub-entity recognized by the corresponding t-RFMO. To be noted that the actual occurrences of the Fishing fleet concept do not necessarily refer or correspond to a recognized country (e.g.: EUR - European Union, FRAT – French territories), nor to a distinct member / contracting party / cooperating, non-contracting party of a t-RFMO (e.g.: EU,ESP - EU (Spain), TWN – Chinese Taipei / Taiwan province of China – for some t-RFMOs). The proposed list of fishing fleet codes also includes a generic reference that applies to fishing operations and catches from unidentified sources (e.g.: NEI - not elsewhere identified).';"),
								  "fishingfleet_label" = paste0("COMMENT ON COLUMN ",paste0(schema_name_for_view,".",database_view_name,".fishingfleet_label")," IS 'fishingfleet_label.';"),
								  "gear" = paste0("COMMENT ON COLUMN ",paste0(schema_name_for_view,".",database_view_name,".gear"),"  IS 'Fishing gear used. Fishing gear are grouped by categories, in accordance with the International Standard Statitical Classficiation of Fishing Gear (ISSCFG) endorsed by the CWP. The number of gears varies a lot depending on the RFMOs. ICCAT, for instance, has around 60 gears while IATTC has 10 gears. This table is a dimension of the data warehouse: a list of codes which gives the context of the values stored in the fact table.';"),
								  "gear_label" = paste0("COMMENT ON COLUMN ",paste0(schema_name_for_view,".",database_view_name,".gear_label")," IS 'gear_label.';"),
								  "gear_group" = paste0("COMMENT ON COLUMN ",paste0(schema_name_for_view,".",database_view_name,".gear_group"),"  IS 'Group of fishing gears.';"),
								  "gear_group_label" = paste0("COMMENT ON COLUMN ",paste0(schema_name_for_view,".",database_view_name,".gear_group_label")," IS 'gear_group_label.';"),
								  "species" = paste0("COMMENT ON COLUMN ",paste0(schema_name_for_view,".",database_view_name,".species"),"  IS 'Species captured. The main tuna species are available in all the RFMOs Depending on the RFMO, some non-target species (e.g. some sharks or turtles) are also reported. This table is a dimension of the data warehouse: a list of codes which gives the context of the values stored in the fact table.';"),
								  "species_label" = paste0("COMMENT ON COLUMN ",paste0(schema_name_for_view,".",database_view_name,".species_label")," IS 'species_label.';"),
								  "schooltype" = paste0("COMMENT ON COLUMN ",paste0(schema_name_for_view,".",database_view_name,".schooltype"),"  IS 'A school is a group of fishes evolving together. The type of school indicates the nature of the school on which the catch has been made: free school, log school, unknown, dolphin.';"),
								  "schooltype_label" = paste0("COMMENT ON COLUMN ",paste0(schema_name_for_view,".",database_view_name,".schooltype_label")," IS 'Type of school.';"),
								  "time" = paste0("COMMENT ON COLUMN ",paste0(schema_name_for_view,".",database_view_name,".time"),"  IS 'Dating of the fact. In Sardara, the dating is provided as two columns: time_start gives the first date of availability of the measure (included) and time_end gives the last date of availability of the measure (not included). The data in Sardara are mainly defined over the following time steps: 1) Nominal catch are mostly defined on 1 year resolution. 2) Georeferenced catch-and-effort and catch-at-size are mostly defined on 1 month resolution.  This table is a dimension of the data warehouse: a list of codes which gives the context of the values stored in the fact table.';"),
								  "time_period" = paste0("COMMENT ON COLUMN ",paste0(schema_name_for_view,".",database_view_name,".time_period")," IS 'Interval of time over which the measure is defined.';"),
								  "time_start" = paste0("COMMENT ON COLUMN ",paste0(schema_name_for_view,".",database_view_name,".time_start")," IS 'Start time of the fact. Starting time - first date of availability of the measure (inclusive).';"),
								  "time_end" = paste0("COMMENT ON COLUMN ",paste0(schema_name_for_view,".",database_view_name,".time_end")," IS 'End time of the fact.Ending time - last date of availability of the measure (exclusive).';"),
								  "month" = paste0("COMMENT ON COLUMN ",paste0(schema_name_for_view,".",database_view_name,".month")," IS 'Month over which the measure is defined (coded as integer from 1 to 12). First month in case of a measure defined over more than one month.';"),
								  "quarter" = paste0("COMMENT ON COLUMN ",paste0(schema_name_for_view,".",database_view_name,".quarter")," IS 'Quarter over which the measure is defined (coded as integer from 1 to 4). First quarter in case of a measure defined over more than one quarter.';"),
								  "year" = paste0("COMMENT ON COLUMN ",paste0(schema_name_for_view,".",database_view_name,".year")," IS 'Year over which the measure is defined. First year in case of a measure defined over more than one year.';"),
								  "id_area" = paste0("COMMENT ON COLUMN ",paste0(schema_name_for_view,".",database_view_name,".id_area")," IS 'Spatial area (zone) where the fact has taken place. The data in Sardara are mainly defined on the following areas: 1) Nominal catch are mostly defined on the areas of competence of the RFMOs. For some RFMOs, the spatial stratification can be thinner: IOTC gives nominal catch at the FAO areas scale and ICCAT gives it at the sampling area scale. 2) Georeferenced catch and effort and catch-at-size are mostly defined on 1 or 5 degree square resolution. In some cases irregular areas are also used (e.g. in IOTC). This may happen when the reporting country/institution does not provide the data at 1/5 degree resolution. This table is a dimension of the data warehouse: a list of codes which gives the context of the values stored in the fact table.';"),
								  "geographic_identifier" = paste0("COMMENT ON COLUMN ",paste0(schema_name_for_view,".",database_view_name,".geographic_identifier")," IS 'String-based Geographic identifier (conventional code name, grid code, transect identifier, location code, etc).';"),
								  "geographic_identifier_label" = paste0("COMMENT ON COLUMN ",paste0(schema_name_for_view,".",database_view_name,".geographic_identifier_label")," IS 'geographic_identifier_label.';"),
								  # "geom_wkt" = paste0("COMMENT ON COLUMN ",paste0(schema_name_for_view,".",database_view_name,".geom_wkt")," IS 'WKT .';"),
								  "the_geom" = paste0("COMMENT ON COLUMN ",paste0(schema_name_for_view,".",database_view_name,".the_geom" )," IS 'Geometry in one of the standard data formats (e.g. GML, WKT).';"),
								  "longitude" = paste0("COMMENT ON COLUMN ",paste0(schema_name_for_view,".",database_view_name,".longitude" )," IS 'Longitude of the centroid of the pixel or point location (EPSG:4326).';"),
								  "latitude" = paste0("COMMENT ON COLUMN ",paste0(schema_name_for_view,".",database_view_name,".latitude")," IS 'Latitude of the centroid of the pixel or point location (EPSG:4326).';"),
								  "catchtype" = paste0("COMMENT ON COLUMN ",paste0(schema_name_for_view,".",database_view_name,".catchtype"),"  IS 'Fate of the catch, i.e. landed, discarded, unknown. Given the nature of the data, only landing data are currently available in SARDARA, with a very few exceptions of discarded fishes for ICCAT. This table is a dimension of the data warehouse: a list of codes which gives the context of the values stored in the fact table.';"),
								  "catchtype_label" = paste0("COMMENT ON COLUMN ",paste0(schema_name_for_view,".",database_view_name,".catchtype_label")," IS 'catchtype_label.';"),
								  "value" = paste0("COMMENT ON COLUMN ",paste0(schema_name_for_view,".",database_view_name,".value" )," IS 'the measure of the fact (variable).';"),
								  "unit" = paste0("COMMENT ON COLUMN ",paste0(schema_name_for_view,".",database_view_name,".unit")," IS 'unit.';"),
								  "unit_label" = paste0("COMMENT ON COLUMN ",paste0(schema_name_for_view,".",database_view_name,".unit_label" )," IS 'unit_label.';")
								  # "catchunit" = paste0("COMMENT ON COLUMN ",paste0(schema_name_for_view,".",database_view_name,".catchunit")," IS 'Unit of catch.';"),
								  # "effortunit" = paste0("COMMENT ON COLUMN ",paste0(schema_name_for_view,".",database_view_name,".effortunit")," IS 'Unit of effort.';"),		             
								  # "cmax" = paste0("COMMENT ON COLUMN ",paste0(schema_name_for_view,".",database_view_name,".cmax" )," IS 'cmax';"),
								  # "cmin" = paste0("COMMENT ON COLUMN ",paste0(schema_name_for_view,".",database_view_name,".cmin" )," IS 'cmin';"),
								  # "ctid" = paste0("COMMENT ON COLUMN ",paste0(schema_name_for_view,".",database_view_name,".ctid" )," IS 'ctid';"),
								  # "tableoid" = paste0("COMMENT ON COLUMN ",paste0(schema_name_for_view,".",database_view_name,".tableoid" )," IS 'tableoid';"),
								  # "xmax" = paste0("COMMENT ON COLUMN ",paste0(schema_name_for_view,".",database_view_name,".xmax" )," IS 'xmax';"),
								  # "xmin" = paste0("COMMENT ON COLUMN ",paste0(schema_name_for_view,".",database_view_name,".xmin" )," IS 'xmin';")
					)
					column_comments <- paste0(column_comments,new_comment)
				}
			}
			dbSendQuery(con,column_comments)
		}
    }
	
	#upload dataset to Google drive
	if(upload_to_googledrive){
		config$logger.info("Upload dataset (CSV) to Google Drive")
		# folder_datasets_id <- drive_get("~/geoflow_tunaatlas/data/outputs/datasets")$id #googledrive 1.0.0 doesn't work for that.. needs the github fix
		folder_datasets_id <- "16fVLytARK13uHCKffho3kYJgm0KopbKL"
		path_to_dataset_new <- file.path(getwd(), "data", paste0(entity$identifiers[["id"]], ".csv"))
		file.rename(from = path_to_dataset, to = path_to_dataset_new)
		id_csv_dataset <- drive_upload(path_to_dataset_new, as_id(folder_datasets_id), overwrite = TRUE)$id
		
		if(upload_to_db){
		
			#store SQL files on job dir google drive
			config$logger.info("Write SQL queries (view/data) to job directory")
			sql_view <- sprintf("SELECT * FROM fact_tables.%s", entity$identifiers[["id"]])
			file_sql_view <-  paste0(entity$identifiers[["id"]],"_view.sql")
			file_csv_view <- paste0(entity$identifiers[["id"]],"_view.csv")
			file_csv_view_without_geom <- paste0(entity$identifiers[["id"]],"_view (without geom).csv")
			sql_data <- sql_query_dataset_extraction$query_CSV_with_labels
			file_sql_data <- paste0(entity$identifiers[["id"]],"_data.sql")
			
			config$logger.info("Upload SQL queries (view/data) to Google Drive")
			# folder_views_id <- drive_get("~/geoflow_tunaatlas/data/outputs/views")$id #googledrive 1.0.0 doesn't work for that.. needs the github fix
			folder_views_id <- "1Rm8TJsUM0DQo1c91LXS5kCzaTLt8__bS"
			entity$data$access <- "googledrive"
			if(create_materialized_view){
				writeLines(sql_view, file.path("data", file_sql_view))
				writeLines(sql_data, file.path("data", file_sql_data))
				this_view <- dbGetQuery(con,paste0("SELECT * FROM ",paste0(schema_name_for_view,".",database_view_name),";"))
				write.csv(this_view, file.path("data", file_csv_view), row.names = FALSE)
				if("geom_wkt" %in% colnames(this_view)){
					this_view_without_geom = this_view
					this_view_without_geom$geom_wkt <- NULL
					write.csv(this_view_without_geom, file.path("data", file_csv_view_without_geom), row.names = FALSE)
					drive_upload(file.path("data", file_csv_view_without_geom), as_id(folder_views_id), overwrite = TRUE)
				}
				drive_upload(file.path("data", file_csv_view), as_id(folder_views_id), overwrite = TRUE)
				drive_upload(file.path("data", file_sql_view), as_id(folder_views_id), overwrite = TRUE)
				drive_upload(file.path("data", file_sql_data), as_id(folder_views_id), overwrite = TRUE)
				
				if("geom_wkt" %in% colnames(this_view)){
					entity$data$source <- list(file_csv_view, file_csv_view_without_geom, file_sql_view, file_sql_data)
				}else{
					entity$data$source <- list(file_csv_view, file_sql_view, file_sql_data)
				}
			}else{
				writeLines(sql_data, file.path("data", file_sql_data))
				drive_upload(file.path("data", file_sql_data), as_id(folder_views_id), overwrite = TRUE)
				entity$data$source <- list(file_sql_data)
			}
			
		}
	}
  }
  #}
  
  
  
  
}
