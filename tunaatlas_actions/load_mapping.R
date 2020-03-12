load_mapping<-function(entity, config, options){
  
  options(stringsAsFactors = FALSE)
  
  #connection to database
  con = config$software$input$dbi
  
  #packages
  if(!require(rtunaatlas)){
    if(!require(devtools)){
      install.packages("devtools")
    }
    require(devtools)
    install_github("ptaconet/rtunaatlas")
    require(rtunaatlas)
  }
  
  if(!require(dplyr)){
    install.packages("dplyr")
    require(dplyr)
  }
  
  filename <- entity$data$source[[1]]
  path_to_dataset <- entity$getJobDataResource(config, filename)
  
  codelist_pid <- entity$identifiers[["id"]]
  table_name <- entity$data$uploadSource[[1]]
  dimension_name <- sub('\\..*', '', table_name)
  
  config$logger.info(sprintf("Load mapping '%s' as table '%s'",codelist_pid, table_name))
  config$logger.info(sprintf("Load mapping from jobdir file '%s'", path_to_dataset))
  df_to_load <- read.csv(path_to_dataset)
  # df_to_load<read.csv(strsplit(x=CFG$src_entities$Data[1],split = "@")[[1]][2])
  
  # df_metadata <- entity
  
  # Check errors: TO DO  (are tables existing? etc.)
  
  # get table_name corresponding to dataset_name of src_codingsystem and trg_codingsystem
  src_codingsystem_table_name<-dbGetQuery(con,paste0("SELECT id_metadata,database_table_name FROM metadata.metadata where identifier='",unique(df_to_load$src_codingsystem),"'"))
  trg_codingsystem_table_name<-dbGetQuery(con,paste0("SELECT id_metadata,database_table_name FROM metadata.metadata where identifier='",unique(df_to_load$trg_codingsystem),"'"))
  
  # src_codingsystem_table_name<-dbGetQuery(con,paste0("SELECT id_metadata, \'",codelist_pid,"\' AS database_table_name FROM metadata.\"metadataDCMI\" WHERE \"Identifier\"='",paste0("id:",codelist_pid),"'"))
  
  DBDimensionName=gsub("\\..*","",src_codingsystem_table_name$database_table_name)
  
  src_codingsystem_table_name$database_table_name<-gsub(".*\\.","",src_codingsystem_table_name$database_table_name)
  trg_codingsystem_table_name$database_table_name<-gsub(".*\\.","",trg_codingsystem_table_name$database_table_name)
  
  # Get the PK of the two tables (DBToTableName and DBFromTableName)
  sql1<- paste("SELECT id_",DBDimensionName,",codesource_",DBDimensionName,
              " FROM ",DBDimensionName,".",DBDimensionName,
              " WHERE tablesource_",DBDimensionName,"='",src_codingsystem_table_name$database_table_name,"'",sep="")   
  FromTable<-dbGetQuery(con, sql1)
  if("codesource_flag" %in% colnames(FromTable)) Encoding(FromTable$codesource_flag) <- "UTF-8" #required on Windows OS
  if("codesource_species" %in% colnames(FromTable)) Encoding(FromTable$codesource_species) <- "UTF-8" #required on Windows OS
  
  sql2<- paste("SELECT id_",DBDimensionName,",codesource_",DBDimensionName,
              " FROM ",DBDimensionName,".",DBDimensionName,
              " WHERE tablesource_",DBDimensionName,"='",trg_codingsystem_table_name$database_table_name,"'",sep="")   
  ToTable<-dbGetQuery(con, sql2)
  
  # Make mapping
  
  MapFromTableWithMappingTable<-merge(FromTable,df_to_load,by.y="src_code",by.x=paste("codesource_",DBDimensionName,sep=""),all.y=T,all.x=F)
  
  MapFinal<-merge(MapFromTableWithMappingTable,ToTable,by.y=paste("codesource_",DBDimensionName,sep=""),by.x="trg_code",all.x=T,all.y=F)
  
  MapFinal <- MapFinal[,c(paste0("id_",DBDimensionName,".x"),paste0("id_",DBDimensionName,".y"))]
  MapFinal$mapping_relation_type<-"NA"
  colnames(MapFinal)<-c(paste0(DBDimensionName,"_mapping_id_from"),paste0(DBDimensionName,"_mapping_id_to"),paste0(DBDimensionName,"_mapping_relation_type"))
  MapFinal <- MapFinal[!is.na(MapFinal[,1]),]
  MapFinal <- MapFinal[!is.na(MapFinal[,2]),]
  
  # Load metadata with FUNUploadDatasetToTableInDB function
  # https://github.com/ptaconet/rtunaatlas/blob/0c819c0262f1abab58ec7307ca6e2e4601d97946/R/functions_load_dataset_in_db.R
  # rs<-FUNUploadDatasetToTableInDB(con,df_metadata,"metadata.metadata")
  # cat("Metadata loaded\n")
  
  ### METADATA
  #------------------------------------------------------------------------------------------------------------------------
  #get geoflow entity data.frame representation
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
  # Add metadata in metadata tables
  #code inherited from rtunaatlas::FUNUploadDatasetToTableInDB
  config$logger.info("Loading codelist metadata entry into DB")
  sql4 <- paste0("COPY  metadata.metadata (", paste0(names(InputMetadataset), collapse = ", "), ") FROM STDIN NULL 'NA' ")
  postgresqlpqExec(con, sql4)
  postgresqlCopyInDataframe(con, InputMetadataset)
  rs <- postgresqlgetResult(con)
  
  # Retrieve the PK of the metadata for the line just inserted
  sql<- "SELECT max(id_metadata) FROM metadata.metadata"
  PK_metadata <- dbGetQuery(con, sql)
  PK_metadata<-as.integer(PK_metadata$max[1])
  config$logger.info(sprintf("Retrieving internal metadata ID from DB: %s", PK_metadata))
  
  
  # Retrieve the PK of the metadata for the line just inserted
  sql<- "SELECT max(id_metadata) FROM metadata.metadata"
  PK_metadata <- dbGetQuery(con, sql)
  PK_metadata<-as.integer(PK_metadata$max[1])
  
  MapFinal$id_metadata<-PK_metadata
  
  # Insert mapping into mapping table
  dbWriteTable(con, c(DBDimensionName, paste(DBDimensionName,"_mapping",sep="")), value = MapFinal,row.names=FALSE,append=TRUE)
  
  ## Update some metadata elements
  
  # metadata_mapping
  sql<-paste0("INSERT INTO metadata.metadata_mapping(metadata_mapping_id_from,metadata_mapping_id_to) VALUES 
       (",PK_metadata,",",src_codingsystem_table_name$id_metadata,"),
       (",PK_metadata,",",trg_codingsystem_table_name$id_metadata,")")
  dbSendQuery(con,sql)
  
  # sql_query_dataset_extraction
  InputMetadataset$id_metadata<-PK_metadata
  sql_query_dataset_extraction<-getSQLSardaraQueries(con,InputMetadataset)
  dbSendQuery(con,paste0("UPDATE metadata.metadata SET sql_query_dataset_extraction='",sql_query_dataset_extraction$query_CSV,"' WHERE identifier='",InputMetadataset$identifier,"'"))
  
  
}
