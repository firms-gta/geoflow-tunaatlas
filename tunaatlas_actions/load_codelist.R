load_codelist <- function(entity, config, options){
  
  #connection to database
  CON = config$software$output$dbi
  
  #packages
  if(!require(rtunaatlas)){
    if(!require(devtools)){
      install.packages("devtools")
    }
    require(devtools)
    install_github("ptaconet/rtunaatlas")
    require(rtunaatlas)
  }
  
  if(!require(readr)){
    install.packages("readr")
    require(readr)
  }
  
  if(!require(dplyr)){
    install.packages("dplyr")
    require(dplyr)
  }
  
  #----------------------------------------------------------------------------------------------------------------------------
  #@geoflow --> with this script 2 objects are pre-loaded
  #config --> the global config of the workflow
  #entity --> the entity you are managing
  #get data from geoflow current job dir
  filename <- entity$data$source[[1]]
  path_to_dataset <- entity$getJobDataResource(config, filename)
  #----------------------------------------------------------------------------------------------------------------------------
  
  codelist_pid <- entity$identifiers[["id"]]
  table_name <- entity$data$uploadSource[[1]]
  dimension_name <- sub('\\..*', '', table_name)
  
  config$logger.info(sprintf("Load codelist '%s' as table '%s'",codelist_pid, table_name))
  config$logger.info(sprintf("Load codelist from jobdir file '%s'", path_to_dataset))
  df_to_load <- as.data.frame(readr::read_csv(path_to_dataset, guess_max=0))
  
  #below code inherited from https://github.com/ptaconet/rtunaatlas/blob/master/R/load_datasets_in_db.R#L394
  #all the codes have been migrated and adapted to geoflow logic except rtunaatlas::getSQLSardaraQueries
  
  ## change all columns to "text" format. in the db, the columns will all be set to "text"
  df_to_load <- df_to_load %>% mutate_all(as.character)
  #to lower colnames
  colnames(df_to_load)<-tolower(colnames(df_to_load))
  # if there are points in the columns of the input code list, replace them with underscores
  colnames(df_to_load)<-gsub('.', '_', colnames(df_to_load),fixed=TRUE)
  
  
  ### Check if information given by the operator is OK
  
  # Check if the column 'code' exists, and is unique not null
  if ( !(any(names(df_to_load)=="code")) ) {
    stop("There is no column 'code' in the code list. Please set at least a column 'code', and possibly a column 'label'")
  }
  if (length(df_to_load$code)!=nrow(df_to_load) | length(which(is.na(df_to_load$code)))>0  | length(which(is.null(df_to_load$code)))>0 ){
    stop("The codes contain NULL values or is are not unique. Codes must be unique and cannot contain NULL values")
  }
  
  # Check if there is no blank in codelist_pid and df_inputPKattributeName
  countSpaces <- function(s) { sapply(gregexpr(" ", s), function(p) { sum(p>=0) } ) }
  if (countSpaces(codelist_pid)>0 | countSpaces("code")>0){
    stop("The name of the code list and the codes in the column \"code\" cannot have blanks. Please check those parameters.")
  } 
  
  # Check if the name of the table does not already exist in database
  sql <- paste("SELECT '",dimension_name,"'||'.'||table_name FROM information_schema.tables WHERE table_schema='",dimension_name,"' UNION SELECT oid::regclass::text FROM pg_class WHERE relkind = 'm';",sep="")
  DB_Dimension_TableNames<-dbGetQuery(CON, sql)
  colnames(DB_Dimension_TableNames)<-"table_name"
  if (paste0(dimension_name,".",codelist_pid) %in% DB_Dimension_TableNames$table_name){
    stop("The name of the code list already exists in the database. Please set another name.")
  }
  
  # Check if there is the right number of df_inputColumnsDataTypes
  #if (length(df_inputColumnsDataTypes)!=ncol(df_to_load)){
  # stop("The number of data types you provided is different from the number of columns of the code list. Please set another number of data types (df_inputColumnsDataTypes) re-run the script")
  #}
  
  # Check if one of the columns of the df_to_load has not the name of a SQL Key word. If yes, add a "_" after the column name
  sql<-"select * from pg_get_keywords() where catdesc='reserved'"
  ReservedWords<-dbGetQuery(CON, sql)
  
  #!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  #Warning message: In postgresqlExecStatement(conn, statement, ...) :
  #RS-DBI driver warning: (unrecognized PostgreSQL field type char (id:18) in column 1)
  
  ColNamesAsReservedWords<-tolower(colnames(df_to_load)) %in% ReservedWords$word
  index.ColNamesAsReservedWords <- which(ColNamesAsReservedWords=="TRUE")
  if(length(index.ColNamesAsReservedWords)){
    colnames(df_to_load)[index.ColNamesAsReservedWords]<-paste(colnames(df_input)[index.ColNamesAsReservedWords],"_",sep="")
  }
  
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
  postgresqlpqExec(CON, sql4)
  postgresqlCopyInDataframe(CON, InputMetadataset)
  rs <- postgresqlgetResult(CON)
  
  # Retrieve the PK of the metadata for the line just inserted
  sql<- "SELECT max(id_metadata) FROM metadata.metadata"
  PK_metadata <- dbGetQuery(CON, sql)
  PK_metadata<-as.integer(PK_metadata$max[1])
  config$logger.info(sprintf("Retrieving internal metadata ID from DB: %s", PK_metadata))
  
  ### DATA
  #------------------------------------------------------------------------------------------------------------------------
  config$logger.info("Managing codelist data into DB")
  ### Add code list table in the DB, with constraints (data types and primary key) and triggers
  #First create table ...
  config$logger.info("Creating codelist data table into DB")
  sql<- paste("CREATE TABLE ",table_name,"()",sep="")
  dbSendQuery(CON, sql)
  for (i in 1:ncol(df_to_load)){
    # columns are all set to "text" type.
    #sql<- paste("ALTER TABLE ",dimension_name,".",codelist_pid," ADD COLUMN ",tolower(colnames(df_input)[i])," ",df_inputColumnsDataTypes[i],sep="")
    column <- colnames(df_to_load)[i]
    type <- "text"
    if(column == "geom") type <- "geometry"
    sql<- paste("ALTER TABLE ",table_name," ADD COLUMN ",tolower(column),paste0(" ",type),sep="")
    dbSendQuery(CON, sql)
  }
  sql<- paste("ALTER TABLE ",table_name," ADD CONSTRAINT ",codelist_pid,"_pkey PRIMARY KEY (code)",sep="")
  dbSendQuery(CON, sql)
  
  ## Add codes and labels in the table metadata.df_inputs_codes_labels_column_names
  #df_inputPKattributeName="code"
  #if ( !(any(names(df_to_load)=="label")) ) {
  #  df_inputLabelattributeName="code" } else {df_inputLabelattributeName="label"}
  
  #df_inputLabelattributeName="label"
  #sql<-paste0("INSERT INTO metadata.codelists_codes_labels_column_names(database_table_name,code_column,label_column) VALUES ('",table_name,"','",df_inputPKattributeName,"','",df_inputLabelattributeName,"')")
  #dbSendQuery(CON, sql)
  config$logger.info("Creating triggers...")
  # Create triggers to automatically fill and update the link dimension table
  sql_trigg_fill_link_dimension_table<-paste("CREATE OR REPLACE FUNCTION ",dimension_name,".func_add_new_record_in_link_table_",codelist_pid,"() RETURNS trigger AS $BODY$ BEGIN INSERT INTO ",dimension_name,".",dimension_name," ( codesource_",dimension_name,",tablesource_",dimension_name,") VALUES (NEW.code,'",codelist_pid,"') ; RETURN NEW; END; $BODY$ LANGUAGE 'plpgsql' VOLATILE;",sep="")
  dbSendQuery(CON, sql_trigg_fill_link_dimension_table)
  sql_trigg_fill_link_dimension_table<-paste("CREATE TRIGGER trig_add_new_record_in_link_table_",codelist_pid," BEFORE INSERT ON ",table_name," FOR EACH ROW EXECUTE PROCEDURE ",dimension_name,".func_add_new_record_in_link_table_",codelist_pid,"();",sep="")
  dbSendQuery(CON, sql_trigg_fill_link_dimension_table)
  
  # ... Then fill table
  #code inherited from rtunaatlas::FUNUploadDatasetToTableInDB
  config$logger.info("Loading codelist data into DB")
  InputDataset <- df_to_load
  InputDataset[is.na(InputDataset)] <- "NA"
  sql4 <- paste0("COPY  ", table_name, "(", 
                 paste0(names(InputDataset), collapse = ", "), ") FROM STDIN NULL 'NA' ")
  postgresqlpqExec(CON, sql4)
  postgresqlCopyInDataframe(CON, InputDataset)
  rs <- postgresqlgetResult(CON)
  
  # Finally fill the dimension link table with the metadata
  sql<-paste("UPDATE ",dimension_name,".",dimension_name," SET id_metadata=",PK_metadata," WHERE tablesource_",dimension_name,"='",codelist_pid,"'",sep="")
  dbSendQuery(CON, sql)
  dbSendQuery(CON,paste0("COMMENT ON TABLE ",table_name," IS '",entity$title,"'"))
  
  test_if_code_list_is_inserted<-dbGetQuery(CON,paste0("SELECT * FROM ",table_name, " LIMIT 1"))
  if(nrow(test_if_code_list_is_inserted)>0){
    config$logger.info(sprintf("The code list was successfully loaded. It is in the table %s of the database", table_name))
  }
  
  # Add a column geometry if geospatial code list. For now: must be a polygon or multipolygon with SRID=4326
  if (dimension_name=="area"){
    config$logger.info("Adding geometry...")
    
    if(!"geom" %in% colnames(df_to_load)){
      # Add the column
      config$logger.info("Adding 'geom' column...")
      sql<-paste("ALTER TABLE ",table_name," ADD COLUMN geom GEOMETRY(MultiPolygon,4326);",sep="")
      dbSendQuery(CON, sql)
      # Calculate the column
      config$logger.info("Update 'geom' column from WKT...")
      sql<-paste("UPDATE ",table_name," SET geom=ST_Multi(ST_GeomFromText(geom_wkt,4326));",sep="")
      dbSendQuery(CON, sql)
      # Remove the column geom_wkt
      config$logger.info("Drop 'geom_wkt'...")
      sql<-paste("ALTER TABLE ",table_name," DROP COLUMN geom_wkt;",sep="")
      dbSendQuery(CON, sql)
    }
  }
  
  ### Updates the view that gives the labels, with the new code list just inserted 
  config$logger.info("Updating materialized view of labels...")
  
  table_name_without_schema<-gsub(".*\\.","",table_name)
  
  name_view_labels<-paste0(dimension_name,"_labels")
  
  colname_view_id<-paste0("id_",dimension_name)
  colname_view_codesource<-paste0("codesource_",dimension_name)
  colname_view_tablesource<-paste0("tablesource_",dimension_name)
  colname_view_label<-"source_label"
  
  query_create_view_label<-dbGetQuery(CON,paste0("select pg_get_viewdef('",dimension_name,".",name_view_labels,"', true)"))
  
  ### Get name of the columns of the view
  columns_names_types_view_label<-dbGetQuery(CON,paste0("SELECT
                                                        a.attname as column,
                                                        pg_catalog.format_type(a.atttypid, a.atttypmod) as datatype
                                                        FROM
                                                        pg_catalog.pg_attribute a
                                                        WHERE
                                                        a.attnum > 0
                                                        AND NOT a.attisdropped
                                                        AND a.attrelid = (
                                                        SELECT c.oid
                                                        FROM pg_catalog.pg_class c
                                                        LEFT JOIN pg_catalog.pg_namespace n ON n.oid = c.relnamespace
                                                        WHERE c.relname = '",name_view_labels,"' 
                                                        )"))
  
  # these are the columns that will not be updated (they will be set with NULL values. User will have then to fill the right columns... to improve in next version)
  columns_names_types_view_label<-columns_names_types_view_label %>% filter (!(column %in% c(colname_view_id,colname_view_codesource,colname_view_tablesource,colname_view_label)))
  
  if ("geom" %in% columns_names_types_view_label$column){
    columns_names_types_view_label = columns_names_types_view_label[columns_names_types_view_label$column != "geom",]
  }
  
  query_null_columns<-NULL
  for (i in 1:nrow(columns_names_types_view_label)){
    query_null_columns_this_column<-paste0("NULL::",columns_names_types_view_label$datatype[i]," as ",columns_names_types_view_label$column[i])
    query_null_columns<-paste(query_null_columns,query_null_columns_this_column,sep=",")
  }
  if (dimension_name=="area"){
    query_null_columns<-paste0(query_null_columns,",geom")
  }
  
  sql_query_for_view_label_new_codelist<-paste0(" UNION SELECT ",colname_view_id,",",colname_view_codesource,",",colname_view_tablesource,",label as source_label")
  sql_query_for_view_label_new_codelist<-paste0(sql_query_for_view_label_new_codelist,query_null_columns," FROM ",table_name," tab JOIN ",dimension_name,".",dimension_name," ON ",dimension_name,".codesource_",dimension_name,"=tab.code::text WHERE ",dimension_name,".tablesource_",dimension_name,"='",table_name_without_schema,"'::text ORDER BY 3,2;")
  
  query_create_view_label<-gsub("ORDER BY 3, 2","",query_create_view_label)
  query_create_view_label<-gsub(";","",query_create_view_label)
  query_create_view_label<-paste("CREATE OR REPLACE VIEW ",dimension_name,".",name_view_labels," AS ",query_create_view_label,sql_query_for_view_label_new_codelist,sep="")
  
  # View of labels for geometry is a bit special since there is the geom. CODE BELOW TO IMPROVE!!!!
  if (dimension_name=="area"){
    pattern="CREATE OR REPLACE VIEW area.area_labels AS  WITH vue AS (.*?) SELECT vue.id_area"
    query_create_view_label<-regmatches(query_create_view_label,regexec(pattern,query_create_view_label))[[1]][1]
    query_create_view_label<-gsub(")\n SELECT vue.id_area","",query_create_view_label)
    query_create_view_label<-paste0(query_create_view_label,sql_query_for_view_label_new_codelist," )
										SELECT vue.id_area,
										vue.codesource_area,
										vue.tablesource_area,
										vue.source_label,
										vue.source_french_label,
										vue.source_spanish_label,
										st_setsrid(vue.geom, 4326) AS geom
										FROM vue")
    query_create_view_label<-gsub(";","",query_create_view_label)
    query_create_view_label<-gsub("CREATE OR REPLACE VIEW","DROP MATERIALIZED VIEW area.area_labels CASCADE; CREATE MATERIALIZED VIEW",query_create_view_label)
    query_create_view_label<-paste0(query_create_view_label,";
COMMENT ON MATERIALIZED VIEW \"area\".\"area_labels\" IS '\"area\".\"area_labels\" is a materialized view which fasters the access to information often needed in data access queries. View gathering all the codes and labels of the code lists available for the dimension area (spatial code lists).';
COMMENT ON COLUMN \"area\".\"area_labels\".\"id_area\" IS '\"id_area\" is the identifier (primary key)  ';
COMMENT ON COLUMN \"area\".\"area_labels\".\"codesource_area\" IS '\"codesource_area\" gives the geometry of the area as text (WKT format)  ';
COMMENT ON COLUMN \"area\".\"area_labels\".\"tablesource_area\" IS '\"tablesource_area\" gives the name of the physical table in the schema where this area is taken from ';
COMMENT ON COLUMN \"area\".\"area_labels\".\"source_label\" IS '\"source_label\" gives the label as it is delivered by the orginal dataset';
COMMENT ON COLUMN \"area\".\"area_labels\".\"source_french_label\" IS '\"source_french_label\"  gives the label of the area in french ';
COMMENT ON COLUMN \"area\".\"area_labels\".\"source_spanish_label\" IS '\"source_spanish_label\" gives the label of the area in spanish';
COMMENT ON COLUMN \"area\".\"area_labels\".\"geom\" IS '\"geom\" is the geometry stored by Postgis (SFS format)';
		                              ")
  }
  
  #finally send the query to recreate the view for the labels with the new code list inserted
  dbSendQuery(CON,query_create_view_label)
  config$logger.info("Materialized view of labels updated")
  
  ## fill-in metadata 'sql_query_dataset_extraction'
  InputMetadataset$id_metadata <- PK_metadata
  sql_query_dataset_extraction <- rtunaatlas::getSQLSardaraQueries(CON, InputMetadataset)
  dbSendQuery(CON, paste0("UPDATE metadata.metadata SET sql_query_dataset_extraction='",sql_query_dataset_extraction$query_CSV,"' WHERE identifier='",entity$identifiers[["id"]],"'"))
  
  config$logger.info("Successfuly loaded codelist into DB")
  
}