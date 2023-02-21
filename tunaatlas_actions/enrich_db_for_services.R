enrich_db_for_services <- function(action,entity, config, options){
  opts <- action$options
  con <- config$software$output$dbi
  schema <- "fact_tables"
  
  #set information required for (meta)data services
  geom_table <- "area.area_labels"
  if(!is.null(entity$resources$geom_table)) geom_table <- entity$resources$geom_table
  
  df_codelists <- read.csv(entity$resources$codelists)
  dimensions <- c(df_codelists$dimension [df_codelists$dimension != "area"], "time_start", "time_end", "year", "quarter", "month", "aggregation_method")
  
  #scripts
  url_scripts_create_own_tuna_atlas <- "https://raw.githubusercontent.com/eblondel/geoflow-tunaatlas/master/tunaatlas_actions"
  source(file.path(url_scripts_create_own_tuna_atlas, "create_plsql_data_getter.R"))
  create_plsql_data_getter(action,entity, config, opts) #create pl/sql function in DB to get fact dataset (generic function, one function per fact)
  
  #entity management
  pid <- entity$identifiers[["id"]]
  fact <- unlist(strsplit(entity$data$uploadSource[[1]], "\\."))[2]
  entity$data$run <- FALSE #deactivate local action (harmonization/generation)
  entity$data$sourceType <- "csv"
  entity$data$uploadType <- "dbquery" #set dbquery as upload type for enabling geoserver sql view data services
  entity$data$uploadSource <- TRUE
  #for feature catalogue / dictionnary
  entity$data$featureType <- fact
  #geoserver sql view properties
  entity$data$layername <- pid
  entity$data$setSql(sprintf("select * from get_fact_dataset_%s('%s', '%s', %s, '%s')", fact, schema, pid, paste0("'%", dimensions,"%'", collapse=","), geom_table))
  entity$data$setGeometryField("geom")
  entity$data$setGeometryType("MultiPolygon")
  
  default_values <- list()
  for(dimension in dimensions){
    regexpValue <- switch(dimension,
                          "time_start" = "^(19|20)\\d\\d([- /.])(0[1-9]|1[012])\\2(0[1-9]|[12][0-9]|3[01])$",
                          "time_end" = "^(19|20)\\d\\d([- /.])(0[1-9]|1[012])\\2(0[1-9]|[12][0-9]|3[01])$",
                          "aggregation_method" = "^[none|avg_by_year|avg_by_quarter|avg_by_month|sum]+$",
                          "^[\\w. +]+$"
    )
    defaultValue <-switch(dimension,
                          "time_start" = as.character(entity$temporal_extent$start), #paste0(substr(as.character(entity$temporal_extent$end), 1, 4),"-01-01"),
                          "time_end" = as.character(entity$temporal_extent$end),
                          "aggregation_method" = "none",
                          "year" = "", #paste(as.integer(format(entity$temporal_extent$start, "%Y")):as.integer(format(entity$temporal_extent$end, "%Y")), collapse="+"),
                          "quarter" = "", #paste(1:4, collapse="+"),
                          "month" = "", #paste(1:12, collapse="+"),
                          "" #paste(dbGetQuery(con, sprintf("select distinct %s from %s.%s",dimension, schema, pid, dimension))[,1], collapse="+")
    )
    print("--------------")
    print(dimension)
    print(regexpValue)
    print(defaultValue)
    default_values <- c(default_values, defaultValue)
    entity$data$setParameter(dimension, dimension, regexpValue, defaultValue)
  }
  
  config$logger.info("Upload SQL main query (based on PL/SQL function to query data) to Google Drive")
  # folder_views_id <- drive_get("~/geoflow_tunaatlas/data/outputs/views")$id #googledrive 1.0.0 doesn't work for that.. needs the github fix
  folder_views_id <- "1Rm8TJsUM0DQo1c91LXS5kCzaTLt8__bS"
  mainSource <- sprintf("select * from get_fact_dataset_%s('%s', '%s', %s, '%s')", fact, schema, pid, paste0("'", default_values,"'", collapse=","),geom_table)
  file_sql_query <- paste0(entity$identifiers[["id"]], "_query.sql")
  writeLines(mainSource, file.path("data", file_sql_query))
  #entity$data$source <- c(file_sql_query, entity$data$source)
  drive_upload(file.path("data", file_sql_query), as_id(folder_views_id), overwrite = TRUE)
}
