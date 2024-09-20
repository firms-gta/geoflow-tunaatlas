enrich_db_for_services <- function(action,entity, config){
  opts <- action$options
  con <- config$software$output$dbi
  
  #for services rely on the public schema dataset
  schema <- "public"
  
  #set information required for (meta)data services
  geom_table <- "area.area_labels"
  if(!is.null(entity$resources$geom_table)) geom_table <- entity$resources$geom_table
  
  #set information required for (meta)data services	
  dimensions <- colnames(entity$data$features)
  dimensions <- c(dimensions[!dimensions %in% c("geographic_identifier", "measurement_value")],"aggregation_method")
  
  #scripts
  source(geoflow::get_config_resource_path(config, "./tunaatlas_actions/create_plsql_data_getter.R"))
  create_plsql_data_getter(action,entity, config) #create pl/sql function in DB to get fact dataset (generic function, one function per fact)
  
  #entity management
  pid <- entity$identifiers[["id"]]
  fact <- entity$resources$fact
  entity$data$run <- FALSE #deactivate local action (harmonization/generation)
  entity$data$sourceType <- "csv"
  entity$data$uploadType <- "dbquery" #set dbquery as upload type for enabling geoserver sql view data services
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
  
}