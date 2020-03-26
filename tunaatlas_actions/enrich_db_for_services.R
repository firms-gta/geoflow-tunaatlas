enrich_db_for_services <- function(entity, config, options){

	con <- config$software$output$dbi
	schema <- "fact_tables"

	#set information required for (meta)data services
	df_codelists <- read.csv(entity$resources$codelists)
	dimensions <- c(df_codelists$dimension [df_codelists$dimension != "area"], "time_start", "time_end", "month", "quarter", "year", "aggregation_method")
	
	#scripts
	url_scripts_create_own_tuna_atlas <- "https://raw.githubusercontent.com/eblondel/geoflow-tunaatlas/master/tunaatlas_actions"
	source(file.path(url_scripts_create_own_tuna_atlas, "create_plsql_data_getter.R"))
	create_plsql_data_getter(entity, config, options) #create pl/sql function in DB to get fact dataset (generic function, one function per fact)
	
	
	#entity management
	pid <- entity$identifiers[["id"]]
	entity$data$run <- FALSE #deactivate local action (harmonization/generation)
	entity$data$uploadType <- "dbquery" #set dbquery as upload type for enabling data services
	entity$data$layername <- pid
	#geoserver sql view properties
	entity$data$setSql(sprintf("select * from get_fact_dataset_%s('%s', '%s', %s)", options$fact, schema, pid, paste0("'%", dimensions,"%'", collapse=",")))
	entity$data$setGeometryField("the_geom")
	entity$data$setGeometryType("Polygon")
	for(dimension in dimensions){
		regexpValue <- switch(dimension,
			"time_start" = "^(19|20)\\d\\d([- /.])(0[1-9]|1[012])\\2(0[1-9]|[12][0-9]|3[01])$",
			"time_end" = "^(19|20)\\d\\d([- /.])(0[1-9]|1[012])\\2(0[1-9]|[12][0-9]|3[01])$",
			"aggregation_method" = "^[none|avg_by_year|avg_by_quarter|avg_by_month|sum]+$",
			"^[\\w +]+$"
		)
		defaultValue <-switch(dimension,
			"time_start" = as.character(entity$temporal_extent$end),
			"time_end" = as.character(entity$temporal_extent$end),
			"aggregation_method" = "none",
			"month" = paste(1:12, collapse="+"),
			"quarter" = paste(1:4, collapse="+"),
			"year" = paste(as.integer(format(entity$temporal_extent$start, "%Y")):as.integer(format(entity$temporal_extent$end, "%Y")), collapse="+"),
			paste(dbGetQuery(con, sprintf("select distinct %s from %s.%s",dimension, schema, pid, dimension))[,1], collapse="+")
		)
		print("--------------")
		print(dimension)
		print(regexpValue)
		print(defaultValue)
		entity$data$setParameter(dimension, dimension, regexpValue, defaultValue)
	}

}