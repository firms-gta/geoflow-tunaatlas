enrich_metadata <- function(entity, config, options){
	config$logger.info("Enrich entity with subjects")
	con <- config$software$input$dbi

	dictionary <- config$getDictionary()
	if(!is.null(dictionary)){
		ft <- dictionary$getFeatureTypeById(options$fact)
		this_view <- dbGetQuery(con,paste0("SELECT * FROM ",paste0("fact_tables.",entity$identifiers[["id"]])," LIMIT 1;"))
		column_names <- colnames(this_view)
		column_names <- column_names[!column_names %in% c("id_area", "longitude", "latitude", "geom_wkt", "geographic_identifier", "year", "month", "quarter", "time_start", "time_end", "time_period", "aggregation_method", "value", "gear_group", "species_group")]
		column_names <- column_names[!sapply(column_names, endsWith, "_label")]
		for(colname in column_names){	
			subject <- NULL
			#query distinct values
			values <- dbGetQuery(con, sprintf("SELECT DISTINCT %s FROM %s ORDER BY %s", colname, paste0("fact_tables.",entity$identifiers[["id"]]), colname))
			values <- values[[colname]]
			#check member availability in dictionary
			member <- ft$getMemberById(colname)
			if(!is.null(member)){
				
				reg <- dictionary$getRegisterById(member$registerId)
				if(!is.null(reg)){
					config$logger.info(sprintf("Enrich entity with subject for column '%s' - using register data", colname))
					values <- reg$data[reg$data$code %in% values,]
					subject <- geoflow_subject$new()
					subject$setName(member$name)
					if(!is.na(member$defSource)){
						def_name <- attr(member$defSource, "description")
						def_uri <- attr(member$defSource, "uri")
						if(!is.null(def_name)) subject$setName(def_name)
						if(!is.null(def_uri)) subject$setUri(def_uri)
					}
					for(i in 1:nrow(values)){
						value <- values[i,]
						if(is.na(value$uri)){
							subject$addKeyword(value$code)
							if(!is.na(value$label)) subject$addKeyword(value$label)
							if(colname == "species") if(!is.na(value$definition)) subject$addKeyword(value$definition)
						}else{
							subject$addKeyword(value$code, value$uri)
							if(!is.na(value$label)) subject$addKeyword(value$label, value$uri)
							if(colname == "species") if(!is.na(value$definition)) subject$addKeyword(value$definition, value$uri)
						}
					}
				}
			}else{
				config$logger.info(sprintf("Enrich entity with subject for column '%s' - with codes only", colname))
				subject <- geoflow_subject$new()
				subject$setName(colname)
				for(value in values){ subject$addKeyword(value)}
			}
			if(!is.null(subject)) entity$addSubject(subject)
		}
	
	}
}