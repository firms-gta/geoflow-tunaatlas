#register_source_authority
register_source_authority <- function(config){
	con <- config$software$input$dbi
	fetched <- dbGetQuery(con, "SELECT * FROM source.source_labels")
	out <- data.frame(
		code = fetched$codesource_source,
		uri = NA,
		label = fetched$source_label,
		definition = NA
	)
	return(out)
}

#register_flag
register_flag <- function(config){
	con <- config$software$input$dbi
	fetched <- dbGetQuery(con, "SELECT * FROM flag.flag_labels WHERE tablesource_flag = 'flag_fao_cwp'")
	out <- data.frame(
		code = fetched$codesource_flag,
		uri = NA,
		label = fetched$source_label,
		definition = NA
	)
	return(out)
}

#register_species
register_species <- function(config){	
	con <- config$software$input$dbi
	fetched <- dbGetQuery(con, "SELECT * FROM species.species_asfis")
	out <- data.frame(
		code = fetched$code,
		uri = NA,
		label = fetched$label,
		definition = fetched$scientific_name
	)
	return(out)	
}

#register_schooltype
register_schooltype <- function(config){
	con <- config$software$input$dbi
	fetched <- dbGetQuery(con, "SELECT * FROM schooltype.schooltype_labels where tablesource_schooltype = 'schooltype_rfmos'")
	out <- data.frame(
		code = fetched$codesource_schooltype,
		uri = NA,
		label = fetched$source_label,
		definition = NA
	)
	return(out)		
}

#register_gear
register_gear <- function(config){
	con <- config$software$input$dbi
	fetched <- dbGetQuery(con, "SELECT * FROM gear.gear_labels where tablesource_gear = 'isscfg_revision_1'")
	out <- data.frame(
		code = fetched$codesource_gear,
		uri = NA,
		label = fetched$source_label,
		definition = NA
	)
	return(out)			
}

#register_catch_type
register_catchtype <- function(config){
	con <- config$software$input$dbi
	fetched <- dbGetQuery(con, "SELECT * FROM catchtype.catchtype_labels where tablesource_catchtype = 'catchtype_tunaatlas'")
	out <- data.frame(
		code = fetched$codesource_catchtype,
		uri = NA,
		label = fetched$source_label,
		definition = NA
	)
	return(out)		
}

#register_unit_catch
register_unit_catch <- function(config){
	con <- config$software$input$dbi
	fetched <- dbGetQuery(con, "SELECT * FROM unit.unit_labels where tablesource_unit = 'catchunit_rfmos'")
	out <- data.frame(
		code = fetched$codesource_unit,
		uri = NA,
		label = fetched$source_label,
		definition = NA
	)
	return(out)		
}

#register_unit_effort
register_unit_effort <- function(config){
	con <- config$software$input$dbi
	fetched <- dbGetQuery(con, "SELECT * FROM unit.unit_labels where tablesource_unit = 'effortunit_rfmos'")
	out <- data.frame(
		code = fetched$codesource_unit,
		uri = NA,
		label = fetched$source_label,
		definition = NA
	)
	return(out)		
}

