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

#register_fishing_fleet
register_fishing_fleet <- function(config){
	con <- config$software$input$dbi
	fetched <- dbGetQuery(con, "SELECT * FROM fishing_fleet.fishing_fleet_labels WHERE tablesource_fishing_fleet = 'fishingfleet_firms'")
	out <- data.frame(
		code = fetched$codesource_fishing_fleet,
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

#register_fishing_mode
register_fishing_mode <- function(config){
	con <- config$software$input$dbi
	fetched <- dbGetQuery(con, "SELECT * FROM fishing_mode.fishing_mode_labels where tablesource_fishing_mode = 'schooltype_rfmos'")
	out <- data.frame(
		code = fetched$codesource_fishing_mode,
		uri = NA,
		label = fetched$source_label,
		definition = NA
	)
	return(out)		
}

#register_gear_type
register_gear_type <- function(config){
	con <- config$software$input$dbi
	fetched <- dbGetQuery(con, "SELECT * FROM gear_type.gear_type_labels where tablesource_gear_type = 'isscfg_revision_1'")
	out <- data.frame(
		code = fetched$codesource_gear_type,
		uri = NA,
		label = fetched$source_label,
		definition = NA
	)
	return(out)			
}

#register_catch_measurement_type
register_catch_measurement_type <- function(config){
	con <- config$software$input$dbi
	fetched <- dbGetQuery(con, "SELECT * FROM measurement_type.measurement_type_labels where tablesource_measurement_type = 'catchtype_tunaatlas'")
	out <- data.frame(
		code = fetched$codesource_measurement_type,
		uri = NA,
		label = fetched$source_label,
		definition = NA
	)
	return(out)		
}

#register_catch_measurement_unit
register_catch_measurement_unit <- function(config){
	con <- config$software$input$dbi
	fetched <- dbGetQuery(con, "SELECT * FROM measurement_unit.measurement_unit_labels where tablesource_measurement_unit = 'catchunit_rfmos'")
	out <- data.frame(
		code = c(fetched$codesource_measurement_unit),
		uri = NA,
		label = c(fetched$source_label),
		definition = NA
	)
	return(out)		
}

#register_effort_measurement_unit
register_effort_measurement_unit <- function(config){
	con <- config$software$input$dbi
	fetched <- dbGetQuery(con, "SELECT * FROM measurement_unit.measurement_unit_labels where tablesource_measurement_unit = 'effortunit_rfmos'")
	out <- data.frame(
		code = fetched$codesource_measurement_unit,
		uri = NA,
		label = fetched$source_label,
		definition = NA
	)
	return(out)		
}

#register_month
register_month <- function(config){
	out <- data.frame(
		code = as.character(1:12),
		uri = NA,
		label = c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"),
		definition = NA
	)
	return(out)		
}

#register_aggregation_method
register_aggregation_method <- function(config){
	out <- data.frame(
		code = c("none","sum", "avg_by_month", "avg_by_quarter", "avg_by_year"),
		uri = NA,
		label = c("None", "Sum", "Monthly average", "Quarterly average", "Yearly average"),
		definition = NA
	)
}