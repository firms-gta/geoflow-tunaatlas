#create_grid_views
create_grid_views <- function(config, software, software_config){

	#load continent layer in public schema
	#TODO it as reference layer in codelists workflow
	library(ows4R)
	WFS = WFSClient$new(url = "https://www.fao.org/fishery/geoserver/fifao/wfs", serviceVersion = "1.0.0", logger = "INFO")
	sf = WFS$getFeatures("fifao:UN_CONTINENT2")
	sf::st_write(obj = sf, dsn = dbi, layer = "area.continent")

	#grid views sql
	sql <- paste0(readLines("https://raw.githubusercontent.com/eblondel/geoflow-tunaatlas/master/tunaatlas_sql/create_view_area_labels_viewer.sql", collapse = "\n")
	return(sql)

}