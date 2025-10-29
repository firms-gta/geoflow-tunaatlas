download_continent_shape <-function(action, entity, config){
    
library(ows4R)
WFS = WFSClient$new(url = "https://www.fao.org/fishery/geoserver/fifao/wfs", serviceVersion = "1.0.0", logger = "INFO")
sf = WFS$getFeatures("fifao:UN_CONTINENT2")
sf::st_crs(sf) <- 4326
dbi <- config$software$input$dbi
sf::st_write(obj = sf, dsn = dbi, layer = "continent")

}
