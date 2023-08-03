recap_all_markdown <- function(action, entity, config, options){
  if(!file.exists("Markdown")){
    return(NULL)
  } else {
    if(!(require(here))){ 
      install.packages("here") 
      (require(here))} 
    if(!(require(usethis))){ 
      install.packages("usethis") 
      (require(usethis))} 
    if(!(require(flextable))){ 
      install.packages("flextable") 
      (require(flextable))} 
    if(!(require(readtext))){ 
      install.packages("readtext") 
      (require(readtext))} 
    if(!(require(sf))){ 
      install.packages("sf") 
      (require(sf))} 
    if(!(require(dplyr))){ 
      install.packages("dplyr") 
      (require(dplyr))} 
    
    if(!require(stringr)){
      install.packages("stringr")
      require(stringr)
    }
    if(!require(tibble)){
      install.packages("tibble")
      require(tibble)
    }
    if(!require(bookdown)){
      install.packages("bookdown")
      require(bookdown)
    }
    opts <- action$options
    debugging <- if(!is.null(opts$debugging)) opts$debugging else FALSE
    url_analysis_markdown <- "https://raw.githubusercontent.com/firms-gta/geoflow-tunaatlas/master/Analysis_markdown/"
    
    copyrmd <- function(x, url_path = url_analysis_markdown ){
      last_path = function(y){tail(str_split(y,"/")[[1]],n=1)}
      if(!file.exists(paste0(gsub(as.character(here::here()),"",as.character(getwd())), paste0("/", last_path(x)))))
        use_github_file(repo_spec =paste0(url_path,x),
                        save_as = paste0(gsub(as.character(here::here()),"",as.character(getwd())), paste0("/", last_path(x))),
                        ref = NULL,
                        ignore = FALSE,
                        open = FALSE,
                        host = NULL
        ) }      
      c <- c("tableau_recap_global_action_effort.Rmd", 
             "comparison.Rmd", 
             "strata_conversion_factor_gihtub.Rmd", 
             "template.tex",
             "dmk-format.csl", 
             "setup_markdown.Rmd", 
             "strata_in_georef_but_no_nominal.Rmd"
             )
      lapply(c,copyrmd)
    
    con <- config$software$input$dbi
    
    url= "https://www.fao.org/fishery/geoserver/wfs" 
    serviceVersion = "1.0.0" 
    logger = "INFO"
    # SOURCE: OGC ####
    library(ows4R)
    WFS = WFSClient$new(url = "https://www.fao.org/fishery/geoserver/fifao/wfs", serviceVersion = "1.0.0", logger = "INFO")
    sf = WFS$getFeatures("fifao:UN_CONTINENT2")
    st_write(sf, "data/continent.csv", layer_options = "GEOMETRY=AS_WKT", append= FALSE)
    
    
    WFS = WFSClient$new(url = url, serviceVersion = serviceVersion, logger = logger)

    library(data.table)
    library(sf)
    library(sp)
    
    
    get_wfs_data <- function(url= "https://www.fao.org/fishery/geoserver/wfs", 
                             version = "1.0.0", 
                             layer_name, output_dir = "data",logger = "INFO") {
      # create output directory if it doesn't exist
      if (!dir.exists(output_dir)) {
        dir.create(output_dir)
      }
      
      # define file paths
      shapefile_path <- file.path(output_dir, paste0(layer_name, ".shp"))
      
      # check if shapefile already exists
      if (file.exists(shapefile_path)) {
        message(paste0("Shapefile for layer '", layer_name, "' already exists, skipping download."))
        return(sf::st_read(shapefile_path) %>% rename(the_geom = geometry))
      }
      
      cwp_sf <- WFS$getFeatures(layer_name)
      cwp <- as.data.table(cwp_sf)
      if("gml_id.1" %in% colnames(cwp)){
        cwp <- cwp %>% select(-"gml_id.1")
      }
      
      # save data as shapefile
      sf::st_write(cwp, shapefile_path, driver = "ESRI Shapefile")
      
      # return data
      return(cwp)
    }
    
    CWP11_ERASED <- get_wfs_data(layer_name = "cwp:cwp-grid-map-1deg_x_1deg_erased")
    CWP55_ERASED <- get_wfs_data(layer_name = "cwp:cwp-grid-map-5deg_x_5deg_erased")
    CWP1010_ERASED <- get_wfs_data(layer_name = "cwp:cwp-grid-map-10deg_x_10deg_erased")
    CWP2020_ERASED <- get_wfs_data(layer_name = "cwp:cwp-grid-map-20deg_x_20deg_erased")
    CWP3030_ERASED <- get_wfs_data(layer_name = "cwp:cwp-grid-map-30deg_x_30deg_erased")
    
    # CWP_GRIDS <- rbindlist(list(CWP11, CWP55, CWP1010, CWP2020, CWP3030))
    shapefile.fix <- rbindlist(list(CWP11_ERASED, CWP55_ERASED, CWP1010_ERASED, CWP2020_ERASED, CWP3030_ERASED))
    st_write(shapefile.fix, "data/world_sf.csv", layer_options = "GEOMETRY=AS_WKT", append= FALSE)
    
    query <- "SELECT  * from area.areas_conversion_factors_numtoweigth_ird"
    areas_conversion_factors_numtoweigth_ird <- st_make_valid(st_read(con, query = query))%>% filter(!st_is_empty(.))
    st_write(areas_conversion_factors_numtoweigth_ird, "data/areas_conversion_factors_numtoweigth_ird.csv", layer_options = "GEOMETRY=AS_WKT", append= FALSE)
    
    dir.create(paste0("tableau_recap_global_action/figures"), recursive = TRUE, showWarnings = FALSE)
    
    
    parameters_child_global <- list(action = action,
                                    entity = entity, config = config, debugging = FALSE, 
                                    fig.path = paste0("tableau_recap_global_action/figures/"))
    child_env_global = new.env()
    list2env(parameters_child_global, env = child_env_global)
    
    rmarkdown::render("tableau_recap_global_action_effort.Rmd"  , 
                      envir =  child_env_global, 
                      output_file = paste0("output.html"),
                      output_format = "html_document", output_dir = "tableau_recap_global_action")


    
    
    
    source("https://raw.githubusercontent.com/firms-gta/geoflow-tunaatlas/master/Analysis_Markdown/functions/strata_in_georef_but_not_in_nominal_report_launching.R")
    

    gc()
  }
}
