recap_all_markdown <- function(action, entity, config, options){
  # if(!file.exists("Markdown")){
  #   return(NULL)
  # } else {
    # required_packages <- c("webshot",
    #   "here", "usethis","ows4R","sp", "data.table", "flextable", "readtext", "sf", "dplyr", "stringr", "tibble",
    #   "bookdown", "knitr", "purrr", "readxl", "base", "remotes", "utils", "DBI", 
    #   "odbc", "rlang", "kableExtra", "readr", "tidyr", "ggplot2", "stats", "RColorBrewer", 
    #   "cowplot", "tmap", "RPostgreSQL", "curl", "officer", "gdata", "tidyr", "knitr", "tmap"
    # )
    # 
    # for (package in required_packages) {
    #   if (!requireNamespace(package, quietly = TRUE)) {
    #     install.packages(package, dependencies = TRUE)
    #   }
    #   library(package, character.only = TRUE)
    # }
    
    opts <- action$options
    debugging <- if(!is.null(opts$debugging)) opts$debugging else FALSE

    last_path = function(y){tail(str_split(y,"/")[[1]],n=1)}
    
    url_analysis_markdown <- "https://raw.githubusercontent.com/firms-gta/geoflow-tunaatlas/master/Analysis_markdown/"
    target_dir <- getwd()  # Current working directory
    
    copyrmd <- function(x, url_path = url_analysis_markdown, target_dirinside = target_dir) {
      target_file <- file.path(target_dirinside, last_path(x))
      
      if (!file.exists(target_file)) {
        download_url <- paste0(url_path, x)
        curl_download(download_url, target_file)
      }
    }
    
    # Existing files
    c_existing <- c(
      "tableau_recap_global_action_effort.Rmd", 
      "comparison.Rmd", 
      "strata_conversion_factor_gihtub.Rmd", 
      "template.tex",
      "dmk-format.csl", 
      "setup_markdown.Rmd", 
      "strata_in_georef_but_no_nominal.Rmd"
    )
    
    # Child Rmd files you want to include
    c_child <- c(
      "Setup_markdown.Rmd",
      "Parameters_settings.Rmd",
      "file_formatting.Rmd",
      "Explenation.Rmd",
      "Filtering_data.Rmd",
      "Groupping_differences.Rmd",
      "Strataloss.Rmd",
      "Summarydifferences.Rmd",
      "Compnumberstratas.Rmd",
      "Timecoverage.Rmd",
      "Spatialcoverage.Rmd",
      "Otherdimensions.Rmd",
      "Timediff.Rmd",
      "Geographicdiff.Rmd",
      "Differences_for_each_dimensions.Rmd",
      "Recap_without_mapping.Rmd",
      "Annexe.Rmd", "Functions_markdown.Rmd"
    )
    
    # Combine existing and child Rmd files
    c_all <- c(c_existing, c_child)
    
    # Download all files
    lapply(c_all, copyrmd)
    
    
    
    con <- config$software$input$dbi
    
    # SOURCE: OGC ####
    WFS = WFSClient$new(url = "https://www.fao.org/fishery/geoserver/fifao/wfs", serviceVersion = "1.0.0", logger = "INFO")
    continent = WFS$getFeatures("fifao:UN_CONTINENT2")
    rm(WFS)
    gc()
    # get_wfs_data <- function(url= "https://www.fao.org/fishery/geoserver/wfs", 
    #                          version = "1.0.0", 
    #                          layer_name, output_dir = "data",logger = "INFO") {
    #   # create output directory if it doesn't exist
    #   if (!dir.exists(output_dir)) {
    #     dir.create(output_dir)
    #   }
    #   
    #   # define file paths
    #   shapefile_path <- file.path(output_dir, paste0(layer_name, ".shp"))
    #   
    #   # check if shapefile already exists
    #   if (file.exists(shapefile_path)) {
    #     message(paste0("Shapefile for layer '", layer_name, "' already exists, skipping download."))
    #     return(sf::st_read(shapefile_path) %>% rename(the_geom = geometry))
    #   }
    #   
    #   cwp_sf <- WFS$getFeatures(layer_name)
    #   cwp <- as.data.table(cwp_sf)
    #   if("gml_id.1" %in% colnames(cwp)){
    #     cwp <- cwp %>% select(-"gml_id.1")
    #   }
    #   
    #   # save data as shapefile
    #   sf::st_write(cwp, shapefile_path, driver = "ESRI Shapefile")
    #   
    #   # return data
    #   return(cwp)
    # }
    # 
    # CWP11 <- get_wfs_data(layer_name = "cwp:cwp-grid-map-1deg_x_1deg")
    # sf::st_crs(CWP11) <- NA
    # 
    # CWP55 <- get_wfs_data(layer_name = "cwp:cwp-grid-map-5deg_x_5deg")
    # CWP1010 <- get_wfs_data(layer_name = "cwp:cwp-grid-map-10deg_x_10deg")
    # CWP2020 <- get_wfs_data(layer_name = "cwp:cwp-grid-map-20deg_x_20deg")
    # CWP3030 <- get_wfs_data(layer_name = "cwp:cwp-grid-map-30deg_x_30deg")    
    # # CWP_GRIDS <- rbindlist(list(CWP11, CWP55, CWP1010, CWP2020, CWP3030))
    # 
    # shapefile.fix <- rbindlist(list(CWP11, CWP55, CWP1010, CWP2020, CWP3030))
    shapefile.fix <- st_read(con,query = "SELECT * from area.cwp_grid")%>% 
      dplyr::rename(GRIDTYPE = gridtype)
    
    shape_without_geom  <- shapefile.fix %>% as_tibble() %>%dplyr::select(-geom) 
    
    # st_write(shapefile.fix, "data/world_sf.csv", layer_options = "GEOMETRY=AS_WKT", append= FALSE)
    
    # query <- "SELECT  * from area.areas_conversion_factors_numtoweigth_ird"
    # areas_conversion_factors_numtoweigth_ird <- st_make_valid(st_read(con, query = query))%>% filter(!st_is_empty(.))
    # st_write(areas_conversion_factors_numtoweigth_ird, "data/areas_conversion_factors_numtoweigth_ird.csv", layer_options = "GEOMETRY=AS_WKT", append= FALSE)
    
    dir.create(paste0("tableau_recap_global_action/figures"), recursive = TRUE, showWarnings = FALSE)
    
    species_group <-  st_read(con,query = "SELECT taxa_order, code from species.species_asfis") %>% janitor::clean_names() %>%  dplyr::select(species_group = taxa_order, species = code) 
    # cl_cwp_gear_level2 <- read_csv(file.path("https://raw.githubusercontent.com/fdiwg/fdi-codelists/main/global/firms/gta/cl_isscfg_pilot_gear.csv")) %>% select(Code = code, Gear = label)
    cl_cwp_gear_level2 <- st_read(con, query = "SELECT * FROM gear_type.isscfg_revision_1")%>% select(Code = code, Gear = label)
    
    # source(file.path(url_analysis_markdown,"functions", "tidying_GTA_data_for_comparison.R"))
    source("~/Documents/geoflow-tunaatlas/Analysis_markdown/functions/tidying_GTA_data_for_comparison.R")
    
    sub_list_dir_2 <- list.files("Markdown", recursive = TRUE,pattern = ".rds", full.names = TRUE)
    details = file.info(sub_list_dir_2)
    details = file.info(sub_list_dir_2)
    details = details[with(details, order(as.POSIXct(mtime))), ]
    sub_list_dir_2 = rownames(details)
    
    for(file in sub_list_dir_2){
      
      print(file)
      data <- readRDS(file)
      data <- tidying_GTA_data_for_comparison(dataframe = data,
      shape = shape_without_geom, 
      species_group_dataframe = species_group,
      cl_cwp_gear_level2_dataframe = cl_cwp_gear_level2)
      print("tidied")
      saveRDS(file = file, object = data)
      print(paste0(file, " is Saved"))
      
    }
    gc()
    
    parameter_filtering <- opts$filtering
    parameter_resolution_filter <- opts$resolution_filter
    
    parameters_child_global <- list(action = action,
                                    entity = entity, config = config, debugging = FALSE, 
                                    fig.path = paste0("tableau_recap_global_action/figures/"), 
                                    outputonly = FALSE, shape_without_geom = shape_without_geom,
                                    continent = continent, shapefile.fix = shapefile.fix, 
                                    parameter_filtering = parameter_filtering, 
                                    parameter_resolution_filter = parameter_resolution_filter)
    
    child_env_global = new.env()
    list2env(parameters_child_global, env = child_env_global)
    
    
    # source(knitr::purl(file.path(url_analysis_markdown, "Functions_markdown.Rmd")), child_env_global)
    # source(file.path(url_analysis_markdown, "Functions_markdown.R"), local = child_env_global)
    
    rmarkdown::render("tableau_recap_global_action_effort.Rmd"  , 
                      envir =  child_env_global, 
                      output_dir = "tableau_recap_global_action")
    
    rmarkdown::render("tableau_recap_global_action_effort.Rmd", 
                      envir =  child_env_global, 
                      output_file = "Recap.pdf",
                      output_dir = "tableau_recap_global_action")
    
    
    source("https://raw.githubusercontent.com/firms-gta/geoflow-tunaatlas/master/Analysis_markdown/functions/strata_in_georef_but_not_in_nominal_report_launching.R")
    

    gc()
  }
}
