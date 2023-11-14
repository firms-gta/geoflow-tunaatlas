Summarising_step = function(main_dir, connectionDB, config){
  ancient_wd <- getwd()
  setwd(main_dir)
  path = getwd()
  
    required_packages <- c("webshot",
                         "here", "usethis","ows4R","sp", "data.table", "flextable", "readtext", "sf", "dplyr", "stringr", "tibble",
                         "bookdown", "knitr", "purrr", "readxl", "base", "remotes", "utils", "DBI", 
                         "odbc", "rlang", "kableExtra", "readr", "tidyr", "ggplot2", "stats", "RColorBrewer", 
                         "cowplot", "tmap", "RPostgreSQL", "curl", "officer", "gdata", "tidyr", "knitr", "tmap"
  )
  
  for (package in required_packages) {
    if (!requireNamespace(package, quietly = TRUE)) {
      install.packages(package, dependencies = TRUE)
    }
    library(package, character.only = TRUE)
  }
  
  species_group <-  st_read(connectionDB,query = "SELECT taxa_order, code from species.species_asfis") %>% janitor::clean_names() %>%  dplyr::select(species_group = taxa_order, species = code) 
  cl_cwp_gear_level2 <- st_read(connectionDB, query = "SELECT * FROM gear_type.isscfg_revision_1")%>% dplyr::select(Code = code, Gear = label)
  
  shapefile.fix <- st_read(connectionDB,query = "SELECT * from area.cwp_grid") %>% 
    dplyr::rename(GRIDTYPE = gridtype)
  
  
  # continent <- st_read(connectionDB,query = "SELECT * from area.gshhs_world_coastlines") # to be changed
  
  continent <- NULL
  if(is.null(continent)){
    library(ows4R)
    WFS = WFSClient$new(url = "https://www.fao.org/fishery/geoserver/fifao/wfs", serviceVersion = "1.0.0", logger = "INFO")
    continent = WFS$getFeatures("fifao:UN_CONTINENT2")
    
  }
  
  shape_without_geom  <- shapefile.fix %>% as_tibble() %>%dplyr::select(-geom)
  
  # PART 1: Identify entities and their respective tRFMOs
  entity_dirs <- list.dirs(file.path(main_dir, "entities"), full.names = TRUE, recursive = FALSE)
  
  parameters_child <- list(
    parameter_colnames_to_keep = c("fishing_fleet", "gear_type", "geographic_identifier",
                                   "fishing_mode", "species", "measurement_unit", "measurement_value", 
                                   "Gear", "species_group", "GRIDTYPE"),
    shapefile.fix = shapefile.fix, 
    outputonly = FALSE, 
    print_map = TRUE, 
    parameter_time_dimension = c("time_start"), 
    unique_analyse = TRUE, child_header = "",continent = continent,
    child = TRUE, parameter_final = NULL
  )
  
  child_env_base <- new.env(parent = environment())
  list2env(parameters_child, env = child_env_base)
  source("https://raw.githubusercontent.com/firms-gta/geoflow-tunaatlas/Developement/Analysis_markdown/functions/Functions_markdown.R", local = child_env_base)
  
  child_env <- list2env(as.list(child_env_base), parent = child_env_base)
  
  entity_dirs <- list.dirs(file.path(main_dir, "entities"), full.names = TRUE, recursive = FALSE)
  i <- 1
  source("https://raw.githubusercontent.com/firms-gta/geoflow-tunaatlas/Developement/Analysis_markdown/functions/copy_project_files.R", local = TRUE)

  
  for (entity_dir in entity_dirs) {
    
    
    entity <- config$metadata$content$entities[[i]]
    action <- entity$data$actions[[1]]
    opts <- action$options
    
    i <- i+1
    entity_name <- basename(entity_dir)
    setwd(entity_dir)
    
    copy_project_files(original_repo_path = here("Analysis_markdown/"), new_repo_path = entity_dir)
    
    
    sub_list_dir_2 <- list.files("Markdown", recursive = TRUE,pattern = ".rds", full.names = TRUE)
    details = file.info(sub_list_dir_2)
    details = file.info(sub_list_dir_2)
    details = details[with(details, order(as.POSIXct(mtime))), ]
    sub_list_dir_2 = rownames(details)
    
    for(file in sub_list_dir_2){
      
    
    source("https://raw.githubusercontent.com/firms-gta/geoflow-tunaatlas/Developement/Analysis_markdown/functions/tidying_GTA_data_for_comparison.R")
      data <- readRDS(file)
      data <- tidying_GTA_data_for_comparison(dataframe = data,
                                              shape = shape_without_geom, 
                                              species_group_dataframe = species_group,
                                              cl_cwp_gear_level2_dataframe = cl_cwp_gear_level2)
      saveRDS(file = file, object = data)
      
    if("gridtype"%in% colnames(data)){
      data_list <- data_list %>% rename(GRIDTYPE = gridtype)
    }
    }
    
    parameter_filtering <- opts$filtering
    parameter_resolution_filter <- opts$resolution_filter
    
    parameters_child_global <- list(fig.path = paste0("tableau_recap_global_action/figures/"), 
                                    parameter_filtering = parameter_filtering, 
                                    parameter_resolution_filter = parameter_resolution_filter,
                                    parameter_time_dimension = c("time_start"), 
                                    parameter_geographical_dimension = "geographic_identifier", 
                                    parameter_geographical_dimension_groupping = "GRIDTYPE",
                                    entity = entity, config = config)
    
    
    
      output_file_name <- paste0(entity_name, "_report.html") # name of the output file
      output_dir <- file.path(entity_dir, output_file_name) # where to save the output file
      
      # Set new environment for rendering the Rmd file, so it doesn't affect the current environment
      render_env <- list2env(as.list(child_env), parent = child_env)
      
      list2env(parameters_child_global,render_env)
      
      # Render the R Markdown file
     
      rmarkdown::render("tableau_recap_global_action_effort.Rmd",
                        output_file = output_dir,
                        envir = render_env, output_format = "html_document2"
      )
      rm(render_env)
    }
  }
  