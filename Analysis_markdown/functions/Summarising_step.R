Summarising_step = function(main_dir, connectionDB, config){
  
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
  
  species_group <-  st_read(con,query = "SELECT taxa_order, code from species.species_asfis") %>% janitor::clean_names() %>%  dplyr::select(species_group = taxa_order, species = code) 
  cl_cwp_gear_level2 <- st_read(con, query = "SELECT * FROM gear_type.isscfg_revision_1")%>% select(Code = code, Gear = label)
  
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
  
  
  # source(file.path(url_analysis_markdown,"functions", "tidying_GTA_data_for_comparison.R"))
  source("https://raw.githubusercontent.com/firms-gta/geoflow-tunaatlas/Developement/Analysis_markdown/functions/tidying_GTA_data_for_comparison.R")
  

  
  
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
  source("https://raw.githubusercontent.com/firms-gta/geoflow-tunaatlas/Developement/Analysis_markdown/Functions_markdown.R", local = child_env_base)
  
  child_env <- list2env(as.list(child_env_base), parent = child_env_base)
  
  entity_dirs <- list.dirs(file.path(main_dir, "entities"), full.names = TRUE, recursive = FALSE)
  i <- 1
  for (entity_dir in entity_dirs) {
    
    entity <- config$metadata$content$entities[[i]]
    action <- entity$data$actions[[1]]
    opts <- action$options
    
    i <- i+1
    entity_name <- basename(entity_dir)
    setwd(entity_dir)
    
      output_file_name <- paste0(entity_name, "_report.html") # name of the output file
      output_dir <- file.path(entity_dir, output_file_name) # where to save the output file
      
      # Set new environment for rendering the Rmd file, so it doesn't affect the current environment
      render_env <- new.env(parent = child_env)
      render_env$parameter_directory <- entity_dir
      render_env$fig.path <- entity_dir
      render_env$entity <- entity
      # Render the R Markdown file
      require(fs)
      copy_project_files <- function(original_repo_path, new_repo_path) {
        # Ensure the new repository directory exists; if not, create it
        if (!dir.exists(new_repo_path)) {
          dir.create(new_repo_path, recursive = TRUE, showWarnings = TRUE)
        }
        
        # Check if original_repo_path is a local directory. If not, you may need to clone/download it first.
        if (!dir.exists(original_repo_path)) {
          stop("The original_repo_path does not exist or is not accessible. Please make sure it's a local path.")
        }
        
        # Define the patterns for the file types we're interested in
        file_patterns <- c("\\.Rmd$")  # add other file types if needed
        
        # Function to copy files based on pattern
        copy_files <- function(pattern) {
          # Find files that match the pattern
          files_to_copy <- list.files(original_repo_path, pattern = pattern, full.names = TRUE, recursive = TRUE)
          
          # Copy each file to the new repository
          for (file in files_to_copy) {
            new_file_path <- file.path(new_repo_path, basename(file))
            file.copy(file, new_file_path)
          }
        }
        
        # Run the copy for each pattern
        for (pattern in file_patterns) {
          copy_files(pattern)
        }
        
        # Message to show it's done
        message("Files have been copied to the new repository.")
      }
      copy_project_files(original_repo_path = here("Analysis_markdown/"), new_repo_path = entity_dir)
      
      rmarkdown::render("tableau_recap_global_action_effort.Rmd",
                        output_file = output_dir,
                        envir = render_env
      )
      rm(render_env)
    }
  }
  