Summarising_invalid_data = function(main_dir, connectionDB){
  path = getwd()
  
  species_group <-  st_read(con,query = "SELECT taxa_order, code from species.species_asfis") %>% janitor::clean_names() %>%  dplyr::select(species_group = taxa_order, species = code) 
  cl_cwp_gear_level2 <- st_read(con, query = "SELECT * FROM gear_type.isscfg_revision_1")%>% select(Code = code, Gear = label)
  
  shapefile.fix <- st_read(connectionDB,query = "SELECT * from area.cwp_grid") %>% 
    dplyr::rename(GRIDTYPE = gridtype)
  
  shape_without_geom  <- shapefile.fix %>% as_tibble() %>%dplyr::select(-geom)
  
  source("https://raw.githubusercontent.com/firms-gta/geoflow-tunaatlas/Developement/Analysis_markdown/functions/tidying_GTA_data_for_comparison.R")
  
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
  
  
  # PART 1: Identify entities and their respective tRFMOs
  entity_dirs <- list.dirs(file.path(main_dir, "entities"), full.names = TRUE, recursive = FALSE)
  
  # Function to determine tRFMO from entity name
  determine_tRFMO <- function(entity_name) {
    trfmo_labels <- c("iattc", "wcpfc", "ccsbt", "iotc", "iccat") # the labels to look for
    matched_tRFMO <- character(0) # empty character vector to store matches
    
    for (trfmo in trfmo_labels) {
      if (grepl(trfmo, entity_name, ignore.case = TRUE)) {
        matched_tRFMO <- toupper(trfmo) # If a match is found, set it to the corresponding tRFMO in uppercase
        break
      }
    }
    
    if (length(matched_tRFMO) == 0) {
      matched_tRFMO <- "Unknown" # If no matches are found, set tRFMO as "Unknown"
    }
    
    return(matched_tRFMO)
  }
  
  entities_trfmo <- lapply(entity_dirs, function(dir) {
    entity_name <- basename(dir)
    trfmo <- determine_tRFMO(entity_name) # Call the function to determine the tRFMO
    
    return(data.frame(Entity = entity_name, tRFMO = trfmo))
  })
  
  # Combine all entities into a single data frame
  entities_df <- do.call(rbind, entities_trfmo)
  
  
  
  # PART 2: Checking for specific .rds files
  
  target_files <- c("negative_values.rds", "not_conform_conversion_factors.rds", "removed_irregular_areas.rds", 
                    "areas_in_land.rds", "outside_juridiction.rds")
  
  
  
  entity_file_existence <- list()
  
  for (entity_dir in entity_dirs) {
    entity_name <- basename(entity_dir)
    current_files <- list.files(paste0(entity_dir, "/data/"), full.names = TRUE)
    file_existence <- setNames(lapply(target_files, function(x) x %in% basename(current_files)), target_files)
    entity_file_existence[[entity_name]] <- file_existence
    
  }
  
  results_df <- do.call(rbind, lapply(entity_file_existence, function(x) {
    data <- as.data.frame(t(unlist(x)))
    rownames(data) <- NULL  
    return(data)
  }))
  
  # Add entity names as a column in the data frame
  results_df$Entity <- rownames(results_df)
  
  # Ensure that 'Entity' columns are factors and have the same levels in both data frames
  entities_df$Entity <- as.factor(entities_df$Entity)
  results_df$Entity <- as.factor(results_df$Entity)
  
  combined_results <- merge(entities_df, results_df, by = "Entity")
  
  # Group data by tRFMO
  grouped_results <- combined_results %>%
    # group_by(tRFMO) %>%
    # summarise(across(where(is.logical), sum)) %>% 
    distinct()
  saveRDS(grouped_results, file.path(main_dir, "grouped_results_invalid_data.rds"))
  
  not_mapped_data_list <- list()
  
  for (entity_dir in entity_dirs) {
    entity_name <- basename(entity_dir)
    
    # Define the path to the 'not_mapped_total.rds' file for the current entity
    not_mapped_file_path <- file.path(entity_dir, "data", "not_mapped_total.rds")
    
    # Check if the file exists
    if (file.exists(not_mapped_file_path)) {
      # Read the .rds file and store the data with the entity's name
      not_mapped_data <- readRDS(not_mapped_file_path)
      not_mapped_data$Entity <- entity_name  # add an 'Entity' column to keep track of the entity
      not_mapped_data_list[[entity_name]] <- not_mapped_data
    }
  }
  
  # Bind all collected data.frames into one
  all_not_mapped_data <- bind_rows(not_mapped_data_list) %>% distinct()
  
  saveRDS(all_not_mapped_data, file.path(main_dir, "all_not_mapped_data.rds"))
  
  recap_mapping_data_list <- list()
  
  for (entity_dir in entity_dirs) {
    entity_name <- basename(entity_dir)
    
    # Define the path to the 'recap_mapping.rds' file for the current entity
    recap_mapping_file_path <- file.path(entity_dir, "data", "recap_mapping.rds")
    
    # Check if the file exists
    if (file.exists(recap_mapping_file_path)) {
      # Read the .rds file and store the data with the entity's name
      recap_mapping_data <- readRDS(recap_mapping_file_path)
      recap_mapping_data$Entity <- entity_name  # add an 'Entity' column to keep track of the entity
      recap_mapping_data_list[[entity_name]] <- recap_mapping_data
    }
  }
  
  # Bind all collected data.frames into one
  all_recap_mapping_data <- bind_rows(recap_mapping_data_list) %>% distinct()
  
  saveRDS(all_recap_mapping_data, file.path(main_dir, "all_recap_mapping.rds"))
  
  # PART 3: Generate a summary CSV for each entity
  `%notin%` <- Negate(`%in%`)
  for (entity_dir in entity_dirs) {
    entity_name <- basename(entity_dir)
    entity_data <- combined_results[combined_results$Entity == entity_name, ]
    
    
    problematic_files <- target_files[as.logical(entity_data[3:ncol(entity_data)])]
    # If 'problematic_files' contains NA values, this line will remove them.
    problematic_files <- na.omit(problematic_files)
    problematic_data <- lapply(problematic_files, function(file) {
      data_path <- file.path(entity_dir, "data", file)
      data_list <- readRDS(data_path)
      if("quadrant.y" %in% colnames(data_list)){
        # Removing columns that end in '.y' and renaming '.x' columns
        data_list <- data_list %>%
          select(-matches("\\.y$")) %>%  # This removes columns ending in '.y'
          rename_with(~ gsub("\\.x$", "", .x), matches("\\.x$"))  # This renames columns, removing '.x'
        
        
      } else if ("quadrant" %notin% colnames(data_list)){
        data_list <- tidying_GTA_data_for_comparison(dataframe = data_list,
                                                     shape = shape_without_geom, 
                                                     species_group_dataframe = species_group,
                                                     cl_cwp_gear_level2_dataframe = cl_cwp_gear_level2)
      }
      if("gridtype"%in% colnames(data_list)){
        data_list <- data_list %>% rename(GRIDTYPE = gridtype)
      }
      saveRDS(data_list, file = data_path)
      
      
      return(data_list)
    })
    
    if (length(problematic_files) > 0) {
      # Combine all problematic data into one data frame with an additional column specifying the input file
      combined_problematic_data <- do.call(rbind, lapply(1:length(problematic_data), function(i) {
        data_frame <- as.data.frame(problematic_data[[i]])
        data_frame$InputFile <- problematic_files[i]
        return(data_frame)
      }))
      
      # Write the combined data frame to a CSV file
      write_csv(combined_problematic_data, file.path(entity_dir, paste0(entity_name, "_summary_invalid_data.csv")), 
                progress = show_progress())
    }
  }
  
  
  
  
  # Directory for the R Markdown template
  base::options(knitr.duplicate.label = "allow")
  
  
  # Parameters for child Rmd
  # load(here(".RData"))
  url= "https://www.fao.org/fishery/geoserver/wfs" 
  serviceVersion = "1.0.0" 
  logger = "INFO"
  # SOURCE: OGC ####
  WFS = WFSClient$new(url = "https://www.fao.org/fishery/geoserver/fifao/wfs", serviceVersion = "1.0.0", logger = "INFO")
  continent = WFS$getFeatures("fifao:UN_CONTINENT2")
  
  
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
  
  copy_project_files(original_repo_path = here("Analysis_markdown/Checking_raw_files_markdown"), new_repo_path = path)
  copy_project_files(original_repo_path = here("Analysis_markdown/"), new_repo_path = path)
  
  
  
  for (entity_dir in entity_dirs) {
    entity_name <- basename(entity_dir)
    entity_data <- combined_results[combined_results$Entity == entity_name, ]
    
    # Identify problematic files
    problematic_files <- target_files[as.logical(entity_data[3:ncol(entity_data)])]
    problematic_files <- na.omit(problematic_files)
    problematic_files <- setdiff(problematic_files, "not_mapped_total.rds")
    
    
    if (length(problematic_files) > 0) {
      #tidy data
      # Now, render the R Markdown for this specific entity
      output_file_name <- paste0(entity_name, "_report.html") # name of the output file
      output_dir <- file.path(entity_dir, output_file_name) # where to save the output file
      
      # Set new environment for rendering the Rmd file, so it doesn't affect the current environment
      render_env <- new.env(parent = child_env)
      render_env$parameter_directory <- entity_dir
      render_env$dataset <- entity_name
      render_env$fig.path <- entity_dir
      # Render the R Markdown file
      require(fs)
      
      # Use the function (make sure to use the correct local paths)
      rmarkdown::render("Report_on_raw_data.Rmd",
                        output_file = output_dir,
                        envir = render_env
      )
      rm(render_env)
    }
  }
  
  
  rmarkdown::render("Recap_on_pre_harmo.Rmd",
                    output_dir = path,
                    envir = environment()
  )
  # 
  # folder_datasets_id <- "16fVLytARK13uHCKffho3kYJgm0KopbKL"
  # drive_upload(file.path(getwd(),"Recap_on_pre_harmo.pdf"), "Recap_on_pre_harmo.pdf", overwrite = TRUE)
  
  
  
}
