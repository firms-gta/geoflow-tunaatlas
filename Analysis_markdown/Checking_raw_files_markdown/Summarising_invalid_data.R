Summarising_invalid_data = function(main_dir, connectionDB){
  ancient_wd <- getwd()
  setwd(main_dir)
  dir.create("Recap_on_pre_harmo")
  path = file.path(getwd())#, "Recap_on_pre_harmo")
  
  species_group <-  st_read(connectionDB,query = "SELECT taxa_order, code from species.species_asfis") %>% janitor::clean_names() %>%  dplyr::select(species_group = taxa_order, species = code) 
  cl_cwp_gear_level2 <- st_read(connectionDB, query = "SELECT * FROM gear_type.isscfg_revision_1")%>% select(Code = code, Gear = label)
  
  shapefile.fix <- st_read(connectionDB,query = "SELECT * from area.cwp_grid") %>% 
    dplyr::rename(GRIDTYPE = gridtype)
  
  continent <- tryCatch({
    st_read(connectionDB, query = "SELECT * from public.continent")
  }, error = function(e) {
    cat("An error occurred:", e$message, "\n")
    NULL  
  })
  
  if(is.null(continent)){
    url= "https://www.fao.org/fishery/geoserver/wfs" 
    serviceVersion = "1.0.0" 
    logger = "INFO"
    # SOURCE: OGC ####
    WFS = WFSClient$new(url = "https://www.fao.org/fishery/geoserver/fifao/wfs", serviceVersion = "1.0.0", logger = "INFO")
    continent = WFS$getFeatures("fifao:UN_CONTINENT2")
    
  }
  shape_without_geom  <- shapefile.fix %>% as_tibble() %>%dplyr::select(-geom)
  
  source("https://raw.githubusercontent.com/firms-gta/geoflow-tunaatlas/master/Analysis_markdown/functions/tidying_GTA_data_for_comparison.R")
  
  
  
  # PART 1: Identify entities and their respective tRFMOs
  entity_dirs <- list.dirs("entities", full.names = TRUE, recursive = FALSE)
  
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
  saveRDS(grouped_results, file.path(path, "grouped_results_invalid_data.rds"))
  
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
  
  saveRDS(all_not_mapped_data, file.path(path, "all_not_mapped_data.rds"))
  
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
  
  saveRDS(all_recap_mapping_data, file.path(path, "all_recap_mapping.rds"))
  
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
      if("Gear.x" %in% colnames(data_list)){
        # Removing columns that end in '.y' and renaming '.x' columns
        data_list <- data_list %>%
          select(-matches("\\.y$")) %>%  # This removes columns ending in '.y'
          rename_with(~ gsub("\\.x$", "", .x), matches("\\.x$"))  # This renames columns, removing '.x'
        
        
      } else if ("GRIDTYPE" %notin% colnames(data_list)){
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
  source("https://raw.githubusercontent.com/firms-gta/geoflow-tunaatlas/master/Analysis_markdown/functions/Functions_markdown.R", local = child_env_base)
  
  child_env <- list2env(as.list(child_env_base), parent = child_env_base)
  source("https://raw.githubusercontent.com/firms-gta/geoflow-tunaatlas/master/Analysis_markdown/functions/copy_project_files.R", local = TRUE)
  # 
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
                        output_dir = entity_dir, output_file =output_file_name ,
                        envir = render_env
      )
      rm(render_env, envir = environment())
    }
  }
  
  rmarkdown::render(file.path(path,"Recap_on_pre_harmo.Rmd"),
                    output_dir = path,
                    envir = environment()
  )
  folder_datasets_id <- "1s8sCv6j_3-zHR1MsOqhrqZrGKhGY3W_Y"
  
  all_files <- list.files(getwd(), pattern = "\\.html$", full.names = TRUE, recursive = TRUE)
  
  sapply(all_files, function(file) {
    destination_file <- file.path(getwd(),"Recap_on_pre_harmo", basename(file))
    file.copy(file, destination_file)
    path_to_dataset_new <- file.path(file)
    drive_upload(path_to_dataset_new, as_id(folder_datasets_id), overwrite = TRUE)
    
  })
  # 
  path_Recap <- file.path(getwd(),"Recap_on_pre_harmo.html")
  drive_upload(path_Recap, as_id(folder_datasets_id), overwrite = TRUE)
  
  
  read_last_csv <- function(path) {
    csv_files <- list.files(path, pattern = "\\.csv$", full.names = TRUE)
    if (length(csv_files) == 0) return(NULL)
    last_csv <- csv_files[order(file.info(csv_files)$mtime, decreasing = TRUE)[1]]
    read_csv(last_csv)
  }
  
  # Liste des tRFMOs, n'inclut pas iattc car pas de binding
  tRFMOs <- c("ccsbt", "wcpfc")
  list_csv <- c()
  
  combined_data_list <- lapply(tRFMOs, function(trfmo) {
    trfmo_paths <- list.dirs(file.path(path, "entities"), recursive = FALSE)
    trfmo_paths <- trfmo_paths[grep(trfmo, trfmo_paths)]
    
    trfmo_data <- lapply(file.path(trfmo_paths, "data"), read_last_csv)
    trfmo_data <- do.call(rbind, trfmo_data)
    
    # Enregistrement du fichier combiné
    name <- paste0(path, "/", trfmo, "_combined_data.csv")
    write_csv(trfmo_data, name)
    
    return(name)
  })
  lapply(combined_data_list, drive_upload, as_id(folder_datasets_id), overwrite = TRUE)
  
  
  setwd(ancient_wd)
}
