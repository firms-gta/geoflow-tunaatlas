strata_with_catches_without_effort = function(main.dir, connectionDB,uploadgoogledrive = TRUE ){
  ancient_wd <- getwd()
  setwd(main.dir)
  path = getwd()
  global_catch_firms_level0_harmonized <- read_csv(file.path(main.dir, "/entities/global_catch_5deg_1m_firms_level1/data/global_catch_5deg_1m_firms_level1_harmonized.csv")) %>% 
    dplyr::mutate(measurement_unit = "t") 
  global_georeferenced_effort_firms_harmonized <- read_csv(file.path(main.dir,"entities/global_georeferenced_effort_firms/data/global_georeferenced_effort_firms_harmonized.csv"))%>% dplyr::mutate(geographic_identifier = as.character(geographic_identifier))
  
  global_catch_firms_level0_harmonized <- global_catch_firms_level0_harmonized %>% dplyr::mutate(geographic_identifier = as.character(geographic_identifier))
  
  effort_year <- unique(global_georeferenced_effort_firms_harmonized$time_start)
  global_catch_firms_level0_harmonized <- global_catch_firms_level0_harmonized %>% filter(time_start %in% effort_year) %>% 
    dplyr::group_by(across(setdiff(everything(), "measurement_value"))) %>% 
    mutate(measurement_value = sum(measurement_value))
  
    #strata_in_georef_but_not_in_nominal_report
    
    species_group <-  st_read(connectionDB,query = "SELECT taxa_order, code from species.species_asfis") %>% janitor::clean_names() %>%  dplyr::select(species_group = taxa_order, species = code) 
    cl_cwp_gear_level2 <- st_read(connectionDB, query = "SELECT * FROM gear_type.isscfg_revision_1")%>% select(Code = code, Gear = label)
    
    shapefile.fix <- st_read(connectionDB,query = "SELECT * from area.cwp_grid") %>% 
      dplyr::rename(GRIDTYPE = gridtype)
    continent <- st_read(connectionDB,query = "SELECT * from public.continent") 
    
    
    shape_without_geom  <- shapefile.fix %>% as_tibble() %>%dplyr::select(-geom)
    
    # source("https://raw.githubusercontent.com/firms-gta/geoflow-tunaatlas/master/Analysis_markdown/functions/tidying_GTA_data_for_comparison.R")
    
    
    # we only keep georef data for which we have an equivalent year in nominal
    
    list_strata <- list(c("geographic_identifier", "time_start", "source_authority"), 
                        c("geographic_identifier", "time_start", "source_authority", "gear_type"))
    
    for (strata in list_strata){
      name <- paste0(toString(strata))
      
    
      catch_no_effort <- anti_join(global_catch_firms_level0_harmonized, global_georeferenced_effort_firms_harmonized, by = strata)
      catch_no_effort <- catch_no_effort %>% mutate(time_start = as.character(time_start))
      
      catch_no_effort_groupped <- catch_no_effort %>% 
        dplyr::group_by(across(setdiff(everything(), "measurement_value"))) %>% 
        mutate(measurement_value = sum(measurement_value))
      
      # global_catch_firms_level0_harmonized_groupped <- global_catch_firms_level0_harmonized %>% group_by_at(strata) %>%
      #   summarise(measurement_value = sum(measurement_value)) 
      # 
      # global_georeferenced_effort_firms_harmonized_groupped <- global_georeferenced_effort_firms_harmonized %>% group_by_at(strata) %>%
      #   summarise(measurement_value = sum(measurement_value))
      
      
      
      
      catch_no_effort_groupped <- catch_no_effort_groupped %>% dplyr::mutate(geographic_identifier = as.character(geographic_identifier))
      catch_no_effort_groupped_all <- tidying_GTA_data_for_comparison(dataframe = catch_no_effort_groupped,
                                                                        shape = shape_without_geom, 
                                                                        species_group_dataframe = species_group,
                                                                        cl_cwp_gear_level2_dataframe = cl_cwp_gear_level2)
      
      # save.image(paste0(name,".Rdata"))
      concerned_trfmos <- unique(catch_no_effort$source_authority)
      source("https://raw.githubusercontent.com/firms-gta/geoflow-tunaatlas/master/Analysis_markdown/functions/copy_project_files.R", local = TRUE)
      # 
      # copy_project_files(original_repo_path = here::here("Analysis_markdown/"), new_repo_path = getwd())
      concerned_trfmos <- c(concerned_trfmos, "all")
      tryCatch({
        for (i in concerned_trfmos){
          
          dir.create(file.path(getwd(),"catch_no_effort/",i, "/"),recursive = TRUE )
          if(i != "all"){
          catch_no_effort_groupped <- as.data.frame(catch_no_effort_groupped_all %>% dplyr::filter(source_authority == i))
          } else {
            catch_no_effort_groupped <- catch_no_effort_groupped_all
          }
          if(nrow(catch_no_effort_groupped)!= 0){
            parameters_child_global <- list(fig.path = paste0("catch_no_effort/",i, "/figures/"), 
                                            parameter_init = catch_no_effort_groupped, 
                                            parameter_titre_dataset_1 = "catch_with_no_effort",
                                            parameter_colnames_to_keep = c("fishing_fleet", "gear_type",
                                                                           "fishing_mode", "measurement_unit", "measurement_value", 
                                                                           "Gear", "species_group", "source_authority"),
                                            shapefile.fix = shapefile.fix, continent = continent,
                                            parameter_geographical_dimension = "geographic_identifier", 
                                            parameter_time_dimension = "time_start",
                                            parameter_geographical_dimension_groupping = "GRIDTYPE", child_header = "-#", 
                                            Add_lines = "Catch_no_effort_add_lines.Rmd",print_map = TRUE, 
                                            title = "Summary of the data being recorded in catch dataset but not in effort dataset")
            
            
            child_env_global = new.env()
            
            list2env(parameters_child_global, envir = child_env_global)
            rmarkdown::render("comparison.Rmd",
                              envir = child_env_global,
                              output_file = "catch_no_effort.html", output_format = "html_document2",
                              output_dir = file.path("catch_no_effort/",i, "/", name))    
            
          }
        }
      }, error = function(e) {
        # Handle the error, e.g., print an error message
        message("An error occurred: ", conditionMessage(e))
      })
      
      
    }
    
    
    # Define directories
    source_directory <- file.path(getwd(),"catch_no_effort")
    target_directory <- source_directory
    
    # List all HTML files
    files <- list.files(source_directory, recursive = TRUE, full.names = TRUE, pattern = "\\.html$")
    
    # Function to create a new name based on the directory path
    create_new_name <- function(file_path) {
      parts <- unlist(strsplit(file_path, "/"))
      new_name <- paste(paste(parts[length(parts)-2],paste(parts[length(parts)-1], parts[length(parts)], sep = "_")))
      return(new_name)
    }
    
    # Copy and rename files
    for (file in files) {
      new_name <- create_new_name(file)
      file.copy(file, file.path(target_directory, new_name))
      
      
      if(uploadgoogledrive){
        # config$logger.info("Upload netcdf to Google Drive")
        folder_datasets_id <- "1vvmdaT80ZFHnDZcJyhyIOsf_mOJjB5tA"
        path_to_dataset_new <- file.path(file)
        drive_upload(path_to_dataset_new, as_id(folder_datasets_id), overwrite = TRUE)
      }
    }
    
    
    CPUE <- full_join(as.data.frame(global_catch_firms_level0_harmonized) %>% 
  dplyr::rename(measurement_unit_catch = measurement_unit, measurement_value_catch= measurement_value), 
  as.data.frame(global_georeferenced_effort_firms_harmonized) %>% 
    dplyr::rename(measurement_unit_effort = measurement_unit, measurement_value_effort= measurement_value),
  by = c("geographic_identifier", "source_authority", "time_start", "time_end"))
    
    # CPUE <- CPUE %>% dplyr::mutate(across(measurement_value_effort, measurement_value_catch))
    
    
    
    setwd(ancient_wd)
  
    return(CPUE)
  
}
