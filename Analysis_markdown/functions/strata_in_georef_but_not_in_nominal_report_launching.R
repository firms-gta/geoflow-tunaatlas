#' Launch Strata in Georef but not in Nominal Report
#'
#' This function launches a report comparing strata in georef data but not in nominal data.
#'
#' @param main.dir Directory containing the main data files.
#' @param connectionDB Database connection object.
#' @param uploadgoogledrive Logical indicating whether to upload the report to Google Drive.
#' @return Data frame containing the upgraded nominal data.
#' @import dplyr
#' @importFrom lubridate year
#' @importFrom sf st_read
#' @importFrom janitor clean_names
#' @importFrom readr read_csv
#' @importFrom rmarkdown render
#' @importFrom googledrive drive_upload as_id
#' @export
strata_in_georef_but_not_in_nominal_report_launching <- function(main.dir, connectionDB, uploadgoogledrive = TRUE) {
  ancient_wd <- getwd()
  setwd(main.dir)
  path <- getwd()
  species_group <-  st_read(connectionDB,query = "SELECT taxa_order, code from species.species_asfis") %>% janitor::clean_names() %>%  dplyr::select(species_group = taxa_order, species = code) 
  cl_cwp_gear_level2 <- st_read(connectionDB, query = "SELECT * FROM gear_type.isscfg_revision_1")%>% select(Code = code, Gear = label)
  
  shapefile.fix <- st_read(connectionDB,query = "SELECT * from area.cwp_grid") %>% 
    dplyr::rename(GRIDTYPE = gridtype)
  source("https://raw.githubusercontent.com/firms-gta/geoflow-tunaatlas/master/Analysis_markdown/functions/compare_georef_nominal.R")
  if (file.exists(file.path("entities/global_catch_firms_level0/data/global_nominal_catch_firms_level0.csv"))) {
    nominal <- as.data.frame(read_csv(file.path("entities/global_catch_firms_level0/data/global_nominal_catch_firms_level0.csv")) %>%
                               mutate(measurement_unit = "Tons")) %>%
      dplyr::mutate(gear_type = as.numeric(gear_type)) %>%
      dplyr::mutate(gear_type = as.character(gear_type))
    
    try(georef_mapped <- readRDS(file.path("entities/global_catch_firms_level0/Markdown/rawdata/rds.rds")))
    try(georef_mapped <- readRDS(file.path("entities/global_catch_firms_level0_/Markdown/rawdata/rds.rds")))
    
    # Compare georef and nominal data
    results <- compare_georef_nominal(georef_mapped, nominal, connectionDB)
    
    concerned_trfmos <- unique(c(unique(georef_mapped$source_authority)))
    
    source("https://raw.githubusercontent.com/firms-gta/geoflow-tunaatlas/master/Analysis_markdown/functions/copy_project_files.R", local = TRUE)
    
    copy_project_files(original_repo_path = here::here("Analysis_markdown/"), new_repo_path = getwd())
    
    tryCatch({
      for (i in concerned_trfmos) {
        for (name in names(results)) {
          dir.create(file.path(getwd(), "georef_not_nominal_markdown", i, name, "figures/georef_sup_nominal"), recursive = TRUE)
          georef_no_nominal_groupped <- results[[name]]$georef_no_nominal_groupped_all %>%
            dplyr::filter(source_authority == i)
          georef_sup_to_nom <- results[[name]]$georef_sup_to_nom_all %>%
            dplyr::filter(source_authority == i)
          nominal_groupped_filtered <- nominal %>%
            dplyr::filter(source_authority == i)
          
          if (nrow(georef_sup_to_nom) != 0 | nrow(georef_no_nominal_groupped) != 0) {
            parameters_child_global <- list(
              strata = name,
              fig.path = paste0("georef_not_nominal_markdown/", i, "/figures/"),
              parameter_init = georef_no_nominal_groupped,
              nominal_groupped = nominal_groupped_filtered,
              parameter_colnames_to_keep = c("fishing_fleet", "gear_type", "geographic_identifier", "fishing_mode", "species", "measurement_unit", "measurement_value", "Gear", "species_group", "GRIDTYPE"),
              georef_sup_to_nom = georef_sup_to_nom,
              shapefile.fix = shapefile.fix,
              parameter_geographical_dimension = "geographic_identifier",
              parameter_geographical_dimension_groupping = "GRIDTYPE"
            )
            
            child_env_global <- new.env()
            
            list2env(parameters_child_global, envir = child_env_global)
            rmarkdown::render("strata_in_georef_but_no_nominal.Rmd",
                              envir = child_env_global,
                              output_file = "strata_in_georef_but_no_nominal.html",
                              output_dir = file.path("georef_not_nominal_markdown", i, name))
            if(nrow(georef_no_nominal_groupped)!=0){saveRDS(georef_no_nominal_groupped, file.path("georef_no_nominal_groupped", i, name, "_data.rds"))}
            if(nrow(georef_no_nominal_groupped)!=0){saveRDS(georef_sup_to_nom, file.path("georef_sup_to_nom", i, name, "_data.rds"))}
          }
        }
      }
    }, error = function(e) {
      # Handle the error, e.g., print an error message
      message("An error occurred: ", conditionMessage(e))
    })
    
    #to uncomment and debug to get the georef upgradded
    # georef_no_nominal_all <- anti_join(georef_mapped, nominal, by = c("species", "year", "source_authority")) %>%
    #   filter(measurement_value > 0)
    # georef_no_nominal_all <- georef_no_nominal_all %>%
    #   dplyr::mutate(year = as.character(year)) %>%
    #   ungroup() %>%
    #   dplyr::select(-geographic_identifier)
    
    # Define directories
    source_directory <- file.path(getwd(), "georef_not_nominal_markdown")
    target_directory <- source_directory
    
    # List all HTML files
    files <- list.files(source_directory, recursive = TRUE, full.names = TRUE, pattern = "\\.html$")
    
    # Function to create a new name based on the directory path
    create_new_name <- function(file_path) {
      parts <- unlist(strsplit(file_path, "/"))
      new_name <- paste(parts[length(parts) - 2], parts[length(parts) - 1], parts[length(parts)], sep = "_")
      return(new_name)
    }
    
    # Copy and rename files
    for (file in files) {
      new_name <- create_new_name(file)
      file.copy(file, file.path(target_directory, new_name))
      
      if (uploadgoogledrive) {
        # config$logger.info("Upload netcdf to Google Drive")
        folder_datasets_id <- "1vvmdaT80ZFHnDZcJyhyIOsf_mOJjB5tA"
        path_to_dataset_new <- file.path(file)
        drive_upload(path_to_dataset_new, as_id(folder_datasets_id), overwrite = TRUE)
      }
    }
    
    # upgradded_nominal <- rbind(nominal, georef_no_nominal_all)
    setwd(ancient_wd)
    # return(upgradded_nominal)
  }
}
