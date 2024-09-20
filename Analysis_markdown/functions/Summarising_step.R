#' Summarising_step
#'
#' This function performs various summarizing steps on data related to species and gear types, retrieving data from a database, 
#' processing it, and rendering output reports.
#'
#' @param main_dir Character. The main directory containing the entities.
#' @param connectionDB Object. The database connection.
#' @param config List. Configuration list containing metadata and options for processing.
#' @return NULL. The function has side effects, such as writing files and rendering reports.
#' @examples
#' Summarising_step(main_dir = "path/to/main/dir", connectionDB = db_connection, config = config_list)
#' @import dplyr
#' @import sf
#' @import futile.logger
#' @export
Summarising_step <- function(main_dir, connectionDB, config) {
  ancient_wd <- getwd()
  flog.info("Starting Summarising_step function")

  species_group <- st_read(connectionDB, query = "SELECT taxa_order, code FROM species.species_asfis") %>%
    janitor::clean_names() %>%
    dplyr::select(species_group = taxa_order, species = code)
  flog.info("Loaded species_group data")

  cl_cwp_gear_level2 <- st_read(connectionDB, query = "SELECT * FROM gear_type.isscfg_revision_1") %>%
    dplyr::select(Code = code, Gear = label)
  flog.info("Loaded cl_cwp_gear_level2 data")

  shapefile.fix <- st_read(connectionDB, query = "SELECT * FROM area.cwp_grid") %>%
    dplyr::rename(GRIDTYPE = gridtype)
  flog.info("Loaded shapefile.fix data")

  continent <- tryCatch({
    st_read(connectionDB, query = "SELECT * FROM public.continent")
  }, error = function(e) {
    flog.error("An error occurred while reading continent data: %s", e$message)
    NULL
  })

  if (is.null(continent)) {
    flog.warn("Continent data not found in the database. Fetching from WFS service.")
    url <- "https://www.fao.org/fishery/geoserver/wfs"
    serviceVersion <- "1.0.0"
    logger <- "INFO"
    WFS <- WFSClient$new(url = "https://www.fao.org/fishery/geoserver/fifao/wfs", serviceVersion = "1.0.0", logger = "INFO")
    continent <- WFS$getFeatures("fifao:UN_CONTINENT2")
    flog.info("Fetched continent data from WFS service")
  }

  shape_without_geom <- shapefile.fix %>%
    as_tibble() %>%
    dplyr::select(-geom)
  flog.info("Processed shapefile.fix data")

  entity_dirs <- list.dirs(file.path(main_dir, "entities"), full.names = TRUE, recursive = FALSE)

  child_env <- new.env(parent = new.env())
  gc()
  flog.info("Initialized child environment")

  i <- 1
  file_path_url <- "https://raw.githubusercontent.com/firms-gta/geoflow-tunaatlas/master/Analysis_markdown/functions"
  source(file.path(file_path_url,"copy_project_files.R"), local = TRUE)
  source(file.path(file_path_url,"tidying_GTA_data_for_comparison.R"))
  source(file.path(file_path_url,"Functions_markdown.R"), local = TRUE)
  source("~/firms-gta/geoflow-tunaatlas/Analysis_markdown/functions/Functions_markdown.R", local = TRUE)
  source("~/firms-gta/geoflow-tunaatlas/Analysis_markdown/functions/compare_temporal_differences_dygraphs.R", local = TRUE)
  source("~/firms-gta/geoflow-tunaatlas/Analysis_markdown/functions/other_dimension_analysis_dygraphs.R", local = TRUE)
  source(file.path(file_path_url,"Groupping_differences.R"), local = TRUE)
  source(file.path(file_path_url,"compare_strata_differences.R"), local = TRUE)
  source(file.path(file_path_url,"compare_dimension_differences.R"), local = TRUE)
  source(file.path(file_path_url,"compare_temporal_differences.R"), local = TRUE)
  source(file.path(file_path_url,"geographic_diff.R"), local = TRUE)
  source(file.path(file_path_url,"time_coverage_analysis.R"), local = TRUE)
  source(file.path(file_path_url,"spatial_coverage_analysis.R"), local = TRUE)
  source(file.path(file_path_url,"other_dimension_analysis.R"), local = TRUE)
  source(file.path(file_path_url,"comprehensive_cwp_dataframe_analysis.R"), local = TRUE)
  source("~/firms-gta/geoflow-tunaatlas/Analysis_markdown/functions/comprehensive_cwp_dataframe_analysis.R", local = TRUE)
  flog.info("Sourced all required functions")
  for (entity_dir in entity_dirs) {
  
    flog.info("Processing entity directory: %s", entity_dir)

    entity <- config$metadata$content$entities[[i]]
    i <- i + 1
    action <- entity$data$actions[[1]]
    opts <- action$options

    if (opts$fact == "effort") {
      flog.warn("Effort dataset not displayed for now")
    } else {
      entity_name <- basename(entity_dir)
      setwd(here::here(entity_dir))
      copy_project_files(original_repo_path = here::here("Analysis_markdown"), new_repo_path = getwd())

      sub_list_dir_2 <- list.files("Markdown", recursive = TRUE, pattern = "data.qs", full.names = TRUE)
      details <- file.info(sub_list_dir_2)
      details <- details[with(details, order(as.POSIXct(mtime))), ]
      sub_list_dir_2 <- rownames(details)
      flog.info("Processed sub_list_dir_2")

      for (file in sub_list_dir_2) {
        `%notin%` <- Negate(`%in%`)
        if (!file.exists(gsub(pattern = basename(file), replacement = "ancient.qs", file))) {
          data <- qs::qread(file)
          file.copy(from = file, to = gsub(pattern = basename(file), replacement = "ancient.qs", file))

          if ("GRIDTYPE" %notin% colnames(data)) {
            data <- data %>%
              dplyr::mutate(geographic_identifier = as.character(geographic_identifier),
                            gear_type = as.character(gear_type))
            data <- tidying_GTA_data_for_comparison(dataframe = data,
                                                    shape = shape_without_geom,
                                                    species_group_dataframe = species_group,
                                                    cl_cwp_gear_level2_dataframe = cl_cwp_gear_level2)
            if ("gridtype" %in% colnames(data)) {
              data <- data %>%
                dplyr::rename(GRIDTYPE = gridtype)
            }

            qs::qsave(data, file = file)
            flog.info("Processed and saved data for file: %s", file)
          }
        }
      }
      
      parameter_resolution_filter <- opts$resolution_filter
      parameter_filtering <- list(species =NULL, gear_type = NULL)
        # 
        # species_list <- c("YFT", "BET", "SKJ", "ALB", "SBF", "BFT", "PBF", "SWO")
      source_authoritylist <- list("CCSBT", "IATTC", "ICCAT", "IOTC", "WCPFC", "all")
      
      for (s in 1:length(source_authoritylist)){
        if(source_authoritylist[s] == "all"){
          parameter_filtering = list(species = NULL, gear_type = NULL)
        } else {
      parameter_filtering <- list(source_authority =source_authoritylist[s])
        }
        
        if(file.exists(paste0(source_authoritylist[s],"renderenv.qs"))){
          render_env <- qs::qread(paste0(source_authoritylist[s],"renderenv.qs"))
        } else {
        
      parameters_child_global <- list(
        fig.path = paste0("tableau_recap_global_action/figures/"),
        parameter_filtering = parameter_filtering,
        parameter_resolution_filter = parameter_resolution_filter
      )

      output_file_name <- paste0(entity_name, "_report.html")

      render_env <- list2env(as.list(child_env), parent = child_env)
      list2env(parameters_child_global, render_env)

      child_env_last_result <- comprehensive_cwp_dataframe_analysis(
        parameter_init = sub_list_dir_2[length(sub_list_dir_2)],
        parameter_final = NULL,
        fig.path = parameters_child_global$fig.path,
        parameter_fact = "catch",
        parameter_colnames_to_keep = c("source_authority", "species", "gear_type", "fishing_fleet",
                                       "fishing_mode", "geographic_identifier",
                                       "measurement_unit", "measurement_value", "GRIDTYPE",
                                       "species_group", "Gear"),
        coverage = TRUE,
        shapefile_fix = shapefile.fix,
        continent = continent,
        parameter_resolution_filter = parameters_child_global$parameter_resolution_filter,
        parameter_filtering = parameters_child_global$parameter_filtering,
        parameter_titre_dataset_1 = entity$identifiers[["id"]],
        unique_analyse = TRUE
      )

      filename <- paste0("Report_on_", entity$identifiers[["id"]])
      new_path <- file.path(render_env$fig.path, filename)
      dir.create(new_path, recursive = TRUE)
      child_env_last_result$fig.path <- new_path
      child_env_last_result$step_title_t_f <- FALSE
      child_env_last_result$parameter_short <- FALSE
      child_env_last_result$child_header <- "#"
      child_env_last_result$unique_analyse <- TRUE
      child_env_last_result$parameter_mapped <- TRUE
      child_env_last_result$parameter_titre_dataset_1 <- entity$identifiers[["id"]]
      child_env_last_result$parameter_titre_dataset_2 <- NULL

      child_env_first_to_last_result <- comprehensive_cwp_dataframe_analysis(
        parameter_init = sub_list_dir_2[1],
        parameter_final = sub_list_dir_2[length(sub_list_dir_2)],
        fig.path = parameters_child_global$fig.path,
        parameter_fact = "catch",
        parameter_mapped = TRUE,
        parameter_colnames_to_keep = c("source_authority", "species", "gear_type", "fishing_fleet",
                                       "fishing_mode", "geographic_identifier",
                                       "measurement_unit", "measurement_value", "GRIDTYPE",
                                       "species_group", "Gear"),
        shapefile_fix = shapefile.fix,
        continent = continent,
        coverage = TRUE,
        parameter_resolution_filter = parameters_child_global$parameter_resolution_filter,
        parameter_filtering = parameters_child_global$parameter_filtering,
        parameter_titre_dataset_1 = "Initial_data",
        parameter_titre_dataset_2 = entity$identifiers[["id"]],
        unique_analyse = FALSE
      )

      new_path <- file.path(parameters_child_global$fig.path, paste0("/Comparison/initfinal_", basename(sub_list_dir_2[1]), "_", basename(sub_list_dir_2[length(sub_list_dir_2)])))
      dir.create(new_path, recursive = TRUE)
      child_env_first_to_last_result$fig.path <- new_path
      child_env_first_to_last_result$step_title_t_f <- FALSE
      child_env_first_to_last_result$parameter_short <- FALSE
      child_env_first_to_last_result$parameter_mapped <- TRUE
      child_env_first_to_last_result$unique_analyse <- FALSE
      child_env_first_to_last_result$parameter_titre_dataset_1 <- "Initial_data"
      child_env_first_to_last_result$parameter_titre_dataset_2 <- entity$identifiers[["id"]]
      child_env_first_to_last_result$child_header <- "#"

      sub_list_dir_3 <- gsub("/data.qs", "", sub_list_dir_2)
      render_env$sub_list_dir_3 <- sub_list_dir_3
      source(file.path(file_path_url,"process_fisheries_data.R"), local = TRUE)
      source("~/firms-gta/geoflow-tunaatlas/Analysis_markdown/functions/process_fisheries_data.R", local = TRUE)
      process_fisheries_data_list <- process_fisheries_data(sub_list_dir_3, parameter_fact = "catch", parameter_filtering)
      flog.info("Processed process_fisheries_data_list")

      render_env$process_fisheries_data_list <- process_fisheries_data_list

      flog.info("Adding to render_env")

      function_multiple_comparison <- function(counting, parameter_short, sub_list_dir, parameters_child_global, fig.path, coverage = FALSE, shapefile.fix, continent) {
        
        gc()
        step_mapping <- sum(which(sub_list_dir == "Markdown/mapping_codelist"))
        parameter_mapped <- ifelse(counting != step_mapping, TRUE, FALSE)
        parameter_init <- paste0(sub_list_dir[counting], "/data.qs")
        parameter_final <- paste0(sub_list_dir[counting + 1], "/data.qs")
        parameter_titre_dataset_1 <- basename(sub_list_dir[counting])
        parameter_titre_dataset_2 <- basename(sub_list_dir[counting + 1])
        
        new_path <- file.path(fig.path, "Comparison", paste0(basename(sub_list_dir[counting]), "_", basename(sub_list_dir[counting + 1])))
        dir.create(new_path, recursive = TRUE)
        
        # Logging the start of comparison
        flog.info(paste("Starting comparison between:", parameter_titre_dataset_1, "and", parameter_titre_dataset_2))
        
        # Check if the datasets are different
        formals(filtering_function, envir = environment())$parameter_filtering = parameter_filtering
        initfiltered <- filtering_function(qs::qread(parameter_init))
        finalfiltered <- filtering_function(qs::qread(parameter_final))
        if (!identical(initfiltered, finalfiltered)) {
          rm(initfiltered)
          rm(finalfiltered)
          # Log datasets difference
          flog.info(paste("Datasets are different:", parameter_titre_dataset_1, "and", parameter_titre_dataset_2))
          
          child_env_result <- comprehensive_cwp_dataframe_analysis(
            parameter_init = parameter_init,
            parameter_final = parameter_final,
            fig.path = new_path,
            parameter_fact = "catch",
            plotting_type = "plot",
            parameter_colnames_to_keep = c("source_authority", "species", "gear_type", "fishing_fleet",
                                           "fishing_mode", "geographic_identifier",
                                           "measurement_unit", "measurement_value", "GRIDTYPE",
                                           "species_group", "Gear"),
            shapefile_fix = shapefile.fix,
            continent = continent,
            coverage = coverage,
            parameter_resolution_filter = parameters_child_global$parameter_resolution_filter,
            parameter_filtering = parameters_child_global$parameter_filtering,
            parameter_titre_dataset_1 = parameter_titre_dataset_1,
            parameter_titre_dataset_2 = parameter_titre_dataset_2,
            unique_analyse = FALSE
          )
          
          # Log successful analysis
          flog.info(paste("Analysis completed for:", parameter_titre_dataset_1, "vs", parameter_titre_dataset_2))
          
          child_env_result$step_title_t_f <- TRUE
          child_env_result$step_title <- paste0(" Treatment : ", basename(sub_list_dir[counting + 1]))
          child_env_result$step <- counting
          child_env_result$parameter_short <- parameter_short
          child_env_result$treatment <- FALSE
          child_env_result$parameter_mapped <- TRUE
          child_env_result$unique_analyse <- FALSE
          child_env_result$parameter_titre_dataset_1 <- basename(sub_list_dir[counting])
          child_env_result$parameter_titre_dataset_2 <- basename(sub_list_dir[counting + 1])
          child_env_result$child_header <- "##"
          
          gc()
          
          flog.info(paste("New result saved to:", new_path))
          return(child_env_result)
        } else {
          # Log identical datasets
          rm(initfiltered)
          rm(finalfiltered)
          flog.info(paste("Datasets are identical, skipping comparison for:", parameter_titre_dataset_1, "and", parameter_titre_dataset_2))
          return(NA)
        }
      }
      

      final_step <- length(sub_list_dir_3) - 1
      all_list <- lapply(1:final_step, function_multiple_comparison, parameter_short = FALSE, sub_list_dir = sub_list_dir_3,
                         shapefile.fix = shapefile.fix,
                         continent = continent,
                         parameters_child_global = parameters_child_global, fig.path = render_env$fig.path, coverage = TRUE)
      flog.info("all_list processed")

      all_list <- all_list[!is.na(all_list)]

      render_env$all_list <- all_list
      render_env$child_env_first_to_last_result <- child_env_first_to_last_result
      render_env$child_env_last_result <- child_env_last_result
      gc()

      base::options(knitr.duplicate.label = "allow")
      render_env$plotting_type <- "plot"
      render_env$fig.path <- new_path
      qs::qsave(render_env, file = paste0(source_authoritylist[s],"renderenv.qs"))
        }
      set_flextable_defaults(fonts_ignore=TRUE)
      # bookdown::render_book("index.Rmd", envir = render_env, output_format = "bookdown::gitbook")
      gc()
      bookdown::render_book("index.Rmd", envir = render_env, 
                            output_format = "bookdown::pdf_document2", 
                            output_dir =paste0(source_authoritylist[s],
"recap"))
      rm(child_env_last_result, envir = render_env)
      rm(child_env_first_to_last_result, envir = render_env)
      rm(render_env)
      gc()

      # drive_upload("tableau_recap_global_action_effort.html", as_id(folder_datasets_id), overwrite = TRUE)
      flog.info("Rendered and uploaded report for entity: %s", entity_dir)
      }

    sprintf("entity: %s is done", entity_dir)

    }
  }
  try(setwd("~/firms-gta/geoflow-tunaatlas"))
  flog.info("Finished Summarising_step function")
  # return(render_env)
}
