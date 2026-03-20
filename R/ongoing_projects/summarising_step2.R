#' Summarising_step
#'
#' This function performs various summarizing steps on data related to species and gear types, retrieving data from a database,
#' processing it, and rendering output reports.
#'
#' @param main_dir Character. The main directory containing the entities. (jobs/entities)
#' @param connectionDB Object. The database connection.
#' @param config List. Configuration list containing metadata and options for processing.
#' @param source_authoritylist Vector. Vector of source_authority to filter on, "all" being all of them.
#' @param savestep Logical TRUE/FALSE, should the .qs result of this be saved ?
#' @param nameoutput Character, name of the .qs if saved
#' @param usesave Logical Should we use the nameoutput .qs instead of rerunning everything ?
#' @param sizepdf Character string. La taille peut prendre les valeurs suivantes :
#'   \itemize{
#'     \item `"long"` (par défaut) : Long with coverage.
#'     \item `"middle"` : Long without coverage
#'     \item `"short"` : Only first characteristics, first differences and main table of steps
#'   }
#' @param parameter_colnames_to_keep_fact Vector: what column to display
#' @return NULL. The function has side effects, such as writing files and rendering reports.
#' @param fast_and_heavy Logical TRUE/FALSE, should we save a .qs for each dataset, and use this .qs in every markdown
#'
#' @examples
#' \dontrun{
#' connectionDB <- DBI::dbConnect(RSQLite::SQLite(), ":memory:") # Connexion temporaire
#' config <- list() # Simule une configuration
#' summarising_step(main_dir = "chemin/vers/dossier", connectionDB, config)
#' }
#' @import dplyr
#' @import sf
#' @importFrom futile.logger flog.info flog.warn flog.error
#' @importFrom qs qread qsave
#' @export
summarising_step2 <- function(main_dir, connectionDB, config, source_authoritylist = c("all","IOTC","WCPFC", "IATTC", "ICCAT", "CCSBT" ), sizepdf = "long",
                             savestep = FALSE, nameoutput = NULL, usesave = FALSE, fast_and_heavy = TRUE, parameter_colnames_to_keep_fact = NULL) {
  
  if(sizepdf == "long"){
    coverage = TRUE
  } else if(sizepdf %in% c("middle", "short")){
    coverage = FALSE
  } else {
    stop('Please provide a correct sizepdf, "short", "middle" or "long"')
  }
  
  futile.logger::flog.info(paste0("Size pdf is:", sizepdf))
  
  ancient_wd <- getwd()
  futile.logger::flog.info("Starting Summarising_step function")
  
  entity_dirs <- list.dirs(file.path(main_dir, "entities"), full.names = TRUE, recursive = FALSE)
  # entity_dirs <- entity_dirs[2]
  child_env <- new.env(parent = new.env())
  gc()
  futile.logger::flog.info("Initialized child environment")
  
  i <- 1
  futile.logger::flog.info("Sourced all required functions")
  cwp_grid_file <- system.file("extdata", "cl_areal_grid.csv", package = "CWP.dataset")
  if (!file.exists(cwp_grid_file)) {
    stop("cl_areal_grid.csv not found in inst/extdata - run data-raw/download_codelists.R")
  }
  shp_raw <- sf::st_read(cwp_grid_file, show_col_types = FALSE)
  shapefile.fix <- sf::st_as_sf(shp_raw, wkt = "geom_wkt", crs = 4326)
  shapefile.fix <- dplyr::rename(shapefile.fix,
                                 cwp_code = CWP_CODE,
                                 geom     = geom_wkt)
  
  continent <- tryCatch({
    
    st_read(connectionDB, query = "SELECT * FROM public.continent")
  }, error = function(e) {
    
    futile.logger::flog.error("An error occurred while reading continent data: %s", e$message)
    
    NULL
    
  })
  
  if (is.null(continent)) {
    
    futile.logger::flog.warn("Continent data not found in the database. Fetching from WFS service.")
    
    url <- "https://www.fao.org/fishery/geoserver/wfs"
    
    serviceVersion <- "1.0.0"
    
    logger <- "INFO"
    
    WFS <- WFSClient$new(url = "https://www.fao.org/fishery/geoserver/fifao/wfs", serviceVersion = "1.0.0", logger = "INFO")
    
    continent <- WFS$getFeatures("fifao:UN_CONTINENT2")
    
    futile.logger::flog.info("Fetched continent data from WFS service")
    
  }
  
  for (entity_dir in entity_dirs) {
    
    futile.logger::flog.info("Processing entity directory: %s", entity_dir)
    
    entity <- config$metadata$content$entities[[i]]
    i <- i + 1
    action <- entity$data$actions[[1]]
    opts <- action$options
    
    if(is.null(parameter_colnames_to_keep_fact)){
      if (opts$fact == "effort") {
        futile.logger::flog.warn("Effort dataset not displayed for now")
        parameter_colnames_to_keep_fact = c("source_authority", "fishing_mode_label", "geographic_identifier","fishing_fleet_label","gear_type_label",
                                            "measurement_unit", "measurement_value", "gridtype","species_group", "species_label", "measurement_type")
      } else {
        parameter_colnames_to_keep_fact = c("source_authority", "fishing_fleet_label",
                                            "fishing_mode_label", "geographic_identifier",
                                            "measurement_unit", "measurement_value", "gridtype",
                                            "species_label", "gear_type_label")
        
      }
    }
    entity_name <- basename(entity_dir)
    setwd(here::here(entity_dir))
    # copy_project_files(original_repo_path = here::here("Analysis_markdown"), new_repo_path = getwd())
    
    sub_list_dir_2 <- list.files("Markdown", recursive = TRUE, pattern = "data.qs", full.names = TRUE)
    details <- file.info(sub_list_dir_2)
    details <- details[with(details, order(as.POSIXct(mtime))), ]
    sub_list_dir_2 <- rownames(details)
    futile.logger::flog.info("Processed sub_list_dir_2")
    
    for (file in sub_list_dir_2) {
      `%notin%` <- Negate(`%in%`)
      if (!file.exists(gsub(pattern = basename(file), replacement = "ancient.qs", file))) {
        data <- qs::qread(file)
        file.copy(from = file, to = gsub(pattern = basename(file), replacement = "ancient.qs", file))
        data <- CWP.dataset::enrich_dataset_if_needed(data, shp_raw = shp_raw)$without_geom
        data <- data%>%dplyr::mutate(measurement_unit = dplyr::case_when(measurement_unit %in% c("MT","t","MTNO", "Tons")~ "Tons",
                                                                         measurement_unit %in% c("NO", "NOMT","no", "Number of fish")~"Number of fish", TRUE ~ measurement_unit))
        
        qs::qsave(data, file = file)
        futile.logger::flog.info("Processed and saved data for file: %s", file)
      } else {
        futile.logger::flog.info("Retrieving processed data: %s", file)
      }
    }
    parameter_resolution_filter <- opts$resolution_filter
    parameter_filtering <- opts$parameter_filtering
    for (s in 1:length(source_authoritylist)){
      if(source_authoritylist[s] == "all"){
        parameter_filtering = opts$parameter_filtering
      } else {
        parameter_filtering$source_authority <- source_authoritylist[s]
      }
      
      prefix <- paste0(sizepdf, source_authoritylist[s])
      
      if(usesave & file.exists(paste0(prefix, "renderenv.qs")) | (sizepdf=="short" && file.exists(paste0("long", paste0(source_authoritylist[s],"renderenv.qs"))))){ # if the size pdf is short but the .qs for long exists we can use it
        if(file.exists(paste0(prefix, "renderenv.qs"))){
          
          render_env <- qs::qread(paste0(sizepdf,paste0(source_authoritylist[s],"renderenv.qs")))
          
        } else if(sizepdf=="short" && file.exists(paste0("long", paste0(source_authoritylist[s],"renderenv.qs")))){
          
          render_env <- qs::qread(paste0(prefix,"renderenv.qs"))
          assign("all_list", NULL, envir = render_env)
        }
      }else {
        
        parameters_child_global <- list(
          fig.path = paste0("tableau_recap_global_action/figures/"),
          parameter_filtering = parameter_filtering,
          parameter_resolution_filter = parameter_resolution_filter
        )
        
        output_file_name <- paste0(entity_name, "_report.html")
        
        render_env <- list2env(as.list(child_env), parent = child_env)
        list2env(parameters_child_global, envir = render_env)
        
        if(usesave && file.exists("process_fisheries_data_list.qs")){
          
          sprintf("Using saved data for process_fisheries_data_list.qs")
          
        }
        
        if(!fast_and_heavy && usesave && file.exists(paste0(prefix,"path_to_qs_final.qs"))){
          
          sprintf("Using saved data for path_to_qs_final.qs")
          child_env_last_result <- NULL
          
        } else {
          
          child_env_last_result <- CWP.dataset::comprehensive_cwp_dataframe_analysis(
            parameter_init = sub_list_dir_2[length(sub_list_dir_2)],
            parameter_final = NULL,
            fig.path = parameters_child_global$fig.path,
            parameter_fact = opts$fact,
            parameter_colnames_to_keep = parameter_colnames_to_keep_fact,
            coverage = TRUE,
            shapefile_fix = shapefile.fix,
            continent = continent,
            parameter_resolution_filter = parameters_child_global$parameter_resolution_filter,
            parameter_filtering = parameters_child_global$parameter_filtering,
            parameter_titre_dataset_1 = entity$identifiers[["id"]],
            parameter_geographical_dimension_groupping = "gridtype",
            unique_analyse = TRUE
          )
          
          filename <- paste0("Report_on_", entity$identifiers[["id"]])
          new_path <- file.path(render_env$fig.path, filename)
          dir.create(new_path, recursive = TRUE)
          child_env_last_result$fig.path <- new_path
          child_env_last_result$step_title_t_f <- FALSE
          # child_env_last_result$parameter_short <- FALSE
          child_env_last_result$child_header <- "#"
          # child_env_last_result$unique_analyse <- TRUE
          child_env_last_result$parameter_titre_dataset_1 <- entity$identifiers[["id"]]
          # child_env_last_result$parameter_titre_dataset_2 <- NULL
          if(!fast_and_heavy){
            qs::qsave(child_env_last_result, paste0(prefix, "path_to_qs_final.qs"))
          }
        }
        
        if(!fast_and_heavy && usesave && file.exists(paste0(prefix,"path_to_qs_summary.qs"))){
          
          sprintf("Using saved data for path_to_qs_summary.qs")
          child_env_first_to_last_result <- NULL
          new_path <- file.path(parameters_child_global$fig.path, paste0("/Comparison/initfinal_", basename(sub_list_dir_2[1]), "_", basename(sub_list_dir_2[length(sub_list_dir_2)])))
        } else {
          
          child_env_first_to_last_result <- CWP.dataset::comprehensive_cwp_dataframe_analysis(
            parameter_init = sub_list_dir_2[1],
            parameter_final = sub_list_dir_2[length(sub_list_dir_2)],
            fig.path = parameters_child_global$fig.path,
            parameter_fact = opts$fact,
            parameter_colnames_to_keep = parameter_colnames_to_keep_fact,
            shapefile_fix = shapefile.fix,
            continent = continent,
            coverage = TRUE,
            parameter_resolution_filter = parameters_child_global$parameter_resolution_filter,
            parameter_filtering = parameters_child_global$parameter_filtering,
            parameter_titre_dataset_1 =  "Initial_data",
            parameter_titre_dataset_2 = entity$identifiers[["id"]],
            parameter_geographical_dimension_groupping = "gridtype",
            unique_analyse = FALSE
          )
          
          new_path <- file.path(parameters_child_global$fig.path, paste0("/Comparison/initfinal_",  "Initial_data", "_", entity$identifiers[["id"]]))
          dir.create(new_path, recursive = TRUE)
          child_env_first_to_last_result$fig.path <- new_path
          child_env_first_to_last_result$step_title_t_f <- FALSE
          # child_env_first_to_last_result$parameter_short <- FALSE
          # child_env_first_to_last_result$unique_analyse <- FALSE
          child_env_first_to_last_result$parameter_titre_dataset_1 <- "Initial_data"
          child_env_first_to_last_result$parameter_titre_dataset_2 <- entity$identifiers[["id"]]
          child_env_first_to_last_result$child_header <- "#"
          
          if(!fast_and_heavy){
            qs::qsave(child_env_first_to_last_result, paste0(prefix,"path_to_qs_summary.qs"))
          }
          
        }
        
        sub_list_dir_3 <- gsub("/data.qs", "", sub_list_dir_2)
        render_env$sub_list_dir_3 <- sub_list_dir_3
        
        if(!fast_and_heavy && usesave && file.exists(paste0(prefix,"process_fisheries_data_list.qs"))){
          
          sprintf("Using saved data for process_fisheries_data_list.qs")
          process_fisheries_data_list <- NULL
          
        } else {
          
          if(opts$fact == "effort"){
            process_fisheries_data_list <- CWP.dataset::process_fisheries_effort_data(sub_list_dir_3,  parameter_filtering)
          } else {
            process_fisheries_data_list <- CWP.dataset::process_fisheries_data(sub_list_dir_3, parameter_fact = "catch", parameter_filtering)
          }
          if(!fast_and_heavy){
            
            qs::qsave(process_fisheries_data_list, paste0(prefix,"process_fisheries_data_list.qs"))
          }
        }
        futile.logger::flog.info("Processed process_fisheries_data_list")
        
        render_env$process_fisheries_data_list <- process_fisheries_data_list
        
        futile.logger::flog.info("Adding to render_env")
        
        if(sizepdf %in% c("long", "middle")){
          
          final_step <- length(sub_list_dir_3) - 1
          fast_and_heavy_t_f <- fast_and_heavy
          
          run_comparisons <- function(final_step,
                                      fast_and_heavy = TRUE,
                                      sub_list_dir_3,
                                      shapefile.fix,
                                      continent,
                                      parameters_child_global,
                                      fig.path,
                                      coverage) {
            
            seq_i <- seq_len(final_step)
            
            if (fast_and_heavy) {
              all_list <- lapply(seq_i, function(i) {
                # 1) calcul du résultat
                res_i <- CWP.dataset::function_multiple_comparison(
                  i,
                  parameter_short         = FALSE,
                  sub_list_dir            = sub_list_dir_3,
                  shapefile.fix           = shapefile.fix,
                  continent               = continent,
                  parameters_child_global = parameters_child_global,
                  fig.path                = fig.path,
                  coverage                = coverage
                )
                
                # 2) si ce n’est pas déjà une liste, on l’emballe
                if (!is.list(res_i)) {
                  res_i <- list(value = res_i)
                }
                
                # 3) si des noms manquent, on les génère
                nms <- names(res_i)
                if (is.null(nms) || any(nms == "")) {
                  nms <- nms %||% rep("", length(res_i))  # %||% = si NULL, remplace par ""
                  empty <- which(nms == "")
                  nms[empty] <- paste0("item", empty)
                  names(res_i) <- nms
                }
                
                res_i
              })
              return(all_list)
            } else {
              all_paths <- lapply(seq_i, function(i) {
                
                out_file <- file.path(fig.path,
                                      sprintf("comparison_step_%02d.qs", i))
                
                if(usesave && file.exists(out_file)){
                  
                  sprintf("comparison_step_%02d.qs", i, " already exists, using the cached data")
                  
                } else {
                  res_i <- CWP.dataset::function_multiple_comparison(
                    i,
                    parameter_short         = FALSE,
                    sub_list_dir            = sub_list_dir_3,
                    shapefile.fix           = shapefile.fix,
                    continent               = continent,
                    parameters_child_global = parameters_child_global,
                    fig.path                = fig.path,
                    coverage                = coverage
                  )
                  
                  
                  qs::qsave(res_i, file = out_file, preset = "high")
                  rm(res_i); gc()
                }
                out_file
              })
              return(all_paths)
            }
          }
          
          
          all_list <- run_comparisons(
            final_step = final_step,
            fast_and_heavy = fast_and_heavy_t_f,
            sub_list_dir_3           = sub_list_dir_3,
            shapefile.fix            = shapefile.fix,
            continent                = continent,
            parameters_child_global  = parameters_child_global,
            fig.path                 = prefix,
            coverage                 = coverage
          )
          
          
          futile.logger::flog.info("all_list processed")
          
          all_list <- all_list[!is.na(all_list)]
          
          render_env$all_list <- all_list
          
        } else{
          
          rm(all_list, envir = render_env)
          assign("all_list", NULL, envir = render_env)
          
        }
        
        render_env$child_env_first_to_last_result <- child_env_first_to_last_result
        render_env$child_env_last_result <- child_env_last_result
        gc()
        
        render_env$plotting_type <- "view"
        render_env$fig.path <- new_path
        render_env$parameter_titre_dataset_1 <- entity$identifiers[["id"]]
        
        
        if(fast_and_heavy){
          if(savestep){
            qs::qsave(render_env, file = paste0(prefix, "renderenv.qs"))
          }
        } else {
          render_env$child_env_first_to_last_result <- NULL
          render_env$child_env_last_result <- NULL
          render_env$process_fisheries_data_list <- NULL
          render_env$path_to_qs_summary <- paste0(prefix, "path_to_qs_summary.qs")
          render_env$path_to_process_fisheries_data_list <- paste0(prefix, "process_fisheries_data_list.qs")
          render_env$path_to_qs_final <- paste0(prefix, "path_to_qs_final.qs")
          
          # 1) créer un env « propre » sans parent
          minimal_env <- new.env(parent = emptyenv())
          
          process_paths <- function(x, start) {
            if (is.character(x)&& grepl("[./]", x)) {
              # transforme chaque élément en chemin absolu
              return(fs::path_abs(x, start = start))
            }
            if (is.list(x)) {
              # rappelle process_paths sur chaque sous-élément
              return(lapply(x, process_paths, start = start))
            }
            # si ce n'est ni caractère ni liste, on ignore
            return(NULL)
          }
          
          # 2) y copier uniquement les bindings de render_env qui vous intéressent
          for (nm in ls(render_env, all.names = TRUE)) {
            val <- render_env[[nm]]
            # si c'est un chemin ou une liste de chemins
            if (is.character(val) || is.list(val)) {
              processed <- process_paths(val, start = getwd())
              # only keep it if there's something non-NULL
              if (!is.null(processed)) {
                minimal_env[[nm]] <- processed
              }
            }
            # sinon, on n'ajoute pas cet objet
          }
          
          minimal_env$tmap_mode <- "view"
          minimal_env$parameter_titre_dataset_1 <- entity$identifiers[["id"]]
          # 3) sauvegarder le minimal_env à la place de render_env
          qs::qsave(
            minimal_env,
            file = paste0(prefix, "renderenvpath.qs"),
          )
          
          gc()
        }
      }
      
      if(is.null(nameoutput)){
        nameoutput <- paste0(prefix,"recappdf")
      }
      
      set_flextable_defaults(fonts_ignore=TRUE)
      base::options(knitr.duplicate.label = "allow")
      bookdown_path <- CWP.dataset::generate_bookdown_yml(new_session = !fast_and_heavy)
      if(sizepdf != "short"){
        if(fast_and_heavy){
          futile.logger::flog.info("gitbook")
          bookdown::render_book(
            input = bookdown_path,
            envir = render_env,
            output_format = "bookdown::gitbook",
            output_dir = nameoutput
          )
        } else {
          
          CWP.dataset::build_book(master_qs_rel = paste0(prefix, "renderenvpath.qs"),
                                  output_format = "bookdown::gitbook",
                                  output_dir = nameoutput)
        }
        
        
        gc()
      }
      futile.logger::flog.info("pdfdocument")
      if(fast_and_heavy){
        bookdown::render_book(".", envir = render_env,
                              output_format = "bookdown::pdf_document2",
                              output_dir = nameoutput)
        
        gc()
      } else {
        CWP.dataset::build_book(master_qs_rel = paste0(prefix, "renderenvpath.qs"),
                                output_format = "bookdown::pdf_document2",
                                output_dir = nameoutput)
      }
      
      unlink("_bookdown.yml")
      nameoutput <- NULL
      rm(child_env_last_result, envir = render_env)
      rm(child_env_first_to_last_result, envir = render_env)
      rm(render_env)
      
      # drive_upload("tableau_recap_global_action_effort.html", as_id(folder_datasets_id), overwrite = TRUE)
      futile.logger::flog.info("Rendered and uploaded report for entity: %s", entity_dir)
    }
    
    sprintf("entity: %s is done", entity_dir)
    
  }
  try(setwd(ancient_wd))
  futile.logger::flog.info("Finished Summarising_step function")
  # return(render_env)
}
