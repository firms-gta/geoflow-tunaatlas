opts <- entity$data$actions[[1]]$options
con <- config$software$input$dbi

if(!exists("parameter_fact") | is.null("parameter_fact")){parameter_fact <-  opts$fact}

if(!is.null(opts$filtering)){parameter_filtering <-opts$filtering} else{ parameter_filtering <-list(species = NULL, fishingfleet = NULL)}

if(is.character(parameter_filtering)){
  parameter_filtering <- eval(parse(text=toString(parameter_filtering)))
}

parameters_child <- list(config = config, entity = entity,opts = opts ,action = action, parameter_filtering = parameter_filtering, parameter_fact = parameter_fact, parameter_con = con,  
child = TRUE, no_recap_options = TRUE, plotting_type = "plot",parameter_colnames_to_keep = c("fishing_fleet",         "gear_type",                 "time_start",                 
  "geographic_identifier","fishing_mode",           "species",                       
"measurement_unit",                 "measurement_value",                "source_authority", "Gear", "species_group"), 
treatment = FALSE, shapefile.fix = shapefile.fix, shape_without_geom = shape_without_geom, continent = continent, fig.path = getwd(), 
outputonly = FALSE, print_map = TRUE, coverage = FALSE, parameter_geographical_dimension = "geographic_identifier", parameter_geographical_dimension_groupping = "GRIDTYPE")
child_env_base <- new.env(parent = environment())
list2env(parameters_child, env = child_env_base)



sublist <- list.dirs(path =paste0(getwd(),"/Markdown"), full.names = FALSE, recursive = FALSE)
sub_list_dir_2 <- file.path("Markdown/",sublist)
details = file.info(paste0(sub_list_dir_2,"/Markdown"))
details = file.info(sub_list_dir_2)
details = details[with(details, order(as.POSIXct(mtime))), ]
sub_list_dir_2 = rownames(details)

source("~/firms-gta/geoflow-tunaatlas/Analysis_markdown/functions/Functions_markdown.R")
source("~/firms-gta/geoflow-tunaatlas/Analysis_markdown/functions/Groupping_differences.R")
source("~/firms-gta/geoflow-tunaatlas/Analysis_markdown/functions/compare_strata_differences.R")
source("~/firms-gta/geoflow-tunaatlas/Analysis_markdown/functions/compare_dimension_differences.R")
source("~/firms-gta/geoflow-tunaatlas/Analysis_markdown/functions/compare_temporal_differences.R")
source("~/firms-gta/geoflow-tunaatlas/Analysis_markdown/functions/geographic_diff.R")
source("~/firms-gta/geoflow-tunaatlas/Analysis_markdown/functions/time_coverage_analysis.R")
source("~/firms-gta/geoflow-tunaatlas/Analysis_markdown/functions/spatial_coverage_analysis.R")
source("~/firms-gta/geoflow-tunaatlas/Analysis_markdown/functions/other_dimension_analysis.R")

knitr::opts_chunk$set(
  echo = FALSE,
  error = 0,
  fig.align = "center",
  message = FALSE,
  warning = FALSE,
  tab.cap.pre = "Table ",
  tab.cap.sep = ": ", results = 'asis'
)
base::options(knitr.duplicate.label = "allow")

# Ensure output.dir is set
knitr::opts_knit$set(output.dir = getwd())
#' Compare Data Across Steps
#'
#' This function compares data between two steps, performs comprehensive analysis,
#' and generates relevant outputs including figures and reports.
#'
#' @param counting Integer indicating the current step in the process.
#' @param parameters List of parameters for the child process (default is `parameters_child`).
#' @param parameter_short Logical indicating if a short parameter list should be used (default is `FALSE`).
#' @param sub_list_dir Character vector of subdirectory paths (default is `sub_list_dir_2`).
#' @param child_env_baseinside Base environment for the child process.
#' @param shapefile_fix Shapefile fix parameter.
#' @param continent Continent parameter.
#' @param fig.path Character string specifying the path for figures (default is `"Data"`).
#'
#' @return Result of the comparison process.
#' @export
function_all_comp <- function(counting, 
                              parameters = parameters_child, 
                              parameter_short = FALSE, 
                              sub_list_dir = sub_list_dir_2, 
                              child_env_baseinside = child_env_base, 
                              shapefile_fix, 
                              continent, 
                              fig.path = "tableau_recap_global_action/figures") {
  
  gc()
  
  step_mapping <- sum(which(sub_list_dir == "Markdown/mapping_codelist"))
  parameter_mapped <- counting != step_mapping
  child_env <- list2env(as.list(child_env_base), parent = child_env_base)
  
  child_env$parameter_init <- paste0(sub_list_dir[counting], "/rds.rds")
  child_env$parameter_final <- paste0(sub_list_dir[counting + 1], "/rds.rds")
  child_env$parameter_titre_dataset_1 <- last_path(sub_list_dir[counting])
  child_env$parameter_titre_dataset_2 <- last_path(sub_list_dir[counting + 1])
  
  child_env$child <- TRUE
  child_env$treatment <- TRUE
  child_env$parameter_mapped <- parameter_mapped
  child_env$parameter_short <- parameter_short
  child_env$step <- counting
  child_env$step_title <- paste0("Treatment: ", last_path(sub_list_dir[counting + 1]))
  child_env$child_header <- "##"
  child_env$unique_analyse <- FALSE
  child_env$debug <- TRUE
  
  new_path <- file.path(fig.path, "Comparison", paste0(gsub("Markdown/", "", sub_list_dir[counting]), "_", gsub("Markdown//", "", sub_list_dir[counting + 1])))
  dir.create(new_path, recursive = TRUE)
  
  child_env$fig.path <- new_path
  
  if (!identical(readRDS(child_env$parameter_init), readRDS(child_env$parameter_final))) {
    
    # if (file.exists(paste0(counting, "res.rds"))) {
    #   res <- readRDS(paste0(counting, "res.rds"))
    # } else {
      child_env_result <- comprehensive_cwp_dataframe_analysis(
        parameter_init = child_env$parameter_init,
        parameter_final = child_env$parameter_final,
        fig.path = child_env$fig.path,
        parameter_fact = "catch",
        parameter_short = child_env$parameter_short,
        parameter_columns_to_keep = c("Precision", "measurement_unit", "Values dataset 1", "Values dataset 2", "Loss / Gain", "Difference (in %)", "Dimension", "Difference in value"),
        parameter_diff_value_or_percent = "Difference (in %)",
        parameter_UNK_for_not_standards_unit = TRUE,
        parameter_mapped = child_env$parameter_mapped,
        parameter_filtering = list(species = NULL, fishing_fleet = NULL),
        parameter_time_dimension = c("time_start"),
        parameter_geographical_dimension = "geographic_identifier",
        parameter_geographical_dimension_groupping = "GRIDTYPE",
        coverage = child_env$coverage,
        parameter_colnames_to_keep = "all",
        outputonly = FALSE,
        print_map = TRUE,
        shapefile_fix = shapefile_fix,
        continent = continent,
        parameter_resolution_filter = NULL,
        parameter_titre_dataset_1 = child_env$parameter_titre_dataset_1,
        parameter_titre_dataset_2 = "Dataset 2",
        unique_analyse = child_env$unique_analyse
      )
      child_env$step_title_t_f <- TRUE
      list2env(as.list(child_env), envir = .GlobalEnv)
      list2env(as.list(child_env_result), envir = .GlobalEnv)
      # saveRDS(child_env_result, paste0(counting, "child_env_result.rds"))
      
      cat("rendering rmd")

      res <- knitr::knit_child("comparison.Rmd")
      saveRDS(res, paste0(counting, "res.rds"))
      
      cat("rmd rendered")
      
      rm(child_env_result)
      gc()
    # }
    
    return(res)
  } else {
    return(NA)
  }
}



final_step <- (length(sub_list_dir_2)-1)
# all <- lapply(1:final_step, function_all_comp, continent = continent, shapefile_fix = shapefile.fix)
# all <- all[!is.na(all)]
# 
# all_rmd <-lapply(all, )
all_rmd <- lapply(1:3, function_all_comp, continent = continent, shapefile_fix = shapefile.fix)

saveRDS(all_rmd, "allrmd.rds")
source("~/firms-gta/geoflow-tunaatlas/Analysis_markdown/functions/process_fisheries_data.R")
process_fisheries_data_list <- process_fisheries_data(sub_list_dir_2, parameter_fact = "catch", parameter_filtering)
saveRDS(process_fisheries_data_list, "process_fisheries_data_list.rds")

# child_env_result$step_title_t_f <- FALSE
# child_env_result$child_header <- ""
fig.path <- "figure"


# child_env_result_env <- new.env(parent = environment())
# list2env(child_env_result, child_env_result_env)
source("~/firms-gta/geoflow-tunaatlas/Analysis_markdown/functions/comprehensive_cwp_dataframe_analysis.R")
source("~/firms-gta/geoflow-tunaatlas/Analysis_markdown/functions/file_formatting.R")
shapefile_fix <- shapefile.fix
rmarkdown::render(input = "tableau_recap.Rmd", envir = environment(), output_format = "html_document2" )



launching_comparison_between_two_datasets = function(child_env_result){
  child_env_result <- comprehensive_cwp_dataframe_analysis(
    parameter_init = child_env_result$parameter_init,
    parameter_final = child_env_result$parameter_final,
    fig.path = child_env_result$fig.path,
    parameter_fact = "catch",
    parameter_short = child_env_result$parameter_short,
    parameter_columns_to_keep = c("Precision", "measurement_unit", "Values dataset 1", "Values dataset 2", "Loss / Gain", "Difference (in %)", "Dimension", "Difference in value"),
    parameter_diff_value_or_percent = "Difference (in %)",
    parameter_UNK_for_not_standards_unit = TRUE,
    parameter_mapped = child_env_result$parameter_mapped,
    parameter_filtering = list(species = NULL, fishing_fleet = NULL),
    parameter_time_dimension = c("time_start"),
    parameter_geographical_dimension = "geographic_identifier",
    parameter_geographical_dimension_groupping = "GRIDTYPE",
    coverage = child_env_result$coverage,
    parameter_colnames_to_keep = "all",
    outputonly = FALSE,
    print_map = TRUE,
    shapefile_fix = shapefile_fix,
    continent = continent,
    parameter_resolution_filter = NULL,
    parameter_titre_dataset_1 = child_env_result$parameter_titre_dataset_1,
    parameter_titre_dataset_2 = "Dataset 2",
    unique_analyse = child_env_result$unique_analyse
  )
}
