## ----globalvariables, include=FALSE---------------------------

if(!exists("fig.path")){
  fig.path <- getwd()
}

formals(qflextable2, envir = environment())$fig.pathinside = fig.path
formals(save_image, envir = environment())$fig.pathinside = fig.path

if(!exists("step_title")){step_title_t_f = FALSE} else{step_title_t_f = TRUE}


if(!exists("parameter_fact") || is.null(parameter_fact)){parameter_fact <-  "catch"}

if(is_null_or_not_exist(parameter_short)){parameter_short <-  FALSE}
if(is_null_or_not_exist(parameter_columns_to_keep)){
  parameter_columns_to_keep <- c("Precision", "measurement_unit", "Values dataset 1",
                                 "Values dataset 2", "Loss / Gain",
                                 "Difference (in %)", "Dimension",
                                 "Difference in value")
}
if(is_null_or_not_exist("parameter_diff_value_or_percent")){
     parameter_diff_value_or_percent <- "Difference (in %)" 
    }
if(is_null_or_not_exist(parameter_UNK_for_not_standards_unit)){
  parameter_UNK_for_not_standards_unit <- TRUE
}

if(is_null_or_not_exist(parameter_mapped)){parameter_mapped <- TRUE}

if(is_null_or_not_exist(parameter_filtering)){parameter_filtering <- list(species = NULL, fishing_fleet = NULL)}
if(is_null_or_not_exist(parameter_time_dimension)){parameter_time_dimension = c("time_start")}

if(is_null_or_not_exist(parameter_geographical_dimension)){parameter_geographical_dimension = "geographic_identifier"}
if(is_null_or_not_exist(parameter_geographical_dimension_groupping)){parameter_geographical_dimension_groupping = "GRIDTYPE"}
if(is_null_or_not_exist(coverage)){coverage = TRUE}


if(is_null_or_not_exist(parameter_colnames_to_keep)){parameter_colnames_to_keep <-NULL}

if (!exists("parameter_titre_dataset_1") || is.null(parameter_titre_dataset_1)){
  titre_1 <- last_path_reduced(as.character(parameter_init))
} else {
  titre_1 <- parameter_titre_dataset_1
}

matchingList <- parameter_filtering %>% purrr::keep( ~ !is.null(.) )

if(is_null_or_not_exist(outputonly)){
  outputonly <- FALSE
}

if(is_null_or_not_exist(print_map)){
  print_map <- FALSE
}

if(is_null_or_not_exist(parameter_resolution_filter)){
  parameter_resolution_filter <- NULL
}

is_null_or_not_exist(parameter_final) {
  unique_analyse <- TRUE } else {
    unique_analyse <- FALSE}

