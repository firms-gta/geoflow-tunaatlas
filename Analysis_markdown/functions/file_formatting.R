## ----filereading----------------------------------------------

library(readr)

# Function to read data based on file type
read_data <- function(file_path) {
  if (grepl("\\.rds$", file_path)) {
    readRDS(file_path)
  } else if (grepl("\\.csv$", file_path)) {
    fread(file_path)
  } else {
    stop("File type not supported")
  }
}

# Process 'parameter_init'
if (is.character(parameter_init)) {
  init <- read_data(parameter_init)
} else if (is.data.frame(parameter_init)) {
  init <- parameter_init
} else {
  stop("Invalid 'parameter_init'")
}

# Process 'parameter_final'
if (unique_analyse) {
  final <- init[0, ]
} else {
  if (is.character(parameter_final)) {
    final <- read_data(parameter_final)
  } else if (is.data.frame(parameter_final)) {
    final <- parameter_final
  } else {
    stop("Invalid 'parameter_final'")
  }
}

if (is_null_or_not_exist(parameter_titre_dataset_2) & !unique_analyse){
  
  if(!is.data.frame(parameter_final)){
    
    
    titre_2 <- last_path_reduced(as.character(parameter_final))
  } else {
    titre_2 <- "Dataset 2"
  }
} else if (unique_analyse) {
  titre_2 <- "NONE"
} else {
    titre_2 <- parameter_titre_dataset_2
}

titre_2 <- gsub("_","-",titre_2)
titre_1 <- gsub("_","-",titre_1)

## ----filetidying----------------------------------------------
parameter_colnames_to_keep <- unique(c(parameter_colnames_to_keep, parameter_geographical_dimension_groupping, parameter_geographical_dimension, parameter_time_dimension))

init <- tidying_data(init, parameter_colnames_to_keep_dataframe = parameter_colnames_to_keep, time_dimension = parameter_time_dimension)
final <- tidying_data(final, parameter_colnames_to_keep_dataframe = parameter_colnames_to_keep, time_dimension = parameter_time_dimension)

colnames_intersect <- intersect(colnames(init), colnames(final))

init <- init %>% dplyr::select(colnames_intersect)
final <- final %>% dplyr::select(colnames_intersect)



## ----geographic-identifier-and-not-standards-effort-----------

formals(function_geographic_identifier_renaming_and_not_standards_unit, envir = environment())$geo_dim = parameter_geographical_dimension
formals(function_geographic_identifier_renaming_and_not_standards_unit, envir = environment())$parameter_fact = parameter_fact
formals(function_geographic_identifier_renaming_and_not_standards_unit, envir = environment())$parameter_UNK_for_not_standards_unit = parameter_UNK_for_not_standards_unit
formals(function_geographic_identifier_renaming_and_not_standards_unit, envir = environment())$geo_dim_group = parameter_geographical_dimension_groupping

init <- function_geographic_identifier_renaming_and_not_standards_unit(init)
final <- function_geographic_identifier_renaming_and_not_standards_unit(final)




## ----filterprinting, echo=FALSE-------------------------------

if (!is.null(parameter_resolution_filter)) {

      filtering_resolution_filter <- function(datatable, first_digit) {
        filtered_data <- datatable[substr(datatable$geographic_identifier, 1, 1) == first_digit, ]
        return(filtered_data)
      }
    
      init <- filtering_resolution_filter(init, opts$resolution_filter)
      
      final <- filtering_resolution_filter(final, opts$resolution_filter)
    }



## ----filteringfunctionondata----------------------------------

formals(filtering_function, envir = environment())$parameter_filtering = parameter_filtering


init <- filtering_function(dataframe_to_filter = init)

if(unique_analyse){final <- init[0,]} else {final <- filtering_function(final)}

#' #' Process Datasets
#' #'
#' #' @param parameter_init Initial parameter, can be a path to a file or a dataframe.
#' #' @param parameter_final Final parameter, can be a path to a file or a dataframe.
#' #' @param unique_analyse Logical, whether to perform unique analysis.
#' #' @param parameter_titre_dataset_2 Title of the second dataset, optional.
#' #' @param parameter_colnames_to_keep Names of columns to keep.
#' #' @param parameter_geographical_dimension_groupping Geographical dimension grouping.
#' #' @param parameter_time_dimension Time dimension parameter.
#' #' @param parameter_geographical_dimension Geographical dimension parameter.
#' #' @param parameter_fact Fact parameter.
#' #' @param parameter_UNK_for_not_standards_unit UNK parameter for non-standard units.
#' #' @param parameter_resolution_filter Resolution filter parameter, optional.
#' #' @param parameter_filtering Filtering parameter.
#' #' @return A list containing processed initial and final datasets and their title
#' #' @export
#' process_datasets <- function(parameter_init, parameter_final, unique_analyse, parameter_titre_dataset_2 = NULL,
#'                              parameter_colnames_to_keep, parameter_geographical_dimension_groupping, 
#'                              parameter_time_dimension, parameter_geographical_dimension,
#'                              parameter_fact, parameter_UNK_for_not_standards_unit = TRUE, 
#'                              parameter_resolution_filter = NULL, parameter_filtering) {
#'   # Read data based on file type
#'   read_data <- function(file_path) {
#'     if (grepl("\\.rds$", file_path)) {
#'       readRDS(file_path)
#'     } else if (grepl("\\.csv$", file_path)) {
#'       fread(file_path)
#'     } else {
#'       stop("File type not supported")
#'     }
#'   }
#'   
#'   # Process 'parameter_init'
#'   if (is.character(parameter_init)) {
#'     init <- read_data(parameter_init)
#'   } else if (is.data.frame(parameter_init)) {
#'     init <- parameter_init
#'   } else {
#'     stop("Invalid 'parameter_init'")
#'   }
#'   
#'   # Process 'parameter_final'
#'   if (unique_analyse) {
#'     final <- init[0, ]
#'   } else {
#'     if (is.character(parameter_final)) {
#'       final <- read_data(parameter_final)
#'     } else if (is.data.frame(parameter_final)) {
#'       final <- parameter_final
#'     } else {
#'       stop("Invalid 'parameter_final'")
#'     }
#'   }
#'   
#'   # Other processing steps...
#'   # This includes handling of 'titre_2', 'titre_1', 'init', and 'final' as per the original script.
#'   # ...
#'   
#'   return(list(init = init, final = final, titre_1 = titre_1, titre_2 = titre_2))
#' }
#' 
#' 
#' 
#' 
