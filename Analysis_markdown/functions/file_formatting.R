## ----filereading----------------------------------------------

if(is.character(parameter_init)){
init <- readRDS(paste0(as.character(parameter_init)))
} else {
  init <- parameter_init
}

if(unique_analyse){
  final <- init[0,]

   if(is.character(parameter_final)){
final <- readRDS(paste0(as.character(parameter_final)))}
   else {
     final <- parameter_final
   }

 }

if (is_null_or_not_exist(parameter_titre_dataset_2) & !unique_analyse){
  titre_2 <- last_path_reduced(as.character(parameter_final))
  } else if (unique_analyse) {
  titre_2 <- "NONE"
} else {
    titre_2 <- parameter_titre_dataset_2
}

titre_2 <- gsub("_","-",titre_2)
titre_1 <- gsub("_","-",titre_1)

## ----filetidying----------------------------------------------

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




