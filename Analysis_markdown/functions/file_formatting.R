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



## ----mappingifnot, include=FALSE, eval=TRUE-------------------


if(!parameter_mapped){
  
  con <- DBI::dbConnect(RPostgreSQL::PostgreSQL(),
host = config$software$input$dbi_config$parameters$host, port = config$software$input$dbi_config$parameters$port,  user = config$software$input$dbi_config$parameters$user,dbname=config$software$input$dbi_config$parameters$dbname, password = config$software$input$dbi_config$parameters$password)
  
mapping_csv_mapping_datasets_url <- "https://raw.githubusercontent.com/fdiwg/fdi-mappings/main/global/firms/gta/codelist_mapping_rfmos_to_global.csv"
      mapping_dataset <-
        read.csv(
          mapping_csv_mapping_datasets_url,
          stringsAsFactors = F,
          colClasses = "character"
        )
  mapping_keep_src_code <- FALSE

  source("https://raw.githubusercontent.com/eblondel/geoflow-tunaatlas/master/tunaatlas_scripts/generation/map_codelists.R")
  init <- map_codelists(con = con, fact = parameter_fact, mapping_dataset = mapping_dataset, dataset_to_map = init, mapping_keep_src_code = FALSE, summary_mapping = TRUE)$dataset_mapped
        
  #this map condelist function is to retrieve the mapping dataset used
  # final <- map_codelists(conn, parameter_fact, mapping_dataset, final, mapping_keep_src_code = FALSE, summary_mapping = FALSE)
  dbDisconnect(con)
}



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




