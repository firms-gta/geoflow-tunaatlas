create_materialized_view_for_shiny_apps <- function(config, software, software_config){
  db_name <- software_config$parameters$dbname
  repository_sql_scripts_database_deployment <- here::here( "./R/tunaatlas_sql")
  outsql <- paste("-- Creation of the materialized view for shiny apps in ", db_name)
  
  fileName <- paste(repository_sql_scripts_database_deployment,"i6i7i8.sql",sep="/")
  outsql <- paste(outsql, '-- view i6i7i8 for tunaatlas_pie_map', sep = "\n")
  sql_create_view <- paste(readLines(fileName), collapse="\n")
  outsql <- paste(outsql, sql_create_view, sep="\n")
  
  ## add all the .sql needed on the basis of the last 4 lines
  return(outsql)
}
