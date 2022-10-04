get_codelist_of_dimension <- function (con, dataset_metadata, dimension_name) 
{
  db_dimensions_parameters <- read.csv(system.file("extdata", 
                                                   "db_dimensions_parameters.csv", package = "rtunaatlas"), 
                                       stringsAsFactors = F)
  db_dimensions_parameters <- db_dimensions_parameters %>% 
    filter(dimension == dimension_name)
  dataset_name_codelist_dimension_query <- paste0("select distinct(", 
                                                  db_dimensions_parameters$db_tablesource_colname, ") from ", 
                                                  dataset_metadata$database_table_name, " tab join ", 
                                                  db_dimensions_parameters$db_tablename, " tab_link on tab_link.", 
                                                  db_dimensions_parameters$db_pkattribute_colname, "=tab.", 
                                                  db_dimensions_parameters$db_pkattribute_colname, " where tab.", 
                                                  db_dimensions_parameters$db_pkattribute_colname, "<>0 and tab.id_metadata=", 
                                                  dataset_metadata$id_metadata)
  dataset_name_codelist_dimension <- dbGetQuery(con, dataset_name_codelist_dimension_query)
  metadata_codelist_dimension = dbGetQuery(con, paste0("SELECT * FROM metadata.metadata where identifier='", 
                                                       dataset_name_codelist_dimension, "'"))
  return(metadata_codelist_dimension)
}
