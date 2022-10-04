list_dataset_available_dimensions = function (con, dataset_metadata) 
{
  if (nrow(dataset_metadata) == 0) {
    stop("There is no dataset that corresponds to your query")
  }
  db_dimensions_parameters <- read.csv(system.file("extdata", 
                                                   "db_dimensions_parameters.csv", package = "rtunaatlas"), 
                                       stringsAsFactors = F)
  table_name = dataset_metadata$database_table_name
  id_metadata = dataset_metadata$id_metadata
  dataset_type = dataset_metadata$dataset_type
  if (!(dataset_type == "raw_dataset")) {
    stop("the dataset provided is not a raw_dataset. You must provide a dataset of type raw_dataset")
  }
  variable <- gsub("fact_tables.", "", table_name)
  columns_fact_tables <- dbGetQuery(con, paste0("select column_name from information_schema.columns where table_name='", 
                                                variable, "'"))
  available_dimensions_in_dataset <- db_dimensions_parameters[which(db_dimensions_parameters$db_fact_table_colname %in% 
                                                                      columns_fact_tables$column_name), ]
  available_dimensions_in_dataset_not_null <- NULL
  for (i in 1:nrow(available_dimensions_in_dataset)) {
    unique_values_dimension <- dbGetQuery(con, paste0("SELECT DISTINCT ", 
                                                      available_dimensions_in_dataset$db_fact_table_colname[i], 
                                                      " FROM ", table_name, " WHERE id_metadata=", id_metadata))
    available_dimensions_in_dataset_not_null <- c(available_dimensions_in_dataset_not_null, 
                                                  available_dimensions_in_dataset$dimension[i])
  }
  return(available_dimensions_in_dataset_not_null)
}
