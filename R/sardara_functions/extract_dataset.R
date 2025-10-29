extract_dataset = function (con, metadata_dataset, labels = FALSE) 
{
  source("https://raw.githubusercontent.com/firms-gta/geoflow-tunaatlas/master/R/sardara_functions/getSQLSardaraQueries.R")
  if (nrow(metadata_dataset) == 0) {
    stop("There is no dataset that corresponds to your query")
  }
  if (labels == FALSE) {
    query <- getSQLSardaraQueries(con, metadata_dataset)$query_CSV
  }
  else {
    query <- getSQLSardaraQueries(con, metadata_dataset)$query_CSV_with_labels
  }
  df <- dbGetQuery(con, query)
  return(df)
}
