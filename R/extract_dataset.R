extract_dataset = function (con, metadata_dataset, labels = FALSE) 
{
  if (nrow(metadata_dataset) == 0) {
    stop("There is no dataset that corresponds to your query")
  }
  if (labels == FALSE) {
    query <- getSQLSardaraQueries(con, metadata_dataset)$query_CSV
  }
  else {
    query <- getSQLSardaraQueries(con, metadata_dataset)$query_CSV_with_labels
  }
  gta_require_con(con, "Reading a dataset stored in the database (extract_dataset)")
  df <- dbGetQuery(con, query)
  return(df)
}
