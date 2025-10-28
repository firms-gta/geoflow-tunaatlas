list_metadata_datasets = function (con, identifier = NULL, source_authority = NULL) 
{
  where_clause <- NULL
  if (!is.null(identifier)) {
    where_clause <- paste0(where_clause, " and identifier = '", 
                           identifier, "'")
  }
  if (!is.null(source_authority)) {
    source_authority <- paste(source_authority, collapse = "','")
    where_clause <- paste0(where_clause, " and source IN ('", 
                           source_authority, "')")
  }
  metadata_datasets <- dbGetQuery(con, paste("SELECT * from metadata.metadata where lineage is not null ", 
                                             where_clause, " order by source,identifier", sep = ))
  if (nrow(metadata_datasets) == 0) {
    cat(paste0("There is no dataset that corresponds to your query"))
  }
  return(metadata_datasets)
}
