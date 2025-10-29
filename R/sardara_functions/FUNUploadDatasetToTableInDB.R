#function to upload a new dataset into database.
FUNUploadDatasetToTableInDB<-function(DBconnection,    # connection to DB
                                      InputDataset,    # Dataset to upload in the DB  (the columns of the dataset must have the same names as the ones of the database (TableNameInDB))
                                      TableNameInDB){  # Name of the table where the dataset will be uploaded, e.g. "fact_tables.total_cathes"
  
  
  InputDataset[is.na(InputDataset)] <- "NA" 
  
  sql4 <- paste0("COPY  ", TableNameInDB, "(",
                 paste0(names(InputDataset), collapse=", "), ") FROM STDIN NULL 'NA' ")
  postgresqlpqExec(DBconnection, sql4)
  postgresqlCopyInDataframe(DBconnection, InputDataset)
  rs <- postgresqlgetResult(DBconnection)
  
  return(rs)
}