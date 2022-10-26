# For a given dimension, some values might exist in the dataset to upload but not in the corresponding table of the database. The function FUNuploadNewRecordsToDB uploads these records in the proper tables of the DB.
# Example: 
# The dataset to upload has a record for time that is:
# time_start = 2020-01-01    time_end=2022-01-01
# This record does not exist in the table "time.time" of the DB. The function FUNuploadNewRecordsToDB will upload this record on the DB so as to get an ID.
FUNuploadNewRecordsToDB<-function(DBconnection,     # connection to DB
                                  inputDatasetMergedWithDBCodeList,   # Dataset to upload, merged with the table in the DB for the dimension to consider (DBTableName). For the considered dimension, the records that are present in the dataset to upload but not present in DBTableName will be marked as NA. 
                                  DBTableName,   # Name of the table in the DB for the dimension to consider. e.g. "time.time"
                                  DB_PK_AttributeName,     # Name of the primary key attribute in the table in the DB. e.g. "id_time"
                                  dimension_colnames_to_retrieve     # Name of the column(s) to fill with new values within the DB, separated by commas if multiple columns. e.g. "time_start,time_end"
){
  
  # Check
  index.na<-which(is.na(inputDatasetMergedWithDBCodeList[,DB_PK_AttributeName]))
  
  if (length(index.na)>0){
    column_names<-strsplit(dimension_colnames_to_retrieve,",")[[1]]
    CodesToLoad<-unique(inputDatasetMergedWithDBCodeList[index.na,column_names])
    if(DBTableName=="time.time"){
      CodesToLoad$time_start<-gsub("12:", "00:", CodesToLoad$time_start)
      CodesToLoad$time_end<-gsub("12:", "00:", CodesToLoad$time_end)
    }
    sql4 <- paste0("COPY  ", DBTableName, "(",dimension_colnames_to_retrieve,") FROM STDIN NULL 'NA' ")
    postgresqlpqExec(DBconnection, sql4)
    postgresqlCopyInDataframe(DBconnection, CodesToLoad)
    rs <- postgresqlgetResult(DBconnection)
    
  }
  return(rs)
}