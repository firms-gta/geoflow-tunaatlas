FUNMergeDimensions_CodeListLike<-function(DBconnection,  # connection to DB
                                          DBTableName,  # Name of the dimension table in DB, i.e. "flag.flag"
                                          DB_PK_AttributeName,   # Name of the primary key attribute in the dimension table in DB, i.e. "id_flag"
                                          DB_CodeRFMO_AttributeName, # Name of attribute to merge in the dimension table in DB, i.e. "code_flag"
                                          DB_TableRFMO_AttributeName, # Name of the tables attribute (attribute that contains the name of the tables for which there can be data) in the dimension table in DB, i.e. "tablesource_flag"
                                          inputDataset,  # Input harmonized dataset
                                          HarmonizedDF_CodeRFMO_AttributeName, # Name of attribute to merge in the harmonized dataset, i.e. "Flag"
                                          DB_Tables_ToLookInto # List of tables that can contain the codes, i.e. "flag_iotc"
){
  
  # Retrieve the dimension code list from the DB
  
  sql<- paste("SELECT ",DB_PK_AttributeName,",",DB_CodeRFMO_AttributeName," FROM ",DBTableName," WHERE ",DB_TableRFMO_AttributeName," IN (",paste("'",gsub(",","','", DB_Tables_ToLookInto),"'",sep=""),")",sep="")
  CodeListFromDB <- dbGetQuery(DBconnection, sql)
  
  # merge code list from db with inputDataset
  
  inputDataset<-data.table(inputDataset)
  inputDatasetMergedWithDBCodeList<-merge(inputDataset,CodeListFromDB,by.x=HarmonizedDF_CodeRFMO_AttributeName,by.y=DB_CodeRFMO_AttributeName,all.x=TRUE)
  inputDatasetMergedWithDBCodeList<-as.data.frame(inputDatasetMergedWithDBCodeList)
  
  return(inputDatasetMergedWithDBCodeList)
}