# This function merges the codes of a dimension of a dataset to load with the ones of the code list in the database (integers)
# It works for the dimensions time and sizeclass

FUNMergeDimensions_NonCodeListLike<-function(DBconnection,   # connection to DB
                                             inputDataset,   # Input harmonized dataset
                                             DB_PK_AttributeName,     # Name of the primary key attribute in the dimension table in DB, i.e. "id_time"
                                             dimension_colnames_to_retrieve,   # Name of the columns that will be used to match inputDataset and DBTableName
                                             DBTableName   # Name of the dimension table in DB, i.e. "time.time"
){
  if(!(require(dplyr))){ 
    install.packages(dplyr) 
    (require(dplyr))} 
  
  if(DBTableName=="time.time"){
    sql<-"select id_time,to_char(time_start,'YYYY-MM-DD HH:MI:SS') as time_start,to_char(time_end,'YYYY-MM-DD HH:MI:SS') as time_end from time.time"
    
    # time in the DB are in format "YYYY-MM-DD HH:MM:SS" (timestamp without time zone). The data that we import must be with the same time format. When we do to_char function on a time without time stamp in postgresql which has HH:MI:SS = 00:00:00 , the output is 12:00:00 . Hence we fill the InputDataset with these value
    SetTimeStampFormat<-function(df,timeColumnName){
      index.time.withoutHours<-which(nchar(df[,timeColumnName])==10)
      if (length(index.time.withoutHours)>0){
        df[index.time.withoutHours,timeColumnName]<-paste(df[,timeColumnName]," 12:00:00",sep="")
      }
      return(df)
    }
    inputDataset<-SetTimeStampFormat(inputDataset,"time_start")
    inputDataset<-SetTimeStampFormat(inputDataset,"time_end")
  } else {
    sql<-paste("SELECT ",DB_PK_AttributeName,",",dimension_colnames_to_retrieve," from ",DBTableName,sep="")
  }
  
  CodeListFromDB<-dbGetQuery(DBconnection, sql)
  
  colnames_to_merge_vectorformat<-strsplit(dimension_colnames_to_retrieve,",")[[1]]
  
  # Merge inputDataset codes with Sardara codes
  inputDataset<-data.table(inputDataset)
  inputDatasetMergedWithDBCodeList<-merge(inputDataset,CodeListFromDB,by=colnames_to_merge_vectorformat, all.x=TRUE)
  inputDatasetMergedWithDBCodeList<-as.data.frame(inputDatasetMergedWithDBCodeList)
  
  return(inputDatasetMergedWithDBCodeList)
}
