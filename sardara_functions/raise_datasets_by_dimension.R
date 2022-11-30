raise_datasets_by_dimension <- function (df1, df2, dimension_missing_df1, dimension_missing_df2) 
{
  cat(paste0("\nRaising ", dimension_missing_df1, " to ", 
             dimension_missing_df2))
  colnames_input_dataset <- unique(c(colnames(df1), c(colnames(df2))))
  RaisingDataset_RaisingDimensionsColNames <- setdiff(colnames_input_dataset, 
                                                      c(dimension_missing_df1, "value"))
  RaisedDataset_RaisedDimensionsColNames <- setdiff(colnames_input_dataset, 
                                                    c(dimension_missing_df2, "value"))
  RaisedDataset_RaisedDimension <- dimension_missing_df1
  RaisingDataset_RaisingDimension <- dimension_missing_df2
  RaisingDataset_ByEachRaisingDimension <- group_by_(df1, 
                                                     .dots = RaisingDataset_RaisingDimensionsColNames) %>% 
    summarise(value = sum(value))
  RaisingDataset_AllRaisingDimension <- group_by_(df1, .dots = setdiff(RaisingDataset_RaisingDimensionsColNames, 
                                                                       RaisingDataset_RaisingDimension)) %>% summarise(value = sum(value))
  Percentage_made_in_each_stratum_byeachRaisingDimension <- merge(RaisingDataset_ByEachRaisingDimension, 
                                                                  RaisingDataset_AllRaisingDimension, by = setdiff(RaisingDataset_RaisingDimensionsColNames, 
                                                                                                                   RaisingDataset_RaisingDimension), all.x = TRUE)
  Percentage_made_in_each_stratum_byeachRaisingDimension$rf <- Percentage_made_in_each_stratum_byeachRaisingDimension$value.x/Percentage_made_in_each_stratum_byeachRaisingDimension$value.y
  RaisedDF <- merge(Percentage_made_in_each_stratum_byeachRaisingDimension, 
                    df2, by.x = setdiff(RaisingDataset_RaisingDimensionsColNames, 
                                        RaisingDataset_RaisingDimension), by.y = setdiff(RaisedDataset_RaisedDimensionsColNames, 
                                                                                         RaisedDataset_RaisedDimension), all.y = TRUE)
  RaisedDF$raised_value <- RaisedDF$rf * RaisedDF$value
  RaisedDF = RaisedDF[!is.na(RaisedDF$raised_value), ]
  colnames(RaisedDF)[which(colnames(RaisedDF) == paste0(RaisingDataset_RaisingDimension, 
                                                        ".x"))] <- RaisingDataset_RaisingDimension
  RaisedDF$value <- NULL
  colnames(RaisedDF)[which(colnames(RaisedDF) == "raised_value")] <- "value"
  RaisedDF <- RaisedDF[colnames_input_dataset]
  return(list(df = RaisedDF))
}
