#' Raise Datasets by Dimension
#'
#' This function harmonizes two datasets that have been stratified by different dimensions
#' by computing proportional factors and applying them to adjust measurement values.
#'
#' @param df1 A data frame representing the first dataset, used for calculating raising factors.
#' @param df2 A data frame representing the second dataset, to which the adjustments will be applied.
#' @param dimension_missing_df1 A character string representing the dimension (column) that is missing in `df1` but present in `df2`.
#' @param dimension_missing_df2 A character string representing the dimension (column) that is missing in `df2` but present in `df1`.
#' @return A list containing the adjusted data frame (`df`).
#' @examples
#' df1 <- data.frame(fishingFleet = c("A", "B"), measurement_value = c(100, 200))
#' df2 <- data.frame(schoolType = c("S1", "S2"), measurement_value = c(50, 150))
#' result <- raise_datasets_by_dimension(df1, df2, dimension_missing_df1 = "schoolType", dimension_missing_df2 = "fishingFleet")
#' @export
raise_datasets_by_dimension <- function(df1, df2, dimension_missing_df1, dimension_missing_df2) {
  
  # Print a message indicating which dimensions are being raised
  cat(paste0("\nRaising ", dimension_missing_df1, " to ", dimension_missing_df2))
  
  # Combine unique column names from both data frames
  colnames_input_dataset <- unique(c(colnames(df1), colnames(df2)))
  
  # Identify the columns used for raising and raised dimensions
  RaisingDataset_RaisingDimensionsColNames <- setdiff(colnames_input_dataset, c(dimension_missing_df1, "measurement_value"))
  RaisedDataset_RaisedDimensionsColNames <- setdiff(colnames_input_dataset, c(dimension_missing_df2, "measurement_value"))
  
  RaisedDataset_RaisedDimension <- dimension_missing_df1
  RaisingDataset_RaisingDimension <- dimension_missing_df2
  
  # Summarize the data in df1 by the raising dimensions
  RaisingDataset_ByEachRaisingDimension <- df1 %>%
    ungroup() %>%
    group_by(across(all_of(RaisingDataset_RaisingDimensionsColNames))) %>%
    summarise(measurement_value = sum(measurement_value))
  
  # Summarize the data in df1 by all raising dimensions except the one being raised
  RaisingDataset_AllRaisingDimension <- df1 %>%
    group_by(across(setdiff(RaisingDataset_RaisingDimensionsColNames, RaisingDataset_RaisingDimension))) %>%
    summarise(measurement_value = sum(measurement_value))
  
  # Merge the summaries to calculate the raising factors (rf)
  Percentage_made_in_each_stratum_byeachRaisingDimension <- merge(
    RaisingDataset_ByEachRaisingDimension, 
    RaisingDataset_AllRaisingDimension, 
    by = setdiff(RaisingDataset_RaisingDimensionsColNames, RaisingDataset_RaisingDimension), 
    all.x = TRUE
  )
  
  # Calculate the raising factors
  Percentage_made_in_each_stratum_byeachRaisingDimension$rf <- Percentage_made_in_each_stratum_byeachRaisingDimension$measurement_value.x / Percentage_made_in_each_stratum_byeachRaisingDimension$measurement_value.y
  
  # Apply the raising factors to df2
  RaisedDF <- merge(
    Percentage_made_in_each_stratum_byeachRaisingDimension, 
    df2, 
    by.x = setdiff(RaisingDataset_RaisingDimensionsColNames, RaisingDataset_RaisingDimension), 
    by.y = setdiff(RaisedDataset_RaisedDimensionsColNames, RaisedDataset_RaisedDimension), 
    all.y = TRUE
  )
  
  # Calculate the raised values
  RaisedDF$raised_value <- RaisedDF$rf * RaisedDF$measurement_value
  
  # Remove rows with NA raised values
  RaisedDF <- RaisedDF[!is.na(RaisedDF$raised_value), ]
  
  # Rename columns for clarity
  colnames(RaisedDF)[which(colnames(RaisedDF) == paste0(RaisingDataset_RaisingDimension, ".x"))] <- RaisingDataset_RaisingDimension
  RaisedDF$measurement_value <- NULL
  colnames(RaisedDF)[which(colnames(RaisedDF) == "raised_value")] <- "measurement_value"
  
  # Ensure the final data frame has the same columns as the input data frames
  RaisedDF <- RaisedDF[colnames_input_dataset]
  
  # Return the adjusted data frame as a list
  return(list(df = RaisedDF))
}
