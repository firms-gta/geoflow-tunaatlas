FUN_efforts_IOTC_CE = function (Path_to_CE_dataset, last_column_not_catch_value) 
{
  efforts_pivot_IOTC <- read.table(Path_to_CE_dataset, sep = ",", 
                                   header = TRUE, stringsAsFactors = FALSE, strip.white = TRUE)
  FScolumns <- colnames(efforts_pivot_IOTC)[grep("-FS", colnames(efforts_pivot_IOTC))]
  LScolumns <- colnames(efforts_pivot_IOTC)[grep("-LS", colnames(efforts_pivot_IOTC))]
  UNCLcolumns <- colnames(efforts_pivot_IOTC)[grep("-UNCL", 
                                                   colnames(efforts_pivot_IOTC))]
  efforts_pivot_IOTC$sumFS <- rowSums(efforts_pivot_IOTC[, 
                                                         FScolumns], na.rm = TRUE)
  efforts_pivot_IOTC$sumLS <- rowSums(efforts_pivot_IOTC[, 
                                                         LScolumns], na.rm = TRUE)
  efforts_pivot_IOTC$sumUNCL <- rowSums(efforts_pivot_IOTC[, 
                                                           UNCLcolumns], na.rm = TRUE)
  indexFS <- which(efforts_pivot_IOTC$sumFS > 0 & efforts_pivot_IOTC$sumLS == 
                     0 & efforts_pivot_IOTC$sumUNCL == 0)
  indexLS <- which(efforts_pivot_IOTC$sumFS == 0 & efforts_pivot_IOTC$sumLS != 
                     0 & efforts_pivot_IOTC$sumUNCL == 0)
  indexUNCL <- setdiff(rownames(efforts_pivot_IOTC), c(indexFS, 
                                                       indexLS))
  efforts_pivot_IOTC <- efforts_pivot_IOTC[, -(last_column_not_catch_value:ncol(efforts_pivot_IOTC))]
  efforts_pivot_IOTC$SchoolEffort <- "IND"
  efforts_pivot_IOTC[indexFS, "SchoolEffort"] <- "FS"
  efforts_pivot_IOTC[indexLS, "SchoolEffort"] <- "LS"
  efforts_pivot_IOTC[indexUNCL, "SchoolEffort"] <- "UNCL"
  efforts_pivot_IOTC <- efforts_pivot_IOTC %>% dplyr::filter(!Effort %in% 
                                                               0)
  return(efforts_pivot_IOTC)
}
