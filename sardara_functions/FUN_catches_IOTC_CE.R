FUN_catches_IOTC_CE = function (Path_to_CE_dataset, last_column_not_catch_value, CE_dataset_nature) 
{
  IOTC_CE <- read.table(Path_to_CE_dataset, sep = ",", header = TRUE, 
                        stringsAsFactors = FALSE, strip.white = TRUE)
  IOTC_CE[, c("Fleet", "iGrid")] <- as.data.frame(apply(IOTC_CE[, 
                                                                c("Fleet", "iGrid")], 2, function(x) {
                                                                  gsub(" *$", "", x)
                                                                }), stringsAsFactors = FALSE)
  for (i in 1:dim(IOTC_CE)[2]) {
    index <- which(is.na(IOTC_CE[, i]))
    if (length(index) > 0) {
      IOTC_CE[index, i] <- 0
    }
  }
  IOTC_CE <- data.table(IOTC_CE)
  IOTC_CE <- melt(IOTC_CE, id.vars = colnames(IOTC_CE)[1:last_column_not_catch_value])
  IOTC_CE <- as.data.frame(IOTC_CE)
  IOTC_CE <- IOTC_CE %>% dplyr::filter(!value %in% 0) %>% 
    dplyr::filter(!is.na(value))
  IOTC_CE$Species <- sub("\\..*", "", IOTC_CE$variable)
  if (CE_dataset_nature == "Surface") {
    IOTC_CE$School <- sub(".*\\.", "", IOTC_CE$variable)
  }
  else {
    IOTC_CE$CatchUnits <- sub(".*\\.", "", IOTC_CE$variable)
    IOTC_CE$School <- "ALL"
  }
  number_of_units_by_strata <- group_by_(IOTC_CE, .dots = setdiff(colnames(IOTC_CE), 
                                                                  c("value", "CatchUnits", "variable"))) %>% summarise(count = n())
  strata_in_number_and_weight <- number_of_units_by_strata[number_of_units_by_strata$count > 
                                                             1, ]
  IOTC_CE <- left_join(IOTC_CE, strata_in_number_and_weight, 
                       by = setdiff(colnames(strata_in_number_and_weight), 
                                    "count"))
  index.catchinweightandnumber <- which(IOTC_CE[, "count"] == 
                                          2 & IOTC_CE[, "CatchUnits"] == "NO")
  IOTC_CE[index.catchinweightandnumber, "CatchUnits"] = "NOMT"
  index.catchinweightandnumber <- which(IOTC_CE[, "count"] == 
                                          2 & IOTC_CE[, "CatchUnits"] == "MT")
  IOTC_CE[index.catchinweightandnumber, "CatchUnits"] = "MTNO"
  return(IOTC_CE)
}
