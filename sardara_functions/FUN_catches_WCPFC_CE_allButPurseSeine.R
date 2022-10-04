FUN_catches_WCPFC_CE_allButPurseSeine = function (DF_Path) 
{
  DF <- read.dbf(DF_Path, as.is = TRUE)
  DF <- melt(DF, id = c(colnames(DF[1:5])))
  DF <- DF %>% filter(!value %in% 0) %>% filter(!is.na(value))
  DF$variable <- as.character(DF$variable)
  colnames(DF)[which(colnames(DF) == "variable")] <- "Species"
  DF$CatchUnits <- substr(DF$Species, nchar(DF$Species), nchar(DF$Species))
  DF$Species <- sub("_C", "", DF$Species)
  DF$Species <- sub("_N", "", DF$Species)
  DF$School <- "OTH"
  DF$EffortUnits <- colnames(DF[5])
  colnames(DF)[5] <- "Effort"
  return(DF)
}
