FUN_catches_ICCAT_CE = function (RFMO_CE, RFMO_CE_species_colnames) 
{
  for (i in 1:dim(RFMO_CE)[2]) {
    index <- which(is.na(RFMO_CE[, i]))
    if (length(index) > 0) {
      RFMO_CE[index, i] <- 0
    }
  }
  RFMO_CE <- data.table(RFMO_CE)
  RFMO_CE <- melt(RFMO_CE, id.vars = setdiff(colnames(RFMO_CE), 
                                             RFMO_CE_species_colnames))
  RFMO_CE <- as.data.frame(RFMO_CE)
  RFMO_CE <- RFMO_CE %>% dplyr::filter(!value %in% 0) %>% 
    dplyr::filter(!is.na(value))
  RFMO_CE$variable <- as.character(RFMO_CE$variable)
  return(RFMO_CE)
}
