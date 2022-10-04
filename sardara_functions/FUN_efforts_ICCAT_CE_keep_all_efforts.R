FUN_efforts_ICCAT_CE_keep_all_efforts = function (CE_dataset_after_FUN_efforts_ICCAT_CE, vector_colnames_efforts, 
          vector_colnames_effortunits) 
{
  colnames_colsToKeep <- setdiff(colnames(CE_dataset_after_FUN_efforts_ICCAT_CE), 
                                 c(vector_colnames_efforts, vector_colnames_effortunits))
  efforts_pivot_ICCAT2 <- NULL
  for (i in 1:length(vector_colnames_efforts)) {
    eff_dataset <- CE_dataset_after_FUN_efforts_ICCAT_CE[colnames_colsToKeep]
    efforts_pivot_ICCAT_temp <- cbind.data.frame(eff_dataset, 
                                                 CE_dataset_after_FUN_efforts_ICCAT_CE[, vector_colnames_efforts[i]], 
                                                 CE_dataset_after_FUN_efforts_ICCAT_CE[, vector_colnames_effortunits[i]], 
                                                 stringsAsFactors = FALSE)
    colnames(efforts_pivot_ICCAT_temp) <- c(colnames_colsToKeep, 
                                            "Effort", "EffortUnits")
    efforts_pivot_ICCAT2 <- rbind.data.frame(efforts_pivot_ICCAT2, 
                                             efforts_pivot_ICCAT_temp)
  }
  efforts_pivot_ICCAT2$Effort <- gsub(",", ".", efforts_pivot_ICCAT2$Effort)
  efforts_pivot_ICCAT2$Effort <- as.numeric(efforts_pivot_ICCAT2$Effort)
  return(efforts_pivot_ICCAT2)
}
