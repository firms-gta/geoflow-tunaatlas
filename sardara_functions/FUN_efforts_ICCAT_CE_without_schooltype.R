FUN_efforts_ICCAT_CE_without_schooltype = function (RFMO_CE, RFMO_CE_species_colnames) 
{
  efforts_pivot_ICCAT <- RFMO_CE[, -which(names(RFMO_CE) %in% 
                                            RFMO_CE_species_colnames)]
  efforts_pivot_ICCAT <- efforts_pivot_ICCAT[, -which(names(efforts_pivot_ICCAT) %in% 
                                                        c("DSetType", "CatchUnit"))]
  efforts_pivot_ICCAT <- efforts_pivot_ICCAT %>% filter(!Eff1 %in% 
                                                          0)
  efforts_pivot_ICCAT <- unique(efforts_pivot_ICCAT)
  return(efforts_pivot_ICCAT)
}
