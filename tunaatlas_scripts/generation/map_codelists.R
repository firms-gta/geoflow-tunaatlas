#re-written from https://raw.githubusercontent.com/ptaconet/rtunaatlas_scripts/master/tunaatlas_world/create_own_tuna_atlas/sourced_scripts/map_code_lists.R
map_codelists <- function(con, fact, mapping_dataset,dataset_to_map, mapping_keep_src_code = FALSE){

  
  # Get the dimensions to map from the mapping_dataset
  if (fact=="catch"){
  dimension_to_map<-c("gear","species","flag","schooltype","catchtype")
  } else if (fact=="effort"){
    dimension_to_map<-c("gear","flag","schooltype","unit")
  }
  # One by one, map the dimensions
  for (i in 1:length(dimension_to_map)){ # i takes the values of the dimensions to map
    dimension <- dimension_to_map[i]
    if (dimension %in% colnames(dataset_to_map)){
      mapping_dataset_this_dimension<-mapping_dataset %>% filter (dimensions_to_map == dimension)  
      df_mapping_final_this_dimension<-NULL
      for (j in 1:nrow(mapping_dataset_this_dimension)){ # With this loop, we extract one by one, for 1 given dimension, the code list mapping datasets from the DB. The last line of the loop binds all the code list mappings datasets for this given dimension.
        df_mapping<-rtunaatlas::extract_dataset(con,list_metadata_datasets(con,identifier=mapping_dataset_this_dimension$db_mapping_dataset_name[j]))  # Extract the code list mapping dataset from the DB
        df_mapping$source_authority<-mapping_dataset_this_dimension$source_authority[j]  # Add the dimension "source_authority" to the mapping dataset. That dimension is not included in the code list mapping datasets. However, it is necessary to map the code list.
        df_mapping_final_this_dimension<-rbind(df_mapping_final_this_dimension,df_mapping)
      }
      dataset_to_map <- rtunaatlas::map_codelist(dataset_to_map,df_mapping_final_this_dimension,dimension,mapping_keep_src_code)$df  # Codes are mapped by tRFMOs (source_authority) 
    }
  }
  
  # fill metadata elements
  lineage <- paste0("Coding systems and nomenclatures used to describe the data may differ according to tRFMOs. Codes used by the tuna RFMOs in their respective datasets were mapped with global code lists for gear (ISSCFG), flag (ISO3 countries codes), and species (ASFIS). These mappings have been done with the collaboration of the tRFMOs Secretariats. Some codes could not be mapped to standard code lists, for some tRFMOs own-defined codes that usually are aggregation of existing codes (e.g. flag “IDPH” standing for Indonesia and Philippines within WCPFC or the species “Otun” standing for other tuna within for ICCAT). In those cases, the code was set to UNK (Unknown). For species and gears, these codes were mapped with more aggregated code lists, i.e. resp. group of species and groups of gears. Information regarding the data that have species set to Unknown, i.e. data for which raw species do not have any correspondence in ASFIS: the catches that have species set to Unknown represent percentage_catches_species_unknown_mt % of the catches expressed in weight and percentage_catches_species_unknown_no % of the catches expressed in number of fishes.")
  
  abstract <- "- Original codes were mapped with standard FAO code lists for gears (ISSCFG), species (ASFIS) and flags (ISO3 countries codes) in collaboration with the tRFMOs Secretariats.\n"
 
  info <- "- Some codes could not be mapped to standard code lists, for some tRFMOs own-defined codes that usually are aggregation of existing codes (e.g. flag “IDPH” standing for Indonesia and Philippines within WCPFC or the species “Otun” standing for other tuna within for ICCAT). In those cases, the code was set to UNK (Unknown). For species and gears, these codes were mapped with more aggregated code lists, i.e. resp. group of species and groups of gears.\n"
  
  return(list(dataset=dataset_to_map,lineage=lineage,abstract = abstract, info = info))
}

