#re-written from https://raw.githubusercontent.com/ptaconet/rtunaatlas_scripts/master/tunaatlas_world/create_own_tuna_atlas/sourced_scripts/map_code_lists.R
map_codelists<-function(con, fact, mapping_dataset,dataset_to_map, mapping_keep_src_code = FALSE, summary_mapping = FALSE, source_authority_to_map = c("IATTC", "CCSBT", "WCPFC))")){
  # Get the dimensions to map from the mapping_dataset
  source("https://raw.githubusercontent.com/firms-gta/geoflow-tunaatlas/master/map_codelist.R")
  source("https://raw.githubusercontent.com/firms-gta/geoflow-tunaatlas/master/sardara_functions/extract_dataset.R")
  source("https://raw.githubusercontent.com/firms-gta/geoflow-tunaatlas/master/sardara_functions/list_metadata_datasets.R")
  
  if (fact=="catch"){
    dimension_to_map<-c("gear","species","fishingfleet","schooltype","catchtype")
    dimension_to_map <- dimension_to_map[dimension_to_map%in%colnames(dataset_to_map)]
  } else if (fact=="effort"){
    dimension_to_map<-c("gear","fishingfleet","schooltype","unit")
    dimension_to_map <- dimension_to_map[dimension_to_map%in%colnames(dataset_to_map)]
  }
  `%notin%` <- Negate(`%in%`)
  data_not_to_map <- dataset_to_map %>% filter(source_authority %notin% source_authority_to_map)
  dataset_to_map <- dataset_to_map %>% filter(source_authority %in% source_authority_to_map)
  
  recap_mapping <- NULL
  not_mapped_total <- NULL
  stats_total <- NULL
  # One by one, map the dimensions
  for (i in 1:length(dimension_to_map)){ # i takes the values of the dimensions to map
    dimension <- dimension_to_map[i]
    if (dimension %in% colnames(dataset_to_map)){
      mapping_dataset_this_dimension<-mapping_dataset %>% dplyr::filter (dimensions_to_map == dimension)
      df_mapping_final_this_dimension<-NULL
      for (j in 1:nrow(mapping_dataset_this_dimension)){ # With this loop, we extract one by one, for 1 given dimension, the code list mapping datasets from the DB. The last line of the loop binds all the code list mappings datasets for this given dimension.
        df_mapping<-extract_dataset(con,list_metadata_datasets(con,identifier=mapping_dataset_this_dimension$db_mapping_dataset_name[j]))  # Extract the code list mapping dataset from the DB
        df_mapping$source_authority<-mapping_dataset_this_dimension$source_authority[j]  # Add the dimension "source_authority" to the mapping dataset. That dimension is not included in the code list mapping datasets. However, it is necessary to map the code list.
        df_mapping_final_this_dimension<-rbind(df_mapping_final_this_dimension,df_mapping)
      }
      recap_mapping <- rbind(df_mapping_final_this_dimension, recap_mapping)
      
      mapping <- map_codelist(dataset_to_map,df_mapping_final_this_dimension,dimension,mapping_keep_src_code)
      dataset_to_map <- mapping$df  # Codes are mapped by tRFMOs (source_authority)
      stats <- mapping$stats
      not_mapped <- mapping$not_mapped
      
      not_mapped_total <- rbind(not_mapped_total, not_mapped)
      
      stats_total <- rbind(stats_total, stats)
    }
  }
  
  dataset_mapped <- rbind(dataset_to_map, data_not_to_map)
  
  if(summary_mapping){dataset_mapped <- list(dataset_mapped = dataset_mapped, summary_mapping =recap_mapping, stats_total = stats_total, not_mapped_total = not_mapped_total)
  }else {
    dataset_mapped <- dataset_mapped
  }
  return(dataset_mapped)
}
