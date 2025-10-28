#' Map Code Lists Across Tuna Atlas Datasets
#'
#' This function maps dimensions such as gear type, species, fishing fleet, fishing mode, and measurement 
#' type or unit from one dataset to another using specified mapping datasets. It allows for the mapping 
#' of data according to source authority (e.g., IATTC, CCSBT, WCPFC) and provides options to keep source 
#' codes and to generate summary mappings.
#'
#' @param con A database connection object, the Global Tuna Atlas database which is created by the beginning of the workflow.
#' @param fact The fact type to be mapped, either "catch" or "effort".
#' @param mapping_dataset A dataset containing the mapping instructions between different code lists.
#' @param dataset_to_map The dataset for which the code lists need to be mapped.
#' @param mapping_keep_src_code Logical, indicating whether to keep the source codes in the mapped dataset.
#' @param summary_mapping Logical, indicating whether to generate a summary of the mappings performed.
#' @param source_authority_to_map A character vector specifying the source authorities to be mapped.
#' @return A list containing the mapped dataset, a recap of the mappings, total statistics of the mappings, 
#' and details of the codes that were not mapped.
#' @examples
#' \donotshow{
#' # Assuming 'con' is a database connection, and 'mapping_dataset' is available
#' mapped_data <- map_codelists(con, fact = "catch",
#'                              mapping_dataset = your_mapping_dataset,
#'                              dataset_to_map = your_dataset_to_map,
#'                              mapping_keep_src_code = FALSE,
#'                              summary_mapping = TRUE,
#'                              source_authority_to_map = c("IATTC", "CCSBT", "WCPFC"))
#' }
#' @export
#' @importFrom dplyr filter bind_rows
#' @seealso \code{\link{extract_dataset}}, \code{\link{list_metadata_datasets}}
#'
#re-written from https://raw.githubusercontent.com/ptaconet/rtunaatlas_scripts/master/tunaatlas_world/create_own_tuna_atlas/sourced_scripts/map_code_lists.R
map_codelists<-function(con, fact, mapping_dataset,dataset_to_map, mapping_keep_src_code = FALSE, summary_mapping = FALSE, source_authority_to_map = c("IATTC", "CCSBT", "WCPFC")){
  # Get the dimensions to map from the mapping_dataset
  source("https://raw.githubusercontent.com/firms-gta/geoflow-tunaatlas/master/R/sardara_functions/map_codelist.R")
  source("https://raw.githubusercontent.com/firms-gta/geoflow-tunaatlas/master/R/sardara_functions/extract_dataset.R")
  source("https://raw.githubusercontent.com/firms-gta/geoflow-tunaatlas/master/R/sardara_functions/list_metadata_datasets.R")
  
  if (fact=="catch"){
    dimension_to_map<-c("gear_type","species","fishing_fleet","fishing_mode","measurement_type")
    dimension_to_map <- dimension_to_map[dimension_to_map%in%colnames(dataset_to_map)]
  } else if (fact=="effort"){
    dimension_to_map<-c("gear_type","fishing_fleet","fishing_mode","measurement_unit")
    dimension_to_map <- dimension_to_map[dimension_to_map%in%colnames(dataset_to_map)]
  }
  data_not_to_map <- dataset_to_map[!dataset_to_map$source_authority %in% source_authority_to_map,]
  dataset_to_map <- dataset_to_map[dataset_to_map$source_authority %in% source_authority_to_map,]
  if(nrow(dataset_to_map) == 0){
    return(list(dataset_mapped = data_not_to_map, recap_mapping  =NULL, stats_total = NULL, 
                not_mapped_total = NULL))
  }
  mapping_dataset <- mapping_dataset[mapping_dataset$source_authority %in% source_authority_to_map,]
  
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
        if(!"source_authority" %in% colnames(df_mapping)) df_mapping$source_authority<-mapping_dataset_this_dimension$source_authority[j]  # Add the dimension "source_authority" to the mapping dataset. That dimension is not included in the code list mapping datasets. However, it is necessary to map the code list.
        df_mapping_final_this_dimension<-rbind(df_mapping_final_this_dimension,df_mapping)
      }
      successful_mappings <- df_mapping_final_this_dimension %>%
        dplyr::filter(src_code %in% dataset_to_map[[dimension]])  
      
      if(summary_mapping) recap_mapping <- rbind(successful_mappings, recap_mapping)
      
      mapping <- map_codelist(dataset_to_map,df_mapping_final_this_dimension,dimension,mapping_keep_src_code)
      dataset_to_map <- mapping$df  # Codes are mapped by tRFMOs (source_authority)
      if(summary_mapping){
        stats <- mapping$stats
        not_mapped <- mapping$not_mapped
        not_mapped_total <- rbind(not_mapped_total, not_mapped)
        stats_total <- rbind(stats_total, stats)
      }
    }
  }
  
  dataset_mapped <- rbind(dataset_to_map, data_not_to_map)
  
  dataset_mapped <- list(dataset_mapped = dataset_mapped, recap_mapping  =recap_mapping, stats_total = stats_total, not_mapped_total = not_mapped_total)
  return(dataset_mapped)
}
