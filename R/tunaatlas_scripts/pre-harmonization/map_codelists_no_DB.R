#' Map Code Lists Across Datasets Without Database
#'
#' This function maps dimensions such as gear type, species, fishing fleet,
#' fishing mode, and measurement type or unit from one dataset to another
#' using specified mapping datasets sourced from URLs. It is designed to handle
#' mappings based on source authority, with options to retain original codes
#' and generate summary mappings.
#'
#' @param fact A character string indicating the fact type to be mapped,
#'        either "catch" or "effort".
#' @param mapping_dataset A dataframe containing the mapping instructions between
#'        different code lists, including `source_authority` and `db_mapping_dataset_name`.
#' @param dataset_to_map A dataframe containing the dataset for which the code
#'        lists need to be mapped.
#' @param mapping_keep_src_code Logical, indicating whether to keep the source
#'        codes in the mapped dataset.
#' @param summary_mapping Logical, indicating whether to generate a summary
#'        of the mappings performed.
#' @param source_authority_to_map A character vector specifying the source
#'        authorities to be mapped.
#'
#' @return A list containing the mapped dataset, a recap of the mappings,
#'         total statistics of the mappings, and details of the codes that
#'         were not mapped.
#'
#' @examples
#' \donotshow{
#' # Example dataset and mapping dataset setup
#' dataset_to_map <- data.frame(
#'   source_authority = c("IATTC", "WCPFC"),
#'   gear_type = c("longline", "purse seine"),
#'   species = c("tuna", "shark")
#' )
#' mapping_dataset <- data.frame(
#'   source_authority = c("IATTC", "WCPFC"),
#'   dimensions_to_map = c("gear_type", "species"),
#'   db_mapping_dataset_name = c("codelist_gear_iattc", "codelist_species_wcpfc")
#' )
#' mapped_data <- map_codelists_no_DB(
#'   fact = "catch",
#'   mapping_dataset = mapping_dataset,
#'   dataset_to_map = dataset_to_map,
#'   mapping_keep_src_code = FALSE,
#'   summary_mapping = TRUE,
#'   source_authority_to_map = c("IATTC", "WCPFC")
#' )
#' }
#'
#' @export
#' 
#' 
library(readr)
library(dplyr)
# Re-written function to map code lists without a database


map_codelists_no_DB <- function(fact, mapping_dataset = "https://raw.githubusercontent.com/fdiwg/fdi-mappings/main/global/firms/gta/codelist_mapping_rfmos_to_global.csv", dataset_to_map, mapping_keep_src_code = FALSE, summary_mapping = FALSE, source_authority_to_map = c("IATTC", "CCSBT", "WCPFC")) {
  
if(!is.data.frame(mapping_dataset)){
  mapping_dataset <-read.csv(mapping_dataset,stringsAsFactors = F,colClasses = "character")
}  
  
  base_url <- "https://raw.githubusercontent.com/fdiwg/fdi-mappings/main/regional-to-global/"
  source("https://raw.githubusercontent.com/firms-gta/geoflow-tunaatlas/master/R/sardara_functions/map_codelist.R")
  
  if (fact == "catch") {
    dimension_to_map <- c("gear_type", "species", "fishing_fleet", "fishing_mode")
  } else if (fact == "effort") {
    dimension_to_map <- c("gear_type", "fishing_fleet", "fishing_mode", "measurement_unit")
  }
  source_authority_mapped <- unique(dataset_to_map$source_authority)
  dimension_to_map <- dimension_to_map[dimension_to_map %in% colnames(dataset_to_map)]
  data_not_to_map <- dataset_to_map[!dataset_to_map$source_authority %in% source_authority_to_map,]
  dataset_to_map <- dataset_to_map[dataset_to_map$source_authority %in% source_authority_to_map,]
  
  if (nrow(dataset_to_map) == 0) {
    return(list(dataset_mapped = data_not_to_map, recap_mapping = NULL, stats_total = NULL, not_mapped_total = NULL))
  }
  
  recap_mapping <- NULL
  not_mapped_total <- NULL
  stats_total <- NULL
  
  for (dimension in dimension_to_map) {
    mapping_dataset_this_dimension <- mapping_dataset %>% filter(dimensions_to_map == dimension, source_authority %in% source_authority_to_map)
    df_mapping_final_this_dimension <- NULL
    
    for (j in 1:nrow(mapping_dataset_this_dimension)) {
      file_name <- paste0(mapping_dataset_this_dimension$db_mapping_dataset_name[j], ".csv")
      mapping_url <- paste0(base_url, mapping_dataset_this_dimension$source_authority[j], "/", file_name)
      
      df_mapping <- tryCatch({
        read_csv(mapping_url)
      }, error = function(e) {
        message(paste("Failed to download or read the mapping file for", mapping_url))
        return(NULL)
      })
      
      if (!is.null(df_mapping)) {
        if (!"source_authority" %in% colnames(df_mapping)) df_mapping$source_authority <- mapping_dataset_this_dimension$source_authority[j]
        df_mapping_final_this_dimension <- rbind(df_mapping_final_this_dimension, df_mapping)
      }
    }
    
    successful_mappings <- df_mapping_final_this_dimension %>%
      dplyr::filter(src_code %in% dataset_to_map[[dimension]])  
    
    if(summary_mapping) recap_mapping <- rbind(successful_mappings, recap_mapping)
    
    if (!is.null(df_mapping_final_this_dimension) && nrow(df_mapping_final_this_dimension) > 0) {
      mapping <- map_codelist(dataset_to_map, df_mapping_final_this_dimension, dimension, mapping_keep_src_code)
      dataset_to_map <- mapping$df  # Codes are mapped by tRFMOs (source_authority)
      
      if (summary_mapping) {
        stats <- mapping$stats
        not_mapped <- mapping$not_mapped
        not_mapped_total <- rbind(not_mapped_total, not_mapped)
        stats_total <- rbind(stats_total, stats)
      }
    }
  }
  recap_mapping <- recap_mapping %>% dplyr::filter(source_authority %in% source_authority_mapped)
  dataset_mapped <- rbind(dataset_to_map, data_not_to_map)
  dataset_mapped <- list(dataset_mapped = dataset_mapped, recap_mapping = recap_mapping, stats_total = stats_total, not_mapped_total = not_mapped_total)
  
  
  return(dataset_mapped)
}




