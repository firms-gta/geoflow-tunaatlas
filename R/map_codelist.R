#' Map a Codelist for a Given Dimension
#'
#' This function performs mapping of a specific dimension (e.g., gear type, species)
#' from a source dataset to a target dataset using a provided mapping table.
#' The function integrates source codes directly into the dataset, with options to
#' keep the original source codes and generate summary statistics for mappings.
#'
#' @param df_input A dataframe containing the data to be mapped.
#' @param df_mapping A dataframe containing the mapping instructions between different
#'        code lists. Expected to have at least two columns: 'src_code' and 'trg_code',
#'        where 'src_code' corresponds to the code in `df_input` and 'trg_code' is the
#'        target mapping.
#' @param dimension_to_map A string specifying the column in `df_input` that needs
#'        to be mapped using `df_mapping`.
#' @param keep_src_code A logical indicating whether to retain the original source
#'        codes in the resulting dataset.
#'
#' @return A list containing three elements:
#'         - `df`: The mapped dataset.
#'         - `stats`: A dataframe of statistics summarizing the mapping results,
#'           including the total of mapped and unmapped values and their respective percentages.
#'         - `not_mapped`: A dataframe listing the values and source authorities that
#'           were not mapped.
#'
#' @examples
#' \donotshow{
#' df_input <- data.frame(
#'   gear_type = c("001", "002", "UNK"),
#'   measurement_unit = c("tons", "tons", "tons"),
#'   measurement_value = c(100, 150, 120),
#'   source_authority = c("IATTC", "CCSBT", "WCPFC")
#' )
#' df_mapping <- data.frame(
#'   src_code = c("001", "002"),
#'   trg_code = c("longline", "purse seine")
#' )
#' mapped_data <- map_codelist(df_input, df_mapping, "gear_type")
#' print(mapped_data)
#' }
#'
#' @importFrom dplyr left_join filter select rename mutate group_by_ summarise group_by
#' @importFrom data.table setDT
#' @importFrom reshape2 dcast
#' @export

map_codelist = function (df_input, df_mapping, dimension_to_map, keep_src_code = FALSE) 
{
  if(!require(data.table)){
    install.packages("data.table")
    require(data.table)
  }
  cat(paste0("\n mapping dimension ", dimension_to_map, " with code list mapping"))
  column_names_df_input <- colnames(df_input)
  colnames(df_mapping)[colnames(df_mapping) == "src_code"] <- dimension_to_map
  df_input <- dplyr::left_join(df_input, df_mapping)
  
  not_mapped <- unique((df_input %>% filter(is.na(trg_code))) %>% dplyr::select({{dimension_to_map}}, source_authority)) %>% 
    dplyr::rename("Value" :={{dimension_to_map}} ) %>% 
    dplyr::mutate("Dimension" = dimension_to_map)
  if (keep_src_code == FALSE) {
    df_input[, dimension_to_map] <- df_input$trg_code
    df_input <- df_input[column_names_df_input]
  }
  else {
    colnames(df_input)[colnames(df_input) == dimension_to_map] <- paste0(dimension_to_map, 
                                                                         "_src_code")
    colnames(df_input)[colnames(df_input) == "trg_code"] <- dimension_to_map
    df_input <- df_input[c(column_names_df_input, paste0(dimension_to_map, 
                                                         "_src_code"))]
  }
  if (keep_src_code == FALSE) {
    df_input <- df_input %>% dplyr::group_by_(.dots = setdiff(column_names_df_input, 
                                                              "measurement_value")) %>% dplyr::summarise(measurement_value = sum(measurement_value))
  }
  df_input <- data.frame(df_input)
  stats_data_not_mapped <- df_input %>% dplyr::mutate(sum_mapped_unmapped = ifelse(is.na(df_input[, 
                                                                                                  dimension_to_map]), "sum_value_not_mapped", "sum_value_mapped")) %>% 
    dplyr::group_by(sum_mapped_unmapped, measurement_unit) %>% dplyr::summarise(sum_value_by_dimension = sum(measurement_value))
  
  if(dimension_to_map=="fishing_fleet"){
    replace_unk <- "NEI"
  } else if(dimension_to_map=="species"){
    replace_unk <- "MZZ"
  }
  if(dimension_to_map=="gear_type"){
    replace_unk <- "99.9"
  } else {replace_unk <- "UNK"}
  
  df_input[, dimension_to_map][which(is.na(df_input[, dimension_to_map]))] = replace_unk
  
  stats_data_not_mapped <- reshape2::dcast(setDT(stats_data_not_mapped), 
                                           measurement_unit ~ sum_mapped_unmapped, sum)
  if (!("sum_value_not_mapped" %in% colnames(stats_data_not_mapped))) {
    stats_data_not_mapped$sum_value_not_mapped = 0
  }
  if (!("sum_value_mapped" %in% colnames(stats_data_not_mapped))) {
    stats_data_not_mapped$sum_value_mapped = 0
  }
  
  stats_data_not_mapped[is.na(stats_data_not_mapped)] <- 0
  stats_data_not_mapped$percentage_not_mapped <- stats_data_not_mapped$sum_value_not_mapped/stats_data_not_mapped$sum_value_mapped * 
    100
  stats_data_not_mapped <- stats_data_not_mapped  %>% 
    dplyr::mutate("Dimension" = dimension_to_map)
  return(list(df = df_input, stats = stats_data_not_mapped, not_mapped = not_mapped))
}
