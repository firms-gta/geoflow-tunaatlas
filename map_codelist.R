map_codelist = function (df_input, df_mapping, dimension_to_map, keep_src_code = FALSE) 
{
  if(!require(data.table)){
    install.packages("data.table")
    require(data.table)
  }
  cat(paste0("\n mapping dimension ", dimension_to_map, " with code list mapping"))
  column_names_df_input <- colnames(df_input)
  colnames(df_mapping)[colnames(df_mapping) == "src_code"] <- dimension_to_map
  df_input <- left_join(df_input, df_mapping)
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
                                                       "value")) %>% dplyr::summarise(value = sum(value))
  }
  df_input <- data.frame(df_input)
  stats_data_not_mapped <- df_input %>% dplyr::mutate(sum_mapped_unmapped = ifelse(is.na(df_input[, 
                                                                                           dimension_to_map]), "sum_value_not_mapped", "sum_value_mapped")) %>% 
    group_by(sum_mapped_unmapped, unit) %>% summarise(sum_value_by_dimension = sum(value))
  df_input[, dimension_to_map][which(is.na(df_input[, dimension_to_map]))] = "UNK"
  stats_data_not_mapped <- reshape2::dcast(setDT(stats_data_not_mapped), 
                                 unit ~ sum_mapped_unmapped, sum)
  if (!("sum_value_not_mapped" %in% colnames(stats_data_not_mapped))) {
    stats_data_not_mapped$sum_value_not_mapped = 0
  }
  stats_data_not_mapped[is.na(stats_data_not_mapped)] <- 0
  stats_data_not_mapped$percentage_not_mapped <- stats_data_not_mapped$sum_value_not_mapped/stats_data_not_mapped$sum_value_mapped * 
    100
  return(list(df = df_input, stats = stats_data_not_mapped))
}
