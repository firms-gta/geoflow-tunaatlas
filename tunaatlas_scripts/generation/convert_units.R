convert_units = function (con, df_input, df_conversion_factor, codelist_geoidentifiers_df_input = NULL, 
          codelist_geoidentifiers_conversion_factors = NULL) 
{
  cat(paste0("\n BEGIN tunaatlas::convert_units() => converting units and measures"))
  columns_df_input = colnames(df_input)
  df_input <- data.table(df_input)
  units_source <- unique(df_conversion_factor$unit)
  units_target <- unique(df_conversion_factor$unit_target)
  df_conversion_factor$conversion_factor = as.numeric(df_conversion_factor$conversion_factor)
  if ("geographic_identifier" %in% colnames(df_conversion_factor)) {
    colnames(df_conversion_factor)[which(colnames(df_conversion_factor) == 
                                           "geographic_identifier")] <- "conv_factor_df_geo_id"
    if (codelist_geoidentifiers_df_input == codelist_geoidentifiers_conversion_factors) {
      df_input$conv_factor_df_geo_id <- df_input$geographic_identifier
    }
    else {
      cat(paste0("\n List the different geographic identifiers in the gridded catch file \n"))
      dataset_distinct_geographic_identifier <- unique(df_input$geographic_identifier)
      dataset_distinct_geographic_identifier <- paste(unique(dataset_distinct_geographic_identifier), 
                                                      collapse = "','")
      cat(paste0("\n List the different geographic identifiers in the conversion factors file \n"))
      conversion_factors_distinct_geographic_identifier <- unique(df_conversion_factor$conv_factor_df_geo_id)
      conversion_factors_distinct_geographic_identifier <- paste(unique(conversion_factors_distinct_geographic_identifier), 
                                                                 collapse = "','")
      cat(paste0("\n Link the two kinds of geographic identifiers by using a ST_Contains spatial relationship in Postgis \n"))
      query <- paste("SELECT \n               u1.codesource_area as geographic_identifier,\n               u2.codesource_area as conv_factor_df_geo_id\n             FROM\n               area.area_labels u1,\n               area.area_labels u2\n             WHERE \n               u2.tablesource_area='", 
                     codelist_geoidentifiers_conversion_factors, 
                     "' \n               AND u2.codesource_area IN ('", 
                     conversion_factors_distinct_geographic_identifier, 
                     "') \n               AND u1.tablesource_area='", 
                     codelist_geoidentifiers_df_input, "' \n               AND u1.codesource_area IN ('", 
                     dataset_distinct_geographic_identifier, "') \n               AND ST_Contains(u2.geom, u1.geom)", 
                     sep = "")
      correspondance_geo_identifiers_input_df_conv_fact_df <- dbGetQuery(con, 
                                                                         query)
      fileConn <- file("/tmp/query.sql")
      writeLines(query, fileConn)
      close(fileConn)
      df_input <- merge(df_input, data.table(correspondance_geo_identifiers_input_df_conv_fact_df))
    }
  }
  cat(paste0("\n Link the two kinds of temporal periods by using a home made function ? \n"))
  if ("time_start" %in% colnames(df_conversion_factor)) {
    colnames(df_conversion_factor)[which(colnames(df_conversion_factor) == 
                                           "time_start")] <- "conv_factor_df_time_start"
    colnames(df_conversion_factor)[which(colnames(df_conversion_factor) == 
                                           "time_end")] <- "conv_factor_df_time_end"
    combination_times_df_conversion_factor <- unique(df_conversion_factor[c("conv_factor_df_time_start", 
                                                                            "conv_factor_df_time_end")])
    combination_times_input_dataset <- unique(data.frame(df_input)[c("time_start", 
                                                                     "time_end")])
    combination_times_df_conversion_factor$conv_factor_df_time_start <- strptime(combination_times_df_conversion_factor$conv_factor_df_time_start, 
                                                                                 "%Y-%m-%d")
    combination_times_df_conversion_factor$conv_factor_df_time_end <- strptime(combination_times_df_conversion_factor$conv_factor_df_time_end, 
                                                                               "%Y-%m-%d")
    combination_times_input_dataset$time_start <- strptime(combination_times_input_dataset$time_start, 
                                                           "%Y-%m-%d")
    combination_times_input_dataset$time_end <- strptime(combination_times_input_dataset$time_end, 
                                                         "%Y-%m-%d")
    combination_times_input_dataset$conv_factor_df_time_start <- NA
    combination_times_input_dataset$conv_factor_df_time_end <- NA
    for (i in 1:nrow(combination_times_input_dataset)) {
      combination_times_input_dataset_this_row <- combination_times_df_conversion_factor[which(combination_times_df_conversion_factor$conv_factor_df_time_start <= 
                                                                                                 combination_times_input_dataset$time_start[i] & 
                                                                                                 combination_times_df_conversion_factor$conv_factor_df_time_end >= 
                                                                                                 combination_times_input_dataset$time_end[i]), 
      ]
      combination_times_input_dataset$conv_factor_df_time_start[i] <- as.character(combination_times_input_dataset_this_row$conv_factor_df_time_start[1])
      combination_times_input_dataset$conv_factor_df_time_end[i] <- as.character(combination_times_input_dataset_this_row$conv_factor_df_time_end[1])
    }
    combination_times_input_dataset$time_start <- as.character(combination_times_input_dataset$time_start)
    combination_times_input_dataset$time_end <- as.character(combination_times_input_dataset$time_end)
    df_input$time_start <- substr(as.character(df_input$time_start), 
                                  1, 10)
    df_input$time_end <- substr(as.character(df_input$time_end), 
                                1, 10)
    df_input <- left_join(df_input, combination_times_input_dataset)
  }
  cat(paste0("\n assign conv_factor_df_geo_id=0 to the concerned data . \n"))
  if ("conv_factor_df_geo_id" %in% colnames(df_conversion_factor)) {
    data_zone_0 <- df_conversion_factor[which(df_conversion_factor$conv_factor_df_geo_id == 
                                                0), ]
    colnames(data_zone_0)[which(names(data_zone_0) == "conv_factor_df_geo_id")] <- "zone0"
    data_zone_0$conversion_factor <- NULL
    df_input <- left_join(df_input, data_zone_0)
    df_input$conv_factor_df_geo_id[which(!is.na(df_input$zone0))] <- "0"
    df_input <- df_input %>% dplyr::select(-zone0, -unit_target)
    class(df_input$value) <- "numeric"
  }
  if (nrow(df_input) == nrow(left_join(df_input, df_conversion_factor) %>% 
                             filter(is.na(conversion_factor)))) {
    if (length(intersect(unique(df_conversion_factor$gear), 
                         unique(df_input$gear))) == 0) {
      df_conversion_factor_no_gear <- df_conversion_factor %>% 
        group_by_at(setdiff(colnames(df_conversion_factor), 
                            "gear")) %>% summarise(conversion_factor = mean(conversion_factor))
      df_input <- left_join(df_input, df_conversion_factor_no_gear)
    }
  }
  else {
    df_input <- left_join(df_input, df_conversion_factor)
  }
  # sum_before_conversion <- df_input %>% group_by(unit) %>% 
  #   summarise(sum_value_before_conversion = sum(value))
  # cat(paste0("\n sum_before_conversion is : ", sum_before_conversion$sum_value_before_conversion, 
  #            " ", sum_before_conversion$unit, " \n"))
  # stats_before_conversion <- df_input %>% group_by(unit, unit_target) %>% 
  #   summarise(sum_unit_source_before_conversion = sum(value)) %>% 
  #   filter(!is.na(unit_target))
  index.not_na.conv_factor <- which(!is.na(df_input$conversion_factor))
  df_input$value[index.not_na.conv_factor] <- df_input$value[index.not_na.conv_factor] *
    df_input$conversion_factor[index.not_na.conv_factor]
  # stats_after_conversion <- df_input %>% group_by(unit, unit_target) %>% 
  #   summarise(sum_unit_target_after_conversion = sum(value)) %>% 
  #   filter(!is.na(unit_target))
  df_input$unit[index.not_na.conv_factor] <- df_input$unit_target[index.not_na.conv_factor]
  df_input <- df_input %>% dplyr::select(all_of(columns_df_input))
  # sum_after_conversion <- df_input %>% group_by(unit) %>% 
  #   summarise(sum_value_after_conversion = sum(value))
  # cat(paste0("\n sum_after_conversion is : ", sum_after_conversion$sum_value_after_conversion, 
  #            " ", sum_after_conversion$unit, " \n"))
  # stats <- merge(stats_before_conversion, stats_after_conversion)
  df_input <- data.frame(df_input)
  return(list(df = df_input))
}
