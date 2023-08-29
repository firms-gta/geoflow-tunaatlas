dimension_filtering_function = function(dataframe_to_filter,
                              filtering_params = matchingList) {
  colnames_to_filter <-
    colnames(dataframe_to_filter %>% dplyr::select(names(filtering_params)))
  names(filtering_params) <- colnames_to_filter
  
  filtering_params <-
    lapply(filtering_params, function(x) {
      #handling single filter
      if (length(x) == 1) {
        x <- c(x, x)
      } else {
        x
      }
    })
  
  if (length(matchingList) != 0) {
    dataframe_to_filter <-
      dataframe_to_filter %>% filter(!!rlang::parse_expr(
        str_c(
          colnames_to_filter,
          matchingList,
          sep = '%in%',
          collapse = "&"
        )
      ))
  } else{
    dataframe_to_filter
  }
  
}