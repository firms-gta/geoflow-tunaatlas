curation_absurd_converted_data <- function(georef_dataset, max_conversion_factor) {

  colnames_georef_dataset_groupping <- setdiff(colnames(georef_dataset), c("value", "unit"))
  
    strata_nomt <- georef_dataset %>% filter(unit %in% c("NOMT"))
  strata_mtno <- georef_dataset %>% filter(unit %in% c("MTNO"))
  strata_converted_level0 <-  rbind(strata_nomt, strata_mtno) %>% ungroup() %>% dplyr::select(-c(value)) %>% dplyr::distinct()
  
  conversion_factor_level0 <- rbind(
    dplyr::inner_join(strata_nomt , strata_mtno, by = setdiff(colnames(strata_mtno), c("value", "unit") )) %>%
      dplyr::rename(NO =value.x, MT = value.y) %>%
      dplyr::group_by(across(colnames_georef_dataset_groupping)) %>%
      dplyr::summarise(NO = sum(NO), MT = sum(MT)) %>% 
      ungroup(),
    dplyr::inner_join(strata_mtno , strata_nomt, by = setdiff(colnames(strata_mtno), c("value", "unit") )) %>%
      dplyr::rename(MT =value.x, NO = value.y)%>%
      dplyr::group_by(across(colnames_georef_dataset_groupping)) %>%
      dplyr::summarise(NO = sum(NO), MT = sum(MT)) %>% 
      ungroup()
  ) %>% 
    dplyr::distinct() %>%
    dplyr::group_by(across(colnames_georef_dataset_groupping)) %>%
    dplyr::summarise(NO = sum(NO), MT = sum(MT)) %>% 
    dplyr::mutate(conversion_factor = MT/NO) %>% 
    dplyr::distinct()
  
  conversion_factor_to_keep <- conversion_factor_level0 %>% left_join(max_conversion_factor, by = "species") %>% filter((conversion_factor < max_weight | conversion_factor > min_weight | is.na(max_weight) | is.na(min_weight)))
  conversion_factor_not_to_keep <- conversion_factor_level0 %>% left_join(max_conversion_factor, by = "species") %>% filter(!(conversion_factor < max_weight | conversion_factor > min_weight | is.na(max_weight) | is.na(min_weight)))
  georef_dataset_without_nomt <- georef_dataset %>% dplyr::filter(unit %in% c("t", "no","MTNO"))
  georef_dataset <- rbind(georef_dataset_without_nomt, conversion_factor_to_keep %>% dplyr::mutate(unit = "NOMT") %>% dplyr::rename(value = NO) %>% dplyr::select(colnames(georef_dataset_without_nomt)))
  
  return(georef_dataset)
}
