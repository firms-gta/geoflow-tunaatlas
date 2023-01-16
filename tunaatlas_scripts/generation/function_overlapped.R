function_overlapped =function(dataset, con, rfmo_to_keep, rfmo_not_to_keep, 
                              strata =c("geographic_identifier",    "species", "year" ,"fishingfleet")
                              , opts = opts, removing_unk = "TRUE"){
  variable <- opts$fact
  columns_to_keep <- NULL
  if (variable == "catch"){
    columns_to_keep<-c("source_authority","species","gear","fishingfleet","schooltype","time_start","time_end","geographic_identifier","catchtype","unit","value")
  } else if (variable=="effort"){
    columns_to_keep<-c("source_authority","gear","fishingfleet","schooltype","time_start","time_end","geographic_identifier","unit","value")
  }
  if("year"%in%strata){
    dataset <- dataset %>% mutate(year = as.character(lubridate::year(time_start)))
    columns_to_keep <- append(setdiff(columns_to_keep, c("time_start", "time_end")), "year")
  }
  strata <- intersect(strata, columns_to_keep)
  rfmo_to_keep_DT <- dataset %>% filter(source_authority == rfmo_to_keep)
  rfmo_not_to_keep_DT <- dataset %>% filter(source_authority == rfmo_not_to_keep)
  rfmo_restant <- dataset %>% 
    filter(source_authority != rfmo_not_to_keep & source_authority!= rfmo_to_keep)
  
  rfmo_not_to_keep_without_equivalent <- dplyr::anti_join(rfmo_not_to_keep_DT, rfmo_to_keep_DT, 
                                                          by = strata)
  overlapping_kept <- rbind(rfmo_not_to_keep_without_equivalent, rfmo_to_keep_DT)
  
  if( removing_unk){# if we keep data from both rfmos we can remove data labelled as "UNK" as they are not precise enough and they can be duplicated
    overlapping_kept_unk_removed <- overlapping_kept %>% 
      dplyr::group_by(across(append(strata, "source_authority"))) %>% dplyr::rowwise()%>% mutate(overlap = n_distinct(source_authority))
    
    overlapping_kept <- overlapping_kept_unk_removed %>% 
      rowwise() %>% 
      filter(!(overlap == 2 && any(c_across(strata) == "UNK")))
  }
  georef_dataset <- rbind(rfmo_restant, overlapping_kept)
  rm(rfmo_to_keep_DT, rfmo_not_to_keep_DT, rfmo_restant, rfmo_not_to_keep_without_equivalent)
  gc()
  georef_dataset <- georef_dataset %>% select(-year)
  

  
  georef_dataset %>% ungroup()
}