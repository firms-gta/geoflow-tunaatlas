function_raise_data<-function(fact,source_authority_filter,dataset_to_raise,dataset_to_compute_rf,nominal_dataset_df,x_raising_dimensions,
                              decrease_when_rf_inferior_to_one = FALSE, remove_corresponding_number = TRUE,
                              do_not_decrease_unk_data = TRUE, 
                              raise_only_on_unk = TRUE, do_not_raise_perfectly_compatible_data = TRUE, 
                              do_not_raise_any_unk = TRUE){
  # we recommend, if runnning with do_not_raise_perfectly_compatible_data to true, to raise only_on_unk .
  number_data_to_raise <- dataset_to_raise %>% dplyr::filter(measurement_unit =="no")
  dataset_to_raise <- dataset_to_raise %>% dplyr::filter(measurement_unit =="t")
  dataset_to_compute_rf <- dataset_to_compute_rf %>% dplyr::filter(measurement_unit =="t")
  
  dataset_to_raise$year <- as.numeric(substr(dataset_to_raise$time_start, 
                                             0, 4))
  nominal_dataset_df$year <- as.numeric(substr(nominal_dataset_df$time_start, 
                                               0, 4))
  dataset_to_compute_rf$year <- as.numeric(substr(dataset_to_compute_rf$time_start, 
                                               0, 4))
  dataset_to_raise<-dataset_to_raise[which(dataset_to_raise$source_authority %in% source_authority_filter),]
  
  dataset_to_compute_rf<-dataset_to_compute_rf[which(dataset_to_compute_rf$source_authority %in% source_authority_filter),]
  
  nominal_dataset_df<-nominal_dataset_df[which(nominal_dataset_df$source_authority %in% source_authority_filter),]
  
  source("https://raw.githubusercontent.com/firms-gta/geoflow-tunaatlas/master/R/sardara_functions/raise_get_rf.R")
  
  dim_perfectly_compatible_data <- c("species", "fishing_mode", "gear_type", "fishing_fleet", "year", "geographic_identifier_nom", "source_authority")
  
  if(do_not_raise_perfectly_compatible_data){
    conds <- list()
    if ("fishing_mode"  %in% names(dataset_to_raise)) conds <- append(conds, expr(fishing_mode  == "UNK"))
    if ("fishing_fleet" %in% names(dataset_to_raise)) conds <- append(conds, expr(fishing_fleet == "NEI"))
    if ("gear_type"     %in% names(dataset_to_raise)) conds <- append(conds, expr(gear_type     == "99.9"))
    
    
    perfectly_compatible_data <- dataset_to_raise %>%
      # 1. Projection+filtrage 
      dplyr::select(all_of(dim_perfectly_compatible_data)) %>%
      dplyr::filter(!!!conds) %>%
      # 2. Inner‐join léger
      dplyr::inner_join(
        nominal_dataset_df %>% dplyr::select(all_of(dim_perfectly_compatible_data)),
        by = dim_perfectly_compatible_data
      ) %>%
      dplyr::distinct() 
    
    perfectly_compatible_data_value <- dplyr::semi_join(dataset_to_raise, perfectly_compatible_data)
    
    dataset_to_raise <- dplyr::anti_join(dataset_to_raise, perfectly_compatible_data)
    nominal_dataset_df <- dplyr::anti_join(nominal_dataset_df, perfectly_compatible_data)
    
  }
  
  if(raise_only_on_unk){
    unk_dim <- setdiff(dim_perfectly_compatible_data, x_raising_dimensions)
    conds <- list()
    if ("fishing_mode"  %in% (unk_dim)) conds <- append(conds, expr(fishing_mode  == "UNK"))
    if ("fishing_fleet" %in% (unk_dim)) conds <- append(conds, expr(fishing_fleet == "NEI"))
    if ("gear_type"     %in% (unk_dim)) conds <- append(conds, expr(gear_type     == "99.9"))
    
    if (length(conds)) {
      # réduire la liste en un unique OR
      cond_or <- reduce(conds, ~ expr( (!!.x) | (!!.y) ))
      
      nominal_dataset_df <- nominal_dataset_df %>%
        dplyr::filter(!!cond_or)
    }
    
  }
  years   <- intersect(dataset_to_compute_rf$year, nominal_dataset_df$year)
  rf_list <- vector("list", length(years))
  
  for (i in seq_along(years)) {
    y <- years[i]
    sub_incomp <- dataset_to_compute_rf[dataset_to_compute_rf$year == y, ]
    sub_total  <- nominal_dataset_df[  nominal_dataset_df$year == y, ]
    
    rf_list[[i]] <- raise_get_rf(
      df_input_incomplete = sub_incomp,
      df_input_total      = sub_total,
      x_raising_dimensions= c(x_raising_dimensions,"measurement_unit")
    ) %>%
      dplyr::mutate(rf = ifelse(is.na(rf), 0, rf))
    
    # libère la mémoire des objets temporaires
    rm(sub_incomp, sub_total)
    gc()
  }
  
  df_rf <- dplyr::bind_rows(rf_list)
  rm(rf_list)
  gc()
  
  if (fact=="catch"){
    raising_dimensions=c(x_raising_dimensions,"measurement_unit")
  } else if (fact=="effort"){
    raising_dimensions=x_raising_dimensions
    df_rf$measurement_unit=NULL
  }
  
  
  # if you want to decrease data but not remove data that has no correspondande run df_rf <- df_rf %>% dplyr::rowwise() %>% dplyr::mutate(rf = ifelse(is.na(rf), 1, rf))
  
  if (decrease_when_rf_inferior_to_one && do_not_decrease_unk_data) {
    cols <- intersect(names(df_rf),
                      c("fishing_mode","fishing_fleet","gear_type"))
    
    if (length(cols)>0) {
      # drapeaux de "unk" ligne à ligne
      flag <- Reduce(`|`, lapply(cols, function(col)
        df_rf[[col]] %in% c("UNK","NEI","99.9")
      ), init = FALSE)
      
      # on ne touche qu'aux lignes concernées
      df_rf$rf[flag & df_rf$rf < 1] <- 1L
    }
  }
  
  
  if (do_not_raise_any_unk) {
    cols <- intersect(
      names(df_rf),
      c("fishing_mode", "fishing_fleet", "gear_type")
    )
    
    if (length(cols) > 0) {
      # sapply() construit une matrice nrow×length(cols)
      mat <- sapply(cols, function(col) {
        df_rf[[col]] %in% c("UNK","NEI","99.9")
      })
      unk_flag <- rowSums(mat, na.rm = TRUE) > 0
    } else {
      unk_flag <- rep(FALSE, nrow(df_rf))
    }
    
    # puis on remplace là où il faut
    df_rf$rf[unk_flag & df_rf$rf > 1] <- 1L #alors on force a 1 pour ne pas raise un unk
    
  }
  
  
  
  source("https://raw.githubusercontent.com/firms-gta/geoflow-tunaatlas/master/R/sardara_functions/raise_incomplete_dataset_to_total_dataset.R")
  
  data_raised<-raise_incomplete_dataset_to_total_dataset(df_input_incomplete = dataset_to_raise,
                                                         df_input_total = nominal_dataset_df,
                                                         df_rf = df_rf,
                                                         x_raising_dimensions = raising_dimensions,
                                                         decrease_when_rf_inferior_to_one = decrease_when_rf_inferior_to_one,
                                                         threshold_rf = NULL)
  rm(dataset_to_raise, nominal_dataset_df)
  data_raised <- rbind(number_data_to_raise, data_raised$df)
  
  if(remove_corresponding_number){
  data_raised <- data_raised %>%
    dplyr::ungroup() %>%
    dplyr::group_by(across(-measurement_value)) %>% 
    dplyr::summarise(measurement_value = sum(measurement_value, na.rm = TRUE), .groups = 'drop')
  
  if(decrease_when_rf_inferior_to_one){
    df_rf <- df_rf
  } else { # si on decrease pas, on enlève pas les nombres qui correspondent à des tonnes qui n'ont pas été augmentées
    df_rf <- df_rf %>% dplyr::filter(round(rf,6) > 1)
  }
  
  corresponding_number_to_raised_data <- data_raised %>% 
    dplyr::filter(measurement_unit == "no") %>% 
    dplyr::mutate(year = lubridate::year(time_start))%>% 
    dplyr::select(x_raising_dimensions) %>% dplyr::distinct() %>% 
    dplyr::inner_join(df_rf %>% 
                        dplyr::select(x_raising_dimensions)%>% dplyr::distinct(),
                      by = x_raising_dimensions) %>% 
    dplyr::mutate(measurement_unit = "no") %>%
    dplyr::distinct()
  
  # inner_join_removed <- data_raised%>% 
  #   dplyr::mutate(year = lubridate::year(time_start)) %>% dplyr::inner_join(corresponding_number_to_raised_data,by = c("source_authority", "species", "gear_type", "fishing_fleet", "measurement_unit", "year"))
  
  data_raised <- data_raised %>% 
    dplyr::mutate(year = lubridate::year(time_start)) %>% 
    anti_join(corresponding_number_to_raised_data, by = c(x_raising_dimensions,"measurement_unit")) %>% 
    dplyr::select(-year) %>% 
    dplyr::mutate(measurement_processing_level = "raised")
  }
  if(do_not_raise_perfectly_compatible_data){
    data_raised <- rbind(data_raised, perfectly_compatible_data_value %>% dplyr::select(-year))
  }
  
  return(list(data_raised = data_raised, df_rf = df_rf))
  
}
