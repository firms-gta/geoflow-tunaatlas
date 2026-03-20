function_raise_data<-function(fact,source_authority_filter = c("IOTC", "CCSBT", "ICCAT", "IATTC", "WCPFC"),dataset_to_raise,dataset_to_compute_rf,nominal_dataset_df,x_raising_dimensions,
                              decrease_when_rf_inferior_to_one = FALSE, remove_corresponding_number = TRUE,
                              do_not_decrease_unk_data = TRUE, 
                              raise_only_on_unk = TRUE, do_not_raise_perfectly_compatible_but_unknown_data = TRUE, 
                              do_not_raise_any_unk = TRUE, 
                              dim_perfectly_compatible_data = c("species", "fishing_mode", "gear_type", "fishing_fleet", "year", "geographic_identifier_nom", "source_authority"),
                              flag_for_measurement_processing_level = "raised"){
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
  
  unk_dim <- setdiff(dim_perfectly_compatible_data, x_raising_dimensions)
  
  # if(do_not_raise_perfectly_compatible_but_unknown_data){ # ca je pense que c'est pour augmenter la donnee que si elle est pas completemetn unk au acs ou ca on lenleve masi pas sur ça peu taussi permettre d'avoir lempreinte spatiale de trucs en 
  #   # NUK complete
  #   # la j'essaye en gros de faire en sorte que les UNK/NEI etc soient pas pris en compte dan sle georef mais que du coup ça soit retiré du nominal, ça revient presque à do_not_raise_any_unk c'est peut êtrre ça le emeiux
  #   conds <- list()
  #   if ("fishing_mode"  %in% (unk_dim)) conds <- append(conds, expr(fishing_mode  == "UNK"))
  #   if ("fishing_fleet" %in% (unk_dim)) conds <- append(conds, expr(fishing_fleet == "NEI"))
  #   if ("gear_type"     %in% (unk_dim)) conds <- append(conds, expr(gear_type     == "99.9"))
  #   
  #   
  #   perfectly_compatible_data <- dataset_to_raise %>%
  #     # 1. Projection+filtrage 
  #     dplyr::select(all_of(dim_perfectly_compatible_data)) %>%
  #     dplyr::filter(!!!conds) %>%
  #     # 2. Inner‐join léger
  #     dplyr::inner_join(
  #       nominal_dataset_df %>% dplyr::select(all_of(dim_perfectly_compatible_data)),
  #       by = dim_perfectly_compatible_data
  #     ) %>%
  #     dplyr::distinct() 
  #   if(nrow(perfectly_compatible_data)!=0){browser()}
  #   perfectly_compatible_data_value <- dplyr::semi_join(dataset_to_raise, perfectly_compatible_data)
  #   
  #   dataset_to_raise <- dplyr::anti_join(dataset_to_raise, perfectly_compatible_data)
  #   nominal_dataset_df <- dplyr::anti_join(nominal_dataset_df, perfectly_compatible_data)
  #   
  # }
  
  # vibe
  if (do_not_raise_perfectly_compatible_but_unknown_data) {
    
    # dims ignorées par le raising = dims absentes de x_raising_dimensions
    unk_dim <- setdiff(dim_perfectly_compatible_data, x_raising_dimensions)
    
    # On construit les conditions UNK uniquement pour ces dims ignorées
    conds <- list()
    if ("fishing_mode"  %in% unk_dim) conds <- append(conds, rlang::expr(fishing_mode  == "UNK"))
    if ("fishing_fleet" %in% unk_dim) conds <- append(conds, rlang::expr(fishing_fleet == "NEI"))
    if ("gear_type"     %in% unk_dim) conds <- append(conds, rlang::expr(gear_type     == "99.9"))
    
    # Si aucune dim qui peut avoir des "UNK" n'est ignorée, il n'y a rien à faire
    if (length(conds) > 0) {
      
      # OR : dès qu'au moins une dimension ignorée est inconnue
      cond_or <- purrr::reduce(conds, ~ rlang::expr((!!.x) | (!!.y)))
      
      # 1) Strates concernées côté georef (clé de jointure = dims complètes)
      strata_unk_georef <- dataset_to_raise %>%
        dplyr::select(dplyr::all_of(dim_perfectly_compatible_data)) %>%
        dplyr::filter(!!cond_or) %>%
        dplyr::distinct()
      
      # 2) On ne garde que celles qui existent aussi dans le nominal (mêmes strates complètes)
      strata_unk_both <- strata_unk_georef %>%
        dplyr::inner_join(
          nominal_dataset_df %>% dplyr::select(dplyr::all_of(dim_perfectly_compatible_data)) %>% dplyr::distinct(),
          by = dim_perfectly_compatible_data
        ) %>%
        dplyr::distinct()
      
      # Si on a des strates à traiter
      if (nrow(strata_unk_both) > 0) {
        
        # 3) Sauvegarde des lignes georef correspondantes (valeurs) -> à réajouter à la fin
        perfectly_compatible_data_value <- dataset_to_raise %>%
          dplyr::semi_join(strata_unk_both, by = dim_perfectly_compatible_data)
        
        # 4) Retirer ces lignes du georef pour qu'elles ne soient pas raisées
        dataset_to_raise <- dataset_to_raise %>%
          dplyr::anti_join(strata_unk_both, by = dim_perfectly_compatible_data)
        
        # 5) Côté nominal : soustraire la contribution déjà présente en georef
        #    On calcule la somme georef (UNK ignoré) par strate complète.
        georef_sub <- perfectly_compatible_data_value %>%
          dplyr::group_by(dplyr::across(dplyr::all_of(dim_perfectly_compatible_data))) %>%
          dplyr::summarise(georef_val = sum(measurement_value, na.rm = TRUE), .groups = "drop")
        
        #    Puis on ajuste le nominal sur ces strates: nominal = max(nominal - georef, 0)
        nominal_dataset_df <- nominal_dataset_df %>%
          dplyr::left_join(georef_sub, by = dim_perfectly_compatible_data) %>%
          dplyr::mutate(
            georef_val = dplyr::coalesce(georef_val, 0),
            measurement_value = measurement_value - georef_val,
            measurement_value = pmax(measurement_value, 0)
          ) %>%
          dplyr::select(-georef_val)
      } else {
        perfectly_compatible_data_value <- NULL
      }
      
    } else {
      perfectly_compatible_data_value <- NULL
    }
  }
  
  if(raise_only_on_unk){ # on ne raise que sur les unk des nominales, mais on pourrait dire qu'on raise sur tout si en georef c'est de l'unK non ? apres on veut pas trop augmenter l'unk du georef mais c'est toujorus 
    # bien d'augmenter l'unK de fishing_mode par exe si ça augmente les captures de SKJ qui est pas unk.
    conds <- list()
    if ("fishing_mode_wcpfc_issue_unk_solved"  %in% (unk_dim)) conds <- append(conds, expr(fishing_mode_wcpfc_issue_unk_solved  == "UNK"))
    if ("fishing_mode"  %in% (unk_dim)) conds <- append(conds, expr(fishing_mode  == "UNK"))
    if ("fishing_fleet" %in% (unk_dim)) conds <- append(conds, expr(fishing_fleet == "NEI"))
    if ("group_gears" %in% (unk_dim)) conds <- append(conds, expr(group_gears == "99.9"))
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
  
  # if (decrease_when_rf_inferior_to_one && do_not_decrease_unk_data) {
  #   cols <- intersect(names(df_rf),
  #                     c("fishing_mode","fishing_fleet","gear_type"))
  #   
  #   if (length(cols)>0) {
  #     # drapeaux de "unk" ligne à ligne
  #     flag <- Reduce(`|`, lapply(cols, function(col)
  #       df_rf[[col]] %in% c("UNK","NEI","99.9")
  #     ), init = FALSE)
  #     
  #     # on ne touche qu'aux lignes concernées
  #     df_rf$rf[flag & df_rf$rf < 1] <- 1L
  #   }
  # }
  if (decrease_when_rf_inferior_to_one) {
    
    cols <- intersect(names(df_rf), c("fishing_fleet", "gear_type"))
    
    mat <- sapply(cols, function(col) {
      df_rf[[col]] %in% c("NEI", "99.9")
    })
    
    unk_flag <- rowSums(mat, na.rm = TRUE) > 0
    
    df_rf$rf[df_rf$rf < 1 & !unk_flag] <- 1
    
  } else if (!decrease_when_rf_inferior_to_one) {
    
    df_rf$rf[df_rf$rf < 1] <- 1
    
  }
  
  # decreased_nei <- df_rf %>% dplyr::filter(rf < 1)
  
  source(here::here("R/sardara_functions/raise_incomplete_dataset_to_total_dataset.R"))
  df_out <- dataset_to_raise %>%
    dplyr::left_join(df_rf %>% filter(!is.na(rf), is.finite(rf)), by = x_raising_dimensions) %>% 
    dplyr::mutate(rf= ifelse(is.na(rf), 0, rf))%>%
    dplyr::mutate(value_raised = ifelse(!is.na(rf), measurement_value * rf, 0))%>%
    dplyr::select(-dplyr::ends_with(".y")) %>%   # enlève les .y
    dplyr::rename_with(~ sub("\\.x$", "", .x)) 
  
  # raise_incomplete_dataset_to_total_dataset<-raise_incomplete_dataset_to_total_dataset(df_input_incomplete = dataset_to_raise,
  #                                                        df_input_total = nominal_dataset_df,
  #                                                        df_rf = df_rf,
  #                                                        x_raising_dimensions = raising_dimensions,
  #                                                        decrease_when_rf_inferior_to_one = TRUE, #it is always better to handle this before in the rf
  #                                                        threshold_rf = NULL)
  if (decrease_when_rf_inferior_to_one) {
    # browser()
    gc()
    # save.image(here::here("test.RData"))
  # df_rf <- raise_incomplete_dataset_to_total_dataset$df_input_incomplete
  # browser()
  df_rf_lowered <- df_out %>% dplyr::filter(rf < 1) %>% dplyr::mutate(decrease =measurement_value - value_raised) 
  
  df_equal <- df_out %>% dplyr::filter(rf == 1)
  
  df_raised <- df_out %>% dplyr::filter(rf >= 1)
  df_rf_lowered_ff_nei <- df_rf_lowered %>% dplyr::filter(fishing_fleet == "NEI" & gear_type != "99.9")
  df_rf_lowered_gg_nei <- df_rf_lowered %>% dplyr::filter(gear_type == "99.9" & fishing_fleet != "NEI")
  df_rf_lowered_gg_ff_nei <- df_rf_lowered %>% dplyr::filter(gear_type == "99.9" & fishing_fleet == "NEI") # pour l'instan je sais pas quoi faire d'eux, surement on laisse et on fait rien c'est tro complexe ce cas si on veut baisser
  df_rf_lowered_known <- df_rf_lowered %>%
    filter(fishing_fleet != "NEI" & gear_type != "99.9")
  
  raised_no_ff <- df_raised %>% dplyr::ungroup()%>% dplyr::mutate(increase =abs(measurement_value - value_raised)) %>% dplyr::group_by(across(setdiff(colnames(df_raised), c("measurement_value", "rf","year", "fishing_fleet",
                                                                                                                                                                             "sum_value_df_input_incomplete", "sum_value_df_input_total", "value_raised" )))) %>%# dplyr::mutate(increase =abs(measurement_value - value_raised)) %>% dplyr::group_by(across(c(setdiff(raising_dimensions, c("fishing_fleet", "year")), "time_start", 
                                                                                                               #                                                          "time_end", "geographic_identifier"))) %>%
    dplyr::summarise(increase=sum(increase)) # on caclule pour chaque strate de combein ca a augmente en tout pour tous les ff

  corresponding_df_rf_raised_ff <-df_rf_lowered_ff_nei  %>% dplyr::left_join(raised_no_ff, by = c(setdiff(colnames(raised_no_ff), c("fishing_fleet", "year", "rf", "value_raised", "increase", "decrease")))) %>% 
    dplyr::mutate(increase = ifelse(is.na(increase), 0, increase)) %>%
    dplyr::mutate(new_value = ifelse(decrease < increase, value_raised, measurement_value - increase ))# en gros si on augmente plus que ce qu'on baisse on accepte la baisse et si on augmente moins on acepte la baisse correspondante a l'augmentation
    

# Pareil pour gear unk ----------------------------------------------------

  raised_no_gg <- df_raised  %>% dplyr::ungroup()%>% dplyr::mutate(increase =abs(measurement_value - value_raised)) %>% dplyr::group_by(across(setdiff(colnames(df_raised), c("measurement_value", "rf","year", "gear_type", 
                                                                                                                                                                              "sum_value_df_input_incomplete", "sum_value_df_input_total", "value_raised" )))) %>%# dplyr::mutate(increase =abs(measurement_value - value_raised)) %>% dplyr::group_by(across(c(setdiff(raising_dimensions, c("fishing_fleet", "year")), "time_start", 
    #                                                          "time_end", "geographic_identifier"))) %>%
    dplyr::summarise(increase=sum(increase)) # on caclule pour chaque strate de combein ca a augmente en tout pour tous les ff
  
  corresponding_df_rf_raised_gg <- df_rf_lowered_gg_nei  %>% dplyr::left_join(raised_no_gg,  by = c(setdiff(colnames(raised_no_gg), c("gear_type","year", "rf", "value_raised","increase", "decrease") ))) %>%  
    dplyr::mutate(increase = ifelse(is.na(increase), 0, increase)) %>%
    dplyr::mutate(new_value = ifelse(decrease < increase, value_raised, measurement_value - increase )) # en gros si on augmente plus que ce qu'on baisse on accepte la baisse et si on augmente moins on acepte la baisse correspondante a l'augmentation  
                  # donc si on augmente plus, on met bien le decreased (value_raised.x) et sinon on met le decrease mais on y rajoute l'increase
  
  # df_raised_t <- df_raised %>% dplyr::mutate(measurement_value = value_raised) %>% dplyr::select(colnames(data_raised$df))
  
  corresponding_df_rf_raised_gg_little <- corresponding_df_rf_raised_gg %>% dplyr::mutate(measurement_valuefinal = new_value ) %>% dplyr::select(-c(decrease, increase, new_value))%>%
    dplyr::select(-dplyr::ends_with(".y")) %>%   # enlève les .y
    dplyr::rename_with(~ sub("\\.x$", "", .x)) 
  corresponding_df_rf_raised_ff_little <- corresponding_df_rf_raised_ff %>% dplyr::mutate(measurement_valuefinal = new_value ) %>% dplyr::select(-c(decrease, increase, new_value))%>%
    dplyr::select(-dplyr::ends_with(".y")) %>%   # enlève les .y
    dplyr::rename_with(~ sub("\\.x$", "", .x)) 
  
  
  saveRDS(corresponding_df_rf_raised_ff_little,file = file.path("data",paste0(Sys.time(), "data_decreased_fishing_fleet_unk.rds")))
  saveRDS(corresponding_df_rf_raised_gg_little,file = file.path("data",paste0(Sys.time(), "data_decreased_gear_unk.rds")))
  
  corresponding_df_rf_raised_ff_little <- corresponding_df_rf_raised_ff_little %>% dplyr::mutate(measurement_value = measurement_valuefinal) %>% dplyr::select(-measurement_valuefinal)
  corresponding_df_rf_raised_gg_little <- corresponding_df_rf_raised_gg_little %>% dplyr::mutate(measurement_value = measurement_valuefinal) %>% dplyr::select(-measurement_valuefinal)
  # browser()
  
  lowered_data <- rbind(corresponding_df_rf_raised_ff_little,corresponding_df_rf_raised_gg_little, df_rf_lowered_gg_ff_nei %>% dplyr::select(-decrease),
                        df_rf_lowered_known %>% dplyr::ungroup() %>% dplyr::select(colnames(df_out))) 
  
  # idx_raised <- !is.na(new_data$rf) & new_data$rf > 1 #shouldntbe
  idx_equal <- lowered_data$rf == 1
  idx_decreased <- !is.na(lowered_data$rf) & lowered_data$rf < 1 & lowered_data$measurement_value > lowered_data$value_raised
  
  lowered_data$measurement_processing_level[idx_decreased] <- "decreased"
  
  df_raised$measurement_processing_level <- flag_for_measurement_processing_level
  df_raised$measurement_value <- df_raised$value_raised
  
  df_raised <-  df_raised%>% dplyr::select(colnames(df_out))
  
  lowered_data <- lowered_data %>% dplyr::select(colnames(df_out))
  
  data_raised <- rbind(df_raised, lowered_data)
  
  data_raised <- data_raised  %>% dplyr::select(colnames(number_data_to_raise))
  # then we do the same thing but for gear_type and fishing_fleet both
  
  # conv_no_nominal <- qs::qread("~/firms-gta/geoflow-tunaatlas/jobs/20260319122849/entities/global_catch_ird_level2_1950_2024/Markdown/Conv_NO_nominal1/data.qs")
  # 
  # data_raised_year_source_species <- conv_no_nominal %>% dplyr::group_by(source_authority, species, time_start) %>% dplyr::summarise(sum = sum(measurement_value))
  # data_raised_year_source_species_rf <- data_raised %>% dplyr::group_by(source_authority, species, time_start) %>% dplyr::summarise(sumrf1 = sum(measurement_value))
  # 
  # t <- data_raised_year_source_species %>% dplyr::full_join(data_raised_year_source_species_rf) %>% dplyr::mutate(t = sumrf1 < sum) %>% dplyr::mutate(diff = sumrf1 - sum)
  # 
  # test_la_ou_cestbaisse <- conv_no_nominal %>% dplyr::full_join(data_raised, c("source_authority", "species", "gear_type", "fishing_fleet", "fishing_mode", "time_start", "time_end", 
  #                                                                              "geographic_identifier", "measurement", "measurement_type", "measurement_unit", "measurement_processing_level",
  #                                                                                         "geographic_identifier_nom", "group_species_iattc_sharks", "fishing_mode_wcpfc_issue_unk_solved", "group_gears"))
  # 
  # 
  # a <- test_la_ou_cestbaisse%>% dplyr::mutate(measurement_value.x = ifelse(is.na(measurement_value.x), 0, measurement_value.x))%>%
  #   dplyr::mutate(measurement_value.y = ifelse(is.na(measurement_value.y), 0, measurement_value.y))%>% dplyr::filter(measurement_value.x > measurement_value.y) 
  # 
  # anti_join <- conv_no_nominal %>% dplyr::full_join(data_raised, c("source_authority", "species", "gear_type", "fishing_fleet", "fishing_mode", "time_start", "time_end", 
  #                                                                  "geographic_identifier", "measurement", "measurement_type", "measurement_unit", "measurement_processing_level",
  #                                                                  "geographic_identifier_nom", "group_species_iattc_sharks", "fishing_mode_wcpfc_issue_unk_solved", "group_gears"))
  
  } else {
    
    if(!is.null(flag_for_measurement_processing_level)){
      df_out <- df_out %>% dplyr::mutate(measurement_value = value_raised)
      data_raised <- df_out
      idx_raised <- !is.na(data_raised$rf) & data_raised$rf > 1
      idx_decreased <- !is.na(data_raised$rf) & data_raised$rf < 1 & data_raised$measurement_value > data_raised$value_raised
      
      data_raised$measurement_processing_level[idx_raised] <- flag_for_measurement_processing_level
      data_raised$measurement_processing_level[idx_decreased] <- "decreased"
      
      data_raised <- data_raised%>% dplyr::select(colnames(number_data_to_raise))
    } else {
      df_out <- df_out %>% dplyr::mutate(measurement_value = value_raised)
      data_raised <- df_out %>% dplyr::select(colnames(number_data_to_raise))
    }
    
  }
  
  data_raised <- rbind(number_data_to_raise, data_raised)
  rm(dataset_to_raise, nominal_dataset_df)
  
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
      dplyr::select(-year) 
  }
  if (do_not_raise_perfectly_compatible_but_unknown_data && !is.null(perfectly_compatible_data_value)) {
    data_raised <- dplyr::bind_rows(data_raised, perfectly_compatible_data_value %>% dplyr::select(-year))
  }
  
  return(list(data_raised = data_raised, df_rf = df_rf))
  
}
