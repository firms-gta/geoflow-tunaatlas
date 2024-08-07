get_rfmos_datasets_level0 <- function(rfmo, entity, config, options){
  
  variable <- options$fact
  columns_to_keep <- NULL
  if (variable == "catch"){
    columns_to_keep<-c("source_authority","species","gear_type","fishing_fleet","fishing_mode","time_start","time_end","geographic_identifier",#"catchtype",
                       "measurement_unit","measurement_value")
  } else if (variable=="effort"){
    columns_to_keep<-c("source_authority","gear_type","fishing_fleet","fishing_mode","time_start","time_end","geographic_identifier","measurement_unit","measurement_value")
  }
  
  #list of dataset files (from entity data sources)
  dataset_files <- sapply(entity$data$source[2:length(entity$data$source)], function(x){ entity$getJobDataResource(config, x) })
  names(dataset_files) <- entity$data$source[2:length(entity$data$source)]
  
  #georeferenced grid datasets
  dataset <- switch(rfmo,
                    #IOTC
                    #--------------------------------------------------------------------------------------
                    "IOTC" = {
                      #For IOTC, only data reading
                      iotc_data <- NULL
                      if(options$include_IOTC){
                        config$logger.info(sprintf("Get %s data", rfmo))
                        dataset_files_iotc <- dataset_files[regexpr("nominal", names(dataset_files)) < 0 & 
                                                              regexpr("iotc", names(dataset_files)) > 0]
                        iotc_data <- do.call("rbind", lapply(dataset_files_iotc, readr::read_csv, guess_max = 0))
                        iotc_data <- as.data.frame(iotc_data)
                        class(iotc_data$measurement_value) <- "numeric"
                        iotc_data<- iotc_data[, columns_to_keep]
                      }else{
                        config$logger.warn(sprintf("Option include_%s = FALSE. Ignoring data...", rfmo))
                      }
                      iotc_data
                    },
                    #WCPFC
                    #--------------------------------------------------------------------------------------	
                    "WCPFC" = {
                      #For WCPFC, only data reading
                      wcpfc_data <- NULL
                      if(options$include_WCPFC){
                        config$logger.info(sprintf("Get %s data", rfmo))
                        dataset_files_wcpfc <- dataset_files[regexpr("nominal", names(dataset_files)) < 0 & 
                                                               regexpr("wcpfc", names(dataset_files)) > 0]
                        wcpfc_data <- do.call("rbind", lapply(dataset_files_wcpfc, readr::read_csv, guess_max = 0))
                        wcpfc_data <- as.data.frame(wcpfc_data)
                        class(wcpfc_data$measurement_value) <- "numeric"
                        wcpfc_data<- wcpfc_data[, columns_to_keep]
                        
                      }else{
                        config$logger.warn(sprintf("Option include_%s = FALSE. Ignoring data...", rfmo))
                      }
                      wcpfc_data
                    },
                    #CCSBT
                    #--------------------------------------------------------------------------------------
                    "CCSBT" = {
                      #For CCSBT, only data reading
                      ccsbt_data <- NULL
                      if(options$include_CCSBT){
                        config$logger.info(sprintf("Get %s data", rfmo))
                        dataset_files_ccsbt <- dataset_files[regexpr("nominal", names(dataset_files)) < 0 & 
                                                               regexpr("ccsbt", names(dataset_files)) > 0]
                        ccsbt_data <- do.call("rbind", lapply(dataset_files_ccsbt, readr::read_csv, guess_max = 0))
                        ccsbt_data <- as.data.frame(ccsbt_data)
                        class(ccsbt_data$measurement_value) <- "numeric"
                        ccsbt_data<- ccsbt_data[, columns_to_keep]
                      }else{
                        config$logger.warn(sprintf("Option include_%s = FALSE. Ignoring data...", rfmo))
                      }
                      ccsbt_data
                    },
                    #ICCAT
                    #--------------------------------------------------------------------------------------
                    "ICCAT" = {
                      #For ICCAT, only data reading (since the move to CWP RH standard exchange format)
                      iccat_data <- NULL
                      if(options$include_ICCAT){
                        config$logger.info(sprintf("Get %s data", rfmo))
                        dataset_files_iccat <- dataset_files[regexpr("nominal", names(dataset_files)) < 0 & 
                                                               regexpr("byschool", names(dataset_files)) < 0 &
                                                               regexpr("iccat", names(dataset_files)) > 0]
                        iccat_data <- do.call("rbind", lapply(dataset_files_iccat, readr::read_csv, guess_max = 0))
                        iccat_data <- as.data.frame(iccat_data)

                        class(iccat_data$measurement_value) <- "numeric"
                        
                        iccat_data<- iccat_data[, columns_to_keep]

                      }else{
                        config$logger.warn(sprintf("Option include_%s = FALSE. Ignoring data...", rfmo))
                      }
                      iccat_data
                    },
                    #IATTC
                    #--------------------------------------------------------------------------------------
                    "IATTC" = {
                      #For  IATTC, some special data procesings, see below
                      iattc_data <- NULL
                      if(options$include_IATTC){
                        
                        
                        ## IATTC PS catch-and-effort are stratified as following:
                        # - 1 dataset for tunas, stratified by type of school (but not fishingfleet)
                        # - 1 dataset for tunas, stratified by fishingfleet (but not type of school)
                        # - 1 dataset for billfishes, stratified by type of school (but not fishingfleet)
                        # - 1 dataset for billfishes, stratified by fishingfleet (but not type of school)
                        # - 1 dataset for sharks, stratified by type of school (but not fishingfleet)
                        # - 1 dataset for sharks, stratified by fishingfleet (but not type of school)
                        ## So in total there are 6 datasets. 
                        
                        # Commentaire Emmanuel Chassot (English below) : L’effort est exprimé ici en nombre de calées. 
                        # Cela signifie dans le cas de l’EPO que les efforts donnés dans certains jeux de données 
                        # peuvent correspondre à une partie de l’effort total alloué à une strate puisqu’il
                        # s’agit de l’effort observé, cà-d. pour lequel il y avait un observateur à bord du 
                        # senneur. De mon point de vue, 
                        # (1) L’effort unique et homogène serait celui des thons tropicaux et 
                        # (2) pour uniformiser le jeu de captures par strate, 
                        # il faut calculer un ratio de captures de requins par calée (observée) 
                        # et de porte-épées par calée (observée) et de les multiplier ensuite par l’effort 
                        # reporté pour les thons tropicaux puisqu’on considère que c’est l’effort de la pêcherie 
                        # (qui cible les thons). Le raising factor est effort thons / effort billfish et effort 
                        # thons / effort sharks.
                        
                        # The effort is expressed here in terms of number of sets. This means in the case of the EPO that the effort given in some datasets may correspond to a part of the total effort allocated to a stratum since it is the observed effort, i.e. for which there was an observer on board the purse seine vessel. From my point of view, (1) the unique and homogeneous effort would be that of tropical tunas and (2) to standardize the set of catches per stratum, it is necessary to calculate a ratio of shark catches per set (observed) and swordfish catches per set (observed) and then multiply them by the effort carried over for tropical tunas since this is considered to be the effort of the fishery (which targets tunas). The raising factor is tuna effort/billfish effort and tuna effort/shark effort.
                        
                        
                        columns_to_keep_effort=c("source_authority","gear_type","fishing_fleet","fishing_mode","time_start","time_end","geographic_identifier","measurement_unit","measurement_value")
                        
                        # Get metadata of Catch datasets (for tuna, billfish and shark, and stratified by fishingfleet and then by type of school)
                        dataset_file_PSSetType_tuna_catch <- "catch_1deg_1m_ps_iattc_level0__tuna_byschool.csv"
                        dataset_file_PSSetType_billfish_catch<- "catch_1deg_1m_ps_iattc_level0__billfish_byschool.csv"
                        dataset_file_PSSetType_shark_catch <- "catch_1deg_1m_ps_iattc_level0__shark_byschool.csv"
                        dataset_file_PSFlag_tuna_catch <- "catch_1deg_1m_ps_iattc_level0__tuna_byflag.csv"
                        dataset_file_PSFlag_billfish_catch <- "catch_1deg_1m_ps_iattc_level0__billfish_byflag.csv"
                        dataset_file_PSFlag_shark_catch <- "catch_1deg_1m_ps_iattc_level0__shark_byflag.csv"
                        
                        # Get metadata of Effort datasets (for tuna, billfish and shark, and stratified by flag and then by type of school)
                        dataset_file_PSSetType_tuna_effort <- "effort_1deg_1m_ps_iattc_level0__tuna_byschool.csv"
                        dataset_file_PSSetType_billfish_effort <- "effort_1deg_1m_ps_iattc_level0__billfish_byschool.csv"
                        dataset_file_PSSetType_shark_effort <- "effort_1deg_1m_ps_iattc_level0__shark_byschool.csv"
                        dataset_file_PSFlag_tuna_effort <- "effort_1deg_1m_ps_iattc_level0__tuna_byflag.csv"
                        dataset_file_PSFlag_billfish_effort <- "effort_1deg_1m_ps_iattc_level0__billfish_byflag.csv"
                        dataset_file_PSFlag_shark_effort <- "effort_1deg_1m_ps_iattc_level0__shark_byflag.csv"
                        
                        #for catch fact
                        if(variable == "catch") {
                          
                          config$logger.info(sprintf("Get %s data", rfmo))
                          dataset_files_iattc <- dataset_files[regexpr("nominal", names(dataset_files)) < 0 & 
                                                                 regexpr("ps", names(dataset_files)) < 0 & 
                                                                 regexpr("effort", names(dataset_files)) < 0 &
                                                                 regexpr("iattc", names(dataset_files)) > 0]
                          iattc_data <- do.call("rbind", lapply(dataset_files_iattc, readr::read_csv, guess_max = 0))
                          iattc_data <- as.data.frame(iattc_data)
                          
                          # Deal with special case of IATTC PS
                          iattc_data <- unique(iattc_data)
                          iattc_data <- iattc_data[, columns_to_keep]
                          
                          
                          config$logger.info(sprintf("Case %s data", variable))
                          
                          # Extract tuna catch
                          df_catch_tuna_flag <- as.data.frame(readr::read_csv(dataset_files[names(dataset_files)==dataset_file_PSFlag_tuna_catch], guess_max = 0))
                          df_catch_tuna_flag <- df_catch_tuna_flag[,columns_to_keep]
                          class(df_catch_tuna_flag$measurement_value) <- "numeric"
                          
                          df_catch_tuna_settype <- as.data.frame(readr::read_csv(dataset_files[names(dataset_files)==dataset_file_PSSetType_tuna_catch], guess_max = 0))
                          df_catch_tuna_settype <- df_catch_tuna_settype[,columns_to_keep]
                          class(df_catch_tuna_settype$measurement_value) <- "numeric"
                          
                          # Extract billfish and shark catch.
                          # If the user decides to raise shark/billfish catch to ratio effort tuna / effort shark/billfish:
                          if (options$iattc_ps_catch_billfish_shark_raise_to_effort){
                            
                            # Function to extract the datasets of catch (for billfish and tuna) and raise them to the ratio effort tuna / effort billfish (or effort shark)
                            function_raise_catch_to_effort <- function(dataset_file_tuna_effort,
                                                                       dataset_file_billfish_or_shark_catch,
                                                                       dataset_file_billfish_or_shark_effort,
                                                                       raising_dimensions){
                              
                              config$logger.info(sprintf("Catch file which will be raised to efffort: %s ", dataset_file_billfish_or_shark_catch))
                              
                              
                              billfish_or_shark_catch <- as.data.frame(readr::read_csv(dataset_files[names(dataset_files)==dataset_file_billfish_or_shark_catch], guess_max = 0))
                              billfish_or_shark_catch <- billfish_or_shark_catch[,columns_to_keep]
                              class(billfish_or_shark_catch$measurement_value) <- "numeric"
                              
                              billfish_or_shark_effort <- as.data.frame(readr::read_csv(dataset_files[names(dataset_files)==dataset_file_billfish_or_shark_effort], guess_max = 0))
                              billfish_or_shark_effort <- billfish_or_shark_effort[,columns_to_keep_effort]
                              class(billfish_or_shark_effort$measurement_value) <- "numeric"
                              
                              tuna_effort <- as.data.frame(readr::read_csv(dataset_files[names(dataset_files)==dataset_file_tuna_effort], guess_max = 0))
                              tuna_effort <- tuna_effort[,columns_to_keep_effort]
                              class(tuna_effort$measurement_value) <- "numeric"
                              
                              # Get RF for effort (rf=effort tuna / effort billfish   or    effort tuna / effort shark)
                              source(geoflow::get_config_resource_path(config, "./sardara_functions/raise_get_rf.R"))
                              df_rf <- raise_get_rf(
                                df_input=billfish_or_shark_effort,
                                df_input_total=tuna_effort,
                                x_raising_dimensions=c(raising_dimensions,"measurement_unit")
                              )
                              
                              df_rf$measurement_unit<-NULL
                              
                              # Raise the data
                              source(geoflow::get_config_resource_path(config, "./sardara_functions/raise_incomplete_dataset_to_total_dataset.R"))
                              catch_raised <- raise_incomplete_dataset_to_total_dataset(
                                df_input_incomplete=billfish_or_shark_catch,
                                df_input_total=billfish_or_shark_catch,
                                df_rf=df_rf,
                                x_raising_dimensions=raising_dimensions,
                                threshold_rf=NULL)
                              
                              return(catch_raised$df)
                            }
                            
                            df_catch_billfish_flag <- function_raise_catch_to_effort(dataset_file_tuna_effort=dataset_file_PSFlag_tuna_effort,
                                                                                     dataset_file_billfish_or_shark_catch=dataset_file_PSFlag_billfish_catch,
                                                                                     dataset_file_billfish_or_shark_effort=dataset_file_PSFlag_billfish_effort,
                                                                                     raising_dimensions=c("gear_type","fishing_fleet","time_start","time_end","geographic_identifier"))
                            
                            df_catch_billfish_settype <- function_raise_catch_to_effort(dataset_file_tuna_effort=dataset_file_PSSetType_tuna_effort,
                                                                                        dataset_file_billfish_or_shark_catch=dataset_file_PSSetType_billfish_catch,
                                                                                        dataset_file_billfish_or_shark_effort=dataset_file_PSSetType_billfish_effort,
                                                                                        raising_dimensions=c("gear_type","fishing_mode","time_start","time_end","geographic_identifier"))
                            
                            df_catch_shark_flag <- function_raise_catch_to_effort(dataset_file_tuna_effort=dataset_file_PSFlag_tuna_effort,
                                                                                  dataset_file_billfish_or_shark_catch=dataset_file_PSFlag_shark_catch,
                                                                                  dataset_file_billfish_or_shark_effort=dataset_file_PSFlag_shark_effort,
                                                                                  raising_dimensions=c("gear_type","fishing_fleet","time_start","time_end","geographic_identifier"))
                            
                            df_catch_shark_settype <- function_raise_catch_to_effort(dataset_file_tuna_effort=dataset_file_PSSetType_tuna_effort,
                                                                                     dataset_file_billfish_or_shark_catch=dataset_file_PSSetType_shark_catch,
                                                                                     dataset_file_billfish_or_shark_effort=dataset_file_PSSetType_shark_effort,
                                                                                     raising_dimensions=c("gear_type","fishing_mode","time_start","time_end","geographic_identifier"))
                            
                          } else { # Else do not raise (i.e. for billfish/shark, keep catch only from billfish / shark)
                            df_catch_billfish_flag <- as.data.frame(readr::read_csv(dataset_files[names(dataset_files)==dataset_file_PSFlag_billfish_catch], guess_max = 0))
                            df_catch_billfish_flag <- df_catch_billfish_flag[,columns_to_keep]
                            class(df_catch_billfish_flag$measurement_value) <- "numeric"
                            
                            df_catch_billfish_settype <- as.data.frame(readr::read_csv(dataset_files[names(dataset_files)==dataset_file_PSSetType_billfish_catch], guess_max = 0))
                            df_catch_billfish_settype <- df_catch_billfish_settype[,columns_to_keep]
                            class(df_catch_billfish_settype$measurement_value) <- "numeric"
                            
                            df_catch_shark_flag <- as.data.frame(readr::read_csv(dataset_files[names(dataset_files)==dataset_file_PSFlag_shark_catch],guess_max = 0))
                            df_catch_shark_flag <- df_catch_shark_flag[,columns_to_keep]
                            class(df_catch_shark_flag$measurement_value) <- "numeric"
                            
                            df_catch_shark_settype <- as.data.frame(readr::read_csv(dataset_files[names(dataset_files)==dataset_file_PSSetType_shark_catch],guess_max = 0))
                            df_catch_shark_settype <- df_catch_shark_settype[,columns_to_keep]
                            class(df_catch_shark_settype$measurement_value) <- "numeric"
                          }
                          
                          if(options$iattc_ps_raise_flags_to_schooltype){
                            source(geoflow::get_config_resource_path(config, "./sardara_functions/raise_datasets_by_dimension.R"))
                            df_catch_billfish<-raise_datasets_by_dimension(df1=df_catch_billfish_flag,
                                                                           df2=df_catch_billfish_settype,
                                                                           dimension_missing_df1="fishing_mode",
                                                                           dimension_missing_df2="fishing_fleet")$df
                            
                            df_catch_shark<-raise_datasets_by_dimension(df1=df_catch_shark_flag,
                                                                        df2=df_catch_shark_settype,
                                                                        dimension_missing_df1="fishing_mode",
                                                                        dimension_missing_df2="fishing_fleet")$df
                            
                            df_catch_tuna<-raise_datasets_by_dimension(df1=df_catch_tuna_flag,
                                                                       df2=df_catch_tuna_settype,
                                                                       dimension_missing_df1="fishing_mode",
                                                                       dimension_missing_df2="fishing_fleet")$df
                            
                            
                          } else {
                            # If user decides to not raise flags to type of school, he chooses to use either the data with stratification by fishing_fleet or the data with stratification by schooltype
                            if (options$iattc_ps_dimension_to_use_if_no_raising_flags_to_schooltype == 'fishing_fleet'){
                              df_catch_billfish <- df_catch_billfish_flag
                              df_catch_shark <- df_catch_shark_flag
                              df_catch_tuna <- df_catch_tuna_flag
                            } else if (options$iattc_ps_dimension_to_use_if_no_raising_flags_to_schooltype == 'fishing_mode'){
                              df_catch_billfish <- df_catch_billfish_settype
                              df_catch_shark <- df_catch_shark_settype
                              df_catch_tuna <- df_catch_tuna_settype
                            }
                          }
                          
                          iattc_data <- rbind(iattc_data, df_catch_billfish, df_catch_shark, df_catch_tuna)
                          
                        }else if (variable=="effort"){
                          config$logger.info(sprintf("Case %s data", variable))
                          
                          dataset_file_effort_flag <- switch(options$iattc_ps_effort_to_extract,
                                                             "tuna" = "effort_1deg_1m_ps_iattc_level0__tuna_byflag.csv",
                                                             "billfish" = "effort_1deg_1m_ps_iattc_level0__billfish_byflag.csv",
                                                             "shark" = "effort_1deg_1m_ps_iattc_level0__shark_byflag.csv"
                          )
                          dataset_file_effort_settype <- switch(options$iattc_ps_effort_to_extract,
                                                                "tuna" = "effort_1deg_1m_ps_iattc_level0__tuna_byschool.csv",
                                                                "billfish" = "effort_1deg_1m_ps_iattc_level0__billfish_byschool.csv",
                                                                "shark" = "effort_1deg_1m_ps_iattc_level0__shark_byschool.csv"
                          )
                          
                          # For the effort data, we keep only effort from one of the files (tuna or billfishes or shark). This is driven by the parameter "iattc_ps_effort_to_extract"
                          
                          df_iattc_effort_PSSetType <- as.data.frame(readr::read_csv(dataset_files[names(dataset_files)==dataset_file_effort_settype], guess_max = 0))
                          df_iattc_effort_PSSetType <- df_iattc_effort_PSSetType[,columns_to_keep_effort]
                          class(df_iattc_effort_PSSetType$measurement_value) <- "numeric"
                          df_iattc_effort_PSFlag <- as.data.frame(readr::read_csv(dataset_files[names(dataset_files)==dataset_file_effort_flag], guess_max = 0))
                          df_iattc_effort_PSFlag <- df_iattc_effort_PSFlag[,columns_to_keep_effort]
                          class(df_iattc_effort_PSFlag$measurement_value) <- "numeric"
                          
                          if (options$iattc_ps_raise_flags_to_schooltype){
                            #Get Tuna effort by raising flags to schooltype
                            df <- raise_datasets_by_dimension(df1=df_iattc_effort_PSFlag,
                                                              df2=df_iattc_effort_PSSetType,
                                                              dimension_missing_df1="fishing_mode",
                                                              dimension_missing_df2="fishing_fleet")$df
                            
                          } else {  # If the user decides not to raise flags to type of school, he chooses to use either the data with stratification by fishing_fleet or the data with stratification by schooltype
                            if (options$iattc_ps_dimension_to_use_if_no_raising_flags_to_schooltype == 'fishing_fleet'){
                              df<-df_iattc_effort_PSFlag
                            } else if (options$iattc_ps_dimension_to_use_if_no_raising_flags_to_schooltype == 'fishing_mode'){
                              df<-df_iattc_effort_PSSetType
                            }
                          }
                          
                          iattc_data <- rbind(iattc_data, df)
                        }
                        
                      }else{
                        config$logger.warn(sprintf("Option include_%s = FALSE. Ignoring data...", rfmo))
                      }
                      iattc_data
                    }
  )
  
  return(dataset)
}
