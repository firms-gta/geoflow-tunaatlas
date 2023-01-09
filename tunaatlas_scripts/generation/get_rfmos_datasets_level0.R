get_rfmos_datasets_level0 <- function(rfmo, entity, config, options){
  
  variable <- options$fact
  columns_to_keep <- NULL
  if (variable == "catch"){
    columns_to_keep<-c("source_authority","species","gear","fishingfleet","schooltype","time_start","time_end","geographic_identifier","catchtype","unit","value")
  } else if (variable=="effort"){
    columns_to_keep<-c("source_authority","gear","fishingfleet","schooltype","time_start","time_end","geographic_identifier","unit","value")
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
                        class(iotc_data$value) <- "numeric"
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
                        class(wcpfc_data$value) <- "numeric"
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
                        class(ccsbt_data$value) <- "numeric"
                      }else{
                        config$logger.warn(sprintf("Option include_%s = FALSE. Ignoring data...", rfmo))
                      }
                      ccsbt_data
                    },
                    #ICCAT
                    #--------------------------------------------------------------------------------------
                    "ICCAT" = {
                      #For ICCAT, some special case, see below
                      iccat_data <- NULL
                      if(options$include_ICCAT){
                        config$logger.info(sprintf("Get %s data", rfmo))
                        dataset_files_iccat <- dataset_files[regexpr("nominal", names(dataset_files)) < 0 & 
                                                               regexpr("byschool", names(dataset_files)) < 0 &
                                                               regexpr("iccat", names(dataset_files)) > 0]
                        iccat_data <- do.call("rbind", lapply(dataset_files_iccat, readr::read_csv, guess_max = 0))
                        iccat_data <- as.data.frame(iccat_data)

                        class(iccat_data$value) <- "numeric"
                        
                        # Deal with special case of ICCAT PS
                        if (options$iccat_ps_include_type_of_school){ 
                          config$logger.info("Option 'iccat_ps_include_type_of_school' is TRUE. Include Type of school...")
                          dataset_iccat_byschool_file <- dataset_files[regexpr("nominal", names(dataset_files)) < 0 & 
                                                                         regexpr("byschool", names(dataset_files)) > 0 &
                                                                         regexpr("iccat", names(dataset_files)) > 0]
                          iccat_ce_WithSchooltypeInfo <- readr::read_csv(dataset_iccat_byschool_file, guess_max = 0)
                          iccat_ce_WithSchooltypeInfo <- as.data.frame(iccat_ce_WithSchooltypeInfo)
                          iccat_ce_WithSchooltypeInfo <- iccat_ce_WithSchooltypeInfo[, columns_to_keep]
                          class(iccat_ce_WithSchooltypeInfo$value) <- "numeric"
                          
                          # We need to map fishingfleet code list, because fishingfleet code list used in iccat task2 by operation mode dataset is different from fishingfleet code list used in ICCAT task2; however we have to use the same fishingfleet code list for data raising. In other words, we express all ICCAT datasets following ICCAT task2 fishingfleet code list.
                          cl_filename <- "codelist_mapping_flag_iccat_from_ncandcas_flag_iccat.csv"
                          cl_id <- googledrive::drive_get(cl_filename)$id
                          googledrive::drive_download(googledrive::as_id(cl_id), cl_filename, overwrite = TRUE)
                          flag_mapping_flag_iccat_from_ncandcas_to_flag_iccat <- as.data.frame(readr::read_csv(cl_filename, guess_max = 0))
                          source("https://raw.githubusercontent.com/firms-gta/geoflow-tunaatlas/master/map_codelist.R")
                          
                          iccat_ce_WithSchooltypeInfo <- map_codelist(iccat_ce_WithSchooltypeInfo, flag_mapping_flag_iccat_from_ncandcas_to_flag_iccat, "fishingfleet")[[1]]
                          
                          strata_in_withoutschooltype_and_not_in_withshooltype <- dplyr::anti_join (iccat_data, iccat_ce_WithSchooltypeInfo, by=setdiff(columns_to_keep,c("value","schooltype")))
                          strata_in_withoutschooltype_and_not_in_withshooltype <- strata_in_withoutschooltype_and_not_in_withshooltype[, columns_to_keep]
                          iccat_ce_WithSchooltypeInfo <- iccat_ce_WithSchooltypeInfo[, columns_to_keep]
                          # Join datasets: Dataset with the type of school + dataset without the type of school from which we have removed the strata that are also available in the dataset with the type of school.
                          iccat_data <- rbind(strata_in_withoutschooltype_and_not_in_withshooltype, iccat_ce_WithSchooltypeInfo)
                        }
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
                        config$logger.info(sprintf("Get %s data", rfmo))
                        dataset_files_iattc <- dataset_files[regexpr("nominal", names(dataset_files)) < 0 & 
                                                               regexpr("ps", names(dataset_files)) < 0 & 
                                                               regexpr("effort", names(dataset_files)) < 0 &
                                                               regexpr("iattc", names(dataset_files)) > 0]
                        iattc_data <- do.call("rbind", lapply(dataset_files_iattc, readr::read_csv, guess_max = 0))
                        iattc_data <- as.data.frame(iattc_data)
                        
                        # Deal with special case of IATTC PS
                        iattc_data <- unique(iattc_data)
                        
                        columns_to_keep_effort=c("source_authority","gear","fishingfleet","schooltype","time_start","time_end","geographic_identifier","unit","value")
                        
                        # Get metadata of Catch datasets (for tuna, billfish and shark, and stratified by fishingfleet and then by type of school)
                        # dataset_file_PSSetType_tuna_catch <- "catch_1deg_1m_ps_iattc_level0__tuna_byschool.csv"
                        # dataset_file_PSSetType_billfish_catch<- "catch_1deg_1m_ps_iattc_level0__billfish_byschool.csv"
                        # dataset_file_PSSetType_shark_catch <- "catch_1deg_1m_ps_iattc_level0__shark_byschool.csv"
                        # dataset_file_PSFlag_tuna_catch <- "catch_1deg_1m_ps_iattc_level0__tuna_byflag.csv"
                        # dataset_file_PSFlag_billfish_catch <- "catch_1deg_1m_ps_iattc_level0__billfish_byflag.csv"
                        # dataset_file_PSFlag_shark_catch <- "catch_1deg_1m_ps_iattc_level0__shark_byflag.csv"
                        
                        # Get metadata of Effort datasets (for tuna, billfish and shark, and stratified by flag and then by type of school)
                        dataset_file_PSSetType_tuna_effort <- "effort_1deg_1m_ps_iattc_level0__tuna_byschool.csv"
                        dataset_file_PSSetType_billfish_effort <- "effort_1deg_1m_ps_iattc_level0__billfish_byschool.csv"
                        dataset_file_PSSetType_shark_effort <- "effort_1deg_1m_ps_iattc_level0__shark_byschool.csv"
                        dataset_file_PSFlag_tuna_effort <- "effort_1deg_1m_ps_iattc_level0__tuna_byflag.csv"
                        dataset_file_PSFlag_billfish_effort <- "effort_1deg_1m_ps_iattc_level0__billfish_byflag.csv"
                        dataset_file_PSFlag_shark_effort <- "effort_1deg_1m_ps_iattc_level0__shark_byflag.csv"
                        
                        if (variable=="effort"){
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
                          class(df_iattc_effort_PSSetType$value) <- "numeric"
                          df_iattc_effort_PSFlag <- as.data.frame(readr::read_csv(dataset_files[names(dataset_files)==dataset_file_effort_flag], guess_max = 0))
                          df_iattc_effort_PSFlag <- df_iattc_effort_PSFlag[,columns_to_keep_effort]
                          class(df_iattc_effort_PSFlag$value) <- "numeric"
                          
                          if (options$iattc_ps_raise_flags_to_schooltype){
                            #Get Tuna effort by raising flags to schooltype
                            source("https://raw.githubusercontent.com/firms-gta/geoflow-tunaatlas/master/sardara_functions/raise_datasets_by_dimension.R")
                            
                            df <- raise_datasets_by_dimension(df1=df_iattc_effort_PSFlag,
                                                              df2=df_iattc_effort_PSSetType,
                                                              dimension_missing_df1="schooltype",
                                                              dimension_missing_df2="fishingfleet")$df
                            
                          } else {  # If the user decides not to raise flags to type of school, he chooses to use either the data with stratification by fishingfleet or the data with stratification by schooltype
                            if (options$iattc_ps_dimension_to_use_if_no_raising_flags_to_schooltype == 'fishingfleet'){
                              df<-df_iattc_effort_PSFlag
                            } else if (options$iattc_ps_dimension_to_use_if_no_raising_flags_to_schooltype == 'schooltype'){
                              df<-df_iattc_effort_PSSetType
                            }
                          }
                          
                          iattc_data <- rbind(iattc_data, df)
                        }
                        
                        ## IATTC PS catch-and-effort are stratified as following:
                        # - 1 dataset for tunas, stratified by type of school (but not fishingfleet)
                        # - 1 dataset for tunas, stratified by fishingfleet (but not type of school)
                        # - 1 dataset for billfishes, stratified by type of school (but not fishingfleet)
                        # - 1 dataset for billfishes, stratified by fishingfleet (but not type of school)
                        # - 1 dataset for sharks, stratified by type of school (but not fishingfleet)
                        # - 1 dataset for sharks, stratified by fishingfleet (but not type of school)
                        ## So in total there are 6 datasets. 
                        
                        # Commentaire Emmanuel Chassot: L’effort est exprimé ici en nombre de calées. Cela signifie dans le cas de l’EPO que les efforts donnés dans certains jeux de données peuvent correspondre à une partie de l’effort total alloué à une strate puisqu’il s’agit de l’effort observé, cà-d. pour lequel il y avait un observateur à bord du senneur. De mon point de vue, (1) L’effort unique et homogène serait celui des thons tropicaux et (2) pour uniformiser le jeu de captures par strate, il faut calculer un ratio de captures de requins par calée (observée) et de porte-épées par calée (observée) et de les multiplier ensuite par l’effort reporté pour les thons tropicaux puisqu’on considère que c’est l’effort de la pêcherie (qui cible les thons). Le raising factor est effort thons / effort billfish et effort thons / effort sharks.
                        
                        
                      }else{
                        config$logger.warn(sprintf("Option include_%s = FALSE. Ignoring data...", rfmo))
                      }
                      iattc_data

                    }
  )
  
  return(dataset)
}
