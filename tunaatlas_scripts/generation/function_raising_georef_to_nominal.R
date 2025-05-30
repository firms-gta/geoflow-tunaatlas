function_raising_georef_to_nominal<-function(con, opts,entity,
                                             config,
                                             dataset_to_raise,
                                             dataset_to_compute_rf,
                                             nominal_dataset_df,
                                             x_raising_dimensions){
  
  fact <- opts$fact
  raising_do_not_raise_wcfpc_data <- opts$raising_do_not_raise_wcfpc_data
  raising_raise_only_for_PS_LL <- opts$raising_raise_only_for_PS_LL
  include_WCPFC <- TRUE
  include_CCSBT <- TRUE
  include_IOTC <- TRUE
  include_IATTC <- TRUE
  include_ICCAT <- TRUE
  
  
  cat("Raising georeferenced dataset to nominal dataset\n")
  config$logger.info("Creating function function_raise_data")
  
  # We have to separate the WCPFC and CCSBT from the other rfmos, because WCPFC and CCSBT georef catches do not have fishing_fleet dimension available (hence we cannot use the fishing_fleet dimension for the raising)
  
  if (raising_raise_only_for_PS_LL==TRUE){
    
    # @juldebar =>  check if gear codes are correct
    gears_PS_LL<-dbGetQuery(con,"SELECT distinct(src_code) FROM gear_type.gear_type_mapping_view WHERE trg_codingsystem='geargroup_tunaatlas' AND trg_code IN ('PS','LL')")$src_code
    # config$logger.info("Now filtering gear codes with those returned by SLQ query",  gears_PS_LL)
    
    dataset_not_PS_LL<-dataset_to_raise %>% dplyr::filter(!(gear_type %in% gears_PS_LL))
    dataset_to_raise<-dataset_to_raise %>% dplyr::filter(gear_type %in% gears_PS_LL)
    
    config$logger.info(paste0("Since option raising_raise_only_for_PS_LL==TRUE, kept rows number is  ",nrow(dataset_to_raise)," and number of removed rows is ",nrow(dataset_not_PS_LL),"\n"))
    
  }
  #@juldebar this condition has to be checked
  config$logger.info(paste0(" checking include_CCSBT and  include_WCPFC options \n"))
  if ( include_CCSBT==TRUE | include_WCPFC==TRUE ) {
    
    #@juldebar what if  include_CCSBT==FASLE and include_WCPFC==TRUE and raising_do_not_raise_wcfpc_data==TRUE ?
    if (raising_do_not_raise_wcfpc_data==TRUE){
      config$logger.info(paste0(" raising_do_not_raise_wcfpc_data is SET to TRUE \n"))
      source_authority_filter=c("CCSBT")
    } else {	
      config$logger.info(paste0(" raising_do_not_raise_wcfpc_data is SET to FALSE \n"))
      source_authority_filter=c("WCPFC","CCSBT")
    }
    
    cat(paste0("Raising georeferenced dataset of CCBST and WCPFC - if included in the Tuna Atlas - by ",paste(setdiff(x_raising_dimensions,"fishing_fleet"),collapse = ","),"\n"))
    config$logger.info(paste0("Raising georeferenced dataset of CCBST and WCPFC - if included in the Tuna Atlas - by ",paste(setdiff(x_raising_dimensions,"fishing_fleet"),collapse = ","),"\n"))
    
    config$logger.info(paste0("Executing function function_raise_data for CCSBT and WCPFC options \n"))
    # class(dataset_to_compute_rf$measurement_value) <- "numeric"
    data_WCPFC_CCSBT_raised<-function_raise_data(fact=fact,
                                                 source_authority_filter = source_authority_filter,
                                                 dataset_to_raise = dataset_to_raise,
                                                 dataset_to_compute_rf=dataset_to_compute_rf,
                                                 nominal_dataset_df = nominal_dataset_df,
                                                 x_raising_dimensions = setdiff(x_raising_dimensions,"fishing_fleet"))
    
    config$logger.info(paste0("Total catch after raising before further filters is ",sum(data_WCPFC_CCSBT_raised$measurement_value),"  \n"))
    
    
    
    if (raising_do_not_raise_wcfpc_data==TRUE){
      
      data_WCPFC_CCSBT_raised<-rbind(dataset_to_raise %>% filter(source_authority=="WCPFC"),data_WCPFC_CCSBT_raised)
      config$logger.info(paste0("Since option raising_do_not_raise_wcfpc_data==TRUE, kept rows number is  ",nrow(data_WCPFC_CCSBT_raised),"  \n"))
      config$logger.info(paste0("Total catch after raising before raising_do_not_raise_wcfpc_data==TRUE filter is ",sum(data_WCPFC_CCSBT_raised$measurement_value),"  \n"))
      
      
    } else {
      
      config$logger.info(paste0("Since option raising_do_not_raise_wcfpc_data==FALSE, kept rows number is  ",nrow(data_WCPFC_CCSBT_raised),"  \n"))
      config$logger.info(paste0("Total catch after raising before raising_do_not_raise_wcfpc_data==FALSE filter is ",sum(data_WCPFC_CCSBT_raised$measurement_value),"  \n"))
      
      
    }
  } else {
    
    data_WCPFC_CCSBT_raised<-NULL
    
    config$logger.info(paste0(" data_WCPFC_CCSBT_raised is set to NULL \n"))
    
  }
  
  config$logger.info(paste0(" checking include_IOTC and  include_ICCAT and  include_IATTC options \n"))
  if ( include_IOTC==TRUE | include_ICCAT==TRUE | include_IATTC==TRUE ) {
    
    cat(paste0("Raising georeferenced dataset of IOTC, ICCAT and IATTC - if included in the Tuna Atlas - by ",paste(x_raising_dimensions,collapse = ","),"\n"))
    config$logger.info(paste0("Raising georeferenced dataset of IOTC, ICCAT and IATTC - if included in the Tuna Atlas - by ",paste(x_raising_dimensions,collapse = ","),"\n"))
    config$logger.info(paste0("Total catch for IOTC / ICCAT / IATTC before raising  is ",sum(dataset_to_raise$measurement_value),"  \n"))
    
    config$logger.info(paste0("Executing function function_raise_data for include_IOTC and  include_ICCAT and  include_IATTC option \n"))
    data_IOTC_ICCAT_IATTC_raised<-function_raise_data(fact,
                                                      source_authority_filter = c("IOTC","ICCAT","IATTC"),
                                                      dataset_to_raise = dataset_to_raise,
                                                      dataset_to_compute_rf=dataset_to_compute_rf,
                                                      nominal_dataset_df = nominal_dataset_df,
                                                      x_raising_dimensions = x_raising_dimensions)
    
    config$logger.info(paste0("Total catch for IOTC / ICCAT / IATTC after raising before further filters is ",sum(data_IOTC_ICCAT_IATTC_raised$measurement_value),"  \n"))
    
    
  } else {
    
    data_IOTC_ICCAT_IATTC_raised<-NULL
    config$logger.info(paste0(" data_IOTC_ICCAT_IATTC_raised is set to NULL \n"))
    
  }
  
  if (raising_raise_only_for_PS_LL==TRUE){
    georef_dataset<-rbind(dataset_not_PS_LL,data_WCPFC_CCSBT_raised,data_IOTC_ICCAT_IATTC_raised)
    config$logger.info(paste0("Total catch for IOTC / ICCAT / IATTC after raising with option raising_raise_only_for_PS_LL==TRUE (data kept only for PS and LL) is ",sum(georef_dataset$measurement_value),"  \n"))
    
  } else {
    georef_dataset<-rbind(data_WCPFC_CCSBT_raised,data_IOTC_ICCAT_IATTC_raised)
    config$logger.info(paste0("Total catch for IOTC / ICCAT / IATTC after raising with option raising_raise_only_for_PS_LL== FALSE is ",sum(georef_dataset$measurement_value),"  \n"))
  }
  
  rm(data_WCPFC_CCSBT_raised)
  rm(data_IOTC_ICCAT_IATTC_raised)
  
  # fill metadata elements
  lineage<-paste0("Catch-and-effort data are data aggregated over spatio-temporal strata that are collected by the CPCs or the tRFMOs in some cases. Generally, catch-and-effort data are defined over one month time period and 1° or 5° size square spatial resolution. Following ICCAT, catch and fishing effort statistics are defined as “the complete species (tuna, tuna like species and sharks) catch composition (in weight <kg> or/and in number of fish) obtained by a given amount of effort (absolute value) in a given stratification or detail level (stratum). T2CE are basically data obtained from sampling a portion of the individual fishing operations of a given fishery in a specified period of time.” (ICCAT Task 2). Hence, geo-referenced catch data and associated effort can represent only part of the total catches. Geo-referenced catches were raised to the total catches ",if (raising_raise_only_for_PS_LL==TRUE) {"only for industrial fisheries (longliners and purse seiners) "},"for all tRFMOs ",if (raising_do_not_raise_wcfpc_data==TRUE) {"except for WCPFC since these data are already raised by the organization"},". Depending on the availability of the vessel fishing_fleet reporting country dimension (currently not available for the geo-referenced catch-and-effort dataset from the Western-Central Pacific Ocean), the dimensions used for the raising are either {fishing_fleet, Species, Year, Gear} or {Species, Year, Gear}. Some catches cannot be raised because the combination {fishing_fleet, Species, Year, Gear} (resp. {Species, Year, Gear}) does exist in the geo-referenced catches but the same combination does not exist in the total catches. In this case, non-raised catch data were kept. Most catch-and-effort data have catches inferior to the catch available in the nominal catch dataset for a given stratum. However, in some cases the value of catch in the catch-and-effort data can be greater than the one in the nominal catch. In this cas, the catch was ''downgraded'' to the nominal catch one.	Information regarding the raising process for this dataset: Before the raising process, perc_georef_data_over_total_dataset_before_raising % of the catches of the nominal catch datasets were available in the catch-and-effort datasets. After the raising process, this percentage reached perc_georef_data_over_total_dataset_after_raising %. This percentage might not be 100 because of the following reasons: i) perc_georef_data_do_not_exist_in_total_data % of the catches available in the catch-and-effort dataset had no correspondance in the nominal catch (i.e. the strata exists in the catch-and-effort dataset but does not exist in the nominal catch dataset); ii) perc_total_data_do_not_exist_in_georef_data % of the catches available in the nominal catch dataset had no correspondance in the catch-and-effort dataset (i.e. the strata exists in the nominal catch dataset but does not exist in the catch-and-effort dataset")
  description<-paste0("- Geo-referenced ",fact," were raised to the total ",fact,if (raising_raise_only_for_PS_LL==TRUE) {" only for industrial fisheries (longliners and purse seiners) "},"\n")
  supplemental_information<-paste0("- Geo-referenced ",fact," were raised to the total catches ",if (raising_raise_only_for_PS_LL==TRUE) {"only for industrial fisheries (longliners and purse seiners) "}, "for all tRFMOs",if (raising_do_not_raise_wcfpc_data==TRUE) {" except for WCPFC since these data are already raised by the organization"},". Depending on the availability of the fishing_fleet dimension (currently not available for the geo-referenced catch-and-effort dataset from the Western-Central Pacific Ocean), the dimensions used for the raising are either {fishing_fleet, Species, Year, Gear} or {Species, Year, Gear}. Some ",fact," cannot be raised because the combination {fishing_fleet, Species, Year, Gear} (resp. {Species, Year, Gear}) does exist in the geo-referenced ",fact," but the same combination does not exist in the total catches. In this case, non-raised ",fact," data were kept. Most catch-and-effort data have catches inferior to the catch available in the nominal catch dataset for a given stratum. However, in some cases the value of catch in the catch-and-effort data can be greater than the one in the nominal catch. In this case, the catch was 'downgraded' to the nominal catch one.\n")
  
  
  #cat("Raising georeferenced dataset to nominal dataset OK\n")
  config$logger.info("Raising georeferenced dataset to nominal dataset OK")
  
  return(list(dataset=georef_dataset,lineage=lineage,description=description,supplemental_information=supplemental_information))
}
