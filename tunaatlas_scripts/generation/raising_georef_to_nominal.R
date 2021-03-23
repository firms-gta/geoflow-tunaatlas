function_raising_georef_to_nominal<-function(fact,dataset_to_raise,dataset_to_compute_rf,nominal_dataset_df,x_raising_dimensions,raising_do_not_raise_wcfpc_data,raising_raise_only_for_PS_LL){

cat("Raising georeferenced dataset to nominal dataset\n")

# We have to separate the WCPFC and CCSBT from the other rfmos, because WCPFC and CCSBT georef catches do not have flag dimension available (hence we cannot use the flag dimension for the raising)

# function to raise the data 
function_raise_data<-function(fact,source_authority_filter,dataset_to_raise,dataset_to_compute_rf,nominal_dataset_df,x_raising_dimensions){
  
  # filter by source_authority
  dataset_to_raise<-dataset_to_raise[which(dataset_to_raise$source_authority %in% source_authority_filter),]
  dataset_to_compute_rf<-dataset_to_compute_rf[which(dataset_to_compute_rf$source_authority %in% source_authority_filter),]
  nominal_dataset_df<-nominal_dataset_df[which(nominal_dataset_df$source_authority %in% source_authority_filter),]
  
  # calculate raising factor dataset
  df_rf <- rtunaatlas::raise_get_rf(
    df_input_incomplete = dataset_to_compute_rf,
    df_input_total = nominal_dataset_df,
    x_raising_dimensions = c(x_raising_dimensions,"unit")
  ) 
  
  if (fact=="catch"){
    raising_dimensions=c(x_raising_dimensions,"unit")
  } else if (fact=="effort"){
    raising_dimensions=x_raising_dimensions
    df_rf$unit=NULL
  }
  
  # raise dataset
  data_raised<-rtunaatlas::raise_incomplete_dataset_to_total_dataset(df_input_incomplete = dataset_to_raise,
                                                                      df_input_total = nominal_dataset_df,
                                                                      df_rf = df_rf,
                                                                      x_raising_dimensions = raising_dimensions,
                                                                      threshold_rf = NULL)
  return(data_raised)
  
}

if (raising_raise_only_for_PS_LL==TRUE){
  gears_PS_LL<-dbGetQuery(rtunaatlas::db_connection_tunaatlas_world(),"SELECT distinct(src_code) FROM gear.gear_mapping_view WHERE trg_codingsystem='geargroup_tunaatlas' AND trg_code IN ('PS','LL')")$src_code
  dataset_not_PS_LL<-dataset_to_raise %>% filter(!(gear %in% gears_PS_LL))
  dataset_to_raise<-dataset_to_raise %>% filter(gear %in% gears_PS_LL)
}

if ( include_CCSBT==TRUE | include_WCPFC==TRUE ) {
  
  if (raising_do_not_raise_wcfpc_data==TRUE){
    source_authority_filter=c("CCSBT")
  } else {
    source_authority_filter=c("WCPFC","CCSBT")
  }
  
  cat(paste0("Raising georeferenced dataset of CCBST and WCPFC - if included in the Tuna Atlas - by ",paste(setdiff(x_raising_dimensions,"flag"),collapse = ","),"\n"))
  data_WCPFC_CCSBT_raised<-function_raise_data(fact,
                                               source_authority_filter = source_authority_filter,
                                                dataset_to_raise = dataset_to_raise,
                                                dataset_to_compute_rf=dataset_to_compute_rf,
                                                nominal_dataset_df = nominal_dataset_df,
                                                x_raising_dimensions = setdiff(x_raising_dimensions,"flag"))
  if (raising_do_not_raise_wcfpc_data==TRUE){
  data_WCPFC_CCSBT_raised<-rbind(dataset_to_raise %>% filter(source_authority=="WCPFC"),data_WCPFC_CCSBT_raised$df)
  } else {
  data_WCPFC_CCSBT_raised<-data_WCPFC_CCSBT_raised$df
  }
} else {
  data_WCPFC_CCSBT_raised<-NULL
}

if ( include_IOTC==TRUE | include_ICCAT==TRUE | include_IATTC==TRUE ) {

  cat(paste0("Raising georeferenced dataset of IOTC, ICCAT and IATTC - if included in the Tuna Atlas - by ",paste(x_raising_dimensions,collapse = ","),"\n"))
  data_IOTC_ICCAT_IATTC_raised<-function_raise_data(fact,
                                                    source_authority_filter = c("IOTC","ICCAT","IATTC"),
                                                     dataset_to_raise = dataset_to_raise,
                                                     dataset_to_compute_rf=dataset_to_compute_rf,
                                                     nominal_dataset_df = nominal_dataset_df,
                                                     x_raising_dimensions = x_raising_dimensions)
  
  data_IOTC_ICCAT_IATTC_raised<-data_IOTC_ICCAT_IATTC_raised$df
} else {
  data_IOTC_ICCAT_IATTC_raised<-NULL
}

if (raising_raise_only_for_PS_LL==TRUE){
  georef_dataset<-rbind(dataset_not_PS_LL,data_WCPFC_CCSBT_raised,data_IOTC_ICCAT_IATTC_raised)
} else {
  georef_dataset<-rbind(data_WCPFC_CCSBT_raised,data_IOTC_ICCAT_IATTC_raised)
}

rm(data_WCPFC_CCSBT_raised)
rm(data_IOTC_ICCAT_IATTC_raised)

# fill metadata elements
lineage<-paste0("Catch-and-effort data are data aggregated over spatio-temporal strata that are collected by the CPCs or the tRFMOs in some cases. Generally, catch-and-effort data are defined over one month time period and 1° or 5° size square spatial resolution. Following ICCAT, catch and fishing effort statistics are defined as “the complete species (tuna, tuna like species and sharks) catch composition (in weight <kg> or/and in number of fish) obtained by a given amount of effort (absolute value) in a given stratification or detail level (stratum). T2CE are basically data obtained from sampling a portion of the individual fishing operations of a given fishery in a specified period of time.” (ICCAT Task 2). Hence, geo-referenced catch data and associated effort can represent only part of the total catches. Geo-referenced catches were raised to the total catches ",if (raising_raise_only_for_PS_LL==TRUE) {"only for industrial fisheries (longliners and purse seiners) "},"for all tRFMOs ",if (raising_do_not_raise_wcfpc_data==TRUE) {"except for WCPFC since these data are already raised by the organization"},". Depending on the availability of the vessel flag reporting country dimension (currently not available for the geo-referenced catch-and-effort dataset from the Western-Central Pacific Ocean), the dimensions used for the raising are either {Flag, Species, Year, Gear} or {Species, Year, Gear}. Some catches cannot be raised because the combination {Flag, Species, Year, Gear} (resp. {Species, Year, Gear}) does exist in the geo-referenced catches but the same combination does not exist in the total catches. In this case, non-raised catch data were kept. Most catch-and-effort data have catches inferior to the catch available in the nominal catch dataset for a given stratum. However, in some cases the value of catch in the catch-and-effort data can be greater than the one in the nominal catch. In this cas, the catch was ''downgraded'' to the nominal catch one.	Information regarding the raising process for this dataset: Before the raising process, perc_georef_data_over_total_dataset_before_raising % of the catches of the nominal catch datasets were available in the catch-and-effort datasets. After the raising process, this percentage reached perc_georef_data_over_total_dataset_after_raising %. This percentage might not be 100 because of the following reasons: i) perc_georef_data_do_not_exist_in_total_data % of the catches available in the catch-and-effort dataset had no correspondance in the nominal catch (i.e. the strata exists in the catch-and-effort dataset but does not exist in the nominal catch dataset); ii) perc_total_data_do_not_exist_in_georef_data % of the catches available in the nominal catch dataset had no correspondance in the catch-and-effort dataset (i.e. the strata exists in the nominal catch dataset but does not exist in the catch-and-effort dataset")
description<-paste0("- Geo-referenced ",fact," were raised to the total ",fact,if (raising_raise_only_for_PS_LL==TRUE) {" only for industrial fisheries (longliners and purse seiners) "},"\n")
supplemental_information<-paste0("- Geo-referenced ",fact," were raised to the total catches ",if (raising_raise_only_for_PS_LL==TRUE) {"only for industrial fisheries (longliners and purse seiners) "}, "for all tRFMOs",if (raising_do_not_raise_wcfpc_data==TRUE) {" except for WCPFC since these data are already raised by the organization"},". Depending on the availability of the flag dimension (currently not available for the geo-referenced catch-and-effort dataset from the Western-Central Pacific Ocean), the dimensions used for the raising are either {Flag, Species, Year, Gear} or {Species, Year, Gear}. Some ",fact," cannot be raised because the combination {Flag, Species, Year, Gear} (resp. {Species, Year, Gear}) does exist in the geo-referenced ",fact," but the same combination does not exist in the total catches. In this case, non-raised ",fact," data were kept. Most catch-and-effort data have catches inferior to the catch available in the nominal catch dataset for a given stratum. However, in some cases the value of catch in the catch-and-effort data can be greater than the one in the nominal catch. In this case, the catch was 'downgraded' to the nominal catch one.\n")

cat("Raising georeferenced dataset to nominal dataset OK\n")

return(list(dataset=georef_dataset,lineage=lineage,description=description,supplemental_information=supplemental_information))
}
