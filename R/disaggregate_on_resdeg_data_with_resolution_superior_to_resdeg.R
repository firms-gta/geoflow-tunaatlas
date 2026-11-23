function_disaggregate_on_resdeg_data_with_resolution_superior_to_resdeg<-function(entity,config,options,georef_dataset,resolution,action_to_do){

con <- config$software$output$dbi

log_info("set parameterization and metadata elements according to selected options")
if (action_to_do=="disaggregate"){
  log_info(paste0("Disaggregating data that are defined on quadrants or areas superior to ",resolution,"\u00b0 quadrant resolution to corresponding ",resolution,"\u00b0 quadrant by dividing the georef_dataset equally on the overlappings ",resolution,"\u00b0 x ",resolution,"\u00b0 quadrants...\n"))
  remove=FALSE 
  lineage<-paste0("Data that were provided at spatial resolutions superior to ",resolution,"\u00b0 x ",resolution,"\u00b0  were disaggregated to the corresponding ",resolution,"\u00b0 x ",resolution,"\u00b0  quadrants by dividing the catch equally on the overlappings ",resolution,"\u00b0 x ",resolution,"\u00b0  quadrants.	Information regarding the spatial disaggregation of data: The data that were expressed on resolutions wider than ",resolution,"\u00b0 grid resolutions and that were disaggregated to the corresponding(s) ",resolution,"\u00b0 quadrants represented stats_data_disaggregated_on_1_deg_weight % of the whole catches expressed in weight in the dataset and stats_data_disaggregated_on_1_deg_number % of the catches expressed in number.")
  description<-paste0("- Data that were provided at resolutions superior to ",resolution,"\u00b0 x ",resolution,"\u00b0 were disaggregated to the corresponding ",resolution,"\u00b0 x ",resolution,"\u00b0quadrants by dividing the catch equally on the overlappings ",resolution,"\u00b0 x ",resolution,"\u00b0 quadrants.\n")
  } else if (action_to_do=="remove"){ 
    log_info(sprintf("Removing data that are defined on quadrants or areas superior to [%s]\u00b0 quadrant resolution ...", resolution))
    remove=TRUE 
    lineage<-paste0("Data that were provided at spatial resolutions superior to ",resolution,"\u00b0 x ",resolution,"\u00b0  were removed.")
    description<-paste0("- Data that were provided at resolutions superior to ",resolution,"\u00b0 x ",resolution,"\u00b0 were removed.\n")
  }


log_info("BEGIN spatial_curation_downgrade_resolution() function")
georef_dataset<-spatial_curation_downgrade_resolution(con,georef_dataset,resolution,remove)
log_info("END spatial_curation_downgrade_resolution() function")
georef_dataset<-georef_dataset$df

log_info(sprintf("Disaggregating / Removing data that are defined on quadrants or areas superior to [%s]\u00b0 quadrant resolution OK", resolution))

return(list(dataset=georef_dataset,lineage=lineage,description=description))

}
