function_disaggregate_on_resdeg_data_with_resolution_superior_to_resdeg<-function(entity,config,options,georef_dataset,resolution,action_to_do){
if (action_to_do=="disaggregate"){ 
  remove=FALSE 
  cat(paste0("Disaggregating data that are defined on quadrants or areas superior to ",resolution,"° quadrant resolution to corresponding ",resolution,"° quadrant by dividing the georef_dataset equally on the overlappings ",resolution,"° x ",resolution,"° quadrants...\n"))
  # fill metadata elements
  lineage<-paste0("Data that were provided at spatial resolutions superior to ",resolution,"° x ",resolution,"°  were disaggregated to the corresponding ",resolution,"° x ",resolution,"°  quadrants by dividing the catch equally on the overlappings ",resolution,"° x ",resolution,"°  quadrants.	Information regarding the spatial disaggregation of data: The data that were expressed on resolutions wider than ",resolution,"° grid resolutions and that were disaggregated to the corresponding(s) ",resolution,"° quadrants represented stats_data_disaggregated_on_1_deg_weight % of the whole catches expressed in weight in the dataset and stats_data_disaggregated_on_1_deg_number % of the catches expressed in number.")
  description<-paste0("- Data that were provided at resolutions superior to ",resolution,"° x ",resolution,"° were disaggregated to the corresponding ",resolution,"° x ",resolution,"°quadrants by dividing the catch equally on the overlappings ",resolution,"° x ",resolution,"° quadrants.\n")
  } else if (action_to_do=="remove"){ 
    remove=TRUE 
    cat(paste0("Removing data that are defined on quadrants or areas superior to ",resolution,"° quadrant resolution ...\n"))
    # fill metadata elements
    lineage<-paste0("Data that were provided at spatial resolutions superior to ",resolution,"° x ",resolution,"°  were removed.")
    description<-paste0("- Data that were provided at resolutions superior to ",resolution,"° x ",resolution,"° were removed.\n")
    
}
  
georef_dataset<-rtunaatlas::spatial_curation_downgrade_resolution(con,georef_dataset,resolution,remove)
georef_dataset<-georef_dataset$df

cat(paste0("Disaggregating / Removing data that are defined on quadrants or areas superior to ",resolution,"° quadrant resolution OK\n"))

return(list(dataset=georef_dataset,lineage=lineage,description=description))

}
