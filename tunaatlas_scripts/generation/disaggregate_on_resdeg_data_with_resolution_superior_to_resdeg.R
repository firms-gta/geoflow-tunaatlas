function_disaggregate_on_resdeg_data_with_resolution_superior_to_resdeg<-function(entity,config,options,georef_dataset,resolution,action_to_do){

con <- config$software$output$dbi

config$logger.info("set parameterization and metadata elements according to selected options")
if (action_to_do=="disaggregate"){
  config$logger.info(paste0("Disaggregating data that are defined on quadrants or areas superior to ",resolution,"° quadrant resolution to corresponding ",resolution,"° quadrant by dividing the georef_dataset equally on the overlappings ",resolution,"° x ",resolution,"° quadrants...\n"))
  remove=FALSE 
  lineage<-paste0("Data that were provided at spatial resolutions superior to ",resolution,"° x ",resolution,"°  were disaggregated to the corresponding ",resolution,"° x ",resolution,"°  quadrants by dividing the catch equally on the overlappings ",resolution,"° x ",resolution,"°  quadrants.	Information regarding the spatial disaggregation of data: The data that were expressed on resolutions wider than ",resolution,"° grid resolutions and that were disaggregated to the corresponding(s) ",resolution,"° quadrants represented stats_data_disaggregated_on_1_deg_weight % of the whole catches expressed in weight in the dataset and stats_data_disaggregated_on_1_deg_number % of the catches expressed in number.")
  description<-paste0("- Data that were provided at resolutions superior to ",resolution,"° x ",resolution,"° were disaggregated to the corresponding ",resolution,"° x ",resolution,"°quadrants by dividing the catch equally on the overlappings ",resolution,"° x ",resolution,"° quadrants.\n")
  } else if (action_to_do=="remove"){ 
    config$logger.info(sprintf("Removing data that are defined on quadrants or areas superior to [%s]° quadrant resolution ...", resolution))
    remove=TRUE 
    lineage<-paste0("Data that were provided at spatial resolutions superior to ",resolution,"° x ",resolution,"°  were removed.")
    description<-paste0("- Data that were provided at resolutions superior to ",resolution,"° x ",resolution,"° were removed.\n")
  }


config$logger.info("BEGIN spatial_curation_downgrade_resolution() function")
source("https://raw.githubusercontent.com/eblondel/geoflow-tunaatlas/master/tunaatlas_scripts/generation/spatial_curation_downgrade_resolution.R")
georef_dataset<-spatial_curation_downgrade_resolution(con,georef_dataset,resolution,remove)
config$logger.info("END spatial_curation_downgrade_resolution() function")
georef_dataset<-georef_dataset$df

config$logger.info(sprintf("Disaggregating / Removing data that are defined on quadrants or areas superior to [%s]° quadrant resolution OK", resolution))

return(list(dataset=georef_dataset,lineage=lineage,description=description))

}
