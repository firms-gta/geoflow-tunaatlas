spatial_curation_data_mislocated<-function(entity,config,df,spatial_curation_data_mislocated){
  con <- config$software$output$dbi
  source("https://raw.githubusercontent.com/eblondel/geoflow-tunaatlas/master/tunaatlas_scripts/generation/spatial_curation_intersect_areas.R")
  config$logger.info("Reallocating data that are in land areas")
  
  #all the data that are inland or do not have any spatial stratification ("UNK/IND",NA) are dealt (either removed - spatial_curation_data_mislocated=="remove" - or reallocated - spatial_curation_data_mislocated=="reallocate" )
  config$logger.info("Executing spatial_curation_intersect_areas")
  #@juldebar => georef_dataset was not set
  georef_dataset <- df
  areas_in_land<-spatial_curation_intersect_areas(con , georef_dataset ,"areas_tuna_rfmos_task2","gshhs_world_coastlines")
  
  areas_in_land<-areas_in_land$df_input_areas_intersect_intersection_layer %>%
    group_by(geographic_identifier_source_layer) %>%
    summarise(percentage_intersection_total=sum(proportion_source_area_intersection))
  
  areas_in_land<-areas_in_land$geographic_identifier_source_layer[which(areas_in_land$percentage_intersection_total==1)]
  
  areas_with_no_spatial_information<-c("UNK/IND",NA)
  
  if (spatial_curation_data_mislocated=="remove"){ # We remove data that is mislocated
    cat("Removing data that are in land areas...\n")
    # remove rows with areas in land
    georef_dataset<-georef_dataset[ which(!(georef_dataset$geographic_identifier %in% c(areas_in_land,areas_with_no_spatial_information))), ]
    
    # fill metadata elements
    lineage<-paste0("Some data might be mislocated: either located on land areas or without any area information. These data were not kept.	Information regarding the reallocation of mislocated data for this dataset: The data that were mislocated represented percentage_of_total_catches_reallocated_weight % of the whole catches expressed in weight in the dataset and percentage_of_total_catches_reallocated_number % of the catches expressed in number. percentage_catches_on_land_reallocated % of the catches that were removed.")
    description<-"- Data located at land or without any spatial information were removed.\n"
    
    cat("Removing data that are in land areas OK\n")
  }
  
  if (spatial_curation_data_mislocated=="reallocate"){   # We reallocate data that is mislocated (they will be equally distributed on areas with same reallocation_dimensions (month|year|gear|flag|species|schooltype).
    cat("Reallocating data that are in land areas...\n")
    source("https://raw.githubusercontent.com/eblondel/geoflow-tunaatlas/master/tunaatlas_scripts/generation/spatial_curation_function_reallocate_data.R")
    
    catch_curate_data_mislocated<-spatial_curation_function_reallocate_data(df_input = georef_dataset,
                                                                            dimension_reallocation = "geographic_identifier",
                                                                            vector_to_reallocate = c(areas_in_land,areas_with_no_spatial_information),
                                                                            reallocation_dimensions = setdiff(colnames(georef_dataset),c("value","geographic_identifier")))
    georef_dataset<-catch_curate_data_mislocated$df
    
    # fill metadata elements
    lineage<-paste0("Some data might be mislocated: either located on land areas or without any area information. These data were equally redistributed on data at sea on areas with same characteristics (same year, month, gear, flag, species, type of school).	Information regarding the reallocation of mislocated data for this dataset: The data that were mislocated represented percentage_of_total_catches_reallocated_weight % of the whole catches expressed in weight in the dataset and percentage_of_total_catches_reallocated_number % of the catches expressed in number. percentage_catches_on_land_reallocated % of the catches that were mislocated were reallocated on areas at sea.")
    description<-"- Data located at land or without any spatial information were equally redistributed on data at sea in areas described by the same stratification factors, i.e. year, month, gear, flag, species, and type of school.\n"
    
    cat("Reallocating data that are in land areas OK\n")
  }
  
  return(list(dataset=georef_dataset,lineage=lineage,description=description))
  
  
}

