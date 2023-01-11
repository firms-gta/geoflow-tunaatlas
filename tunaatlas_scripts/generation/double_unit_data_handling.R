double_unit_data_handling = function(con, entity, config,fact,unit_conversion_csv_conversion_factor_url,unit_conversion_codelist_geoidentifiers_conversion_factors,mapping_map_code_lists = FALSE, georef_dataset){
  
  ## For catches: Convert MTNO to MT and remove NOMT (we do not keep the data that were expressed in number with corresponding value in weight)
  #@juldebar => see what to do with redundant action in Level 0 workflow
  #@eblondel => to refactor to align on standard units
  if (fact=="catch"){
    config$logger.info("Dealing with cacth => Removing old NOMT / MTNO units if any")
    georef_dataset$unit[which(georef_dataset$unit == "MTNO")]<-"MT"
    georef_dataset<-georef_dataset[!(georef_dataset$unit=="NOMT"),]
  } else if (fact=="effort"){
    config$logger.info("Dealing with effort => harmonization of units")
    ## For efforts: 
    # Les strates sont potentiellement exprimées avec plusieurs unités par strates. 
    # Si les states sont exprimées dans au moins une des unités standard, on isole l'unité standard et on supprime les autres unités.
    # Si les strantes ne sont exprimées dans aucune des unités standard:
    # - s'il existe un facteur de conversion pour au moins une des unités, on conserve cette unité et on supprime les autres.
    # - sinon on conserve toutes les unités disponibles 
    
    column_names_df_input<-colnames(georef_dataset)
    vector_standard_effortunits<-c("HOOKS","FDAYS")
    
    # get the units available in each stratum, separated by commas
    df_units_available_in_strata<-aggregate(unit ~., georef_dataset[,setdiff(colnames(georef_dataset),c("value","schooltype","unit_src_code","gear_src_code"))], toString)
    
    colnames(df_units_available_in_strata)[which(names(df_units_available_in_strata) == "unit")] <- "units_available"
    
    # Check for each strata if it is expressed in at least 1 of the standard unit
    df_units_available_in_strata$standard_unit_available_in_strata <- grepl(paste(vector_standard_effortunits,collapse="|"),df_units_available_in_strata$units_available)
    
    # Merge with dataset
    georef_dataset<-left_join(georef_dataset,df_units_available_in_strata)
    
    # Check if there is a conversion factor available for the strata
    df_conversion_factor$conversion_factor_available_in_line<-TRUE
    georef_dataset<-left_join(georef_dataset,df_conversion_factor)
    df_conversion_factor$conversion_factor_available_in_line<-NULL
    
    strata_with_conv_factor_available<-unique(georef_dataset[which(georef_dataset$conversion_factor_available_in_line==TRUE & georef_dataset$standard_unit_available_in_strata==FALSE),c("source_authority","fishingfleet","gear","schooltype","time_start","time_end","geographic_identifier","conversion_factor_available_in_line")])
    colnames(strata_with_conv_factor_available)[which(names(strata_with_conv_factor_available) == "conversion_factor_available_in_line")] <- "conversion_factor_available_in_strata"
    
    georef_dataset<-left_join(georef_dataset,strata_with_conv_factor_available)
    
    georef_dataset$conversion_factor_available_in_line[which(is.na(georef_dataset$conversion_factor_available_in_line))]=FALSE
    georef_dataset$conversion_factor_available_in_strata[which(is.na(georef_dataset$conversion_factor_available_in_strata))]=FALSE
    
    # Remove the unrelevant lines
    # 1) lignes dont les strates équivalentes existent dans une des unités standard et dont la ligne n'est pas exprimée dans une unité standard
    index_to_remove_1<-which(!(georef_dataset$unit %in% vector_standard_effortunits) & georef_dataset$standard_unit_available_in_strata==TRUE)
    # 2) ignes dont les strates équivalentes existent dans aucune des unités standard mais pour lesquelles il existe un facteur de conversion, et dont la ligne n'est pas exprimée dans l'unité correspondant au facteur de conversion
    index_to_remove_2<-which(!(georef_dataset$unit %in% vector_standard_effortunits) & georef_dataset$standard_unit_available_in_strata==FALSE & georef_dataset$conversion_factor_available_in_strata==TRUE & georef_dataset$conversion_factor_available_in_line==FALSE)
    
    index_rows_to_remove<-c(index_to_remove_1,index_to_remove_2)
    
    if (length(index_rows_to_remove)>0){
      georef_dataset<-georef_dataset[-index_rows_to_remove,] 
    }
    
    # Remove the columns added during data processing
    georef_dataset <- georef_dataset[column_names_df_input]
    
  }
}
