#' Convert the units of a dataset with conversion factors
#'
#' Converts the measures of a georeferenced dataset (catch in number of fish,
#' or effort units) with a table of conversion factors, through convert_units()
#' (R/tunaatlas_scripts/generation/convert_units.R). When the code lists were
#' not mapped, the gears (and the effort units) are first mapped to the code
#' lists of the conversion factors, from the mappings of the database.
#'
#' @param georef_dataset The dataset to convert (data frame).
#' @param df_conversion_factor The conversion factors (data frame); the columns
#'   `gear`, `unit` and `measurement_value` are accepted for `gear_type`,
#'   `measurement_unit` and `conversion_factor`.
#' @param con Connection to the Tuna Atlas database, or NULL. Only needed when
#'   the code lists of the dataset were not mapped (mappings stored in the
#'   database) or for areas of conversion factors other than the CWP grid and
#'   areas_conversion_factors_numtoweight_ird.
#' @param fact `"catch"` or `"effort"`.
#' @param unit_conversion_codelist_geoidentifiers_conversion_factors Code list
#'   of the areas of the conversion factors.
#' @param mapping_map_code_lists Whether the code lists of the dataset were
#'   already mapped.
#' @param removing_numberfish_final For catch, remove the rows still in number
#'   of fish after the conversion.
#' @return The converted dataset (data frame).
#' @export
gta_unit_conversion <- function(georef_dataset, df_conversion_factor, con = NULL, fact = "catch",
                                unit_conversion_codelist_geoidentifiers_conversion_factors,
                                mapping_map_code_lists = FALSE, removing_numberfish_final = TRUE) {

  if("gear" %in% colnames(df_conversion_factor)){
    df_conversion_factor <- df_conversion_factor %>% dplyr::rename(gear_type = gear)
  }
  
  if("unit" %in% colnames(df_conversion_factor)){
    df_conversion_factor <- df_conversion_factor %>% dplyr::rename(measurement_unit = unit)
  }
  
  
  
  if("measurement_value" %in% colnames(df_conversion_factor)){
    df_conversion_factor <- df_conversion_factor %>% dplyr::rename(conversion_factor = measurement_value)
  }
  
  df_conversion_factor <- df_conversion_factor %>%
    dplyr::mutate(
      measurement_unit = dplyr::case_when(
        measurement_unit %in% c("MT", "t") ~ "t",
        measurement_unit %in% c("NO", "no") ~ "no",
        TRUE ~ as.character(measurement_unit)
      )
    ) %>%
    dplyr::mutate(
      unit_target = dplyr::case_when(
        unit_target %in% c("MT", "t") ~ "t",
        unit_target %in% c("NO", "no") ~ "no",
        TRUE ~ as.character(unit_target)
      )
    )
  
  
  if (!mapping_map_code_lists){
    message("'mapping_map_code_lists' option is set to TRUE")
    
    source_authority<-c("IOTC","ICCAT","IATTC","WCPFC","CCSBT")
    db_mapping_dataset_name<-c("codelist_mapping_gear_iotc_isscfg_revision_1","codelist_mapping_gear_iccat_isscfg_revision_1","codelist_mapping_gear_iattc_isscfg_revision_1","codelist_mapping_gear_wcpfc_isscfg_revision_1","codelist_mapping_gear_ccsbt_isscfg_revision_1")
    mapping_dataset<-data.frame(source_authority,db_mapping_dataset_name)
    df_mapping_final_this_dimension<-NULL
    for (j in 1:nrow(mapping_dataset)){ 
      df_mapping<-extract_dataset(con,list_metadata_datasets(con,identifier=mapping_dataset$db_mapping_dataset_name[j]))  # Extract the code list mapping dataset from the DB
      df_mapping$source_authority<-as.character(mapping_dataset$source_authority[j])  # Add the dimension "source_authority" to the mapping dataset. That dimension is not included in the code list mapping datasets. However, it is necessary to map the code list.
      df_mapping_final_this_dimension<-rbind(df_mapping_final_this_dimension,df_mapping)
    }
    #georef_dataset with source coding system for gears mapped with isscfg codes:
    georef_dataset<-map_codelist(georef_dataset,df_mapping_final_this_dimension,"gear_type",TRUE)$df
    
    if (fact=="effort"){
      ## If we have not mapped the code lists (i.e. if mapping_map_code_lists==FALSE), we need to map the source unit coding system with tuna atlas coding system. In fact, the conversion factors dataset is expressed with tuna atlas coding system for units, while the primary tRFMOs datasets are expressed with their own unit coding system.
      source_authority<-c("IOTC","ICCAT","IATTC","WCPFC","CCSBT")
      db_mapping_dataset_name<-c("codelist_mapping_effortunit_iotc_effortunit_rfmos","codelist_mapping_effortunit_iccat_effortunit_rfmos","codelist_mapping_effortunit_iattc_effortunit_rfmos","codelist_mapping_effortunit_wcpfc_effortunit_rfmos","codelist_mapping_effortunit_ccsbt_effortunit_rfmos")
      mapping_dataset<-data.frame(source_authority,db_mapping_dataset_name)
      df_mapping_final_this_dimension<-NULL
      for (j in 1:nrow(mapping_dataset)){ 
        
        df_mapping<-extract_dataset(con,list_metadata_datasets(con,identifier=mapping_dataset$db_mapping_dataset_name[j]))  # Extract the code list mapping dataset from the DB
        df_mapping$source_authority<-as.character(mapping_dataset$source_authority[j])  # Add the dimension "source_authority" to the mapping dataset. That dimension is not included in the code list mapping datasets. However, it is necessary to map the code list.
        df_mapping_final_this_dimension<-rbind(df_mapping_final_this_dimension,df_mapping)
      }
      #georef_dataset with source coding system for units mapped with tuna atlas codes:
      
      georef_dataset<-map_codelist(georef_dataset,df_mapping_final_this_dimension,"measurement_unit",TRUE)$df
    }
    
  }
  message("'mapping_map_code_lists' option is set to FALSE")
  
  
  
  message("Execute rtunaatlas::convert_units() function")
  message(sprintf("Gridded catch dataset before tunaatlas::convert_units() has [%s] lines", nrow(georef_dataset)))
  if(fact == "catch"){
    sum_no_before <- georef_dataset %>% filter(measurement_unit=="no")  %>% select(measurement_value)  %>% sum()
    species_no_before <- georef_dataset %>% filter(measurement_unit=="no") %>% distinct(species)
    cat(species_no_before$species)
    cat(intersect(species_no_before$species,unique(df_conversion_factor$species)))
  }
  
  georef_dataset<-convert_units(con = con,
                                df_input = georef_dataset,
                                df_conversion_factor = df_conversion_factor,
                                codelist_geoidentifiers_df_input = "cwp_grid",
                                codelist_geoidentifiers_conversion_factors = unit_conversion_codelist_geoidentifiers_conversion_factors
  )
  # to get stats on the process (useful for metadata)
  stats<-georef_dataset$stats
  georef_dataset<-georef_dataset$df
  message(sprintf("fact is", fact))
  
  #check what species didn't get  conversion factors from IRD file
  if(fact == "catch"){
    species_no_after <- georef_dataset %>% filter(measurement_unit=="no") %>% distinct(species)
    cat(setdiff(species_no_before$species,species_no_after$species))
    cat(intersect(species_no_after$species,unique(df_conversion_factor$species)))
    #@juldebar => issue with SKJ and IOTC
    # test <- df_conversion_factor %>% filter(species=='SKJ')
    # df_conversion_factor %>% filter(species=='SKJ',source_authority=='IOTC')
  }
  message(sprintf("Statitstics are : \n [%s]", georef_dataset$stats))
  message("rtunaatlas::convert_units() function executed !")
  message(sprintf("Gridded catch dataset after tunaatlas::convert_units() has [%s] lines", nrow(georef_dataset)))
  
  
  #filter by unit MT
  #@juldebar => must be "t" now with changes on Level 0
  #@eblondel => to refactor to align on standard units
  if(fact == "catch"){
    sum_no_after<- georef_dataset %>% filter(measurement_unit=="no")  %>% select(measurement_value)  %>% sum()
    nrow_no <- nrow(georef_dataset %>% filter(measurement_unit=="no")  %>% select(measurement_value))
    # sum_t <- df %>% filter(measurement_unit=="t")  %>% select(measurement_value)  %>% sum()
    message(sprintf("Gridded catch dataset has [%s] lines using 'number' as unit of measure", nrow_no))
    message(sprintf("Now removing all lines still using 'number' (NO) as unit of measure and still representing a total of [%s] inidviduals", sum_no_after))
    if(removing_numberfish_final){
      georef_dataset <- georef_dataset[georef_dataset$measurement_unit == "t", ]}
    
    #georef_dataset <- georef_dataset[georef_dataset$measurement_unit == "t", ]
    message(sprintf("Ratio of converted numbers is [%s] due to lack on conversion factors for some dimensions (species, time, gears..)", 1-sum_no_after/sum_no_before))
  }
  
  if (mapping_map_code_lists=="FALSE"){
    # resetting gear coding system to primary gear coding system
    georef_dataset$gear_type<-NULL
    colnames(georef_dataset)[colnames(georef_dataset) == 'gear_src_code'] <- 'gear_type'
    if (fact=="effort"){
      # We keep tuna atlas effort unit codes although this is not perfect, but going back to tRFMOs codes is too complicated 
      georef_dataset$unit_src_code<-NULL
    }
  }
  
  # fill metadata elements
  georef_dataset
}
