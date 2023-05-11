do_unit_conversion  <- function(entity, config,fact,unit_conversion_csv_conversion_factor_url,unit_conversion_codelist_geoidentifiers_conversion_factors,mapping_map_code_lists = FALSE, georef_dataset, removing_numberfish_final = TRUE, converting_dataset_mapped  = TRUE){
  source("https://raw.githubusercontent.com/firms-gta/geoflow-tunaatlas/master/sardara_functions/extract_dataset.R")
  source("https://raw.githubusercontent.com/firms-gta/geoflow-tunaatlas/master/tunaatlas_scripts/generation/convert_units.R")
  source("https://raw.githubusercontent.com/firms-gta/geoflow-tunaatlas/master/sardara_functions/list_metadata_datasets.R")
  source("https://raw.githubusercontent.com/firms-gta/geoflow-tunaatlas/master/map_codelist.R")
  con <- config$software$output$dbi
  
  if(is.data.frame(unit_conversion_csv_conversion_factor_url)){
    
    df_conversion_factor <- unit_conversion_csv_conversion_factor_url
  } else {
  
  config$logger.info("Reading the conversion factors dataset")
  googledrive_baseurl <- "https://drive.google.com/open?id="
  if(startsWith(unit_conversion_csv_conversion_factor_url, googledrive_baseurl)){
    #managing download through google drive
    config$logger.info("Downloading file using Google Drive R interface")
    drive_id <- unlist(strsplit(unit_conversion_csv_conversion_factor_url, "id="))[2]
    drive_id <- unlist(strsplit(drive_id, "&export"))[1] #control in case export param is appended
    googledrive::drive_download(file = googledrive::as_id(drive_id), path = file.path("data", paste0(entity$identifiers[["id"]], "_conversion_factors.csv")), overwrite = TRUE)
    df_conversion_factor <- as.data.frame(readr::read_csv(file.path("data", paste0(entity$identifiers[["id"]], "_conversion_factors.csv")),guess_max=0))
  }else{
    df_conversion_factor <- as.data.frame(readr::read_csv(unit_conversion_csv_conversion_factor_url, guess_max=0))
  }
  }
  ## If we have not mapped the code lists (i.e. if mapping_map_code_lists==FALSE), we need to map the source gear coding system with ISSCFG coding system. In fact, the conversion factors dataset is expressed with ISSCFG coding system for gears, while the primary tRFMOs datasets are expressed with their own gear coding system.
  config$logger.info("Checking if 'mapping_map_code_lists' option is set to TRUE")
  
  if("value" %in% colnames(df_conversion_factor)){
    df_conversion_factor <- df_conversion_factor %>% dplyr::rename(conversion_factor = value)
  }
  
  
  if (!mapping_map_code_lists){
    config$logger.info("'mapping_map_code_lists' option is set to TRUE")
    
    source_authority<-c("IOTC","ICCAT","IATTC","WCPFC","CCSBT")
    db_mapping_dataset_name<-c("codelist_mapping_gear_iotc_isscfg_revision_1","codelist_mapping_gear_iccat_isscfg_revision_1","codelist_mapping_gear_iattc_isscfg_revision_1","codelist_mapping_gear_wcpfc_isscfg_revision_1","codelist_mapping_gear_ccsbt_isscfg_revision_1")
    mapping_dataset<-data.frame(source_authority,db_mapping_dataset_name)
    df_mapping_final_this_dimension<-NULL
    for (j in 1:nrow(mapping_dataset)){ 
      source("https://raw.githubusercontent.com/firms-gta/geoflow-tunaatlas/master/sardara_functions/extract_dataset.R")
      df_mapping<-extract_dataset(con,list_metadata_datasets(con,identifier=mapping_dataset$db_mapping_dataset_name[j]))  # Extract the code list mapping dataset from the DB
      df_mapping$source_authority<-as.character(mapping_dataset$source_authority[j])  # Add the dimension "source_authority" to the mapping dataset. That dimension is not included in the code list mapping datasets. However, it is necessary to map the code list.
      df_mapping_final_this_dimension<-rbind(df_mapping_final_this_dimension,df_mapping)
    }
    #georef_dataset with source coding system for gears mapped with isscfg codes:
    source("https://raw.githubusercontent.com/firms-gta/geoflow-tunaatlas/master/map_codelist.R")
    georef_dataset<-map_codelist(georef_dataset,df_mapping_final_this_dimension,"gear",TRUE)$df
    
    if (fact=="effort"){
      ## If we have not mapped the code lists (i.e. if mapping_map_code_lists==FALSE), we need to map the source unit coding system with tuna atlas coding system. In fact, the conversion factors dataset is expressed with tuna atlas coding system for units, while the primary tRFMOs datasets are expressed with their own unit coding system.
      source_authority<-c("IOTC","ICCAT","IATTC","WCPFC","CCSBT")
      db_mapping_dataset_name<-c("codelist_mapping_effortunit_iotc_effortunit_rfmos","codelist_mapping_effortunit_iccat_effortunit_rfmos","codelist_mapping_effortunit_iattc_effortunit_rfmos","codelist_mapping_effortunit_wcpfc_effortunit_rfmos","codelist_mapping_effortunit_ccsbt_effortunit_rfmos")
      mapping_dataset<-data.frame(source_authority,db_mapping_dataset_name)
      df_mapping_final_this_dimension<-NULL
      for (j in 1:nrow(mapping_dataset)){ 
        source("https://raw.githubusercontent.com/firms-gta/geoflow-tunaatlas/master/sardara_functions/extract_dataset.R")
        
        df_mapping<-extract_dataset(con,list_metadata_datasets(con,identifier=mapping_dataset$db_mapping_dataset_name[j]))  # Extract the code list mapping dataset from the DB
        df_mapping$source_authority<-as.character(mapping_dataset$source_authority[j])  # Add the dimension "source_authority" to the mapping dataset. That dimension is not included in the code list mapping datasets. However, it is necessary to map the code list.
        df_mapping_final_this_dimension<-rbind(df_mapping_final_this_dimension,df_mapping)
      }
      #georef_dataset with source coding system for units mapped with tuna atlas codes:
      source("https://raw.githubusercontent.com/firms-gta/geoflow-tunaatlas/master/map_codelist.R")
      
      georef_dataset<-map_codelist(georef_dataset,df_mapping_final_this_dimension,"unit",TRUE)$df
    }
    
  }
  config$logger.info("'mapping_map_code_lists' option is set to FALSE")
  
  
  
  config$logger.info("Execute rtunaatlas::convert_units() function")
  config$logger.info(sprintf("Gridded catch dataset before tunaatlas::convert_units() has [%s] lines", nrow(georef_dataset)))
  if(fact == "catch"){
    sum_no_before <- georef_dataset %>% filter(unit=="no")  %>% select(value)  %>% sum()
    species_no_before <- georef_dataset %>% filter(unit=="no") %>% distinct(species)
    cat(species_no_before$species)
    cat(intersect(species_no_before$species,unique(df_conversion_factor$species)))
  }
  source("https://raw.githubusercontent.com/eblondel/geoflow-tunaatlas/master/tunaatlas_scripts/generation/convert_units.R")
  georef_dataset<-convert_units(con = con,
                                df_input = georef_dataset,
                                df_conversion_factor = df_conversion_factor,
                                codelist_geoidentifiers_df_input = "areas_tuna_rfmos_task2",
                                codelist_geoidentifiers_conversion_factors = unit_conversion_codelist_geoidentifiers_conversion_factors
  )
  # to get stats on the process (useful for metadata)
  stats<-georef_dataset$stats
  georef_dataset<-georef_dataset$df
  config$logger.info(sprintf("fact is", fact))
  
  #check what species didn't get  conversion factors from IRD file
  if(fact == "catch"){
    species_no_after <- georef_dataset %>% filter(unit=="no") %>% distinct(species)
    cat(setdiff(species_no_before$species,species_no_after$species))
    cat(intersect(species_no_after$species,unique(df_conversion_factor$species)))
    #@juldebar => issue with SKJ and IOTC
    # test <- df_conversion_factor %>% filter(species=='SKJ')
    # df_conversion_factor %>% filter(species=='SKJ',source_authority=='IOTC')
  }
  config$logger.info(sprintf("Statitstics are : \n [%s]", georef_dataset$stats))
  config$logger.info("rtunaatlas::convert_units() function executed !")
  config$logger.info(sprintf("Gridded catch dataset after tunaatlas::convert_units() has [%s] lines", nrow(georef_dataset)))
  
  
  #filter by unit MT
  #@juldebar => must be "t" now with changes on Level 0
  #@eblondel => to refactor to align on standard units
  if(fact == "catch"){
    sum_no_after<- georef_dataset %>% filter(unit=="no")  %>% select(value)  %>% sum()
    nrow_no <- nrow(georef_dataset %>% filter(unit=="no")  %>% select(value))
    # sum_t <- df %>% filter(unit=="t")  %>% select(value)  %>% sum()
    config$logger.info(sprintf("Gridded catch dataset has [%s] lines using 'number' as unit of measure", nrow_no))
    config$logger.info(sprintf("Now removing all lines still using 'number' (NO) as unit of measure and still representing a total of [%s] inidviduals", sum_no_after))
    if(removing_numberfish_final){
      georef_dataset <- georef_dataset[georef_dataset$unit == "t", ]}
    
    #georef_dataset <- georef_dataset[georef_dataset$unit == "t", ]
    config$logger.info(sprintf("Ratio of converted numbers is [%s] due to lack on conversion factors for some dimensions (species, time, gears..)", 1-sum_no_after/sum_no_before))
  }
  
  if (mapping_map_code_lists=="FALSE"){
    # resetting gear coding system to primary gear coding system
    georef_dataset$gear<-NULL
    colnames(georef_dataset)[colnames(georef_dataset) == 'gear_src_code'] <- 'gear'
    if (fact=="effort"){
      # We keep tuna atlas effort unit codes although this is not perfect, but going back to tRFMOs codes is too complicated 
      georef_dataset$unit_src_code<-NULL
    }
  }
  
  # fill metadata elements
  config$logger.info("Fill metadata elements accordingly")
  
  lineage <- ""
  description <- ""
  info <- NULL
  if(!is.data.frame(unit_conversion_csv_conversion_factor_url)){
  if (unit_conversion_csv_conversion_factor_url=="https://drive.google.com/open?id=1csQ5Ww8QRTaYd1DG8chwuw0UVUOGkjNL"){
    lineage <- paste0("The units used to express catches may vary between tRFMOs datasets. Catches are expressed in weight, or in number of fishes, or in both weights and numbers in the same stratum. Values expressed in weight were kept and numbers were converted into weight using simple conversion matrices (A. Fonteneau, pers. com). These conversion factors depend on the species, the gear, the year and the main geographical area (equatorial or tropical). They were computed from the Japanese and Taiwanese size-frequency data as well as from the Japanese total catches and catch-and-effort data. The factors of conversion are available here: ",unit_conversion_csv_conversion_factor_url," and the methodology to compute these factors is available here: http://data.d4science.org/ZWFMa3JJUHBXWk9NTXVPdFZhbU5BUFEyQnhUeWd1d3lHbWJQNStIS0N6Yz0. Some data might not be converted at all because no conversion factor exists for the stratum: these data were kept in number. Information regarding the conversions of catch units for this dataset: ratio_converted_number % of the the catches that were originally expressed in number have been converted into weight through the conversion factors.")
    description <- "- Values of catch were expressed in weight converting numbers using matrices of average weights varying with species, fishing gear, year and large geographical areas, i.e. equatorial or tropical (A. Fonteneau, pers.com). Average weights were computed from the Japanese and Taiwanese size-frequency data as well as from the Japanese total catches and catch-and-effort data. Some data might not be converted at all because no conversion factor exists for the stratum: those data were kept and the unit of catch was set to Number of fishes harvested."
    info <- "- Data provided in number of fishes harvested for the Southern Bluefin tuna (SBF) were not converted into weight of fishes, because no factors of conversion are available for this species. This might represent a great amount of the data for SBF."
  } else if (unit_conversion_csv_conversion_factor_url=="https://drive.google.com/open?id=15HpUd9muFwjjpjBnZE1a8eDzvISMgz1j"){
    lineage <- paste0("The units used to express efforts may vary between tRFMOs datasets. Values were harmonized at best using conversion factors. These conversion factors depend on the tRFMO and the gears. The factors of conversion are available here: ",unit_conversion_csv_conversion_factor_url,". Some data might not be converted at all because no conversion factor exists for the stratum: these data were kept in the unit they are originally expressed.")
    description <- "- Values of efforts were harmonized using conversion factors. Efforts were converted into number of hooks for longliners and number of fishing days for the other gears. Some data might not be converted at all because no conversion factor exists for the stratum: those data were kept in the unit they are originally expressed."
    info <- NULL
  } else {
    lineage <- paste0("The units used to express the measure may vary between tRFMOs datasets. The measures were harmonized through unit conversion factors located here: ",unit_conversion_csv_conversion_factor_url,". Information regarding the conversions of units for this dataset: ratio_converted_number % of the the catches that were originally expressed in number have been converted into weight through the conversion factors.")
    description <- "- Units for the measures were harmonized."
    info <- NULL
  }
  }
  #@juldebar modify FIRMS-Secretariat@fao.org 
  #@juldebar begin commented
  #@config$logger.info("Fill contact metadata elements")
  #@firms_contact <- config$getContacts()[sapply(config$getContacts(), function(x){x$id == "FIRMS-Secretariat@fao.org"})][[1]]
  #@firms_contact$setRole("processor")
  #@conversion_step <- geoflow_process$new()
  #@conversion_step$setRationale(lineage)
  #@conversion_step$setProcessor(firms_contact)
  #@entity$provenance$processes <- c(entity$provenance$processes, conversion_step)
  #@juldebar end commented
  
  entity$descriptions[["abstract"]] <- paste0(entity$descriptions[["abstract"]], "\n", description)
  if(!is.null(info)){	
    if(!is.null(entity$descriptions[["info"]])){
      entity$descriptions[["info"]] <- paste0(entity$descriptions[["info"]], "\n", info)
    }else{
      entity$setDescription("info", info)
    }
    
  }
  
  config$logger.info("Converting units of georef_dataset OK")
  return(georef_dataset)
}
