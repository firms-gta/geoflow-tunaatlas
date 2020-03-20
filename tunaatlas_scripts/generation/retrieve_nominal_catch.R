retrieve_nominal_catch <- function(entity, config, include_IOTC, include_IATTC, include_WCPFC, include_CCSBT, include_ICCAT,
									   iccat_nominal_catch_spatial_stratification = "sampling_area"){
									   
	
	con <- config$software$output$dbi
	
	firms_contact <- config$getContacts()[sapply(config$getContacts(), function(x){x$id == "firms-secretariat@fao.org"})][[1]]
	firms_contact$setRole("processor")
	
	include_rfmo<-c(include_IOTC,include_IATTC,include_WCPFC,include_CCSBT,include_ICCAT)

	# There are 2 ICCAT datasets for nominal catch: one that provides the stratification by Sampling areas, and one that provides the stratification by Stock areas. For nominal catch, the user decides as input parameter which one he wants to keep.
	if (iccat_nominal_catch_spatial_stratification=="sampling_area"){
		iccat_nominal_catch_dataset_permanent_identifier<-"atlantic_nominal_catch_iccat_level0__bysamplingarea"
	} else if (iccat_nominal_catch_spatial_stratification=="stock_area"){
		iccat_nominal_catch_dataset_permanent_identifier<-"atlantic_nominal_catch_iccat_level0__bystockarea"
	}

	rfmo<-c("IOTC","IATTC","WCPFC","CCSBT","ICCAT")
	nominal_catch_datasets_permanent_identifiers<-c("indian_nominal_catch_iotc_level0","east_pacific_nominal_catch_iattc_level0","west_pacific_nominal_catch_wcpfc_level0","southern_hemisphere_nominal_catch_ccsbt_level0__bygear",iccat_nominal_catch_dataset_permanent_identifier)
	nominal_catch_contact_originators<-c("fabio.fiorellato@iotc.org","nvogel@iattc.org","peterw@spc.int","cmillar@ccsbt.org","carlos.palma@iccat.int")
	nominal_catch_datasets_permanent_identifiers_to_keep<-NULL
	for (i in 1:length(include_rfmo)){
	  if (include_rfmo[i]=="TRUE"){
		nominal_catch_datasets_permanent_identifiers_to_keep<-paste0(nominal_catch_datasets_permanent_identifiers_to_keep,",'",nominal_catch_datasets_permanent_identifiers[i],"'")
		
		# fill metadata elements
		rfmo_contact <- config$getContacts()[sapply(config$getContacts(), function(x){x$id == nominal_catch_contact_originators[i]})][[1]]
		rfmo_contact$setRole("originator")
		rfmo_step <- geoflow_process$new()
		rfmo_step$setRationale(paste0("Public domain datasets from ",rfmo[i]," were collated through the RFMO website. Their structure (i.e. column organization and names) was harmonized and they were loaded in the Tuna atlas database."))
		rfmo_step$setProcessor(firms_contact) #TODO define who's the processor
		entity$addContact(rfmo_contact)
		entity$provenance$processes <- c(entity$provenance$processes, rfmo_step)
	  }
	}
	nominal_catch_datasets_permanent_identifiers_to_keep<-substring(nominal_catch_datasets_permanent_identifiers_to_keep, 2)

	rfmo_nominal_catch_metadata<-dbGetQuery(con,paste0("SELECT * from metadata.metadata where persistent_identifier IN (",nominal_catch_datasets_permanent_identifiers_to_keep,")"))
	nominal_catch<-rtunaatlas::extract_and_merge_multiple_datasets(con,rfmo_nominal_catch_metadata,columns_to_keep=c("source_authority","species","gear","flag","time_start","time_end","geographic_identifier","unit","value"))

	# For ICCAT Nominal catch, we need to map flag code list, because flag code list used in nominal catch dataset is different from flag code list used in ICCAT task2; however we have to use the same flag code list for data raising. In other words, we express all ICCAT datasets following ICCAT task2 flag code list.
	if (include_ICCAT){
	  # extract mapping
	  df_mapping<-rtunaatlas::extract_dataset(con,rtunaatlas::list_metadata_datasets(con,identifier="codelist_mapping_flag_iccat_from_ncandcas_flag_iccat"))
	  df_mapping$source_authority<-"ICCAT"
	  
	  nominal_catch_iccat<-nominal_catch %>% filter (source_authority=="ICCAT")
	  nominal_catch_other_rfmos<-nominal_catch %>% filter (source_authority!="ICCAT")
	  
	  nominal_catch_iccat<-rtunaatlas::map_codelist(nominal_catch_iccat,df_mapping,"flag")$df 
	  
	  nominal_catch<-rbind(nominal_catch_other_rfmos,nominal_catch_iccat)
	  
	  # fill metadata elements
	  iccat_contact <- config$getContacts()[sapply(config$getContacts(), function(x){x$id == "carlos.palma@iccat.int"})][[1]]
	  iccat_contact$setRole("originator")
	  iccat_step <- geoflow_process$new()
	  iccat_step$setRationale("Public domain datasets from ICCAT were collated through the RFMO website (www.iccat.int). Their structure (i.e. column organization and names) was harmonized and they were loaded in the Tuna atlas database.")
	  iccat_step$setProcessor(firms_contact) #TODO define who's the processor
	  entity$addContact(iccat_contact)
	  entity$provenance$processes <- c(entity$provenance$processes, iccat_step)
	  
	}

	nominal_catch$time_start<-substr(as.character(nominal_catch$time_start), 1, 10)
	nominal_catch$time_end<-substr(as.character(nominal_catch$time_end), 1, 10)
	return(nominal_catch)
}	