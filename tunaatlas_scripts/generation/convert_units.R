do_unit_conversion <- function(entity, config,fact,unit_conversion_csv_conversion_factor_url,unit_conversion_codelist_geoidentifiers_conversion_factors,mapping_map_code_lists = FALSE, georef_dataset){
  
	con <- config$software$output$dbi
  
	config$logger.info("Converting units of georef_dataset...\n")

	config$logger.info("Reading the conversion factors dataset\n")

	googledrive_baseurl <- "https://drive.google.com/open?id="
	if(startsWith(unit_conversion_csv_conversion_factor_url, googledrive_baseurl)){
		#managing download through google drive
		config$logger.info("Downloading file using Google Drive R interface")
		drive_id <- unlist(strsplit(unit_conversion_csv_conversion_factor_url, "id="))[2]
		drive_id <- unlist(strsplit(drive_id, "&export"))[1] #control in case export param is appended
		googledrive::drive_download(file = googledrive::as_id(drive_id), path = file.path("data", paste0(entity$identifiers[["id"]], "_conversion_factors.csv")))
		df_conversion_factor <- as.data.frame(readr::read_csv(file.path("data", paste0(entity$identifiers[["id"]], "_conversion_factors.csv")),guess_max=0))
	}else{
		df_codelists <- as.data.frame(readr::read_csv(unit_conversion_csv_conversion_factor_url, guess_max=0))
	}

	## If we have not mapped the code lists (i.e. if mapping_map_code_lists==FALSE), we need to map the source gear coding system with ISSCFG coding system. In fact, the conversion factors dataset is expressed with ISSCFG coding system for gears, while the primary tRFMOs datasets are expressed with their own gear coding system.
	if (!mapping_map_code_lists){
	  source_authority<-c("IOTC","ICCAT","IATTC","WCPFC","CCSBT")
	  db_mapping_dataset_name<-c("codelist_mapping_gear_iotc_isscfg_revision_1","codelist_mapping_gear_iccat_isscfg_revision_1","codelist_mapping_gear_iattc_isscfg_revision_1","codelist_mapping_gear_wcpfc_isscfg_revision_1","codelist_mapping_gear_ccsbt_isscfg_revision_1")
	  mapping_dataset<-data.frame(source_authority,db_mapping_dataset_name)
	  df_mapping_final_this_dimension<-NULL
	  for (j in 1:nrow(mapping_dataset)){ 
		df_mapping<-rtunaatlas::extract_dataset(con,list_metadata_datasets(con,identifier=mapping_dataset$db_mapping_dataset_name[j]))  # Extract the code list mapping dataset from the DB
		df_mapping$source_authority<-as.character(mapping_dataset$source_authority[j])  # Add the dimension "source_authority" to the mapping dataset. That dimension is not included in the code list mapping datasets. However, it is necessary to map the code list.
		df_mapping_final_this_dimension<-rbind(df_mapping_final_this_dimension,df_mapping)
	  }
	  #georef_dataset with source coding system for gears mapped with isscfg codes:
	  georef_dataset<-rtunaatlas::map_codelist(georef_dataset,df_mapping_final_this_dimension,"gear",TRUE)$df
	  
	  if (fact=="effort"){
	  ## If we have not mapped the code lists (i.e. if mapping_map_code_lists==FALSE), we need to map the source unit coding system with tuna atlas coding system. In fact, the conversion factors dataset is expressed with tuna atlas coding system for units, while the primary tRFMOs datasets are expressed with their own unit coding system.
		source_authority<-c("IOTC","ICCAT","IATTC","WCPFC","CCSBT")
		db_mapping_dataset_name<-c("codelist_mapping_effortunit_iotc_effortunit_rfmos","codelist_mapping_effortunit_iccat_effortunit_rfmos","codelist_mapping_effortunit_iattc_effortunit_rfmos","codelist_mapping_effortunit_wcpfc_effortunit_rfmos","codelist_mapping_effortunit_ccsbt_effortunit_rfmos")
		mapping_dataset<-data.frame(source_authority,db_mapping_dataset_name)
		df_mapping_final_this_dimension<-NULL
		for (j in 1:nrow(mapping_dataset)){ 
		  df_mapping<-rtunaatlas::extract_dataset(con,list_metadata_datasets(con,identifier=mapping_dataset$db_mapping_dataset_name[j]))  # Extract the code list mapping dataset from the DB
		  df_mapping$source_authority<-as.character(mapping_dataset$source_authority[j])  # Add the dimension "source_authority" to the mapping dataset. That dimension is not included in the code list mapping datasets. However, it is necessary to map the code list.
		  df_mapping_final_this_dimension<-rbind(df_mapping_final_this_dimension,df_mapping)
		}
		#georef_dataset with source coding system for units mapped with tuna atlas codes:
		georef_dataset<-rtunaatlas::map_codelist(georef_dataset,df_mapping_final_this_dimension,"unit",TRUE)$df

		
		}
	}
	## For catches: Convert MTNO to MT and remove NOMT (we do not keep the data that were expressed in number with corresponding value in weight)
	if (fact=="catch"){
		georef_dataset$unit[which(georef_dataset$unit == "MTNO")]<-"MT"
		georef_dataset<-georef_dataset[!(georef_dataset$unit=="NOMT"),]
	} else if (fact=="effort"){
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

		strata_with_conv_factor_available<-unique(georef_dataset[which(georef_dataset$conversion_factor_available_in_line==TRUE & georef_dataset$standard_unit_available_in_strata==FALSE),c("source_authority","flag","gear","schooltype","time_start","time_end","geographic_identifier","conversion_factor_available_in_line")])
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
	
	config$logger.info("Execute rtunaatlas::convert_units() function ...\n")

	georef_dataset<-rtunaatlas::convert_units(con = con,
									 df_input = georef_dataset,
									 df_conversion_factor = df_conversion_factor,
									 codelist_geoidentifiers_df_input = "areas_tuna_rfmos_task2",
									 codelist_geoidentifiers_conversion_factors = unit_conversion_codelist_geoidentifiers_conversion_factors
	)
	
	config$logger.info("rtunaatlas::convert_units() function executed !\n")

	# to get stats on the process (useful for metadata)
	# georef_dataset$stats

	georef_dataset<-georef_dataset$df
	
	#filter by unit MT
	#@juldebar => must be "t" now with changes on Level 0
	georef_dataset <- georef_dataset[georef_dataset$unit == "t", ]

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
	config$logger.info("Fill metadata elements accordingly\n")

	lineage <- 
	description <- ""
	info <- NULL
	if (unit_conversion_csv_conversion_factor_url=="https://drive.google.com/open?id=14mey1_WO2JVOBEly_FpiGDyg-9H8Zpz2"){
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
	#@juldebar modify FIRMS-Secretariat@fao.org 
	#@juldebar begin commented
	#@config$logger.info("Fill contact metadata elements \n")
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
