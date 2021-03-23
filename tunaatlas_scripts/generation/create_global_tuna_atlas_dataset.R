######################################################################
##### 52North WPS annotations ##########
######################################################################
# wps.des: id = create_own_tuna_atlas_catch_effort, title = Create your own georeferenced Tuna Atlas dataset of catch or efforts, abstract = This algorithm allows to create own regional or global tuna atlas of geo-referenced gridded catch or efforts. It takes as input the public domain datasets of the five Tuna Regional Fisheries Management Organizations (tRFMOs) (IOTC ICCAT WCPFC IATTC CCSBT) stored within the Tuna atlas database. It proposes a set of parameters to customize the computation of the tuna atlas. ;
# wps.in: id = fact, type = string, title = Variable output of the tuna atlas (catch or effort), value = "catch|effort";
# wps.in: id = include_IOTC, type = string, title = Include IOTC data (Indian Ocean) in the atlas (TRUE or FALSE), value = "TRUE|FALSE";
# wps.in: id = include_ICCAT, type = string, title = Include ICCAT data (Atlantic Ocean) in the tuna atlas?, value = "TRUE|FALSE";
# wps.in: id = include_IATTC, type = string, title = Include IATTC data (Eastern Pacific Ocean) in the tuna atlas?, value = "TRUE|FALSE";
# wps.in: id = include_WCPFC, type = string, title = Include WCPFC data (Western Pacific Ocean) in the tuna atlas?, value = "TRUE|FALSE";
# wps.in: id = include_CCSBT, type = string, title = Include CCSBT data (Southern hemisphere Oceans - only Southern Bluefin Tuna) in the atlas?, value = "TRUE|FALSE";
# wps.in: id = datasets_year_release, type = string, title = Year of release of the datasets by the tRFMOs. First available year is 2017. Usually datasets released in the year Y contain the time series from the beginning of the fisheries (e.g. 1950) to year Y-2 (included). For instance 2017 will extract the datasets released in 2017 and that cover the time series from 1950 to 2015 (included), value = "2017";
# wps.in: id = iccat_ps_include_type_of_school, type = string, title = Concerns ICCAT Purse Seine data. Use only if parameter include_ICCAT is set to TRUE. ICCAT disseminates two catch-and-efforts datasets: one that provides the detail of the type of school (Fad|Free school) for purse seine fisheries only and that starts in 1994 (called Task II catch|effort by operation mode Fad|Free school) and one that does not provide the information of the type of school and that covers all the time period (from 1950) (called Task II catch|effort). These data are redundant (i.e. the data from the dataset Task II catch|effort by operation mode are also available in the dataset Task II catch|effort but in the latter the information on the type of school is not available). Combine both datasets to produce a dataset with fishing mode information (Fad | Free school)? TRUE : both datasets will be combined to produce a dataset with fishing mode information (Fad | free school). FALSE : Only the dataset without the type of school will be used. In that case the output dataset will not have the information on the fishing mode for ICCAT., value = "TRUE|FALSE";
# wps.in: id = iattc_ps_raise_flags_to_schooltype, type = string, title = Concerns IATTC Purse Seine data. Use only if parameter include_IATTC is set to TRUE. For confidentiality policies information on fishing country (flag) and type of school for the geo-referenced catches is available in separate files for the Eastern Pacific Ocean purse seine datasets. IATTC hence provides two public domain dataset: one with the information on the type of school and one with the information on the flag. Both datasets can be combined - using raising methods - to estimate the catches by both flag and type of school for purse seine fisheries. Combine both datasets? TRUE : both datasets (by fishing mode and by flag) will be combined - using raising methods i.e. dataset with the information on the flag will be raised to the dataset with the information on the type of school - to have the detail on both fishing mode and flag for each stratum. FALSE : Only one dataset will be used (either Flag or Type of school). The parameter dimension_to_use_if_no_raising_flags_to_schooltype allows to decide which dataset to use., value = "TRUE|FALSE";
# wps.in: id = iattc_ps_dimension_to_use_if_no_raising_flags_to_schooltype, type = string, title = Concerns IATTC Purse Seine data. Use only if parameter include_IATTC is set to TRUE and parameter iattc_raise_flags_to_schooltype is set to FALSE. In the case IATTC purse seine datasets are not combined (see description of parameter iattc_raise_flags_to_schooltype) which dataset to use? flag : use dataset with the information on flag. Information on type of school will therefore not be available. schooltype : use dataset with the information on type of school. Information on flag will therefore not be available., value = "flag|schooltype";
# wps.in: id = iattc_ps_catch_billfish_shark_raise_to_effort, type = boolean, title = Concerns IATTC Purse Seine data. Use only if parameter include_IATTC is set to TRUE and parameter variable is set to 'catch'. In addition to the separation flag / schooltype (see above) IATTC Purse seine catch-and-effort are available in 3 separate files according to the group of species: tuna, billfishes, sharks. This is due to the fact that PS data is collected from 2 sources: observer and fishing vessel logbooks. Observer records are used when available, and for unobserved trips logbooks are used. Both sources collect tuna data but only observers collect shark and billfish data. So as an example a strata may have observer effort and the number of sets from the observed trips would be counted for tuna and shark and billfish. But there may have also been logbook data for unobserved sets in the same strata so the tuna catch and number of sets for a cell would be added. This would make a higher total number of sets for tuna catch than shark or billfish. So efforts in the billfish and shark datasets might represent only a proportion of the total effort allocated in some strata since it is the observed effort i.e. for which there was an observer onboard. As a result catch in the billfish and shark datasets might represent only a proportion of the total catch allocated in a some strata. TRUE: Raise billfish (resp. shark) catch to the ratio  effort tuna / effort billfish (resp. shark). FALSE: Keep billfish (resp. shark) catch as they are provided in the billfish (resp. shark) catch datasets., value = "TRUE|FALSE";
# wps.in: id = mapping_map_code_lists, type = string, title = Map code lists (gears|species|flags|schooltypes|catchtype)? When using multiple sources of data (i.e. multiple RFMOS) code lists used by the various tRFMOs might be different. They should therefore be mapped to single code lists in order to be able to compare the data. TRUE : map code lists. The url to the datasets providing the code list mappings to use must be set in the parameter mapping_source_mappings. See parameter mapping_source_mappings for more details. FALSE : do not map code lists. Output data will use input codes., value = "TRUE|FALSE" ;
# wps.in: id = mapping_csv_mapping_datasets_url, type = string, title = Use only if parameter mapping_map_code_lists is set to TRUE. Path to the CSV file containing the dimensions that must be mapped and the name of the mapping dataset for each dimension mapped. The mapping datasets must be available in Tuna atlas database.  A template can be found here: https://goo.gl/YZmeDV . , value="http://data.d4science.org/ZWFMa3JJUHBXWk9NTXVPdFZhbU5BUFEyQnhUeWd1d3lHbWJQNStIS0N6Yz0" ;
# wps.in: id = mapping_keep_src_code, type = string, title = Use only if parameter mapping_map_code_lists is set to TRUE. In case of code list mapping (mapping_map_code_lists is TRUE) keep source coding system column? TRUE : conserve in the output dataset both source and target coding systems columns. FALSE : conserve only target coding system. , value="FALSE|TRUE" ;
# wps.in: id = gear_filter, type = string, title = Filter data by gear. Gear codes in this parameter must by the same as the ones used in the catch dataset (i.e. raw tRFMOs gear codes if no mapping or in case of mapping (mapping_map_code_lists is TRUE) codes used in the mapping code list). NULL : do not filter. If you want to filter you must write the codes to filter by separated by a comma in case of multiple codes.  , value = "NULL";
# wps.in: id = unit_conversion_convert, type = string, title = Convert units of measure? if TRUE you must fill in the parameter unit_conversion_df_conversion_factor. , value = "FALSE|TRUE";
# wps.in: id = unit_conversion_csv_conversion_factor_url, type = string, title = Use only if parameter unit_conversion_convert is set to TRUE. If units are converted path to the csv containing the conversion factors dataset. The conversion factor dataset must be properly structured. A template can be found here: https://goo.gl/i7QJYC . The coding systems used in the dimensions of the conversion factors must be the same as the ones used in the catch dataset (i.e. raw tRFMOs codes or in case of mapping (mapping_map_code_lists isTRUE) codes used in the mapping code lists) except for spatial code list. Additional information on the structure are provided here: https://ptaconet.github.io/rtunaatlas//reference/convert_units.html , value = "http://data.d4science.org/Z3V2RmhPK3ZKVStNTXVPdFZhbU5BTTVaWnE3VFAzaElHbWJQNStIS0N6Yz0";
# wps.in: id = unit_conversion_codelist_geoidentifiers_conversion_factors, type = string, title = Use only if parameter unit_conversion_convert is set to TRUE. If units are converted name of the coding system of the spatial dimension used in the conversion factor dataset (i.e. identifier of the layer in the Tuna atlas database)., value = "areas_conversion_factors_numtoweigth_ird";
# wps.in: id = raising_georef_to_nominal, type = string, title =  Geo-referenced catch data and associated effort can represent only part of the total catches. Raise georeferenced catches to nominal catches? Depending on the availability of the flag dimension (currently not available for the geo-referenced catch-and-effort dataset from the WCPFC and CCSBT) the dimensions used for the raising are either {Flag|Species|Year|Gear} or {Species|Year|Gear}. Some catches cannot be raised because the combination {Flag|Species|Year|Gear} (resp. {Species|Year|Gear}) does exist in the geo-referenced catches but the same combination does not exist in the total catches. In this case non-raised catch data are kept. TRUE : Raise georeferenced catches to total catches. FALSE : Do not raise., value = "TRUE|FALSE";
# wps.in: id = raising_do_not_raise_wcfpc_data, type = string, title =  WCPFC georeferenced data are already raised [terminer la description]. TRUE : Do not Raise WCPFC georeferenced catches to total catches. FALSE : Raise., value = "TRUE|FALSE";
# wps.in: id = raising_raise_only_for_PS_LL, type = string, title =  Raise the data only for Purse Seiners and Longliners (i.e. do not raise for artisanal fisheries) [terminer la description]. TRUE : Raise only for PS and LL. FALSE : Raise for all gears., value = "TRUE|FALSE";
# wps.in: id = aggregate_on_5deg_data_with_resolution_inferior_to_5deg, type = string, title =  Aggregate data that are defined on quadrants or areas inferior to 5° quadrant resolution to corresponding 5° quadrant? TRUE : Aggregate. Data that are provided at spatial resolutions superior to 5° x 5°  will be aggregated to the corresponding 5° quadrant. FALSE : Do not aggregate. Data that are provided at spatial resolutions superior to 5° x 5° will be will be kept as so. , value = "TRUE|FALSE";
# wps.in: id = disaggregate_on_5deg_data_with_resolution_superior_to_5deg, type = string, title = What to do with data that are defined on quadrants or areas superior to 5° quadrant resolution to 5° quadrant? none: Do not do anything. Data that are provided at spatial resolutions inferior to 5° x 5° will be kept as so. disaggregate : data that are provided at spatial resolutions superior to 5° x 5°  will be disaggregated to the corresponding 5°  x 5°  quadrants by dividing the catch equally on the overlappings 5° x 5°  quadrants. remove : Data that are provided at spatial resolutions superior to 5° x 5°  will be removed from the dataset. , value = "none|disaggregate|remove";
# wps.in: id = disaggregate_on_1deg_data_with_resolution_superior_to_1deg, type = string, title = Same as parameter disaggregate_on_5deg_data_with_resolution_superior_to_5deg but for 1° resolutions  , value = "none|disaggregate|remove";
# wps.in: id = spatial_curation_data_mislocated, type = string, title = Some data might be mislocated: either located on land areas or without any area information. This parameter allows to control what to do with these data. reallocate : Reallocate the mislocated data (equally distributed on areas with same dimensions (month|gear|flag|species|schooltype). no_reallocation : do not reallocate mislocated data. The output dataset will keep these data with their original location (eg on land or with no area information). remove : remove the mislocated data., value = "remove|reallocate|no_reallocation";
# wps.in: id = overlapping_zone_iattc_wcpfc_data_to_keep, type = string, title = Concerns IATTC and WCPFC data. IATTC and WCPFC have an overlapping area in their respective area of competence. Which data should be kept for this zone? IATTC : keep data from IATTC. WCPFC : keep data from WCPFC. NULL : Keep data from both tRFMOs. Caution: with the option NULL data in the overlapping zone are likely to be redundant., value = "IATTC|WCPFC|NULL";
# wps.in: id = SBF_data_rfmo_to_keep, type = string, title = Concerns Southern Bluefin Tuna (SBF) data. Use only if parameter fact is set to 'catch' and parameter include_CCSBT is set to TRUE. SBF tuna data do exist in both CCSBT data and the other tuna RFMOs data. Wich data should be kept? CCSBT : CCSBT data are kept for SBF. other_trfmos : data from the other TRFMOs are kept for SBF. NULL : Keep data from all the tRFMOs. Caution: with the option NULL data in the overlapping zones are likely to be redundant., value = "CCSBT|other_trfmos|NULL";
# wps.out: id = zip_namefile, type = text/zip, title = Outputs are 3 csv files: the dataset of georeferenced catches + a dataset of metadata (including informations on the computation, i.e. how the primary datasets were transformed by each correction) [TO DO] + a dataset providing the code lists used for each dimension (column) of the output dataset [TO DO]. All outputs and codes are compressed within a single zip file. ; 

#packages
if(!require(rtunaatlas)){
  if(!require(devtools)){
    install.packages("devtools")
  }
  require(devtools)
  install_github("ptaconet/rtunaatlas")
  require(rtunaatlas)
}

if(!require(dplyr)){
  install.packages("dplyr")
  require(dplyr)
}

if(!require(data.table)){
  install.packages("data.table")
  require(data.table)
}

#scripts
url_scripts_create_own_tuna_atlas <- "https://raw.githubusercontent.com/eblondel/geoflow-tunaatlas/master/tunaatlas_scripts/generation"
source(file.path(url_scripts_create_own_tuna_atlas, "get_rfmos_datasets_level0.R")) #modified for geoflow
source(file.path(url_scripts_create_own_tuna_atlas, "retrieve_nominal_catch.R")) #modified for geoflow
source(file.path(url_scripts_create_own_tuna_atlas, "map_codelists.R")) #modified for geoflow
source(file.path(url_scripts_create_own_tuna_atlas, "convert_units.R")) #modified for geoflow

# connect to Tuna atlas database
con <- config$software$output$dbi

#### 1) Retrieve tuna RFMOs data from Tuna atlas DB at level 0. Level 0 is the merging of the tRFMOs primary datasets, with the more complete possible value of georef_dataset per stratum (i.e. duplicated or splitted strata among the datasets are dealt specifically -> this is the case for ICCAT and IATTC)  ####
config$logger.info("Begin: Retrieving primary datasets from Tuna atlas DB... ")

fact <- options$fact

#LEVEL
DATA_LEVEL <- unlist(strsplit(entity$identifiers[["id"]], "_level"))[2]

switch(DATA_LEVEL,

	#-----------------------------------------------------------------------------------------------------------------------------------------------------------
	#LEVEL 0
	#-----------------------------------------------------------------------------------------------------------------------------------------------------------
	"0" = {

		### 1.1 Retrieve georeferenced catch or effort (+ processings for ICCAT and IATTC)
		#-------------------------------------------------------------------------------------------------------------------------------------
		#-------------------------------------------------------------------------------------------------------------------------------------
		dataset <- do.call("rbind", lapply(c("IOTC", "WCPFC", "CCSBT", "ICCAT", "IATTC"), get_rfmos_datasets_level0, entity, config, options))
		dataset$time_start<-substr(as.character(dataset$time_start), 1, 10)
		dataset$time_end<-substr(as.character(dataset$time_end), 1, 10)
		georef_dataset<-dataset
		class(georef_dataset$value) <- "numeric"
		rm(dataset)

		### 1.2 If data will be raised, retrieve nominal catch datasets (+ processings: codelist mapping for ICCAT)
		#-------------------------------------------------------------------------------------------------------------------------------------
		#-------------------------------------------------------------------------------------------------------------------------------------
		if(!is.null(options$raising_georef_to_nominal)) if (options$raising_georef_to_nominal){  
			config$logger.info("Retrieving RFMOs nominal catch...")
			nominal_catch <-retrive_nominal_catch(entity, config, options)
			config$logger.info("Retrieving RFMOs nominal catch OK")
		}

		config$logger.info("Retrieving primary datasets from the Tuna atlas DB OK")


		#### 2) Map code lists 
		#-----------------------------------------------------------------------------------------------------------------------------------------------------------
		#-----------------------------------------------------------------------------------------------------------------------------------------------------------
		if (!is.null(options$mapping_map_code_lists)) if(options$mapping_map_code_lists){
		  
		  config$logger.info("Reading the CSV containing the dimensions to map + the names of the code list mapping datasets. Code list mapping datasets must be available in the database.")
		  mapping_csv_mapping_datasets_url <- entity$getJobDataResource(config, entity$data$source[[1]])
		  mapping_dataset <- read.csv(mapping_csv_mapping_datasets_url, stringsAsFactors = F,colClasses = "character")
		  mapping_keep_src_code <- FALSE
		  if(!is.null(options$mapping_keep_src_code)) mapping_keep_src_code = options$mapping_keep_src_code
		  
		  config$logger.info("Mapping code lists of georeferenced datasets...")
		  georef_dataset <- map_codelists(con, "catch", mapping_dataset, georef_dataset, mapping_keep_src_code)
		  config$logger.info("Mapping code lists of georeferenced datasets OK")
		   
		  if(!is.null(options$raising_georef_to_nominal)) if(options$raising_georef_to_nominal){
			config$logger.info("Mapping code lists of nominal catch datasets...")
			nominal_catch <- map_codelists(con, "catch", mapping_dataset, nominal_catch, mapping_keep_src_code)
			config$logger.info("Mapping code lists of nominal catch datasets OK")
		  }
		}

		#### 3) Filters
		#-----------------------------------------------------------------------------------------------------------------------------------------------------------
		#-----------------------------------------------------------------------------------------------------------------------------------------------------------
		#### 3.1 Filter data by groups of gears
		if (!is.null(options$gear_filter)){
			gear_filter<-unlist(strsplit(options$gear_filter, split=","))
			config$logger.info(sprintf("Filtering by gear(s) [%s]", paste(gear_filter, collapse=",")))	
			georef_dataset<-georef_dataset %>% filter(gear %in% gear_filter)
			config$logger.info("Filtering gears OK")
		}

		#### 3.2) Southern Bluefin Tuna (SBF): SBF data: keep data from CCSBT or data from the other tuna RFMOs?
		if (fact=="catch" && options$include_CCSBT && !is.null(options$SBF_data_rfmo_to_keep)){
			config$logger.info(paste0("Keeping only data from ",options$SBF_data_rfmo_to_keep," for the Southern Bluefin Tuna..."))
			if (options$SBF_data_rfmo_to_keep=="CCSBT"){
			  georef_dataset <- georef_dataset[ which(!(georef_dataset$species %in% "SBF" & georef_dataset$source_authority %in% c("ICCAT","IOTC","IATTC","WCPFC"))), ]
			} else {
			  georef_dataset <- georef_dataset[ which(!(georef_dataset$species %in% "SBF" & georef_dataset$source_authority == "CCSBT")), ]
			}
			config$logger.info(paste0("Keeping only data from ",options$SBF_data_rfmo_to_keep," for the Southern Bluefin Tuna OK"))
		}
		
		#### 3.3 Grid spatial resolution filter
		if (!is.null(options$resolution_filter)){
			georef_dataset <- georef_dataset[startsWith(georef_dataset$geographic_identifier, options$resolution_filter),]
		}
		
		#### 4) Aggregates
		#-----------------------------------------------------------------------------------------------------------------------------------------------------------
		#-----------------------------------------------------------------------------------------------------------------------------------------------------------
		## Aggregate data on 5° resolution quadrants
		if(!is.null(options$aggregate_on_5deg_data_with_resolution_inferior_to_5deg)) if (options$aggregate_on_5deg_data_with_resolution_inferior_to_5deg) {
		 
			config$logger.info("Aggregating data that are defined on quadrants or areas inferior to 5° quadrant resolution to corresponding 5° quadrant...")
			georef_dataset<-rtunaatlas::spatial_curation_upgrade_resolution(con, georef_dataset, 5)
			georef_dataset<-georef_dataset$df
		
			# fill metadata elements
			# lineage<-"Data that were provided at spatial resolutions inferior to 5° x 5°  were aggregated to the corresponding 5° x 5°  quadrant."
			# aggregate_step = geoflow_process$new()
			# aggregate_step$setRationale(lineage)
			# aggregate_step$setProcessor(firms_contact)  #TODO define who's the processor
			# entity$provenance$processes <- c(entity$provenance$processes, aggregate_step)	
			# entity$descriptions[["abstract"]] <- paste0(entity$descriptions[["abstract"]], "\n", "- Data that were provided at resolutions inferior to 5° x 5°  were aggregated to the corresponding 5° x 5°  quadrant.")
			# 
			config$logger.info("Aggregating data that are defined on quadrants or areas inferior to 5° quadrant resolution to corresponding 5° quadrant OK")
		
		}

		#### 5) Overlapping zone (IATTC/WCPFC): keep data from IATTC or WCPFC?
		#-----------------------------------------------------------------------------------------------------------------------------------------------------------
		#-----------------------------------------------------------------------------------------------------------------------------------------------------------
		if (options$include_IATTC && options$include_WCPFC && !is.null(options$overlapping_zone_iattc_wcpfc_data_to_keep)) {
		 
			overlapping_zone_iattc_wcpfc_data_to_keep <- options$overlapping_zone_iattc_wcpfc_data_to_keep
			config$logger.info(paste0("Keeping only data from ",overlapping_zone_iattc_wcpfc_data_to_keep," in the IATTC/WCPFC overlapping zone..."))
			# query the database to get the codes of IATTC and WCPFC overlapping areas (stored under the view area.iattc_wcpfc_overlapping_cwp_areas)
			query_areas_overlapping_zone_iattc_wcpfc <- "SELECT codesource_area from
			(WITH iattc_area_of_competence AS (
					 SELECT rfmos_convention_areas_fao.geom
					   FROM area.rfmos_convention_areas_fao
					  WHERE code::text = 'IATTC'::text
					), wcpfc_area_of_competence AS (
					 SELECT rfmos_convention_areas_fao.geom
					   FROM area.rfmos_convention_areas_fao
					  WHERE code::text = 'WCPFC'::text
					), geom_iattc_wcpfc_intersection AS (
					 SELECT st_collectionextract(st_intersection(iattc_area_of_competence.geom, wcpfc_area_of_competence.geom), 3) AS geom
					   FROM iattc_area_of_competence,
						wcpfc_area_of_competence
					)
			 SELECT area_labels.id_area,
				area_labels.codesource_area
			   FROM area.area_labels,
				geom_iattc_wcpfc_intersection
			  WHERE area_labels.tablesource_area = 'cwp_grid'::text AND st_within(area_labels.geom, geom_iattc_wcpfc_intersection.geom))tab;
			"

			overlapping_zone_iattc_wcpfc <- dbGetQuery(con, query_areas_overlapping_zone_iattc_wcpfc)

			if (overlapping_zone_iattc_wcpfc_data_to_keep=="IATTC"){
			  # If we choose to keep the data of the overlapping zone from the IATTC, we remove the data of the overlapping zone from the WCPFC dataset.
			  georef_dataset<-georef_dataset[ which(!(georef_dataset$geographic_identifier %in% overlapping_zone_iattc_wcpfc$codesource_area & georef_dataset$source_authority == "WCPFC")), ]
			} else if (overlapping_zone_iattc_wcpfc_data_to_keep=="WCPFC"){
			  # If we choose to keep the data of the overlapping zone from the WCPFC, we remove the data of the overlapping zone from the IATTC dataset
			  georef_dataset<-georef_dataset[ which(!(georef_dataset$geographic_identifier %in% overlapping_zone_iattc_wcpfc$codesource_area & georef_dataset$source_authority == "IATTC")), ]
			}

			# fill metadata elements
			# overlap_lineage<-paste0("Concerns IATTC and WCPFC data. IATTC and WCPFC have an overlapping area in their respective area of competence. Data from both RFMOs may be redundant in this overlapping zone. In the overlapping area, only data from ",overlapping_zone_iattc_wcpfc_data_to_keep," were kept.	Information regarding the data in the IATTC / WCPFC overlapping area: after the eventual other corrections applied, e.g. raisings, catch units conversions, etc., the ratio between the catches from IATTC and those from WCPFC was of: ratio_iattc_wcpf_mt for the catches expressed in weight and ratio_iattc_wcpf_no for the catches expressed in number.")
			# overlap_step <- geoflow_process$new()
			# overlap_step$setRationale(overlap_lineage)
			# overlap_step$setProcessor(firms_contact)  #TODO define who's the processor
			# entity$provenance$processes <- c(entity$provenance$processes, overlap_step)	
			# entity$descriptions[["abstract"]] <- paste0(entity$descriptions[["abstract"]], "\n", "- In the IATTC/WCPFC overlapping area of competence, only data from ",overlapping_zone_iattc_wcpfc_data_to_keep," were kept\n")

			config$logger.info(paste0("Keeping only data from ",overlapping_zone_iattc_wcpfc_data_to_keep," in the IATTC/WCPFC overlapping zone OK"))
		  
		}
		
		
		### Units harmonization
		#-----------------------------------------------------------------------------------------------------------------------------------------------------------
		if(any(georef_dataset$unit == "MT")) georef_dataset[georef_dataset$unit == "MT", ]$unit <- "t"
		if(any(georef_dataset$unit == "NO")) georef_dataset[georef_dataset$unit == "NO", ]$unit <- "no"
		if(any(georef_dataset$unit == "MTNO")) georef_dataset[georef_dataset$unit == "MTNO", ]$unit <- "t"
		if(any(georef_dataset$unit == "NOMT")) georef_dataset[georef_dataset$unit == "NOMT", ]$unit <- "no"
	},
	
	#-----------------------------------------------------------------------------------------------------------------------------------------------------------
	#LEVEL 1
	#-----------------------------------------------------------------------------------------------------------------------------------------------------------
	"1" = {
		
		#### 1) Read Level 0 dataset
		#-----------------------------------------------------------------------------------------------------------------------------------------------------------
		#-----------------------------------------------------------------------------------------------------------------------------------------------------------
		dataset <- readr::read_csv(entity$getJobDataResource(config, entity$data$source[[1]]), guess_max = 0)
		dataset$time_start<-substr(as.character(dataset$time_start), 1, 10)
		dataset$time_end<-substr(as.character(dataset$time_end), 1, 10)
		georef_dataset<-dataset
		class(georef_dataset$value) <- "numeric"
		rm(dataset)
		
		#### 2) Convert units
		#-----------------------------------------------------------------------------------------------------------------------------------------------------------
		#-----------------------------------------------------------------------------------------------------------------------------------------------------------
		if(!is.null(options$unit_conversion_convert)) if (options$unit_conversion_convert){
			mapping_map_code_lists <- TRUE
			if(!is.null(options$mapping_map_code_lists)) mapping_map_code_lists = options$mapping_map_code_lists
			if(is.null(options$unit_conversion_csv_conversion_factor_url)) stop("Conversion of unit requires parameter 'unit_conversion_csv_conversion_factor_url'")
			if(is.null(options$unit_conversion_codelist_geoidentifiers_conversion_factors)) stop("Conversion of unit requires parameter 'unit_conversion_codelist_geoidentifiers_conversion_factors'")
			georef_dataset <- do_unit_conversion(entity, config, fact, options$unit_conversion_csv_conversion_factor_url, options$unit_conversion_codelist_geoidentifiers_conversion_factors, mapping_map_code_lists, georef_dataset)
		}
		
	},
	
	#-----------------------------------------------------------------------------------------------------------------------------------------------------------
	#LEVEL 2 - TODO --> ADAPT R CODE (NOT YET INTEGRATED IN GEOFLOW-TUNAATLAS)
	#-----------------------------------------------------------------------------------------------------------------------------------------------------------
	"2" = {
	
		#### 1) Read Level 1 dataset
		#-----------------------------------------------------------------------------------------------------------------------------------------------------------
		#-----------------------------------------------------------------------------------------------------------------------------------------------------------
		dataset <- readr::read_csv(entity$getJobDataResource(config, entity$data$source[[1]]), guess_max = 0)
		dataset$time_start<-substr(as.character(dataset$time_start), 1, 10)
		dataset$time_end<-substr(as.character(dataset$time_end), 1, 10)
		georef_dataset<-dataset
		class(georef_dataset$value) <- "numeric"
		rm(dataset)

		
		### 1.2 If data will be raised, retrieve nominal catch datasets (+ processings: codelist mapping for ICCAT)
		#-------------------------------------------------------------------------------------------------------------------------------------
		#-------------------------------------------------------------------------------------------------------------------------------------
		if(!is.null(options$raising_georef_to_nominal)) if (options$raising_georef_to_nominal){  
			config$logger.info("Retrieving RFMOs nominal catch...")
			nominal_catch <- readr::read_csv(entity$getJobDataResource(config, entity$data$source[[2]]), guess_max = 0)
			head(nominal_catch)
			class(nominal_catch$value) <- "numeric"

			#nominal_catch <-retrive_nominal_catch(entity, config, options)
			config$logger.info("Retrieving RFMOs nominal catch OK")
		}

		config$logger.info("Retrieving Level1 and nominal catch datasets from the Tuna atlas Google drive OK")

		# TODO --> ADAPT R CODE (NOT YET INTEGRATED IN GEOFLOW-TUNAATLAS)
		#### 5) Raise georeferenced to total (nominal) dataset
		#-----------------------------------------------------------------------------------------------------------------------------------------------------------
		#-----------------------------------------------------------------------------------------------------------------------------------------------------------
		if (options$raising_georef_to_nominal) {   
		  source(file.path(url_scripts_create_own_tuna_atlas, "raising_georef_to_nominal.R")) #modified for geoflow
		  
		  
		  if (fact=="catch"){
			  config$logger.info("Fact=catch !")
			  
			dataset_to_compute_rf=georef_dataset
			x_raising_dimensions=c("flag","gear","species","year","source_authority")
		  } else if (fact=="effort"){    ## If we raise the efforts, the RF is calculated using the georeferenced catch data. Hence, we need to retrieve the georeferenced catch data.
			cat("Catch datasets must be retrieved and processed in order to raise efforts. \nRetrieving georeferenced catch datasets from the Tuna atlas database...\n")
			dataset_catch<-NULL
			if (include_IOTC=="TRUE"){
			  rfmo_dataset<-rtunaatlas::get_rfmos_datasets_level0("IOTC","catch",datasets_year_release)
			  dataset_catch<-rbind(dataset_catch,rfmo_dataset)
			  rm(rfmo_dataset)
			}
			if (include_WCPFC=="TRUE"){
			  rfmo_dataset<-rtunaatlas::get_rfmos_datasets_level0("WCPFC","catch",datasets_year_release)
			  dataset_catch<-rbind(dataset_catch,rfmo_dataset)
			  rm(rfmo_dataset)
			}
			if (include_CCSBT=="TRUE"){
			  rfmo_dataset<-rtunaatlas::get_rfmos_datasets_level0("CCSBT","catch",datasets_year_release)
			  dataset_catch<-rbind(dataset_catch,rfmo_dataset)
			  rm(rfmo_dataset)
			}
			if (include_IATTC=="TRUE"){
			  rfmo_dataset<-rtunaatlas::get_rfmos_datasets_level0("IATTC",
																  "catch",
																  datasets_year_release,
																  iattc_ps_raise_flags_to_schooltype=iattc_ps_raise_flags_to_schooltype,
																  iattc_ps_dimension_to_use_if_no_raising_flags_to_schooltype=iattc_ps_dimension_to_use_if_no_raising_flags_to_schooltype,
																  iattc_ps_catch_billfish_shark_raise_to_effort=TRUE)
			  dataset_catch<-rbind(dataset_catch,rfmo_dataset)
			  rm(rfmo_dataset)
			}
			if (include_ICCAT=="TRUE"){
			  rfmo_dataset<-rtunaatlas::get_rfmos_datasets_level0("ICCAT",
																  "catch",
																  datasets_year_release,
																  iccat_ps_include_type_of_school=iccat_ps_include_type_of_school)
			  dataset_catch<-rbind(dataset_catch,rfmo_dataset)
			  rm(rfmo_dataset)
			}
			
			
			if (mapping_map_code_lists=="TRUE"){
			  dataset_catch<-function_map_code_lists("catch",mapping_csv_mapping_datasets_url,dataset_catch,mapping_keep_src_code)$dataset
			}
			
			if (!is.null(gear_filter)){
			  dataset_catch<-function_gear_filter(gear_filter,dataset_catch)$dataset
			}
			dataset_catch$time_start<-substr(as.character(dataset_catch$time_start), 1, 10)
			dataset_catch$time_end<-substr(as.character(dataset_catch$time_end), 1, 10)
			if (unit_conversion_convert=="TRUE"){ 
			  # We use our conversion factors (IRD). This should be an input parameter of the script
			  dataset_catch<-function_unit_conversion_convert(con,fact="catch",unit_conversion_csv_conversion_factor_url="http://data.d4science.org/Z3V2RmhPK3ZKVStNTXVPdFZhbU5BTTVaWnE3VFAzaElHbWJQNStIS0N6Yz0",unit_conversion_codelist_geoidentifiers_conversion_factors="areas_conversion_factors_numtoweigth_ird",mapping_map_code_lists,dataset_catch)$dataset
			}
			
			dataset_to_compute_rf=dataset_catch
			rm(dataset_catch)
			x_raising_dimensions=c("flag","gear","year","source_authority")
		  }
		
			
			config$logger.info("Executing function function_raising_georef_to_nominal")

			
			georef_dataset<-function_raising_georef_to_nominal(entity,
									   config,
									   georef_dataset,
									   dataset_to_compute_rf,
									   nominal_catch,
									   x_raising_dimensions)
			config$logger.info("function function_raising_georef_to_nominal has been executed !")
			
			rm(dataset_to_compute_rf)
			metadata$description<-paste0(metadata$description,georef_dataset$description)
			metadata$lineage<-c(metadata$lineage,georef_dataset$lineage)
			metadata$supplemental_information<-paste0(metadata$supplemental_information,georef_dataset$supplemental_information)
			georef_dataset<-georef_dataset$dataset
		  
		} 

		#### 6) Spatial Aggregation / Disaggregation of data
		#-----------------------------------------------------------------------------------------------------------------------------------------------------------
		#-----------------------------------------------------------------------------------------------------------------------------------------------------------
		## 6.1 Aggregate data on 5° resolution quadrants
		#if (options$aggregate_on_5deg_data_with_resolution_inferior_to_5deg) { 
		# 
		#	config$logger.info("Aggregating data that are defined on quadrants or areas inferior to 5° quadrant resolution to corresponding 5° quadrant...")
		#	georef_dataset<-rtunaatlas::spatial_curation_upgrade_resolution(con, georef_dataset, 5)
		#	georef_dataset<-georef_dataset$df
		#
		#	# fill metadata elements
		#	lineage<-"Data that were provided at spatial resolutions inferior to 5° x 5°  were aggregated to the corresponding 5° x 5°  quadrant."
		#	aggregate_step = geoflow_process$new()
		#	aggregate_step$setRationale(lineage)
		#	aggregate_step$setProcessor(firms_contact)  #TODO define who's the processor
		#	entity$provenance$processes <- c(entity$provenance$processes, aggregate_step)	
		#	entity$descriptions[["abstract"]] <- paste0(entity$descriptions[["abstract"]], "\n", "- Data that were provided at resolutions inferior to 5° x 5°  were aggregated to the corresponding 5° x 5°  quadrant.")
		#
		#	config$logger.info("Aggregating data that are defined on quadrants or areas inferior to 5° quadrant resolution to corresponding 5° quadrant OK")
		#
		#} 

		# TODO --> ADAPT R CODE (NOT YET INTEGRATED IN GEOFLOW-TUNAATLAS)
		## 6.2 Disggregate data on 5° resolution quadrants
		if (options$disaggregate_on_5deg_data_with_resolution_superior_to_5deg %in% c("disaggregate","remove")) {
		  source(file.path(url_scripts_create_own_tuna_atlas, "disaggregate_on_resdeg_data_with_resolution_superior_to_resdeg.R")) #modified for geoflow
		  
		  georef_dataset<-function_disaggregate_on_resdeg_data_with_resolution_superior_to_resdeg(con,georef_dataset,5,disaggregate_on_5deg_data_with_resolution_superior_to_5deg)
		  metadata$description<-paste0(metadata$description,georef_dataset$description)
		  metadata$lineage<-c(metadata$lineage,georef_dataset$lineage)
		  georef_dataset<-georef_dataset$dataset
		}

		# TODO --> ADAPT R CODE (NOT YET INTEGRATED IN GEOFLOW-TUNAATLAS)
		## 6.3 Disggregate data on 1° resolution quadrants
		if (options$disaggregate_on_1deg_data_with_resolution_superior_to_1deg %in% c("disaggregate","remove")) { 
		  source(file.path(url_scripts_create_own_tuna_atlas, "disaggregate_on_resdeg_data_with_resolution_superior_to_resdeg.R")) #modified for geoflow
		  
		  georef_dataset<-function_disaggregate_on_resdeg_data_with_resolution_superior_to_resdeg(con,georef_dataset,1,disaggregate_on_1deg_data_with_resolution_superior_to_1deg)
		  metadata$description<-paste0(metadata$description,georef_dataset$description)
		  metadata$lineage<-c(metadata$lineage,georef_dataset$lineage)
		  georef_dataset<-georef_dataset$dataset
		} 

		# TODO --> ADAPT R CODE (NOT YET INTEGRATED IN GEOFLOW-TUNAATLAS)
		#### 7) Reallocation of data mislocated (i.e. on land areas or without any spatial information) (data with no spatial information have the dimension "geographic_identifier" set to "UNK/IND" or NA)
		#-----------------------------------------------------------------------------------------------------------------------------------------------------------
		#-----------------------------------------------------------------------------------------------------------------------------------------------------------
		if (options$spatial_curation_data_mislocated %in% c("reallocate","remove")){
		  source(file.path(url_scripts_create_own_tuna_atlas, "spatial_curation_data_mislocated.R")) #modified for geoflow
		  
		  georef_dataset<-function_spatial_curation_data_mislocated(con,georef_dataset,spatial_curation_data_mislocated)
		  metadata$description<-paste0(metadata$description,georef_dataset$description)
		  metadata$lineage<-c(metadata$lineage,georef_dataset$lineage)
		  georef_dataset<-georef_dataset$dataset
		}
	}
)

#final step
dataset<-georef_dataset %>% group_by(.dots = setdiff(colnames(georef_dataset),"value")) %>% dplyr::summarise(value=sum(value))
dataset<-data.frame(dataset)

#----------------------------------------------------------------------------------------------------------------------------
#@eblondel additional formatting for next time support
dataset$time_start <- as.Date(dataset$time_start)
dataset$time_end <- as.Date(dataset$time_end)
#we enrich the entity with temporal coverage
dataset_temporal_extent <- paste(as.character(min(dataset$time_start)), as.character(max(dataset$time_end)), sep = "/")
entity$setTemporalExtent(dataset_temporal_extent)
#if there is any entity relation with name 'codelists' we read the file
df_codelists <- NULL
cl_relations <- entity$relations[sapply(entity$relations, function(x){x$name=="codelists"})]
if(length(cl_relations)>0){
	config$logger.info("Appending codelists to global dataset generation action output")
	googledrive_baseurl <- "https://drive.google.com/open?id="
    if(startsWith(cl_relations[[1]]$link, googledrive_baseurl)){
		#managing download through google drive
		config$logger.info("Downloading file using Google Drive R interface")
		drive_id <- unlist(strsplit(cl_relations[[1]]$link, "id="))[2]
		drive_id <- unlist(strsplit(drive_id, "&export"))[1] #control in case export param is appended
		googledrive::drive_download(file = googledrive::as_id(drive_id), path = file.path("data", paste0(entity$identifiers[["id"]], "_codelists.csv")))
		df_codelists <- read.csv(file.path("data", paste0(entity$identifiers[["id"]], "_codelists.csv")))
	}else{
		df_codelists <- read.csv(cl_relations[[1]]$link)
	}
}
#@geoflow -> output structure as initially used by https://raw.githubusercontent.com/ptaconet/rtunaatlas_scripts/master/workflow_etl/scripts/generate_dataset.R
dataset <- list(
	dataset = dataset, 
	additional_metadata = NULL, #nothing here
	codelists = df_codelists #in case the entity was provided with a link to codelists
)


if (fact=="effort" & DATA_LEVEL %in% c("1", "2")){
  # Levels 1 and 2 of non-global datasets should be expressed with tRFMOs code lists. However, for the effort unit code list and in those cases, we take the tuna atlas effort unit codes although this is not perfect. but going back to tRFMOs codes is too complicated 
  df_codelists$code_list_identifier[which(df_codelists$dimension=="unit")]<-"effortunit_rfmos"
}

#@geoflow -> export as csv
output_name_dataset <- file.path("data", paste0(entity$identifiers[["id"]], "_harmonized.csv"))
write.csv(dataset$dataset, output_name_dataset, row.names = FALSE)
output_name_codelists <- file.path("data", paste0(entity$identifiers[["id"]], "_codelists.csv"))
write.csv(dataset$codelists, output_name_codelists, row.names = FALSE)
#----------------------------------------------------------------------------------------------------------------------------  
entity$addResource("harmonized", output_name_dataset)
entity$addResource("codelists", output_name_codelists)
entity$addResource("geom_table", options$geom_table)

#### END
config$logger.info("End: Your tuna atlas dataset has been created!")
