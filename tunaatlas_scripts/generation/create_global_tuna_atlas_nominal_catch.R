######################################################################
##### 52North WPS annotations ##########
######################################################################
# wps.des: id = create_own_tuna_atlas_nominal_catch, title = Create your own georeferenced nominal catch Tuna Atlas dataset, abstract = This algorithm allows to create own regional or global tuna altas. It takes as input the public domain datasets of the five Tuna Regional Fisheries Management Organizations (tRFMOs) (IOTC|ICCAT|WCPFC|IATTC|CCSBT) stored within the Sardara database. It proposes a set of parameters to customize the computation of the tuna atlas. ;
# wps.in: id = include_IOTC, type = string, title = Include IOTC data (Indian Ocean) in the atlas (TRUE or FALSE), value = TRUE;
# wps.in: id = include_ICCAT, type = string, title = Include ICCAT data (Atlantic Ocean) in the tuna atlas?, value = TRUE;
# wps.in: id = include_IATTC, type = string, title = Include IATTC data (Eastern Pacific Ocean) in the tuna atlas?, value = TRUE;
# wps.in: id = include_WCPFC, type = string, title = Include WCPFC data (Western Pacific Ocean) in the tuna atlas?, value = TRUE;
# wps.in: id = include_CCSBT, type = string, title = Include CCSBT data (Southern hemisphere Oceans - only Southern Bluefin Tuna) in the atlas?, value = TRUE;
# wps.in: id = datasets_year_release, type = string, title = Year of release of the datasets by the tRFMOs. First available year is 2017. Usually, datasets released in the year Y contain the time series from the beginning of the fisheries (e.g. 1950) to year Y-2 (included). For instance 2017 will extract the datasets released in 2017 and that cover the time series from 1950 to 2015 (included), value = 2017;
# wps.in: id = iccat_nominal_catch_spatial_stratification, type = string, title = Concerns ICCAT Nominal catch data. Use only if parameter include_ICCAT is set to TRUE. ICCAT nominal catch datasets can be spatially stratified either by sampling areas or by stock areas. Which spatial stratification to select for the output dataset? sampling_area or stock_area. ,value = "sampling_area|stock_area"
# wps.in: id = mapping_map_code_lists, type = string, title = Map code lists (gears, species, flags, schooltypes, catchtype)? When using multiple sources of data (i.e. multiple RFMOS), code lists used by the various tRFMOs might be different. They should therefore be mapped to single code lists in order to be able to compare the data. TRUE : map code lists. The url to the datasets providing the code list mappings to use must be set in the parameter mapping_source_mappings. See parameter mapping_source_mappings for more details. FALSE : do not map code lists. Output data will use input codes., value = "TRUE" ;
# wps.in: id = mapping_csv_mapping_datasets_url, type = string, title = Use only if parameter mapping_map_code_lists is set to TRUE. Path to the CSV file containing the dimensions that must be mapped and the name of the mapping dataset for each dimension mapped. The mapping datasets must be available in Sardara database. An example of this CSV can be found here: https://goo.gl/2hA1sq. , value="http://data.d4science.org/ZWFMa3JJUHBXWk9NTXVPdFZhbU5BUFEyQnhUeWd1d3lHbWJQNStIS0N6Yz0" ;
# wps.in: id = mapping_keep_src_code, type = string, title = Use only if parameter mapping_map_code_lists is set to TRUE. In case of code list mapping (mapping_map_code_lists==TRUE) keep source coding system column? TRUE : conserve in the output dataset both source and target coding systems columns. FALSE : conserve only target coding system. , value= "FALSE" ;
# wps.in: id = SBF_data_rfmo_to_keep, type = string, title = Concerns Southern Bluefin Tuna (SBF) data. Use only if parameter include_CCSBT is set to TRUE. SBF tuna data do exist in both CCSBT data and the other tuna RFMOs data. Wich data should be kept? CCSBT : CCSBT data are kept for SBF. other_trfmos : data from the other TRFMOs are kept for SBF. NULL : Keep data from all the tRFMOs. Caution: with the option NULL, data in the overlapping zones are likely to be redundant., value = "CCSBT|other_trfmos|NULL";
# wps.out: id = zip_namefile, type = text/zip, title = Outputs are 3 csv files: the dataset of georeferenced catches + a dataset of metadata (including informations on the computation, i.e. how the primary datasets were transformed by each correction) [TO DO] + a dataset providing the code lists used for each dimension (column) of the output dataset [TO DO]. All outputs and codes are compressed within a single zip file. ; 

firms_contact <- config$getContacts()[sapply(config$getContacts(), function(x){x$id == "firms-secretariat@fao.org"})][[1]]
firms_contact$setRole("processor")
ird_contact <- config$getContacts()[sapply(config$getContacts(), function(x){x$id == "paul.taconet@ird.fr"})][[1]]
ird_contact$setRole("processor")


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

# connect to Tuna atlas database
con <- config$software$output$dbi

#scripts
url_scripts_create_own_tuna_atlas <- "https://raw.githubusercontent.com/eblondel/geoflow-tunaatlas/master/tunaatlas_scripts/generation"
source(file.path(url_scripts_create_own_tuna_atlas, "map_codelists.R")) #modified for geoflow
source(file.path(url_scripts_create_own_tuna_atlas, "retrieve_nominal_catch.R"))

#### 1) Retrieve tuna RFMOs data from Sardara DB at level 0. 
config$logger.info("Retrieving RFMOs nominal catch...")
nominal_catch <-retrieve_nominal_catch(entity, config, options)
config$logger.info("Retrieving RFMOs nominal catch OK")

entity$descriptions[["abstract"]] <- paste0(entity$descriptions[["abstract"]],"\n- The primary nominal (also called total) catch datasets released by the tuna RFMOs were merged.\n")
merge_step <- geoflow_process$new();
merge_step$setRationale("All the datasets were merged")
merge_step$setProcessor(firms_contact)
entity$provenance$processes <- c(entity$provenance$processes, merge_step)


#### 2) Map code lists 

if (!is.null(options$mapping_map_code_lists)){
 
	config$logger.info("Reading the CSV containing the dimensions to map + the names of the code list mapping datasets. Code list mapping datasets must be available in the database.")
	filename <- entity$data$source[[1]]
	mapping_csv_mapping_datasets_url <- entity$getJobDataResource(config, filename)
	mapping_dataset <- read.csv(mapping_csv_mapping_datasets_url, stringsAsFactors = F,colClasses = "character")
	mapping_keep_src_code <- FALSE
	if(!is.null(options$mapping_keep_src_code)) mapping_keep_src_code = options$mapping_keep_src_code
  
	config$logger.info("Mapping code lists of georeferenced datasets...")
	output <- map_codelists(con, "catch", mapping_dataset, nominal_catch, mapping_keep_src_code)
	config$logger.info("Mapping code lists of georeferenced datasets OK")
 
	#dataset mapped with codelists
	nominal_catch <- output$dataset

	#more metadata
	entity$descriptions[["abstract"]] <- paste0(entity$descriptions[["abstract"]], "\n", output$abstract)
	if(!is.null(entity$descriptions[["info"]])){
	entity$setDescription("info", output$info)
	}else{
	entity$descriptions[["info"]] <- paste0(entity$descriptions[["info"]], output$info)
	}
	lineage_step <- geoflow_process$new()
	lineage_step$setRationale(output$lineage)
	lineage_step$setProcessor(firms_contact) #TODO define who's the processor
	entity$provenance$processes <- c(entity$provenance$processes, lineage_step)
 
}


#### 9) Southern Bluefin Tuna (SBF): SBF data: keep data from CCSBT or data from the other tuna RFMOs?

if (!is.null(options$SBF_data_rfmo_to_keep)){
  
	config$logger.info(paste0("Keeping only data from ",options$SBF_data_rfmo_to_keep," for the Southern Bluefin Tuna..."))
	if (options$SBF_data_rfmo_to_keep=="CCSBT"){
		nominal_catch <- nominal_catch[ which(!(nominal_catch$species %in% "SBF" & nominal_catch$source_authority %in% c("ICCAT","IOTC","IATTC","WCPFC"))), ]
	} else {
		nominal_catch <- nominal_catch[ which(!(nominal_catch$species %in% "SBF" & nominal_catch$source_authority == "CCSBT")), ]
	}

	# fill metadata elements
	lineage<-paste0("Concerns Southern Bluefin Tuna (SBF) data: SBF tuna data do exist in both CCSBT data and the other tuna RFMOs data. Data from CCSBT and the other RFMOs may be redundant. For the Southern Bluefin Tuna, only data from ",options$SBF_data_rfmo_to_keep," were kept.	Information regarding the SBF data: after the potential other corrections applied, e.g. raisings, units conversions, etc., the ratio between the catches from CCSBT and those from the other RFMOs for SBF was of: ratio_ccsbt_otherrfmos_mt for the catches expressed in weight. A total of catches_sbf_ccsbt_no fishes were available in the CCSBT datasets - while no data in number were available in the other RFMOs datasets.")
	sbf_step <- geoflow_process$new()
	sbf_step$setRationale(lineage)
	sbf_step$setProcessor(firms_contact)  #TODO define who's the processor
	entity$provenance$processes <- c(entity$provenance$processes, sbf_step)

	entity$descriptions[["abstract"]] <- paste0(entity$descriptions[["abstract"]], "\n", "- For the Southern Bluefin Tuna, only data from ",options$SBF_data_rfmo_to_keep," were kept")

	config$logger.info(paste0("Keeping only data from ",options$SBF_data_rfmo_to_keep," for the Southern Bluefin Tuna OK")) 
}

#final step
# dataset<-nominal_catch %>% group_by_(.dots = setdiff(colnames(nominal_catch),"value")) %>% dplyr::summarise(value=sum(value))
dataset<-nominal_catch
dataset<-data.frame(dataset)


## fill some metadata elements
entity$setDescription("info", paste0("- Catches in the Pacific ocean are over-estimated. In fact, IATTC and WCPFC, who report the data for the Eastern Pacific and Western-Central Pacific ocean, respectively, have an overlapping area in their respective area of competence. Data from both RFMOs may be redundant in this overlapping zone.
- Geographical stratification in this dataset is: major FAO fishing area for the Indian ocean (IOTC), ",options$iccat_nominal_catch_spatial_stratification," for the Atlantic ocean (ICCAT), whole areas of competence of the respective RFMOs for the Pacific ocean (IATTC and WCPFC), area of competence of the CCSBT for the Southern Bluefin tuna."))
entity$descriptions[["abstract"]] <- paste0(entity$descriptions[["abstract"]], "\n More details on the processes are provided in the supplemental information and in the lineage section.")


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

#@geoflow -> export as csv
output_name_dataset <- file.path("data", paste0(entity$identifiers[["id"]], "_harmonized.csv"))
write.csv(dataset$dataset, output_name_dataset, row.names = FALSE)
output_name_codelists <- file.path("data", paste0(entity$identifiers[["id"]], "_codelists.csv"))
write.csv(dataset$codelists, output_name_codelists, row.names = FALSE)
#----------------------------------------------------------------------------------------------------------------------------  
entity$addResource("harmonized", output_name_dataset)
entity$addResource("codelists", output_name_codelists)

#### END
config$logger.info("End: Your tuna atlas dataset has been created!")
