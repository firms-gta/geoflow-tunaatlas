# rm(list=ls())
require(stringr)
library(googledrive)
library(geoflow)
library(writexl)

#PARAMS
#---------------------------------------------------------------------------------------------

#workspace
user <- "eblondel"
wd <- switch(user,
	"eblondel" = "D:/",
	"juldebar" = "/home/juldebar/Bureau/CODES"
)
setwd(file.path(wd, "geoflow-tunaatlas"))

#params (dataset to include in conversion or not)
codelists <- TRUE #include codelists
mappings <- FALSE #include mappings 
datasets <- FALSE #include primary datasets
upload <- FALSE #upload to google drive?

#FUNCTIONS
#---------------------------------------------------------------------------------------------

#sardara_to_geoflow_metadata
sardara_to_geoflow_metadata <- function(sardara_metadata_csv){
  
	sep <- geoflow::get_line_separator()
	
	sardara_metadata_csv$title <- gsub("year_","temporal_extent:",sardara_metadata_csv$title)
    sardara_metadata_csv$lineage <- gsub("step","process",sardara_metadata_csv$lineage)
    sardara_metadata_csv$lineage <- gsub("[0-9]+","",sardara_metadata_csv$lineage)
	
	out <- do.call("rbind", lapply(1:nrow(sardara_metadata_csv), function(i) {
    
		Identifier <- NULL
		Title <- NULL
		Description <- NULL
		# Subject <- NULL
		# Creator <- NULL
		# Date <- NULL
		# Format <- NULL
		# Type <- NULL
		# Language <- NULL
		Relation  <- NULL
		Rights <- NULL
		# Source <- NULL
		Provenance <- NULL
		Data <- NULL
    
		Identifier <- paste0("id:",sardara_metadata_csv$persistent_identifier[i])
		cat(paste0(Identifier,"\n"))  
		Title <- paste0(sardara_metadata_csv$title[i])
		cat("################################## DESCRIPTION  ##################################\n")
		Description <- gsub(" \\d{4}"," year",sardara_metadata_csv$description[i])
		Description <- gsub("year to year+","%temporal_extent:start% - %temporal_extent:end%",Description)
		if(!is.na(sardara_metadata_csv$supplemental_information[i])){
			Description <- paste0("abstract:",Description,"\n", sardara_metadata_csv$supplemental_information[i])
		}else{
			Description <- paste0("abstract:",Description)
		}
		cat("################################## SUBJECT  ##################################\n")
		if(!is.na(sardara_metadata_csv$subject[i])){
			Subject <- paste0("th:",sardara_metadata_csv$subject[i])
		}else{
			Subject <- "th:no keywords"
		}
		cat("################################## CREATOR  ##################################\n")
		owner  <- paste0("owner:", gsub("\n", ",", sardara_metadata_csv$contact_owner[i]),",",sardara_metadata_csv$contact_originator[i])
		# originator  <- paste0("owner:", gsub("\n", ",", sardara_metadata_csv$contact_originator[i])) # check if owner
		metadata  <- paste0("metadata:", gsub("\n", ",", sardara_metadata_csv$contact_metadata[i]))
		PointOfContact  <- paste0("pointOfContact:", gsub("\n", ",", sardara_metadata_csv$contact_PointOfContact[i]))
		PrincipalInvestigator  <- paste0("principalInvestigator:", gsub("\n", ",", sardara_metadata_csv$contact_PrincipalInvestigator[i]))
		publisher  <- paste0("publisher:", gsub("\n", ",", sardara_metadata_csv$contact_publisher[i]))
		processor  <- paste0("processor:", gsub("\n", ",", sardara_metadata_csv$contact_processor[i]))
		# Creator <- paste(owner,originator,metadata,PointOfContact,PrincipalInvestigator,publisher,processor,sep=sep)
		Creator <- paste(owner,metadata,PointOfContact,PrincipalInvestigator,publisher,sep=sep)
  
		if(!is.na(sardara_metadata_csv$contact_data_structure_definition[i])){
			data_structure_definition  <- paste0("dsd:", gsub("\n", ",", sardara_metadata_csv$contact_data_structure_definition[i]))
			Creator <- paste(Creator,data_structure_definition,sep=sep)
		}
		cat("################################## DATE  ##################################\n")
		date_publication  <- paste0("publication:", gsub("\n", ",", sardara_metadata_csv$date_publication[i]))
		date_download  <- paste0("access:", gsub("\n", ",", sardara_metadata_csv$date_publication[i]))
  
		Date <- paste(date_publication,date_download,sep=sep)
		# Date  <- as.Date(sardara_metadata_csv$Year[i],"%Y")
		cat("################################## FORMAT TYPE LANGUAGE COVERAGE##################################\n")
		Format <- sardara_metadata_csv$format[i]
		Type <- "dataset" #paste0(sardara_metadata_csv$dataset_type[i])
  
		if(!is.na(sardara_metadata_csv$langage[i])){
			Language <- paste0(sardara_metadata_csv$langage[i])
		}else{Language <- "eng"}
		SpatialCoverage  <- ""
		TemporalCoverage  <- ""
  
		cat("################################## RIGHTS SOURCE  PROVENANCE ##################################\n")
		Rights <- paste0(sardara_metadata_csv$rights[i])
		Source <- paste0(sardara_metadata_csv$source[i])
  
		count <-str_count(pattern = "process",string = sardara_metadata_csv$lineage[i])
		list_processor <- "processor:"
		for(c in 1:count-1){
			list_processor<- gsub("processor:","",paste(list_processor,processor,sep=","))
		}
		Provenance <- paste(paste("statement:Data management workflow",sardara_metadata_csv$lineage[i],sep=sep),paste("processor:",sub(",","",list_processor)),sep=sep)
  
		cat("################################## RELATION  & DATA ##################################\n")
		if(!is.na(sardara_metadata_csv$relation_source_download[i])){Relation <- paste0("http:website[Source_download website]@",sardara_metadata_csv$relation_source_download[i])}
		# cat(paste0(sardara_metadata_csv$relation_source_dataset[i],"\n"))
		if(!is.na(sardara_metadata_csv$relation_source_dataset[i])){Relation <- paste(Relation,paste0("http:data[Source dataset]@", sardara_metadata_csv$relation_source_dataset[i]),sep=sep)}

		#if dataset
		if(!"path_to_dataset" %in% names(sardara_metadata_csv)){
			if(sardara_metadata_csv$relation_source_metadata[i]!=""){Relation <- paste(Relation,paste0("http:metadata[Source metadata]@", sardara_metadata_csv$relation_source_metadata[i]),sep=sep)}
			if(sardara_metadata_csv$path_to_codelists_used_in_dataset[i]!=""){Relation <- paste(Relation,paste0("http:codelists_used_in_dataset[codelists_used_in_dataset]@", sardara_metadata_csv$path_to_codelists_used_in_dataset[i]),sep=sep)}
			if(sardara_metadata_csv$path_to_script_dataset_generation[i]!=""){Relation <- paste(Relation,paste0("http:dataset_generation[dataset_generation]@", sardara_metadata_csv$path_to_script_dataset_generation[i]),sep=sep)}
			if(sardara_metadata_csv$parameter_path_to_raw_dataset[i]!=""){Relation <- paste(Relation,paste0("http:raw_dataset[raw datase path]@", sardara_metadata_csv$parameter_path_to_raw_dataset[i]),sep=sep)}
			if(sardara_metadata_csv$parameter_path_to_effort_dataset[i]!=""){Relation <- paste(Relation,paste0("http:effort_dataset[effort dataset path]@", sardara_metadata_csv$parameter_path_to_effort_dataset[i]),sep=sep)}
			if(sardara_metadata_csv$parameter_spatial_stratification[i]!=""){Relation <- paste0(Relation,"http:spatial_stratification[spatial_stratification]@",sardara_metadata_csv$parameter_spatial_stratification[i])}
		#else if codelist
		}else if("relation_source_metadata" %in% names(sardara_metadata_csv)){
			if(sardara_metadata_csv$path_to_dataset[i]!=""){Relation <- paste(Relation,"http:path_to_dataset[path_to_dataset]@", sardara_metadata_csv$path_to_dataset[i],sep=sep)}
			if(sardara_metadata_csv$relation_source_metadata[i]!=""){Relation <- paste(Relation,"http:metadata[Source metadata]@", sardara_metadata_csv$relation_source_metadata[i])}
			#else if codelist mapping
		}else{Relation <- paste0(Relation,"http:path_to_dataset[path_to_dataset]@", sardara_metadata_csv$path_to_dataset[i])}
  


		# database_table_name & database_view_name not used for now
		# database_table_name  <- paste0("view:", gsub("\n", ",", sardara_metadata_csv$database_table_name[i]))
		# database_view_name  <- paste0("table:", gsub("\n", ",", sardara_metadata_csv$database_view_name[i]))
		run <- "run:false"
		#if dataset
		if(!"path_to_dataset" %in% names(sardara_metadata_csv)){
			source  <- paste0(gsub("t2ce_PS91-16_bySchool.csv","TO BE DONE","source:t2ce_PS91-16_bySchool.csv@"), sardara_metadata_csv$path_to_codelists_used_in_dataset[i])
			if(sardara_metadata_csv$path_to_script_dataset_generation[i]!=""){action  <- paste0(gsub("atlantic_ocean_catch_1deg_1m_ps_tunaatlasiccat_level0__byschool.R",sardara_metadata_csv$path_to_script_dataset_generation[i],"action:atlantic_ocean_catch_1deg_1m_ps_tunaatlasiccat_level0__byschool.R[R harmonization script]@"), sardara_metadata_csv$path_to_script_dataset_generation[i])}
			Data <- paste(source,action,run,sep=sep)
		}else if("relation_source_metadata" %in% names(sardara_metadata_csv)){
			source  <- paste("source:TO_BE_DONE@", sardara_metadata_csv$path_to_dataset[i],sep=sep)
			action  <- "TO BE DONE"
			Data <- paste(source,run,sep=sep)
			if(sardara_metadata_csv$relation_source_metadata[i]!=""){Relation <- paste(Relation,"http:metadata[Source metadata]@", sardara_metadata_csv$relation_source_metadata[i])}
			#else if codelist mapping
		}else{
			source  <- paste("source:TO_BE_DONE@", sardara_metadata_csv$path_to_dataset[i],sep=sep)
			action  <- "TO BE DONE"
			Data <- paste(source,run,sep=sep)
		}
  
    
		cat("################################## BIND NEW ROW  ##################################\n")
		# columns not used so far: "parameter_spatial_stratification", database_table_name, database_view_name
  
		newRow <- data.frame(Identifier=Identifier,Title=Title,Description=Description, Subject=Subject, Creator=Creator,Date=Date,Type=Type,Language=Language,SpatialCoverage=SpatialCoverage, TemporalCoverage=TemporalCoverage, Relation=Relation, Rights=Rights, Provenance=Provenance, Data=Data, Source=Source)
		return(newRow)
	}))
	return(out)
}

upload_file_on_drive_repository <- function(google_drive_path,file_name){
  # check <- drive_find(pattern = file_name)
  # drive_get(path = google_drive_path, id = file_name, team_drive = NULL, corpus = NULL,verbose = TRUE)
  check <- drive_ls(path = google_drive_path, pattern = file_name, recursive = FALSE)
  check
  if(nrow(check)>0){
    google_drive_file <- drive_update(file=as_id(check$id[1]), name=file_name, media=file_name)
    # google_drive_file <- drive_upload(media=file_name, path = google_drive_path,name=file_name)
    
  }else{
    google_drive_file <- drive_upload(media=file_name, path = google_drive_path,name=file_name)
  }
  # If to update the content or metadata of an existing Drive file, use drive_update()
  google_drive_file_url <- paste0("https://drive.google.com/open?id=",google_drive_file$id)
  google_drive_file %>% drive_reveal("permissions")
  google_drive_file %>% drive_reveal("published")
  google_drive_file <- google_drive_file %>% drive_share(role = "reader", type = "anyone")
  google_drive_file %>% drive_reveal("published")
  file_id <- google_drive_file$id
  
  return(file_id)
}

#BUSINESS CODE
#---------------------------------------------------------------------------------------------

#read metadata from metadata_and_parameterization_primary_datasets_2017.csv
sardara_datasets <- NULL
if(codelists){
	sardara_codelists_csv <- read.csv("https://raw.githubusercontent.com/ptaconet/rtunaatlas_scripts/master/tunaatlas_world/metadata_and_parameterization_files/metadata_codelists_2017.csv")
	sardara_datasets <- rbind(sardara_datasets, sardara_codelists_csv)
}
if(mappings){
	sardara_mappings_csv <- read.csv("https://raw.githubusercontent.com/ptaconet/rtunaatlas_scripts/master/tunaatlas_world/metadata_and_parameterization_files/metadata_mappings_2017.csv")
	sardara_datasets <- rbind(sardara_datasets, sardara_mappings_csv)
}
if(datasets){
	sardara_datasets_csv <- read.csv("https://raw.githubusercontent.com/ptaconet/rtunaatlas_scripts/master/tunaatlas_world/metadata_and_parameterization_files/metadata_and_parameterization_primary_datasets_2017.csv")
	sardara_datasets <- rbind(sardara_datasets, sardara_datasets_csv)
}

#conversion to geoflow
geoflow_metadata <- sardara_to_geoflow_metadata(sardara_datasets)

file_name <-"geoflow_metadata"
write.csv(geoflow_metadata,file = paste0(file_name,".csv"),row.names = F)
write_xlsx(geoflow_metadata, paste0(file_name, ".xlsx"))

#testing google drive
if(upload){
	google_drive_path <- drive_get(id= "1SQpBH3nYEQH1MzG29JsNmhLXYvAfxfuc")
	google_drive_path
	upload_file_on_drive_repository(google_drive_path, paste0(file_name, ".csv"))
	sardara_metadata_csv <- NULL
}
