# rm(list=ls())
require(stringr)
library(googledrive)



setwd("/home/juldebar/Bureau/CODES/geoflow-tunaatlas")

# metadata_and_parameterization_primary_datasets_2017.csv
sardara_datasets_csv <- read.csv("https://raw.githubusercontent.com/ptaconet/rtunaatlas_scripts/master/tunaatlas_world/metadata_and_parameterization_files/metadata_and_parameterization_primary_datasets_2017.csv")
# metadata_codelists_2017.csv
sardara_codelists_csv <- read.csv("https://raw.githubusercontent.com/ptaconet/rtunaatlas_scripts/master/tunaatlas_world/metadata_and_parameterization_files/metadata_codelists_2017.csv")
# metadata_mappings_2017.csv
sardara_mapping_csv <- read.csv("https://raw.githubusercontent.com/ptaconet/rtunaatlas_scripts/master/tunaatlas_world/metadata_and_parameterization_files/metadata_mappings_2017.csv")
sep="_\n"

saradara_datasets <- list(sardara_datasets_csv,sardara_codelists_csv,sardara_mapping_csv)
geoflow_metadata <- data.frame(Identifier=character(),
                               Title=character(),
                               Description=character(),
                               Subject=character(),
                               Creator=character(),
                               Date=character(), 
                               Type=character(),
                               Language=character(), 
                               SpatialCoverage=character(), 
                               TemporalCoverage=character(), 
                               Relation=character(), 
                               Rights=character(), 
                               Provenance=character(), 
                               Data=character())

for(d in saradara_datasets){
  geoflow_metadata <- sardara_to_geoflow_metadata(geoflow_metadata,d,sep)
}

file_name <-"geoflow_metadata.csv"
write.csv(geoflow_metadata,file = file_name,row.names = F)
google_drive_path <- drive_get(id="1SQpBH3nYEQH1MzG29JsNmhLXYvAfxfuc")
google_drive_path
upload_file_on_drive_repository(google_drive_path,file_name)



sardara_to_geoflow_metadata <- function(geoflow_metadata,sardara_metadata_csv, sep){
  
  
  for (i in 1:nrow(sardara_metadata_csv)) {
    
    sardara_metadata_csv$title <- gsub("year_","temporal_extent:",sardara_metadata_csv$title)
    sardara_metadata_csv$lineage <- gsub("step","process:",sardara_metadata_csv$lineage)
    
  cat("################################## IDENTIFIER  ##################################\n")
  Identifier <- paste0("id:",sardara_metadata_csv$persistent_identifier[i])
  cat("################################## TITLE  ##################################\n")
  Title <- paste0(sardara_metadata_csv$title[i])
  cat("################################## DESCRIPTION  ##################################\n")
  if(!is.na(sardara_metadata_csv$supplemental_information[i])){
    Description <- paste0("abstract:",sardara_metadata_csv$description[i],"\n", sardara_metadata_csv$supplemental_information[i])
  }else{
    Description <- paste0("abstract:",sardara_metadata_csv$description[i])
    }
  cat("################################## SUBJECT  ##################################\n")
  if(!is.na(sardara_metadata_csv$subject[i])){
    Subject <- paste0("th:",sardara_metadata_csv$subject[i])
  }else{
    Subject <- "th:no keywords"
  }
  cat("################################## CREATOR  ##################################\n")
  owner  <- paste0("owner:", gsub("\n", ",", sardara_metadata_csv$contact_owner[i]))
  originator  <- paste0("owner:", gsub("\n", ",", sardara_metadata_csv$contact_originator[i])) # check if owner
  metadata  <- paste0("metadata:", gsub("\n", ",", sardara_metadata_csv$contact_metadata[i]))
  PointOfContact  <- paste0("pointOfContact:", gsub("\n", ",", sardara_metadata_csv$contact_PointOfContact[i]))
  PrincipalInvestigator  <- paste0("principalInvestigator:", gsub("\n", ",", sardara_metadata_csv$contact_PrincipalInvestigator[i]))
  publisher  <- paste0("publisher:", gsub("\n", ",", sardara_metadata_csv$contact_publisher[i]))
  processor  <- paste0("processor:", gsub("\n", ",", sardara_metadata_csv$contact_processor[i]))
  if(!is.na(sardara_metadata_csv$contact_data_structure_definition[i])){
    data_structure_definition  <- paste0("dsd:", gsub("\n", ",", sardara_metadata_csv$contact_data_structure_definition[i]))
    Creator <- paste(owner,originator,metadata,PointOfContact,PrincipalInvestigator,publisher,processor,data_structure_definition,sep=sep)
  }else{
    Creator <- paste(owner,originator,metadata,PointOfContact,PrincipalInvestigator,publisher,processor,sep=sep)
  }
  
  
  cat("################################## DATE  ##################################\n")
  date_publication  <- paste0("publication:", gsub("\n", ",", sardara_metadata_csv$date_publication[i]))
  date_download  <- paste0("access:", gsub("\n", ",", sardara_metadata_csv$date_publication[i]))
  
  Date <- paste(date_publication,date_download,sep=sep)
    # Date  <- as.Date(sardara_metadata_csv$Year[i],"%Y")
  
  cat("################################## FORMAT  ##################################\n")
  Format <- sardara_metadata_csv$format[i]
  
  cat("################################## TYPE  ##################################\n")
  Type <- paste0(sardara_metadata_csv$dataset_type[i])
  
  cat("################################## LANGUAGE  ##################################\n")
  if(!is.na(sardara_metadata_csv$langage[i])){
    Language <- paste0(sardara_metadata_csv$langage[i])
  }else{
    Language <- "eng"
    
    }
  
  cat("################################## COVERAGE  ##################################\n")
  SpatialCoverage  <- ""
  TemporalCoverage  <- ""
  
  cat("################################## RELATION  ##################################\n")
  if(is.null(sardara_metadata_csv$path_to_dataset[i])){
    Relation <- relation_source_metadata
  }else if(!is.null(sardara_metadata_csv$path_to_dataset[i])){
    path_to_dataset  <- paste0("http:path_to_dataset[path_to_dataset]@", sardara_metadata_csv$path_to_dataset[i])
    Relation <- paste(path_to_dataset,
                      relation_source_download,
                      relation_source_dataset,
                      relation_source_metadata,
                      sep=sep)
  }else{
    relation_source_download  <- paste0("http:website[Source website]@",sardara_metadata_csv$relation_source_download[i])
    relation_source_dataset  <- paste0("http:data[Source dataset]@", sardara_metadata_csv$relation_source_dataset[i])
    relation_source_metadata  <- paste0("http:metadata[Source metadata]@", sardara_metadata_csv$relation_source_metadata[i])
    parameter_path_to_raw_dataset  <- paste0("http:raw_dataset[raw datase path ]@", sardara_metadata_csv$parameter_path_to_raw_dataset[i])
    parameter_path_to_effort_dataset  <- paste0("http:effort_dataset[effort_dataset]@", sardara_metadata_csv$parameter_path_to_effort_dataset[i])
    relation_source_metadata  <- paste0("http:effort_dataset[effort_dataset]@", sardara_metadata_csv$relation_source_metadata[i])
    
    Relation <- paste(relation_source_download,
                      relation_source_dataset,
                      relation_source_metadata,
                      path_to_codelists_used_in_dataset,
                      path_to_script_dataset_generation,
                      parameter_path_to_raw_dataset,
                      parameter_path_to_effort_dataset,
                      sep=sep)
  }
  


  cat("################################## RIGHTS  ##################################\n")
  Rights <- paste0(sardara_metadata_csv$rights[i])
  
  cat("################################## PROVENANCE  ##################################\n")
  Provenance <- paste0(sardara_metadata_csv$lineage[i])
  
  cat("################################## DATA  ##################################\n")
  # database_table_name  <- paste0("view:", gsub("\n", ",", sardara_metadata_csv$database_table_name[i]))
  # database_view_name  <- paste0("table:", gsub("\n", ",", sardara_metadata_csv$database_view_name[i]))
  
  if(!is.null(sardara_metadata_csv$path_to_script_dataset_generation[i])){

    run <- "run:false"
    source  <- paste0(gsub("t2ce_PS91-16_bySchool.csv","TO BE DONE","source:t2ce_PS91-16_bySchool.csv@"), sardara_metadata_csv$path_to_codelists_used_in_dataset[i])
    action  <- paste0(gsub("atlantic_ocean_catch_1deg_1m_ps_tunaatlasiccat_level0__byschool.R",sardara_metadata_csv$path_to_script_dataset_generation[i],"action:atlantic_ocean_catch_1deg_1m_ps_tunaatlasiccat_level0__byschool.R[R harmonization script]@"), sardara_metadata_csv$path_to_script_dataset_generation[i])
    Data <- paste(source,action,run,sep=sep)
    }else{
      source  <- path_to_dataset
      action  <- "TO BE DONE"
      Data <- paste(source,run,sep=sep)
      }
    
  
  cat("################################## BIND NEW ROW  ##################################\n")
  # ,,,source,,,  # ,,,parameter_spatial_stratification
  
  newRow <- data.frame(Identifier=Identifier,Title=Title,Description=Description, Subject=Subject, Creator=Creator,Date=Date,Type=Type,Language=Language,SpatialCoverage=SpatialCoverage, TemporalCoverage=TemporalCoverage, Relation=Relation, Rights=Rights, Provenance=Provenance, Data=Data)
  geoflow_metadata <- rbind(geoflow_metadata,newRow)
}
  return(geoflow_metadata)
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

