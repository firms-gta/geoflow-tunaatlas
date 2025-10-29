# rm(list=ls())
require(stringr)
library(googledrive)
library(geoflow)
library(writexl)
library(httr)

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
codelists <- FALSE #include codelists
mappings <- TRUE #include mappings 
datasets <- FALSE #include primary datasets
upload <- FALSE #upload to google drive?

#FUNCTIONS
#---------------------------------------------------------------------------------------------

#sardara_to_geoflow_metadata
sardara_to_geoflow_metadata <- function(sardara_metadata_csv){
  
  
  sep <- geoflow::get_line_separator()
  
  sardara_metadata_csv$title <- gsub("year_","temporal_extent:",sardara_metadata_csv$title)
  sardara_metadata_csv$lineage <- gsub("\nstep",paste0(sep,"process"),sardara_metadata_csv$lineage)
  sardara_metadata_csv$lineage <- gsub("step","process",sardara_metadata_csv$lineage)
  sardara_metadata_csv$lineage <- gsub("process: ", "process:", sardara_metadata_csv$lineage) #doesn't work
  sardara_metadata_csv$lineage <- gsub("[0-9]+","",sardara_metadata_csv$lineage)
  
  sardara_metadata_csv <- sardara_metadata_csv[sardara_metadata_csv$persistent_identifier != "source_trfmos",]
  
  out <- do.call("rbind", lapply(1:nrow(sardara_metadata_csv), function(i) {
    
    Identifier <- NULL
    Title <- NULL
    Description <- NULL
    Subject <- NULL
    Creator <- NULL
    Date <- NULL
    Format <- NULL
    Type <- NULL
    Language <- NULL
    Relation  <- NULL
    Rights <- NULL
    Source <- NULL
    Provenance <- NULL
    Data <- NULL
    
    Identifier <- paste0("id:",sardara_metadata_csv$persistent_identifier[i])
    cat(paste0(Identifier,"\n"))  
    Title <- paste0(sardara_metadata_csv$title[i])
    cat("################################## DESCRIPTION  ##################################\n")
    Description <- sardara_metadata_csv$description[i]
    Description <- gsub("\\d{4} to \\d{4}","%temporal_extent:start% - %temporal_extent:end%",Description)
    if(!is.na(sardara_metadata_csv$supplemental_information[i])){
      Description <- paste0("abstract:",Description,"\n", sardara_metadata_csv$supplemental_information[i])
    }else{
      if(Description==""){Description <- "To be done"}
      Description <- paste0("abstract:",Description)
    }
    cat("################################## SUBJECT  ##################################\n")
    if(!is.na(sardara_metadata_csv$subject[i])){
      Subject <- paste0("th:",sardara_metadata_csv$subject[i])
    }else{
      Subject <- "" # leave empty for now - "th:no keywords"
    }
    cat("################################## CREATOR  ##################################\n")
    owner  <- paste0("owner:", gsub("\n", ",", sardara_metadata_csv$contact_owner[i]),",",sardara_metadata_csv$contact_originator[i])
    # originator  <- paste0("owner:", gsub("\n", ",", sardara_metadata_csv$contact_originator[i])) # check if owner
    metadata  <- paste0("metadata:", gsub("\n", ",", sardara_metadata_csv$contact_metadata[i]))
    PointOfContact  <- paste0("pointOfContact:", gsub("\n", ",", sardara_metadata_csv$contact_PointOfContact[i]))
    PrincipalInvestigator  <- paste0("principalInvestigator:", gsub("\n", ",", sardara_metadata_csv$contact_PrincipalInvestigator[i]))
    #publisher  <- paste0("publisher:", gsub("\n", ",", sardara_metadata_csv$contact_publisher[i]))
    processor  <- paste0("processor:", gsub("\n", ",", sardara_metadata_csv$contact_processor[i]))
    # Creator <- paste(owner,originator,metadata,PointOfContact,PrincipalInvestigator,publisher,processor,sep=sep)
    Creator <- paste(owner,metadata,PointOfContact,PrincipalInvestigator,sep=sep)
    
    if(!is.na(sardara_metadata_csv$contact_data_structure_definition[i])){
      data_structure_definition  <- paste0("dsd:", gsub("\n", ",", sardara_metadata_csv$contact_data_structure_definition[i]))
      Creator <- paste(Creator,data_structure_definition,sep=sep)
    }
    cat("################################## DATE  ##################################\n")
    Date <- ""
    if(!is.na(sardara_metadata_csv$date_publication[i])) if(sardara_metadata_csv$date_publication[i] != ""){
      date_publication  <- paste0("publication:", gsub("\n", ",", sardara_metadata_csv$date_publication[i]))
      date_publication <- gsub(" ", "", date_publication)
      Date <- date_publication
    }
    if(!is.na(sardara_metadata_csv$date_publication[i])) if(sardara_metadata_csv$date_publication[i] != ""){
      date_download  <- paste0("access:", gsub("\n", ",", sardara_metadata_csv$date_publication[i]))	
      date_download <- gsub(" ", "", date_download)
      if(Date == ""){
        Date <- date_download
      }else{
        Date <- paste(Date, date_download, sep = sep)
      }
    }
    # Date  <- as.Date(sardara_metadata_csv$Year[i],"%Y")
    cat("################################## FORMAT TYPE LANGUAGE COVERAGE##################################\n")
    Format <- sardara_metadata_csv$format[i]
    Type <- "dataset"
	if(sardara_metadata_csv$dataset_type[i] %in% c("mapping", "codelist")){
		Type <- paste0(Type, "_", sardara_metadata_csv$dataset_type[i])
	}
    
    if(!is.na(sardara_metadata_csv$langage[i])){
      Language <- paste0(sardara_metadata_csv$langage[i])
    }else{Language <- "eng"}
    SpatialCoverage  <- ""
    TemporalCoverage  <- ""
    
    cat("################################## RIGHTS SOURCE  PROVENANCE ##################################\n")
    Rights <- "" #paste0(sardara_metadata_csv$rights[i])
    Source <- paste0(sardara_metadata_csv$source[i])
    
	if(!is.na(sardara_metadata_csv$lineage[i])){
		count <-str_count(pattern = "process",string = sardara_metadata_csv$lineage[i])
		list_processor <- "processor:"
		for(c in 1:count-1){
		  list_processor<- gsub("processor:","",paste(list_processor,processor,sep=","))
		}
		Provenance <- paste(paste("statement:Data management workflow",sardara_metadata_csv$lineage[i],sep=sep),paste("processor:",sub(",","",list_processor)),sep=sep)
    }
    cat("################################## RELATION  & DATA ##################################\n")
    #source website --> RELATION
    path_source_download <- sardara_metadata_csv$relation_source_download[i]
    if(!is.na(path_source_download)) if(path_source_download != ""){
      if(!is.null(Relation)) Relation <- paste0(Relation, sep)
      Relation <- paste0("http:website[Source website]@", path_source_download)
    }
    # cat(paste0(sardara_metadata_csv$relation_source_dataset[i],"\n"))
    #Source dataset --> RELATION
    path_source_dataset <- sardara_metadata_csv$relation_source_dataset[i]
    if(!is.na(path_source_dataset)) if(path_source_dataset != ""){
      if(!is.null(Relation)) Relation <- paste0(Relation, sep)
      Relation <- paste0(Relation,paste0("http:data[Source dataset]@", path_source_dataset))
    }
    
    #if dataset
    if(!"path_to_dataset" %in% names(sardara_metadata_csv)){
      #metadata --> RELATION
      path_md <- sardara_metadata_csv$relation_source_metadata[i]
      if(!is.na(path_md)) if(path_md != ""){
        if(!is.null(Relation)) Relation <- paste0(Relation, sep)
        Relation <- paste0(Relation,paste0("http:metadata[Source metadata]@", path_md))
      }
      #codelist --> RELATION
      path_cl <- sardara_metadata_csv$path_to_codelists_used_in_dataset[i]
      if(!is.na(path_cl)) if(path_cl != ""){
        if(!is.null(Relation)) Relation <- paste0(Relation, sep)
        Relation <- paste0(Relation,paste0("http:codelists[Source codelists]@", path_cl))
      }
      #raw_dataset --> DATA
      path_raw_dataset <- sardara_metadata_csv$parameter_path_to_raw_dataset[i]
      if(!is.na(path_raw_dataset)) if(path_raw_dataset!= ""){
        if(!is.null(Data)) Data <- paste0(Data, sep)
        Data <- paste0(Data,paste0("source:","raw_dataset.csv","@", path_raw_dataset))
      }
      #effort_dataset (assumes there is already a path raw dataset) --> DATA
      path_effort_dataset <- sardara_metadata_csv$parameter_path_to_effort_dataset[i]
      if(!is.na(path_effort_dataset)) if(path_effort_dataset != ""){
        Data <- paste(Data,paste0("effort_dataset.csv@", path_effort_dataset),sep=",")
      }
      
      # name of table to load the database in --> DATA
      database_table_name <- sardara_metadata_csv$database_table_name[i]
      if(!is.na(database_table_name)) if(database_table_name != ""){
        Data <- paste0(Data, sep, "sourceType:other", sep) #new in geoflow, we define type of source
        Data <- paste0(Data, "uploadSource:", sardara_metadata_csv$database_table_name[i], sep) #new in geoflow, we define the uploadSource (this is our db table)
        Data <- paste0(Data, "uploadType:dbtable") #we define the uploadType
      }
      
      database_view_name <- sardara_metadata_csv$database_view_name[i]
      if(!is.na(database_view_name)) if(database_view_name != ""){
        Data <- paste0(Data,sep, "dbview:",database_view_name) #we define the uploadType
      }
      
      #script --> DATA
      path_script <- sardara_metadata_csv$path_to_script_dataset_generation[i]
	  path_script <- gsub(
		"ptaconet/rtunaatlas_scripts/master/tunaatlas_world/transform",
		"eblondel/geoflow-tunaatlas/blob/master/R/tunaatlas_scripts/pre-harmonization",
		path_script
	  )
      if(!is.na(path_script)) if(path_script != ""){
        if(!is.null(Data)) Data <- paste0(Data, sep)
        # url <- as.character(strsplit(x =path_script,split = "/")[[1]][length(strsplit(x =path_script,split = "/")[[1]])])
        list_parameters <- NULL
        Data <- paste0(Data,paste0("action:","dataset","[R harmonization script]@",path_script))
        Data <- paste0(Data, sep, "run:true")
      }
      #else if codelist
    }else if("relation_source_metadata" %in% names(sardara_metadata_csv)){
      #codelist metadata --> RELATION
      path_md <- sardara_metadata_csv$relation_source_metadata[i]
      if(!is.na(path_md)) if(path_md != ""){
        if(!is.null(Relation)) Relation <- paste0(Relation, sep)
        Relation <- paste0(Relation,"http:metadata[Source metadata]@", path_md)
      }
      #codelist dataset --> DATA
      path_to_dataset <- sardara_metadata_csv$path_to_dataset_drive[i] #switch to google drive
      if(!is.na(path_to_dataset)) if(path_to_dataset!=""){
        Data <- paste0(Data,"source:codelist.csv@", path_to_dataset, sep) #here we can keep a generic filename (codelist.csv)
        Data <- paste0(Data, "sourceType:other", sep) #new in geoflow, we define type of source
        Data <- paste0(Data, "uploadSource:", sardara_metadata_csv$database_table_name[i], sep) #new in geoflow, we define the uploadSource (this is our db table)
        Data <- paste0(Data, "uploadType:dbtable") #we define the uploadType
      }
      #else if codelist mapping
    }else{
      #mapping dataset --> DATA
      path_to_dataset <- sardara_metadata_csv$path_to_dataset_drive[i] #switch to google drive
      if(!is.na(path_to_dataset)) if(path_to_dataset != ""){
        Data <- paste0(Data,"source:mapping.csv@", path_to_dataset, sep) #here we can keep a generic filename (mapping.csv)
        Data <- paste0(Data, "sourceType:other", sep) #new in geoflow, we define type of source
        Data <- paste0(Data, "uploadSource:", sardara_metadata_csv$database_table_name[i], sep) #new in geoflow, we define the uploadSource (this is our db table)
        Data <- paste0(Data, "uploadType:dbtable") #we define the uploadType
      }
    }
    
    if(is.null(Identifier)) Identifier <- ""
    if(is.null(Title)) Title <- ""
    if(is.null(Description)) Description <- ""
    if(is.null(Subject)) Subject <- ""
    if(is.null(Creator)) Creator <- ""
    if(is.null(Date)) Date <- ""
    if(is.null(Format)) Format <- ""
    if(is.null(Type)) Type <- ""
    if(is.null(Language)) Language <- ""
    if(is.null(Relation)) Relation <- ""
    if(is.null(Rights)) Rights <- ""
    if(is.null(Source)) Source <- ""
    if(is.null(Provenance)) Provenance <- ""
    if(is.null(Data)) Data <- ""
    
    cat("################################## BIND NEW ROW  ##################################\n")
    newRow <- data.frame(Identifier=Identifier,Title=Title,Description=Description, Subject=Subject, Creator=Creator,Date=Date,Type=Type,Language=Language,SpatialCoverage=SpatialCoverage, TemporalCoverage=TemporalCoverage, Relation=Relation, Rights=Rights, Provenance=Provenance, Data=Data)
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


#google drive rep
getGoogleDriveResources <- function(folder, mimeType = NULL, fieldname = NULL){
	drive_ls_res <- NULL
	if(!is.null(mimeType)){
		drive_ls_res <- drive_ls(folder, type = drive_mime_type(mimeType))
	}else{
		drive_ls_res <- drive_ls(folder)
	}
	res <- as.data.frame(drive_ls_res)
	res$pid = sapply(res$name, function(x){unlist(strsplit(x,paste0(".",mimeType)))[1]})
	res$id <- sapply(res$id, function(x){sprintf("https://drive.google.com/open?id=%s", x)})
	
	res[,"path_to_dataset_drive"] <- NA #for codelists and mappings
	res[,"parameter_path_to_raw_dataset_drive"] <- NA #for datasets
	res[,"parameter_path_to_effort_dataset_drive"] <- NA #for datasets
	res[,"path_to_codelists_used_in_dataset_drive"] <- NA #for datasets
	if(regexpr("codelists", folder)>0){
		res[,"path_to_dataset_drive"] <- res$id
	}else if(regexpr("mappings", folder)>0){
		res[,"path_to_dataset_drive"] <- res$id
	}else{
		res[,"parameter_path_to_raw_dataset_drive"] <- res$id
	}
	return(res)
}

#BUSINESS CODEsardara_datasets <- N
#---------------------------------------------------------------------------------------------

#read metadata from metadata_and_parameterization_primary_datasets_2017.csv
sardara_datasets <- NULL
if(codelists){
	codelists_drive <- getGoogleDriveResources ("~/geoflow_tunaatlas/data/codelists", "csv", "path_to_dataset_drive")
	sardara_codelists_csv <- read.csv("https://raw.githubusercontent.com/ptaconet/rtunaatlas_scripts/master/tunaatlas_world/metadata_and_parameterization_files/metadata_codelists_2017.csv")
	sardara_codelists_csv <- merge(sardara_codelists_csv, codelists_drive, all.x = TRUE, all.y = TRUE, by.x = "persistent_identifier", by.y = "pid")
	sardara_codelists_csv <- sardara_to_geoflow_metadata(sardara_codelists_csv)
	sardara_datasets <- rbind(sardara_datasets, sardara_codelists_csv)
}
if(mappings){
	mappings_drive <- getGoogleDriveResources ("~/geoflow_tunaatlas/data/mappings", "csv", "path_to_dataset_drive")
	sardara_mappings_csv <- read.csv("https://raw.githubusercontent.com/ptaconet/rtunaatlas_scripts/master/tunaatlas_world/metadata_and_parameterization_files/metadata_mappings_2017.csv")
	sardara_mappings_csv <- merge(sardara_mappings_csv, mappings_drive, all.x = TRUE, all.y = TRUE, by.x = "persistent_identifier", by.y = "pid")
	sardara_mappings_csv <- sardara_to_geoflow_metadata(sardara_mappings_csv)
	sardara_datasets <- rbind(sardara_datasets, sardara_mappings_csv)
}
if(datasets){
	datasets_drive_iattc <- getGoogleDriveResources ("~/geoflow_tunaatlas/data/datasets/IATTC")
	datasets_drive_iotc <- getGoogleDriveResources ("~/geoflow_tunaatlas/data/datasets/IOTC")
	datasets_drive_iccat <- getGoogleDriveResources ("~/geoflow_tunaatlas/data/datasets/ICCAT")
	datasets_drive_wcpfc <- getGoogleDriveResources ("~/geoflow_tunaatlas/data/datasets/WCPFC")
	datasets_drive_ccsbt <- getGoogleDriveResources ("~/geoflow_tunaatlas/data/datasets/CCSBT")
	dataset_drive <- rbind(
		datasets_drive_iattc,
		datasets_drive_iotc,
		datasets_drive_iccat,
		datasets_drive_wcpfc,
		datasets_drive_ccsbt
	)
	dataset_drive_dimensions <- getGoogleDriveResources("~/geoflow_tunaatlas/data/dimensions")
	sardara_datasets_csv <- read.csv("https://raw.githubusercontent.com/ptaconet/rtunaatlas_scripts/master/tunaatlas_world/metadata_and_parameterization_files/metadata_and_parameterization_primary_datasets_2017.csv")
	#for the time being we restrain our effort to catch datasets (we exclude 'effort' and 'catch_at_size')
	sardara_datasets_csv <- sardara_datasets_csv[regexpr("effort", sardara_datasets_csv$persistent_identifier) <0,]
	sardara_datasets_csv <- sardara_datasets_csv[regexpr("catch_at_size", sardara_datasets_csv$persistent_identifier) <0,]
	
	sardara_datasets_csv <- sardara_to_geoflow_metadata(sardara_datasets_csv)
	sardara_datasets <- rbind(sardara_datasets, sardara_datasets_csv)
}

#conversion to geoflow
geoflow_metadata <- sardara_datasets

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
