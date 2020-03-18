######################################################################
##### 52North WPS annotations ##########
######################################################################
# wps.des: id = east_pacific_ocean_effort_1deg_1m_ps_tunaatlasiattc_level0__byFlag, title = Harmonize data structure of IATTC PS ByFlag effort datasets, abstract = Harmonize the structure of IATTC catch-and-effort datasets: 'PublicPSBillfishFlag' and 'PublicPSTunaFlag' (pid of output file = east_pacific_ocean_effort_1deg_1m_ps_tunaatlasIATTC_level0__billfish_byFlag or east_pacific_ocean_effort_1deg_1m_ps_tunaatlasIATTC_level0__tuna_byFlag). The only mandatory field is the first one. The metadata must be filled-in only if the dataset will be loaded in the Tuna atlas database. ;
# wps.in: id = path_to_raw_dataset, type = String, title = Path to the input dataset to harmonize. Input file must be structured as follow: https://goo.gl/Q1w7Ur, value = "https://goo.gl/Q1w7Ur";
# wps.in: id = path_to_metadata_file, type = String, title = NULL or path to the csv of metadata. The template file can be found here: https://raw.githubusercontent.com/ptaconet/rtunaatlas_scripts/master/sardara_world/transform_trfmos_data_structure/metadata_source_datasets_to_database/metadata_source_datasets_to_database_template.csv . If NULL, no metadata will be outputted., value = "NULL";
# wps.out: id = zip_namefile, type = text/zip, title = Dataset with structure harmonized + File of metadata (for integration within the Tuna Atlas database) + File of code lists (for integration within the Tuna Atlas database) ; 



# '# This script works with any data that has the first 5 columns named and ordered as follow: {Year|Month|Flag|LatC1|LonC1|NumSets}

#----------------------------------------------------------------------------------------------------------------------------
#@geoflow --> with this script 2 objects are pre-loaded
#config --> the global config of the workflow
#entity --> the entity you are managing
#get data from geoflow current job dir
filename <- entity$data$source[[1]]
path_to_raw_dataset <- entity$getJobDataResource(config, filename)
config$logger.info(sprintf("Pre-harmonization of dataset '%s'", entity$identifiers[["id"]]))
opts <- options()
options(encoding = "UTF-8")
#----------------------------------------------------------------------------------------------------------------------------


if(!require(rtunaatlas)){
  if(!require(devtools)){
    install.packages("devtools")
  }
  require(devtools)
  install_github("ptaconet/rtunaatlas")
}
require(rtunaatlas)


##Efforts

# Reach the efforts pivot DSD using a function in IATTC_functions.R
efforts_pivot_IATTC <-FUN_efforts_IATTC_CE_allbutLLTunaBillfish(path_to_raw_dataset,"NumSets","Flag","PS")

colToKeep_efforts <- c("Flag","Gear","time_start","time_end","AreaName","School","EffortUnits","Effort")
efforts<-IATTC_CE_efforts_pivotDSD_to_harmonizedDSD(efforts_pivot_IATTC,colToKeep_efforts)

colnames(efforts)<-c("flag","gear","time_start","time_end","geographic_identifier","schooltype","unit","value")
efforts$source_authority<-"IATTC"


### Compute metadata
#if (path_to_metadata_file!="NULL"){
#  source("https://raw.githubusercontent.com/ptaconet/rtunaatlas_scripts/master/tunaatlas_world/transform/compute_metadata.R")
#} else {
#  df_metadata<-NULL
#  df_codelists<-NULL
#}

## To check the outputs:
# str(dataset)
# str(df_metadata)
# str(df_codelists)

#----------------------------------------------------------------------------------------------------------------------------
#@eblondel additional formatting for next time support
efforts$time_start <- as.Date(efforts$time_start)
efforts$time_end <- as.Date(efforts$time_end)
#we enrich the entity with temporal coverage
dataset_temporal_extent <- paste(as.character(min(efforts$time_start)), as.character(max(efforts$time_end)), sep = "/")
entity$setTemporalExtent(dataset_temporal_extent)
#if there is any entity relation with name 'codelists' we read the file
df_codelists <- NULL
cl_relations <- entity$relations[sapply(entity$relations, function(x){x$name=="codelists"})]
if(length(cl_relations)>0){
	config$logger.info("Appending codelists to pre-harmonization action output")
	df_codelists <- read.csv(cl_relations[[1]]$link)
}
#@geoflow -> output structure as initially used by https://raw.githubusercontent.com/ptaconet/rtunaatlas_scripts/master/workflow_etl/scripts/generate_dataset.R
dataset <- list(
	dataset = efforts, 
	additional_metadata = NULL, #nothing here
	codelists = df_codelists #in case the entity was provided with a link to codelists
)
#@geoflow -> export as csv
output_name_dataset <- gsub(filename, paste0(unlist(strsplit(filename,".csv"))[1], "_harmonized.csv"), path_to_raw_dataset)
write.csv(dataset$dataset, output_name_dataset, row.names = FALSE)
output_name_codelists <- gsub(filename, paste0(unlist(strsplit(filename,".csv"))[1], "_codelists.csv"), path_to_raw_dataset)
write.csv(dataset$codelists, output_name_codelists, row.names = FALSE)
#----------------------------------------------------------------------------------------------------------------------------  
entity$addResource("source", path_to_raw_dataset)
entity$addResource("harmonized", output_name_dataset)
entity$addResource("codelists", output_name_codelists)