######################################################################
##### 52North WPS annotations ##########
######################################################################
# wps.des: id = effort_1deg_1m_ps_iattc_level0__byFlag, title = Harmonize data structure of IATTC PS ByFlag effort datasets, abstract = Harmonize the structure of IATTC catch-and-effort datasets: 'PublicPSBillfishFlag' and 'PublicPSTunaFlag' (pid of output file = pacific_ocean_effort_1deg_1m_ps_tunaatlasIATTC_level0__billfish_byFlag or pacific_ocean_effort_1deg_1m_ps_tunaatlasIATTC_level0__tuna_byFlag). The only mandatory field is the first one. The metadata must be filled-in only if the dataset will be loaded in the Tuna atlas database. ;
# wps.in: id = path_to_raw_dataset, type = String, title = Path to the input dataset to harmonize. Input file must be structured as follow: https://goo.gl/Q1w7Ur, value = "https://goo.gl/Q1w7Ur";
# wps.in: id = path_to_metadata_file, type = String, title = NULL or path to the csv of metadata. The template file can be found here: https://raw.githubusercontent.com/ptaconet/rtunaatlas_scripts/master/sardara_world/transform_trfmos_data_structure/metadata_source_datasets_to_database/metadata_source_datasets_to_database_template.csv . If NULL, no metadata will be outputted., value = "NULL";
# wps.out: id = zip_namefile, type = text/zip, title = Dataset with structure harmonized + File of metadata (for integration within the Tuna Atlas database) + File of code lists (for integration within the Tuna Atlas database) ; 

# '# This script works with any data that has the first 5 columns named and ordered as follow: {Year|Month|Flag|LatC1|LonC1|NumSets}
function(action, entity, config){
  
#packages

  

#----------------------------------------------------------------------------------------------------------------------------
#@geoflow --> with this script 2 objects are pre-loaded
#config --> the global config of the workflow
#entity --> the entity you are managing
#get data from geoflow current job dir
filename1 <- entity$data$source[[1]] #data
filename2 <- entity$data$source[[2]] #structure
path_to_raw_dataset <- entity$getJobDataResource(config, filename1)
config$logger.info(sprintf("Pre-harmonization of dataset '%s'", entity$identifiers[["id"]]))
opts <- options()
options(encoding = "UTF-8")
#----------------------------------------------------------------------------------------------------------------------------

##Efforts

# Reach the efforts pivot DSD using a function in IATTC_functions.R
source("https://raw.githubusercontent.com/firms-gta/geoflow-tunaatlas/master/sardara_functions/FUN_efforts_IATTC_CE_allbutLLTunaBillfish.R")
efforts_pivot_IATTC <-FUN_efforts_IATTC_CE_allbutLLTunaBillfish(path_to_raw_dataset,"NumSets","Flag","PS")

colToKeep_efforts <- c("FishingFleet","Gear","time_start","time_end","AreaName","School","EffortUnits","Effort")
source("https://raw.githubusercontent.com/firms-gta/geoflow-tunaatlas/master/sardara_functions/IATTC_CE_efforts_pivotDSD_to_harmonizedDSD.R")
efforts<-IATTC_CE_efforts_pivotDSD_to_harmonizedDSD(efforts_pivot_IATTC,colToKeep_efforts)

colnames(efforts)<-c("fishing_fleet","gear_type","time_start","time_end","geographic_identifier","fishing_mode","measurement_unit","measurement_value")
efforts$source_authority<-"IATTC"

#----------------------------------------------------------------------------------------------------------------------------
#@eblondel additional formatting for next time support
efforts$time_start <- as.Date(efforts$time_start)
efforts$time_end <- as.Date(efforts$time_end)
#we enrich the entity with temporal coverage
dataset_temporal_extent <- paste(
	paste0(format(min(efforts$time_start), "%Y"), "-01-01"),
	paste0(format(max(efforts$time_end), "%Y"), "-12-31"),
	sep = "/"
)
entity$setTemporalExtent(dataset_temporal_extent)

#@geoflow -> export as csv
output_name_dataset <- gsub(filename1, paste0(unlist(strsplit(filename1,".csv"))[1], "_harmonized.csv"), path_to_raw_dataset)
write.csv(efforts, output_name_dataset, row.names = FALSE)
output_name_codelists <- gsub(filename1, paste0(unlist(strsplit(filename1,".csv"))[1], "_codelists.csv"), path_to_raw_dataset)
file.rename(from = entity$getJobDataResource(config, filename2), to = output_name_codelists)
#----------------------------------------------------------------------------------------------------------------------------  
entity$addResource("source", path_to_raw_dataset)
entity$addResource("harmonized", output_name_dataset)
entity$addResource("codelists", output_name_codelists)}
