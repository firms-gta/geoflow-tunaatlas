#' Harmonize WCPFC Nominal Catch Datasets (geoflow action)
#'
#' geoflow action of the entity: finds its files, runs the core function gta_rawdata_wcpfc_nominal_all()
#' (R/rawdata_wcpfc_nominal_all.R), writes the outputs and declares the resources of
#' the entity. The processing itself is in the core function, usable without geoflow.
#'
#' @param action The action context from geoflow.
#' @param entity The entity context from geoflow.
#' @param config The configuration context from geoflow.
function(action, entity, config){
#packages

if(!require(dplyr)){
  install.packages("dplyr")
  require(dplyr)
}
if(!require(reshape)){
  install.packages("reshape")
  require(reshape)
}
if(!require(lubridate)){
    install.packages("lubridate")
    require(lubridate)
}

  if(!require(data.table)){
    install.packages("data.table")
    require(data.table)
  }  
  if(!require(reshape2)){
    install.packages("reshape2")
    require(reshape2)
  }
 #----------------------------------------------------------------------------------------------------------------------------
#@geoflow --> with this script 2 objects are pre-loaded
#config --> the global config of the workflow
#entity --> the entity you are managing
#get data from geoflow current job dir
filename1 <- entity$data$source[[1]] #WCPFC data
filename2 <- entity$data$source[[2]] #WCPO data
filename3 <- entity$data$source[[3]] #structure
path_to_raw_dataset1 <- entity$getJobDataResource(config, filename1) #WCPFC data
path_to_raw_dataset2 <- entity$getJobDataResource(config, filename2) #WCPO data

opts <- options()
options(encoding = "UTF-8")

NC <- gta_rawdata_wcpfc_nominal_all(path_to_raw_dataset1, path_to_raw_dataset2)

dataset_temporal_extent <- paste(
	paste0(format(min(NC$time_start), "%Y"), "-01-01"),
	paste0(format(max(NC$time_end), "%Y"), "-12-31"),
	sep = "/"
)
entity$setTemporalExtent(dataset_temporal_extent)

base1 <- tools::file_path_sans_ext(basename(filename1))
#@geoflow -> export as csv
# output in same folder as path_to_raw_dataset 
output_name_dataset   <- file.path(dirname(path_to_raw_dataset1), paste0(base1, "_harmonized.csv"))
output_name_codelists <- file.path(dirname(path_to_raw_dataset1), paste0(base1, "_codelists.csv"))

file.rename(  from = entity$getJobDataResource(config, filename3),  to   = output_name_codelists)
#----------------------------------------------------------------------------------------------------------------------------  
write.csv(NC, output_name_dataset, row.names = FALSE)
#----------------------------------------------------------------------------------------------------------------------------  
entity$addResource("source", c(path_to_raw_dataset1, path_to_raw_dataset2))
entity$addResource("harmonized", output_name_dataset)
entity$addResource("codelists", output_name_codelists)}
