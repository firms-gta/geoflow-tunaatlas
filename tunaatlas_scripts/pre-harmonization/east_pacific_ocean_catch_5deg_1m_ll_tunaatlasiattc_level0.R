######################################################################
##### 52North WPS annotations ##########
######################################################################
# wps.des: id = catch_5deg_1m_ll_iattc_level0, title = Harmonize data structure of IATTC LL (longline) catch datasets, abstract = Harmonize the structure of IATTC catch-and-effort datasets: 'Shark' and 'Tuna_Billfish' (pid of output file = pacific_ocean_catch_5deg_1m_ll_tunaatlasIATTC_level0__shark or pacific_ocean_catch_5deg_1m_ll_tunaatlasIATTC_level0__tuna_billfish). The only mandatory field is the first one. The metadata must be filled-in only if the dataset will be loaded in the Tuna atlas database. ;
# wps.in: id = path_to_raw_dataset, type = String, title = Path to the catch dataset. Input file must be structured as follow: https://goo.gl/ObIRfj, value = "https://goo.gl/ObIRfj";
# wps.in: id = path_to_effort_dataset, type = String, title = Path to the effort dataset. Input file must be structured as follow: https://goo.gl/U0zyWa, value = "https://goo.gl/U0zyWa";
# wps.in: id = path_to_metadata_file, type = String, title = NULL or path to the csv of metadata. The template file can be found here: https://raw.githubusercontent.com/ptaconet/rtunaatlas_scripts/master/sardara_world/transform_trfmos_data_structure/metadata_source_datasets_to_database/metadata_source_datasets_to_database_template.csv . If NULL, no metadata will be outputted., value = "NULL";
# wps.out: id = zip_namefile, type = text/zip, title = Dataset with structure harmonized + File of metadata (for integration within the Tuna Atlas database) + File of code lists (for integration within the Tuna Atlas database) ; 


#' @author Paul Taconet, IRD \email{paul.taconet@ird.fr}
#' 
#' @keywords Inter-American-Tropical-Tuna-Commission IATTC tuna RFMO Sardara Global database on tuna fishieries
#'
#' @seealso \code{\link{convertDSD_iattc_nc}} to convert IATTC nominal catch data structure, code{\link{convertDSD_iattc_ce_LLTunaBillfish_LLShark}} to convert IATTC task 2 LLTunaBillfish and LLShark data structure, \code{\link{convertDSD_iattc_ce_LPTunaFlag}} to convert IATTC task 2 LPTunaFlag data structure, \code{\link{convertDSD_iattc_ce_LLOrigFormat}} to convert IATTC task 2 Longline original format data structure, \code{\link{convertDSD_iattc_ce_PSSharkSetType}} to convert IATTC task 2 'PublicPSSharkSetType' data structure, \code{\link{convertDSD_iattc_ce_PSSharkFlag}} to convert IATTC task 2 'PublicPSSharkFlag' data structure, \code{\link{convertDSD_iattc_ce_PSSharkFlag}} to convert IATTC task 2 'PublicPSBillfishSetType' and 'PublicPSSharkSetType' and 'PublicPSTunaSetType' data structure, \code{\link{convertDSD_iattc_ce_PSFlag}} to convert IATTC task 2 'PublicPSBillfishFlag' and 'PublicPSSharkFlag' and 'PublicPSTunaFlag' data structure
#'

# Catch input data sample:
# Record Spp DTypeID Number Weight
#  11407 ALB       2     17     NA
#  11407 BUM       2      4     NA
#  11407 BLM       2      2     NA
#  11407 SWO       2     10     NA
#  11407 BET       2    403     NA
#  11407 BIL       2      1     NA


# Catch: final data sample:
# Flag Gear time_start   time_end AreaName School Species CatchType CatchUnits Catch
#  USA   LL 1992-07-01 1992-08-01  6425135    ALL     BSH       ALL         NO     4
#  USA   LL 1993-04-01 1993-05-01  6425135    ALL     BSH       ALL         NO    75
#  USA   LL 1993-04-01 1993-05-01  6430135    ALL     BSH       ALL         NO    15
#  USA   LL 1993-05-01 1993-06-01  6425135    ALL     BSH       ALL         NO    24
#  USA   LL 1994-03-01 1994-04-01  6425135    ALL     BSH       ALL         NO    14
#  USA   LL 1994-03-01 1994-04-01  6430135    ALL     BSH       ALL         NO     4

#packages
if(!require(rtunaatlas)){
  if(!require(devtools)){
    install.packages("devtools")
  }
  require(devtools)
  install_github("ptaconet/rtunaatlas")
  require(rtunaatlas)
}
if(!require(reshape)){
  install.packages("reshape")
  require(reshape)
}
if(!require(dplyr)){
  install.packages("dplyr")
  require(dplyr)
}

#----------------------------------------------------------------------------------------------------------------------------
#@geoflow --> with this script 2 objects are pre-loaded
#config --> the global config of the workflow
#entity --> the entity you are managing
#get data from geoflow current job dir
filename_catch <- entity$data$source[[1]] #catch data
filename_effort <- entity$data$source[[2]] #effort data
filename_str <- entity$data$source[[3]] #structure
path_to_raw_dataset_catch <- entity$getJobDataResource(config, filename_catch)
path_to_raw_dataset_effort <- entity$getJobDataResource(config, filename_effort)
config$logger.info(sprintf("Pre-harmonization of dataset '%s'", entity$identifiers[["id"]]))
opts <- options()
options(encoding = "UTF-8")
#----------------------------------------------------------------------------------------------------------------------------

##Catches
catches<-read.csv(path_to_raw_dataset_catch, stringsAsFactors = F)
efforts<-read.csv(path_to_raw_dataset_effort, stringsAsFactors = F)
nrow(catches)
head(catches)
# catches<-melt(catches, id.vars=c("Record","Spp","DTypeID"))  #@juldebar error with melt function from reshape package
# catches <-melt(as.data.table(catches),id.vars=c("Record","Spp","DTypeID"))
catches <- catches %>% tidyr::gather(variable, value, -c("Record","Spp","DTypeID"))
# remove values=0
catches <- catches  %>% 
  filter( ! value %in% 0 ) %>%
  filter( ! is.na(value)) 

# Set catchunit values
# DType code 1 means that the data was submitted in both weight and number for the same catch
# DType code 2 means that the data was submitted as number only
# DType code 3 means that the data was submitted as weight only

#Initialisation
catches$CatchUnits<-'NA'

index.CatchunitMT<-which(catches[,"DTypeID"]==3 & catches[,"variable"]=="Weight")
index.CatchunitNO<-which(catches[,"DTypeID"]==2 & catches[,"variable"]=="Number")
index.CatchunitMTNO<-which(catches[,"DTypeID"]==1 & catches[,"variable"]=="Weight")
index.CatchunitNOMT<-which(catches[,"DTypeID"]==1 & catches[,"variable"]=="Number")

catches$CatchUnits[index.CatchunitMT]<-"MT"
catches$CatchUnits[index.CatchunitNO]<-"NO"
catches$CatchUnits[index.CatchunitMTNO]<-"MTNO"
catches$CatchUnits[index.CatchunitNOMT]<-"NOMT"

# Merge catches and efforts to have the strata in the catch file
catches<-left_join(catches,efforts,by=c("Record"))
catches <- catches[c("Spp","value","CatchUnits","Year","Month","FlagAbv","Lat","Lon")]

colnames(catches)<-c("variable","value","CatchUnits","Year","Month","Flag","Lat","Lon")

catches$SquareSize<-5
catches$CodeSquareSize<-6
catches$Gear<-"LL"
catches$SetType<-"ALL"

catches$variable[which(catches[,"variable"]=="BuM")]<-"BUM"

# Reach the catches harmonized DSD using a function in IATTC_functions.R
colToKeep_captures <- c("Flag","Gear","time_start","time_end","AreaName","School","Species","CatchType","CatchUnits","Catch")
catches<-IATTC_CE_catches_pivotDSD_to_harmonizedDSD(catches,colToKeep_captures)

colnames(catches)<-c("flag","gear","time_start","time_end","geographic_identifier","schooltype","species","catchtype","unit","value")
catches$source_authority<-"IATTC"
  
#----------------------------------------------------------------------------------------------------------------------------
#@eblondel additional formatting for next time support
catches$time_start <- as.Date(catches$time_start)
catches$time_end <- as.Date(catches$time_end)
#we enrich the entity with temporal coverage
dataset_temporal_extent <- paste(
	paste0(format(min(catches$time_start), "%Y"), "-01-01"),
	paste0(format(max(catches$time_end), "%Y"), "-12-31"),
	sep = "/"
)
entity$setTemporalExtent(dataset_temporal_extent)

#@geoflow -> export as csv
output_name_dataset <- gsub(filename_catch, paste0(unlist(strsplit(filename_catch,".csv"))[1], "_harmonized.csv"), path_to_raw_dataset_catch)
write.csv(catches, output_name_dataset, row.names = FALSE)
output_name_codelists <- gsub(filename_catch, paste0(unlist(strsplit(filename_catch,".csv"))[1], "_codelists.csv"), path_to_raw_dataset_catch)
file.rename(from = entity$getJobDataResource(config, filename_str), to = output_name_codelists)
#----------------------------------------------------------------------------------------------------------------------------  
entity$addResource("source", path_to_raw_dataset_catch)
entity$addResource("harmonized", output_name_dataset)
entity$addResource("codelists", output_name_codelists)
 
