######################################################################
##### 52North WPS annotations ##########
######################################################################
# wps.des: id = nominal_catch_iattc_level0, title = Harmonize data structure of IATTC nominal catch, abstract = Harmonize the structure of IATTC nominal catch dataset (pid of output file = pacific_ocean_nominal_catch_tunaatlasiattc_level0). The only mandatory field is the first one. The metadata must be filled-in only if the dataset will be loaded in the Tuna atlas database. ;
# wps.in: id = path_to_raw_dataset, type = String, title = Path to the input dataset to harmonize. Input file must be structured as follow: https://goo.gl/4FexgR, value = "https://goo.gl/4FexgR";
# wps.in: id = path_to_metadata_file, type = String, title = NULL or path to the csv of metadata. The template file can be found here: https://raw.githubusercontent.com/ptaconet/rtunaatlas_scripts/master/sardara_world/transform_trfmos_data_structure/metadata_source_datasets_to_database/metadata_source_datasets_to_database_template.csv. , value = "NULL";
# wps.out: id = zip_namefile, type = text/zip, title = Dataset with structure harmonized + File of metadata (for integration within the Tuna Atlas database) + File of code lists (for integration within the Tuna Atlas database) ; 


#' @author Paul Taconet, IRD \email{paul.taconet@ird.fr}
#' 
#' @keywords Inter-American-Tropical-Tuna-Commission IATTC tuna RFMO Sardara Global database on tuna fishieries
#'
#' @seealso \code{\link{convertDSD_iattc_nc}} to convert IATTC nominal catch data structure, code{\link{convertDSD_iattc_ce_LLTunaBillfish_LLShark}} to convert IATTC task 2 LLTunaBillfish and LLShark data structure, \code{\link{convertDSD_iattc_ce_LPTunaFlag}} to convert IATTC task 2 LPTunaFlag data structure, \code{\link{convertDSD_iattc_ce_LLOrigFormat}} to convert IATTC task 2 Longline original format data structure, \code{\link{convertDSD_iattc_ce_PSSharkSetType}} to convert IATTC task 2 'PublicPSSharkSetType' data structure, \code{\link{convertDSD_iattc_ce_PSSharkFlag}} to convert IATTC task 2 'PublicPSSharkFlag' data structure, \code{\link{convertDSD_iattc_ce_PSSharkFlag}} to convert IATTC task 2 'PublicPSBillfishSetType' and 'PublicPSSharkSetType' and 'PublicPSTunaSetType' data structure, \code{\link{convertDSD_iattc_ce_PSFlag}} to convert IATTC task 2 'PublicPSBillfishFlag' and 'PublicPSSharkFlag' and 'PublicPSTunaFlag' data structure


  # Input data sample:
  # AnoYear BanderaFlag ArteGear EspeciesSpecies    t
  #    1918         OTR       LP             SKJ 1361
  #    1918         OTR       LP             YFT    0
  #    1919         OTR       LP             SKJ 3130
  #    1919         OTR       LP             YFT  136
  #    1920         OTR       LP             SKJ 3583
  #    1920         OTR       LP             YFT  907
  
  # Catch: final data sample:
  # FishingFleet Gear time_start   time_end AreaName School Species CatchType CatchUnits Catch
  #  BLZ   LL 2001-01-01 2002-01-01    IATTC    ALL     ALB       ALL         MT  4854
  #  BLZ   LL 2001-01-01 2002-01-01    IATTC    ALL     BET       ALL         MT  1987
  #  BLZ   LL 2001-01-01 2002-01-01    IATTC    ALL     BIL       ALL         MT   122
  #  BLZ   LL 2001-01-01 2002-01-01    IATTC    ALL     PBF       ALL         MT   131
  #  BLZ   LL 2001-01-01 2002-01-01    IATTC    ALL     SFA       ALL         MT    93
  #  BLZ   LL 2001-01-01 2002-01-01    IATTC    ALL     SKH       ALL         MT  1326
function(action, entity, config){
  source("https://raw.githubusercontent.com/firms-gta/geoflow-tunaatlas/master/sardara_functions/format_time_db_format.R")
#packages

  
  
if(!require(dplyr)){
  install.packages("dplyr")
  require(dplyr)
}


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
   
### Nominal NC

NC <- read.csv(path_to_raw_dataset, header=TRUE, stringsAsFactors=FALSE, strip.white=TRUE)

colToKeep_NC<-c("AnoYear","BanderaFlag","ArteGear","EspeciesSpecies","t")
NC_harm_IATTC<-NC[,colToKeep_NC]
colnames(NC_harm_IATTC)<-c("Year", "Flag","Gear","Species","Catch")

NC_harm_IATTC$AreaName<-"EPO"
NC_harm_IATTC$AreaCWPgrid<-NA
NC_harm_IATTC$School<-"ALL"
NC_harm_IATTC$CatchType<-"ALL"
NC_harm_IATTC$CatchUnits<-"t"
NC_harm_IATTC$RFMO<-"IATTC"
NC_harm_IATTC$Ocean<-"PAC_E"

NC_harm_IATTC$MonthStart<-1
NC_harm_IATTC$Period<-12
#Format inputDataset time to have the time format of the DB, which is one column time_start and one time_end
NC_harm_IATTC<-format_time_db_format(NC_harm_IATTC)

#NC <- NC_harm_IATTC  %>%  filter( ! Catch %in% 0 )
NC <- NC_harm_IATTC[NC_harm_IATTC$Catch != 0,]

rm(NC_harm_IATTC)

colnames(NC)[colnames(NC)=="Flag"] <- "FishingFleet"

colToKeep_captures <- c("FishingFleet","Gear","time_start","time_end","AreaName","School","Species","CatchType","CatchUnits","Catch")
NC <-NC[,colToKeep_captures]
# remove 0 and NA values 
#NC <- NC  %>% 
#  filter( ! Catch %in% 0 ) %>%
#  filter( ! is.na(Catch)) 
NC <- NC[NC$Catch != 0,]
NC <- NC[!is.na(NC$Catch),] 

#NC <- NC %>% 
#  group_by(FishingFleet,Gear,time_start,time_end,AreaName,School,Species,CatchType,CatchUnits) %>% 
#  summarise(Catch = sum(Catch))
#NC<-as.data.frame(NC)
NC <- aggregate(NC$Catch,
		FUN = sum,
		by = list(
			FishingFleet = NC$FishingFleet,
			Gear = NC$Gear,
			time_start = NC$time_start,
			time_end = NC$time_end,
			AreaName = NC$AreaName,
			School = NC$School,
			Species = NC$Species,
			CatchType = NC$CatchType,
			CatchUnits = NC$CatchUnits
		)
	)

colnames(NC)<-c("fishing_fleet","gear_type","time_start","time_end","geographic_identifier","fishing_mode","species","measurement_type","measurement_unit","measurement_value")
NC$source_authority<-"IATTC"

  
#----------------------------------------------------------------------------------------------------------------------------
#@eblondel additional formatting for next time support
NC$time_start <- as.Date(NC$time_start)
NC$time_end <- as.Date(NC$time_end)
#we enrich the entity with temporal coverage
dataset_temporal_extent <- paste(
	paste0(format(min(NC$time_start), "%Y"), "-01-01"),
	paste0(format(max(NC$time_end), "%Y"), "-12-31"),
	sep = "/"
)
entity$setTemporalExtent(dataset_temporal_extent)

#@geoflow -> export as csv
output_name_dataset <- gsub(filename1, paste0(unlist(strsplit(filename1,".csv"))[1], "_harmonized.csv"), path_to_raw_dataset)
write.csv(NC, output_name_dataset, row.names = FALSE)
output_name_codelists <- gsub(filename1, paste0(unlist(strsplit(filename1,".csv"))[1], "_codelists.csv"), path_to_raw_dataset)
file.rename(from = entity$getJobDataResource(config, filename2), to = output_name_codelists)
#----------------------------------------------------------------------------------------------------------------------------  
entity$addResource("source", path_to_raw_dataset)
entity$addResource("harmonized", output_name_dataset)
entity$addResource("codelists", output_name_codelists)

 

}
