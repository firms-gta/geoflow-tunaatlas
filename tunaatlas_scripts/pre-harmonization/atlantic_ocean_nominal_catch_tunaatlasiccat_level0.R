######################################################################
##### 52North WPS annotations ##########
######################################################################
# wps.des: id = nominal_catch_iccat_level0, title = Harmonize data structure of ICCAT nominal catch, abstract = Harmonize the structure of ICCAT nominal catch dataset (pid of output file = atlantic_ocean_nominal_catch_tunaatlasICCAT_level0__bySamplingArea or atlantic_ocean_nominal_catch_tunaatlasICCAT_level0__byStockArea). The only mandatory field is the first one. The metadata must be filled-in only if the dataset will be loaded in the Tuna atlas database. ;
# wps.in: id = path_to_raw_dataset, type = String, title = Path to the input dataset to harmonize. Input file must be structured as follow: https://goo.gl/lEw8oK, value = "https://goo.gl/lEw8oK";
# wps.in: id = spatial_stratification, type = String, title = Spatial stratification to keep. SampAreaCode is for Sampling areas and Stock is for stock areas, value = "SampAreaCode|Stock";
# wps.in: id = keep_fleet_instead_of_flag, type = Boolean, title = By default the column "flag" is kept. By setting this argument to TRUE the column "fleet" will be kept (and "flag" will be removed), value = FALSE;
# wps.in: id = path_to_metadata_file, type = String, title = NULL or path to the csv of metadata. The template file can be found here: https://raw.githubusercontent.com/ptaconet/rtunaatlas_scripts/master/sardara_world/transform_trfmos_data_structure/metadata_source_datasets_to_database/metadata_source_datasets_to_database_template.csv. , value = "NULL";
# wps.out: id = zip_namefile, type = text/zip, title = Dataset with structure harmonized + File of metadata (for integration within the Tuna Atlas database) + File of code lists (for integration within the Tuna Atlas database) ; 

#' @author Paul Taconet, IRD \email{paul.taconet@ird.fr}
#' 
#' @keywords Internal Commission for the Conservation of Atlantic Tuna ICCAT tuna RFMO Sardara Global database on tuna fishieries
#'
#' @seealso \code{\link{convertDSD_iccat_ce_task2}} to convert ICCAT task 2 , \code{\link{convertDSD_iccat_nc}} to convert ICCAT nominal catch data structure
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
opts <- base::options()
base::options(encoding = "UTF-8")
#----------------------------------------------------------------------------------------------------------------------------

keep_fleet_instead_of_flag=FALSE
spatial_stratification <- switch(unlist(strsplit(entity$identifiers[["id"]], "__"))[2],
	"bysamplingarea" = "SampAreaCode",
	"bystockarea" = "Stock",
	NULL
)
if(is.null(spatial_stratification)) stop("Hum! Something went wrong in getting the spatial stratification!")


#library(readxl) # devtools::install_github("hadley/readxl") 
#ICCAT_NC<-read_excel(path_to_raw_dataset, sheet = "dsT1NC", col_names = TRUE, col_types = NULL,na = "", skip = 3)
ICCAT_NC<-read.csv(path_to_raw_dataset)
colnames(ICCAT_NC)[colnames(ICCAT_NC)=="Flag"] <- "FishingFleet"

colToKeep_NC<-c("Species","YearC","FishingFleet",spatial_stratification,"GearCode","Qty_t","CatchTypeCode")  ### Previously CatchTypeCode was named DataType
NC_harm_ICCAT<-ICCAT_NC[,colToKeep_NC]

if(keep_fleet_instead_of_flag==TRUE){
  # We rename the column 'Fleet' to 'Flag' so that the script below work
  colnames(NC_harm_ICCAT)[colnames(NC_harm_ICCAT) == 'Fleet'] <- 'FishingFleet'
}

colnames(NC_harm_ICCAT)<-c("Species", "Year","FishingFleet","AreaName","Gear","Catch","CatchType")

NC_harm_ICCAT$AreaCWPgrid<-NA
NC_harm_ICCAT$School<-"ALL"
NC_harm_ICCAT$CatchUnits<-"MT"
NC_harm_ICCAT$RFMO<-"ICCAT"
NC_harm_ICCAT$Ocean<-"ATL"

NC_harm_ICCAT$MonthStart<-1
NC_harm_ICCAT$Period<-12

#Format inputDataset time to have the time format of the DB, which is one column time_start and one time_end
NC_harm_ICCAT<-as.data.frame(NC_harm_ICCAT)
NC_harm_ICCAT<-format_time_db_format(NC_harm_ICCAT)

NC<-NC_harm_ICCAT
rm(NC_harm_ICCAT)

colToKeep_captures <- c("FishingFleet","Gear","time_start","time_end","AreaName","School","Species","CatchType","CatchUnits","Catch")
NC <-NC[,colToKeep_captures]
# remove 0 and NA values 
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
	
colnames(NC)<-c("fishingfleet","gear","time_start","time_end","geographic_identifier","schooltype","species","catchtype","unit","value")
NC$source_authority<-"ICCAT"
NC %>% mutate_if(is.factor, as.character) -> NC

if(any(NC$fishingfleet=="Côte d'Ivoire")) NC$fishingfleet[NC$fishingfleet=="Côte d'Ivoire"] <- "Côte D Ivoire"
if(any(NC$fishingfleet=="Serbia & Montenegro")) NC$fishingfleet[NC$fishingfleet=="Serbia & Montenegro"] <- "Serbia and Montenegro"

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
base::options(opts)
}
