######################################################################
##### 52North WPS annotations ##########
######################################################################
# wps.des: id = atlantic_ocean_nominal_catch_tunaatlasiccat_level0, title = Harmonize data structure of ICCAT nominal catch, abstract = Harmonize the structure of ICCAT nominal catch dataset (pid of output file = atlantic_ocean_nominal_catch_tunaatlasICCAT_level0__bySamplingArea or atlantic_ocean_nominal_catch_tunaatlasICCAT_level0__byStockArea). The only mandatory field is the first one. The metadata must be filled-in only if the dataset will be loaded in the Tuna atlas database. ;
# wps.in: id = path_to_raw_dataset, type = String, title = Path to the input dataset to harmonize. Input file must be structured as follow: https://goo.gl/lEw8oK, value = "https://goo.gl/lEw8oK";
# wps.in: id = spatial_stratification, type = String, title = Spatial stratification to keep. SampAreaCode is for Sampling areas and Stock is for stock areas, value = "SampAreaCode|Stock";
# wps.in: id = keep_fleet_instead_of_flag, type = Boolean, title = By default the column "flag" is kept. By setting this argument to TRUE the column "fleet" will be kept (and "flag" will be removed), value = FALSE;
# wps.in: id = path_to_metadata_file, type = String, title = NULL or path to the csv of metadata. The template file can be found here: https://raw.githubusercontent.com/ptaconet/rtunaatlas_scripts/master/sardara_world/transform_trfmos_data_structure/metadata_source_datasets_to_database/metadata_source_datasets_to_database_template.csv. , value = "NULL";
# wps.out: id = zip_namefile, type = text/zip, title = Dataset with structure harmonized + File of metadata (for integration within the Tuna Atlas database) + File of code lists (for integration within the Tuna Atlas database) ; 

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

keep_fleet_instead_of_flag=FALSE
spatial_stratification <- switch(unlist(strsplit(entity$identifiers[["id"]], "__"))[2],
	"bysamplingarea" = "SampAreaCode",
	"bystockarea" = "Stock",
	NULL
)
if(is.null(spatial_stratification)) stop("Hum! Something went wrong in getting the spatial stratification!")


if(!require(rtunaatlas)){
  if(!require(devtools)){
    install.packages("devtools")
  }
  require(devtools)
  install_github("ptaconet/rtunaatlas")
}
if(!require(dplyr)){
  install.packages("dplyr")
}
require(rtunaatlas)
require(dplyr)

#' @author Paul Taconet, IRD \email{paul.taconet@ird.fr}
#' 
#' @keywords Internal Commission for the Conservation of Atlantic Tuna ICCAT tuna RFMO Sardara Global database on tuna fishieries
#'
#' @seealso \code{\link{convertDSD_iccat_ce_task2}} to convert ICCAT task 2 , \code{\link{convertDSD_iccat_nc}} to convert ICCAT nominal catch data structure


#library(readxl) # devtools::install_github("hadley/readxl") 

#ICCAT_NC<-read_excel(path_to_raw_dataset, sheet = "dsT1NC", col_names = TRUE, col_types = NULL,na = "", skip = 3)
ICCAT_NC<-read.csv(path_to_raw_dataset)

colToKeep_NC<-c("Species","YearC","Flag",spatial_stratification,"GearCode","Qty_t","CatchTypeCode")  ### Previously CatchTypeCode was named DataType
NC_harm_ICCAT<-ICCAT_NC[,colToKeep_NC]

if(keep_fleet_instead_of_flag==TRUE){
  # We rename the column 'Fleet' to 'Flag' so that the script below work
  colnames(NC_harm_ICCAT)[colnames(NC_harm_ICCAT) == 'Fleet'] <- 'Flag'
}

colnames(NC_harm_ICCAT)<-c("Species", "Year","Flag","AreaName","Gear","Catch","CatchType")

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

colToKeep_captures <- c("Flag","Gear","time_start","time_end","AreaName","School","Species","CatchType","CatchUnits","Catch")
NC <-NC[,colToKeep_captures]
# remove 0 and NA values 
NC <- NC[NC$Catch != 0,]
NC <- NC[!is.na(NC$Catch),] 

#NC <- NC %>% 
#  group_by(Flag,Gear,time_start,time_end,AreaName,School,Species,CatchType,CatchUnits) %>% 
#  summarise(Catch = sum(Catch))
#NC<-as.data.frame(NC)
NC <- aggregate(NC$Catch,
		FUN = sum,
		by = list(
			Flag = NC$Flag,
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
	
colnames(NC)<-c("flag","gear","time_start","time_end","geographic_identifier","schooltype","species","catchtype","unit","value")
NC$source_authority<-"ICCAT"
NC %>% mutate_if(is.factor, as.character) -> NC

if(any(NC$flag=="Côte d'Ivoire")) NC$flag[NC$flag=="Côte d'Ivoire"] <- "Côte D Ivoire"
if(any(NC$flag=="Serbia & Montenegro")) NC$flag[NC$flag=="Serbia & Montenegro"] <- "Serbia and Montenegro"

#dataset<-NC

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
NC$time_start <- as.Date(NC$time_start)
NC$time_end <- as.Date(NC$time_end)
#we enrich the entity with temporal coverage
dataset_temporal_extent <- paste(as.character(min(NC$time_start)), as.character(max(NC$time_end)), sep = "/")
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
	dataset = NC, 
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
options(opts)