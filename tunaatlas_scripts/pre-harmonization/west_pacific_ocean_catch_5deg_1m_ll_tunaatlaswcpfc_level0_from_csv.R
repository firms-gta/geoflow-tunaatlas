######################################################################
##### 52North WPS annotations ##########
######################################################################
# wps.des: id = catch_5deg_1m_ll_wcpfc_level0_from_csv, title = Harmonize data structure of WCPFC Longline catch datasets, abstract = Harmonize the structure of WCPFC catch-and-effort datasets provided by the WCPFC in CSV format: 
# wps.in: id = path_to_raw_dataset, type = String, title = Path to the input dataset to harmonize. Input file must be structured as follow: http://data.d4science.org/NWdZYTRPK3c3c3JQWkpZUExic0RFTHZlbUMxUTgwMmtHbWJQNStIS0N6Yz0, value = "http://data.d4science.org/NWdZYTRPK3c3c3JQWkpZUExic0RFTHZlbUMxUTgwMmtHbWJQNStIS0N6Yz0";
# wps.in: id = path_to_metadata_file, type = String, title = NULL or path to the csv of metadata. The template file can be found here: https://raw.githubusercontent.com/ptaconet/rtunaatlas_scripts/master/sardara_world/transform_trfmos_data_structure/metadata_source_datasets_to_database/metadata_source_datasets_to_database_template.csv . If NULL, no metadata will be outputted., value = "NULL";
# wps.out: id = zip_namefile, type = text/zip, title = Dataset with structure harmonized + File of metadata (for integration within the Tuna Atlas database) + File of code lists (for integration within the Tuna Atlas database) ; 

#' This script works with any dataset that has the first 5 columns named and ordered as follow: {YY|MM|LAT5|LON5|HHOOKS} followed by a list of columns specifing the species codes with "_N" for catches expressed in number and "_T" for catches expressed in tons
#' 
#' @author Paul Taconet, IRD \email{paul.taconet@ird.fr}
#' 
#' @keywords Western and Central Pacific Fisheries Commission WCPFC tuna RFMO Sardara Global database on tuna fishieries
#'

# Input data sample:
# YY MM LAT5 LON5   HHOOKS ALB_C ALB_N   YFT_C YFT_N   BET_C BET_N MLS_C MLS_N  BLM_C BLM_N  BUM_C BUM_N  SWO_C SWO_N OTH_C OTH_N
# 2000  1  00N 120E 12391.11 0.000     0 267.338 10056  58.850  1537 0.627    15 11.391   249 18.203   314  9.998   189 0.120     4
# 2000  1  00N 125E 16349.59 0.000     0 352.417 13256  77.975  2036 0.827    19 15.030   329 24.018   414 13.192   249 0.158     5
# 2000  1  00N 130E  7091.08 0.000     0 130.454  4630  37.695   903 0.200     5  3.870    83  6.418   109  4.714    93 0.038     1
# 2000  1  00N 135E  6113.85 1.276    73  75.469  2431 115.868  2575 0.037     1  0.058     1  6.948    90  2.719    38 0.245     4
# 2000  1  00N 140E  9904.92 1.350    77 176.963  6266 251.303  6084 0.462    11  1.527    38 12.150   187  4.200    52 0.296     9
# 2000  1  00N 145E  8679.03 0.428    24 122.945  4613 144.910  3579 0.537    12 11.062   237  8.748   137  6.326   110 0.000     0

# Catch: pivot data sample:
# YY MM LAT5 LON5 Effort Species value CatchUnits School EffortUnits Gear
# 1983 11  35S 170W    133     ALB   886         NO    ALL        DAYS    D
# 1983 12  35S 170W    133     ALB   870         NO    ALL        DAYS    D
# 1983 12  40S 170W    248     ALB  3822         NO    ALL        DAYS    D
# 1984  1  35S 165E     85     ALB    53         NO    ALL        DAYS    D
# 1984  1  40S 170W    704     ALB  3850         NO    ALL        DAYS    D
# 1984  1  40S 175W     88     ALB   966         NO    ALL        DAYS    D
function(action, entity, config){
  source("https://raw.githubusercontent.com/firms-gta/geoflow-tunaatlas/master/sardara_functions/harmo_time_2.R")
  source("https://raw.githubusercontent.com/firms-gta/geoflow-tunaatlas/master/sardara_functions/harmo_spatial_3.R")
#packages

  

if(!require(readr)){
  install.packages("readr")
  require(readr)
}

if(!require(tidyr)){
  install.packages("tidyr")
  require(tidyr)
}

if(!require(dplyr)){
  install.packages("dplyr")
  require(dplyr)
}


if(!require(reshape)){
  install.packages("reshape")
  require(reshape)
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


##Catches
DF <- read.table(path_to_raw_dataset, sep=",", header=TRUE, stringsAsFactors=FALSE,strip.white=TRUE)

#2020-11-13 @eblondel
#Changes
#	- Flag column added add UNK where missing
#	- Change id upper index for melting
#---------------------------------------
DF$cwp_grid=NULL # remove column cwp_grid
colnames(DF)<-toupper(colnames(DF))
if(any(DF$FLAG_ID == "")) DF[DF$FLAG_ID == "",]$FLAG_ID <- "UNK"
# DF<-melt(DF, id=c(colnames(DF[1:6]))) 
# DF <- melt(as.data.table(DF), id=c(colnames(DF[1:6]))) 
DF <- DF %>% tidyr::gather(variable, value, -c(colnames(DF[1:6])))

DF<- DF %>% 
  dplyr::filter( ! value %in% 0 ) %>%
  dplyr::filter( ! is.na(value)) 
DF$variable<-as.character(DF$variable)
colnames(DF)[which(colnames(DF) == "variable")] <- "Species"

DF$CatchUnits<-substr(DF$Species, nchar(DF$Species), nchar(DF$Species))

DF$Species<-sub('_C', '', DF$Species)
DF$Species<-sub('_N', '', DF$Species)

DF$School<-"OTH"

DF$EffortUnits<-colnames(DF[6])    
colnames(DF)[6]<-"Effort"


catches_pivot_WCPFC=DF
catches_pivot_WCPFC$Gear<-"L"

# Catchunits
# Check data that exist both in number and weight

number_of_units_by_strata<- dplyr::summarise(group_by_(catches_pivot_WCPFC,.dots=setdiff(colnames(catches_pivot_WCPFC),c("value","CatchUnits"))), count = n())

strata_in_number_and_weight<-number_of_units_by_strata[number_of_units_by_strata$count>1,]

catches_pivot_WCPFC<-left_join (catches_pivot_WCPFC,strata_in_number_and_weight,by=setdiff(colnames(strata_in_number_and_weight),"count"))

index.catchinweightandnumber <- which(catches_pivot_WCPFC[,"count"]==2 & catches_pivot_WCPFC[,"CatchUnits"]=="N")
catches_pivot_WCPFC[index.catchinweightandnumber,"CatchUnits"]="NOMT"

index.catchinweightandnumber <- which(catches_pivot_WCPFC[,"count"]==2 & catches_pivot_WCPFC[,"CatchUnits"]=="C")
catches_pivot_WCPFC[index.catchinweightandnumber,"CatchUnits"]="MTNO"

index.catchinweightonly <- which(catches_pivot_WCPFC[,"CatchUnits"]=="C")
catches_pivot_WCPFC[index.catchinweightonly,"CatchUnits"]="MT"

index.catchinnumberonly <- which(catches_pivot_WCPFC[,"CatchUnits"]=="N")
catches_pivot_WCPFC[index.catchinnumberonly,"CatchUnits"]="NO"

# School
catches_pivot_WCPFC$School<-"ALL"

### Reach the catches harmonized DSD using a function in WCPFC_functions.R
colToKeep_captures <- c("FishingFleet","Gear","time_start","time_end","AreaName","School","Species","CatchType","CatchUnits","Catch")
#catches<-WCPFC_CE_catches_pivotDSD_to_harmonizedDSD(catches_pivot_WCPFC,colToKeep_captures)
#2020-11-13 @eblondel
catches_pivot_WCPFC$RFMO <- "WCPFC"
catches_pivot_WCPFC$Ocean <- "PAC_W"
catches_pivot_WCPFC$FishingFleet <- catches_pivot_WCPFC$FLAG_ID #@eblondel added
catches_pivot_WCPFC <- harmo_time_2(catches_pivot_WCPFC, 
	"YY", "MM")
catches_pivot_WCPFC <- harmo_spatial_3(catches_pivot_WCPFC, 
	"LAT_SHORT", "LON_SHORT", 5, 6) #@eblondel change column names LAT5 -> LAT_SHORT, LON5 -> LON_SHORT
catches_pivot_WCPFC$CatchType <- "ALL"
catches_pivot_WCPFC$Catch <- catches_pivot_WCPFC$value
catches <- catches_pivot_WCPFC[colToKeep_captures]
rm(catches_pivot_WCPFC)
catches[, c("AreaName", "FishingFleet")] <- as.data.frame(apply(catches[, 
	c("AreaName", "FishingFleet")], 2, function(x) {
	gsub(" *$", "", x)
}), stringsAsFactors = FALSE)
catches <- catches %>% filter(!Catch %in% 0) %>% filter(!is.na(Catch))
catches <- as.data.frame(catches)
catches <- aggregate(catches$Catch,
	by = list(
		FishingFleet = catches$FishingFleet,
		Gear = catches$Gear,
		time_start = catches$time_start,
		time_end = catches$time_end,
		AreaName = catches$AreaName,
		School = catches$School,
		Species = catches$Species,
		CatchType = catches$CatchType,
		CatchUnits = catches$CatchUnits
	),
	FUN = sum)
colnames(catches)[colnames(catches)=="x"] <- "Catch"

colnames(catches)<-c("fishingfleet","gear","time_start","time_end","geographic_identifier","schooltype","species","catchtype","unit","value")
catches$source_authority<-"WCPFC"

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
output_name_dataset <- gsub(filename1, paste0(unlist(strsplit(filename1,".csv"))[1], "_harmonized.csv"), path_to_raw_dataset)
write.csv(catches, output_name_dataset, row.names = FALSE)
output_name_codelists <- gsub(filename1, paste0(unlist(strsplit(filename1,".csv"))[1], "_codelists.csv"), path_to_raw_dataset)
file.rename(from = entity$getJobDataResource(config, filename2), to = output_name_codelists)
#----------------------------------------------------------------------------------------------------------------------------
entity$addResource("source", path_to_raw_dataset)
entity$addResource("harmonized", output_name_dataset)
entity$addResource("codelists", output_name_codelists)
}
