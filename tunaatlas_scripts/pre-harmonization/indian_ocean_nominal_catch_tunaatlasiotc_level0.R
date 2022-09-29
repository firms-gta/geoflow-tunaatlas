######################################################################
##### 52North WPS annotations ##########
######################################################################
# wps.des: id = nominal_catch_iotc_level0, title = Harmonize data structure of IOTC nominal catch, abstract = Harmonize the structure of IOTC nominal catch dataset (pid of output file = indian_ocean_nominal_catch_tunaatlasiotc_level0). The only mandatory field is the first one. The metadata must be filled-in only if the dataset will be loaded in the Tuna atlas database. ;
# wps.in: id = path_to_raw_dataset, type = String, title = Path to the input dataset to harmonize. Input file must be structured as follow: https://goo.gl/Gt8vn0, value = "https://goo.gl/Gt8vn0";
# wps.in: id = path_to_metadata_file, type = String, title = NULL or path to the csv of metadata. The template file can be found here: https://raw.githubusercontent.com/ptaconet/rtunaatlas_scripts/master/sardara_world/transform_trfmos_data_structure/metadata_source_datasets_to_database/metadata_source_datasets_to_database_template.csv. , value = "NULL";
# wps.out: id = zip_namefile, type = text/zip, title = Dataset with structure harmonized + File of metadata (for integration within the Tuna Atlas database) + File of code lists (for integration within the Tuna Atlas database) ; 


#' @author Paul Taconet, IRD \email{paul.taconet@ird.fr}
#' 
#' @keywords Indian Ocean Tuna Commission IOTC tuna RFMO Sardara Global database on tuna fishieries
#'
#' @seealso \code{\link{convertDSD_iotc_ce_LonglineCoastal}} to convert IOTC task 2 CECoastal and CELongline data structure, \code{\link{convertDSD_iotc_ce_Surface}} to convert IOTC task 2 CESurface data structure
#'

  
  # Input data sample:
  # FlSort FlCde     Fleet    Flotte ArCde             AreaIOTC           ZoneCTOI Year/An TFCde       TypeFishery          TypePêcherie GrSort GrCde         Gear        Engin GrGroup GrGroupe GrMult SpSort SpCde
  #      1   AUS AUSTRALIA AUSTRALIE   F57 Eastern Indian Ocean   Océan Indien Est    1950   ART Artisanal Fishing Pêcheries artisanales      9  UNCL Unclassified   Non classé   Other   Autres      1     16   KGX
  #      7   COM   COMOROS   COMORES   F51 Western Indian Ocean Océan Indien Ouest    1950   ART Artisanal Fishing Pêcheries artisanales      6  HAND    Hand line Ligne à main    Line   Lignes      0      1   YFT
  #      7   COM   COMOROS   COMORES   F51 Western Indian Ocean Océan Indien Ouest    1950   ART Artisanal Fishing Pêcheries artisanales      6  HAND    Hand line Ligne à main    Line   Lignes      0      2   BET
  #      7   COM   COMOROS   COMORES   F51 Western Indian Ocean Océan Indien Ouest    1950   ART Artisanal Fishing Pêcheries artisanales      6  HAND    Hand line Ligne à main    Line   Lignes      0      3   SKJ
  #      7   COM   COMOROS   COMORES   F51 Western Indian Ocean Océan Indien Ouest    1950   ART Artisanal Fishing Pêcheries artisanales      6  HAND    Hand line Ligne à main    Line   Lignes      0     10   KAW
  #      7   COM   COMOROS   COMORES   F51 Western Indian Ocean Océan Indien Ouest    1950   ART Artisanal Fishing Pêcheries artisanales      6  HAND    Hand line Ligne à main    Line   Lignes      0     17   SWO
  # Species             Espèce              SpLat  SpGroup   SpGroupe SpWP/SpGT SpIOTC SpMult Catch/Capture(t) Fgrounds CatalogGroup NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA
  # Seerfishes nei       Thazards nca Scomberomorus spp. SEERFISH   THAZARDS      NERI      1      1       100.000000  COASTAL         OSEE NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA
  # Yellowfin tuna           Albacore  Thunnus albacares    TUNAS      THONS      TROP      1      0       180.000000  PELAGIC          YFT NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA
  #    Bigeye tuna Patudo; Thon obèse     Thunnus obesus    TUNAS      THONS      TROP      1      0        13.610946  PELAGIC          BET NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA
  #  Skipjack tuna             Listao Katsuwonus pelamis    TUNAS      THONS      TROP      1      0        20.000000  PELAGIC          SKJ NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA
  #       Kawakawa  Thonine orientale  Euthynnus affinis    TUNAS      THONS      NERI      1      0         1.285176  PELAGIC         STUN NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA
  #      Swordfish            Espadon    Xiphias gladius BILLFISH PORTE-ÉPÉE      BILL      1      0        13.087126  PELAGIC          SWO NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA
  
  # Catch: final data sample:
  # FishingFleet Gear time_start   time_end AreaName School Species CatchType CatchUnits    Catch
  #  ARE GILL 1950-01-01 1951-01-01      F51    ALL     COM       ALL         MT 603.4760
  #  ARE GILL 1950-01-01 1951-01-01      F51    ALL     LOT       ALL         MT 517.2712
  #  ARE GILL 1951-01-01 1952-01-01      F51    ALL     COM       ALL         MT 603.4760
  #  ARE GILL 1951-01-01 1952-01-01      F51    ALL     LOT       ALL         MT 517.2712
  #  ARE GILL 1952-01-01 1953-01-01      F51    ALL     COM       ALL         MT 603.4760
  #  ARE GILL 1952-01-01 1953-01-01      F51    ALL     LOT       ALL         MT 517.2712
function(action, entity, config){
  
#packages
if(!require(dplyr)){
  install.packages("dplyr")
  require(dplyr)
}
if(!require(rtunaatlas)){
  if(!require(devtools)){
    install.packages("devtools")
  }
  require(devtools)
  install_github("ptaconet/rtunaatlas")
  require(rtunaatlas)
}
if(!require(readxl)){
  install.packages("readxl")
  require(readxl)
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
# opts <- options()
options(encoding = "UTF-8")
#----------------------------------------------------------------------------------------------------------------------------  
  
  #require(readxl) # devtools::install_github("hadley/readxl") 
NC<-readxl::read_excel(path_to_raw_dataset, sheet = "Catches_Captures", col_names = TRUE, col_types = NULL,na = "")  
#NC <- read.csv(path_to_raw_dataset , header=TRUE, stringsAsFactors=FALSE, strip.white=TRUE)

colToKeep_NC<-c("FlCde","ArCde","Year/An","GrCde","SpCde","Catch/Capture(t)")
NC_harm_IOTC<-NC[,colToKeep_NC]
colnames(NC_harm_IOTC)<-c("FishingFleet", "AreaName","Year","Gear","Species","Catch")

NC_harm_IOTC$Catch<-as.numeric(NC_harm_IOTC$Catch)
NC_harm_IOTC$AreaCWPgrid<-NA
NC_harm_IOTC$School<-"ALL"
NC_harm_IOTC$CatchType<-"ALL"
NC_harm_IOTC$CatchUnits<-"MT"
NC_harm_IOTC$RFMO<-"IOTC"
NC_harm_IOTC$Ocean<-"IND"

NC_harm_IOTC$MonthStart<-1
NC_harm_IOTC$Period<-12
#Format inputDataset time to have the time format of the DB, which is one column time_start and one time_end
NC_harm_IOTC<-as.data.frame(NC_harm_IOTC)
NC_harm_IOTC<-format_time_db_format(NC_harm_IOTC)

#NC <- NC_harm_IOTC  %>% filter( ! Catch %in% 0 )
NC <- NC_harm_IOTC[NC_harm_IOTC$Catch != 0,]

rm(NC_harm_IOTC)

colToKeep_captures <- c("FishingFleet","Gear","time_start","time_end","AreaName","School","Species","CatchType","CatchUnits","Catch")
NC <-NC[colToKeep_captures]
# remove 0 and NA values 
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
NC$source_authority<-"IOTC"

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
# options(opts)
}