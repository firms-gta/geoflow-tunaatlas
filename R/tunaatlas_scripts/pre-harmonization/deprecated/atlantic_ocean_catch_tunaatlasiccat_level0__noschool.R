######################################################################
##### 52North WPS annotations ##########
######################################################################
# wps.des: id = catch_iccat_level0__noschool, title = Harmonize data structure of ICCAT catch dataset, abstract = Harmonize the structure of ICCAT catch-and-effort datasets: (pid of output file = atlantic_ocean_catch_tunaatlasiccat_level0__noschool). The only mandatory field is the first one. The metadata must be filled-in only if the dataset will be loaded in the Tuna atlas database. ;
# wps.in: id = path_to_raw_dataset, type = String, title = Path to the input dataset to harmonize (Miscroft Access (.mdb)). The input database being voluminous, the execution of the function might take long time. Input file must be structured as follow: https://goo.gl/A6qVhb, value = "https://goo.gl/A6qVhb";
# wps.in: id = path_to_metadata_file, type = String, title = NULL or path to the csv of metadata. The template file can be found here: https://raw.githubusercontent.com/ptaconet/rtunaatlas_scripts/master/sardara_world/transform_trfmos_data_structure/metadata_source_datasets_to_database/metadata_source_datasets_to_database_template.csv . If NULL, no metadata will be outputted., value = "NULL";
# wps.in: id = keep_fleet_instead_of_flag, type = Boolean, title = By default the column "flag" is kept. By setting this argument to TRUE the column "fleet" will be kept (and "flag" will be removed), value = FALSE;
# wps.out: id = zip_namefile, type = text/zip, title = Dataset with structure harmonized + File of metadata (for integration within the Tuna Atlas database) + File of code lists (for integration within the Tuna Atlas database) ; 

#' @author Paul Taconet, IRD \email{paul.taconet@ird.fr}
#' 
#' @keywords Internal Commission for the Conservation of Atlantic Tuna tuna RFMO Sardara Global database on tuna fishieries
#'
#' @seealso \code{\link{convertDSD_iccat_ce_task2_ByOperationMode}} to convert ICCAT task 2 "by operation mode", \code{\link{convertDSD_iccat_nc}} to convert ICCAT nominal catch data structure

#--------------------------------------------------------------------------------------------------------------------------------------------------------------------
  # Input data sample: No sample. Miscrosoft Acces DB. However after the commands that read the input DB the sample is the following:
  # StrataID DSetID FleetID GearGrpCode GearCode FileTypeCode YearC TimePeriodID SquareTypeCode QuadID Lat Lon Eff1 Eff1Type Eff2 Eff2Type DSetTypeID CatchUnit ALB BET     BFT BUM
  #         1      1 021ES00          TP     TRAP       OF-REP  1950           17            1x1      4  36   5    4 NO.TRAPS   NA                  nw        kg   0   0 6725000   0
  #         2      1 021ES00          TP     TRAP       OF-REP  1950           17            1x1      4  36   5    4 NO.TRAPS   NA                  nw        nr   0   0   52928   0
  #         3      2 026YU00          PS       PS       OF-REP  1950           17            5x5      1  40  15   14 NO.BOATS   NA                  -w        kg   0   0  657000   0
  #         4      3 021ES00          TP     TRAP       OF-REP  1951           17            1x1      4  36   5    4 NO.TRAPS   NA                  nw        kg   0   0 3072000   0
  #         5      3 021ES00          TP     TRAP       OF-REP  1951           17            1x1      4  36   5    4 NO.TRAPS   NA                  nw        nr   0   0   28654   0
  #         6      4 026YU00          PS       PS       OF-REP  1951           17            5x5      1  40  15   14 NO.BOATS   NA                  -w        kg   0   0  531000   0
  # SAI SKJ SWO WHM YFT BLF BLT BON BOP BRS CER FRI KGM KGX LTA MAW SLT SSM WAH oSmt BIL BLM MLS SBF SPF oTun BSH POR SMA MAK oSks FleetCode       FleetName FlagID FlagCode
  #    0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0    0   0   0   0   0   0    0   0   0   0   0    0    EU.ESP       EU.España     21   EU.ESP
  #    0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0    0   0   0   0   0   0    0   0   0   0   0    0    EU.ESP       EU.España     21   EU.ESP
  #    0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0    0   0   0   0   0   0    0   0   0   0   0    0       YUG Yugoslavia Fed.     26      YUG
  #    0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0    0   0   0   0   0   0    0   0   0   0   0    0    EU.ESP       EU.España     21   EU.ESP
  #    0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0    0   0   0   0   0   0    0   0   0   0   0    0    EU.ESP       EU.España     21   EU.ESP
  #    0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0    0   0   0   0   0   0    0   0   0   0   0    0       YUG Yugoslavia Fed.     26      YUG
  # FlagName StatusCode
  #        EU.España         CP
  #        EU.España         CP
  #  Yugoslavia Fed.        NCO
  #        EU.España         CP
  #        EU.España         CP
  #  Yugoslavia Fed.        NCO
  
  
  # Catch: final data sample:
  # fishingfleet gear time_start   time_end geographic_identifier schooltype species catchtype catchunits value source_authority
  #  ARG   LL 1960-01-01 1960-02-01  6320020    ALL     ALB         C       MTNO 107.1	ICCAT
  #  ARG   LL 1960-01-01 1960-02-01  6320020    ALL     SWO         C       MTNO  46.6	ICCAT
  #  ARG   LL 1960-01-01 1960-02-01  6330045    ALL     ALB         C       MTNO   7.1	ICCAT
  #  ARG   LL 1960-01-01 1960-02-01  6330045    ALL     BET         C       MTNO  27.6	ICCAT
  #  ARG   LL 1960-01-01 1960-02-01  6330045    ALL     SWO         C       MTNO   1.4	ICCAT
  #  ARG   LL 1960-01-01 1960-02-01  6330045    ALL     YFT         C       MTNO   0.4	ICCAT
function(action, entity, config){
  
#packages

  
if(!require(readr)){
  install.packages("readr")
  require(readr)
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
filename1 <- entity$data$source[[1]] #data
filename2 <- entity$data$source[[2]] #structure
path_to_raw_dataset <- entity$getJobDataResource(config, filename1)
config$logger.info(sprintf("Pre-harmonization of dataset '%s'", entity$identifiers[["id"]]))
#----------------------------------------------------------------------------------------------------------------------------

keep_fleet_instead_of_flag=FALSE  
  
## Catches

t2ce <- as.data.frame(readr::read_csv(path_to_raw_dataset))

ICCAT_CE_species_colnames<-setdiff(colnames(t2ce),c("StrataID","DSetID","FleetID","GearGrpCode","GearCode","FileTypeCode","YearC","TimePeriodID","SquareTypeCode","QuadID","Lat","Lon","Eff1","Eff1Type","Eff2","Eff2Type","DSetTypeID","CatchUnit", "FleetCode", "FleetName", "FlagID", "FlagCode"))
source("https://raw.githubusercontent.com/firms-gta/geoflow-tunaatlas/master/R/sardara_functions/FUN_catches_ICCAT_CE.R")

config$logger.info(paste0("BEGIN  function   \n"))
catches_pivot_ICCAT<-FUN_catches_ICCAT_CE(RFMO_CE=t2ce,
                                          RFMO_CE_species_colnames=ICCAT_CE_species_colnames
                                          )

config$logger.info(paste0(" END function   \n"))

#School
catches_pivot_ICCAT$School<-"UNK"

#FishingFleet
catches_pivot_ICCAT$FishingFleet<-catches_pivot_ICCAT$FlagCode

#CatchUnits
catches_pivot_ICCAT$CatchUnits<-catches_pivot_ICCAT$CatchUnit

config$logger.info(paste0(" Change units  \n"))


index.kg <- which( catches_pivot_ICCAT[,"CatchUnits"] == "kg" & catches_pivot_ICCAT[,"DSetTypeID"] == ".w" )
catches_pivot_ICCAT[index.kg,"CatchUnits"]<- "t"

index.nr <- which( catches_pivot_ICCAT[,"CatchUnits"] == "nr"  & catches_pivot_ICCAT[,"DSetTypeID"] == "n." )
catches_pivot_ICCAT[index.nr,"CatchUnits"]<- "no"               


config$logger.info(paste0(" Change units  \n"))


index.kgnr <- which( catches_pivot_ICCAT[,"CatchUnits"] == "kg" & catches_pivot_ICCAT[,"DSetTypeID"] == "nw" )
catches_pivot_ICCAT[index.kgnr,"CatchUnits"]<- "MTNO"

index.nrkg <- which( catches_pivot_ICCAT[,"CatchUnits"] == "nr"  & catches_pivot_ICCAT[,"DSetTypeID"] == "nw" )
catches_pivot_ICCAT[index.nrkg,"CatchUnits"]<- "NOMT"            

if(any(catches_pivot_ICCAT$value == "NULL")) {
	catches_pivot_ICCAT[catches_pivot_ICCAT$value == "NULL",]$CatchUnits <- "t"
	catches_pivot_ICCAT[catches_pivot_ICCAT$value == "NULL",]$value <- 0
}
class(catches_pivot_ICCAT$value) = "numeric"
head(catches_pivot_ICCAT)
### Reach the catches harmonized DSD using a function in ICCAT_functions.R
  
## If we want in the output dataset the column 'FleetCode' instead of 'flag'
if(keep_fleet_instead_of_flag==TRUE){
	catches_pivot_ICCAT$FishingFleet<-NULL
	names(catches_pivot_ICCAT)[names(catches_pivot_ICCAT) == 'FleetCode'] <- 'FishingFleet'
}
  
colToKeep_captures <- c("FishingFleet","Gear","time_start","time_end","AreaName","School","Species","CatchType","CatchUnits","Catch")
  
source("https://raw.githubusercontent.com/firms-gta/geoflow-tunaatlas/master/R/sardara_functions/ICCAT_CE_catches_pivotDSD_to_harmonizedDSD.R")
catches<-ICCAT_CE_catches_pivotDSD_to_harmonizedDSD(catches_pivot_ICCAT=catches_pivot_ICCAT,
                                                    colToKeep_captures=colToKeep_captures)

colnames(catches)<-c("fishingfleet","gear","time_start","time_end","geographic_identifier","schooltype","species","catchtype","unit","value")
catches$source_authority<-"ICCAT"

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

readr::write_csv(catches, output_name_dataset)
output_name_codelists <- gsub(filename1, paste0(unlist(strsplit(filename1,".csv"))[1], "_codelists.csv"), path_to_raw_dataset)
file.rename(from = entity$getJobDataResource(config, filename2), to = output_name_codelists)
#----------------------------------------------------------------------------------------------------------------------------
entity$addResource("source", path_to_raw_dataset)
entity$addResource("harmonized", output_name_dataset)
entity$addResource("codelists", output_name_codelists)}
