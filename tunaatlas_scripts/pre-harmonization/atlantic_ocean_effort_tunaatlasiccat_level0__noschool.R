#' Harmonize Data Structure of ICCAT Effort Dataset
#'
#' This function harmonizes the data structure of ICCAT effort datasets from voluminous Microsoft Access databases.
#' It handles datasets by potentially including metadata and code lists for integration within the Tuna Atlas database.
#' Depending on the parameter settings, either 'fleet' or 'flag' column will be kept in the output.
#'
#' @param action Specifies the action to be executed within the function.
#' @param entity Specifies the entity (dataset) being processed.
#' @param config Provides configuration specifics for processing.
#' @param keep_fleet_instead_of_flag Boolean parameter to decide whether to keep 'fleet' instead of 'flag'.
#' @return Writes a harmonized dataset, metadata file, and code lists to CSV files.
#' @export
#'
#' @author Paul Taconet, IRD \email{paul.taconet@ird.fr}
#' @author Bastien Grasset, IRD \email{bastien.grasset@ird.fr}
#'
#' @keywords International Commission for the Conservation of Atlantic Tunas (ICCAT), RFMO, Sardara Global Database
#'
#' @seealso \code{\link{convertDSD_iccat_ce_task2_ByOperationMode}}, \code{\link{convertDSD_iccat_nc}} for other ICCAT data structure conversions.
#'
#' @examples
#' # Assuming 'action', 'entity', 'config', and 'keep_fleet_instead_of_flag' are predefined and suitable for processing
#' harmonize_iccat_effort_dataset(action, entity, config, keep_fleet_instead_of_flag = FALSE)
#'
#'

function(action, entity, config){
  

keep_fleet_instead_of_flag=FALSE

#packages

if(!require(readr)){
  install.packages("readr")
  require(readr)
}
if(!require(dplyr)){
  install.packages("dplyr")
  require(dplyr)
}
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

# Effort: final data sample:
# Flag Gear time_start   time_end AreaName School EffortUnits Effort
#  ARG   LL 1967-02-01 1967-03-01  7330040    ALL    NO.HOOKS  13000
#  ARG   LL 1967-03-01 1967-04-01  7330040    ALL    NO.HOOKS  67000
#  ARG   LL 1967-04-01 1967-05-01  7330040    ALL    NO.HOOKS 107000
#  ARG   LL 1967-05-01 1967-06-01  7330040    ALL    NO.HOOKS  88000
#  ARG   LL 1967-06-01 1967-07-01  7330040    ALL    NO.HOOKS  66000
#  ARG   LL 1967-07-01 1967-08-01  7330040    ALL    NO.HOOKS  50000

## download database
filename1 <- entity$data$source[[1]] #data
# Historical name for the dataset at source  t2ce_noSchool.csv
filename2 <- entity$data$source[[2]] #structure
# Historical name for the dataset at source  iccat_effort_code_lists.csv
path_to_raw_dataset <- entity$getJobDataResource(config, filename1)
config$logger.info(sprintf("Pre-harmonization of dataset '%s'", entity$identifiers[["id"]]))
#----------------------------------------------------------------------------------------------------------------------------

keep_fleet_instead_of_flag=FALSE  

t2ce <- as.data.frame(readr::read_csv(path_to_raw_dataset))
ICCAT_CE_species_colnames<-setdiff(colnames(t2ce),c("StrataID","DSetID","FleetID","GearGrpCode","GearCode","FileTypeCode","YearC","TimePeriodID","SquareTypeCode","QuadID","Lat","Lon","Eff1","Eff1Type","Eff2","Eff2Type","DSetTypeID","CatchUnit", "FleetCode", "FleetName", "FlagID", "FlagCode"))

# Flags<-mdb.get(paste0(working_directory_init,"/db.mdb"),tables='Flags',stringsAsFactors=FALSE,strip.white=TRUE)
config$logger.info(paste0("BEGIN  function   \n"))

# data_pivot_ICCAT<-left_join(t2ce,Flags,by="FleetID")  # equivalent to "select FlagCode,FlagID,t2ce.* from t2ce, Flags where t2ce.FleetID=Flags.FleetID"

## Efforts

## Reach the efforts pivot DSD using a function in ICCAT_functions.R

## If we want in the output dataset the column 'FleetCode' instead of 'flag'

source("https://raw.githubusercontent.com/firms-gta/geoflow-tunaatlas/master/sardara_functions/FUN_efforts_ICCAT_CE_without_schooltype.R")
efforts_pivot_ICCAT<-FUN_efforts_ICCAT_CE_without_schooltype(RFMO_CE = t2ce,ICCAT_CE_species_colnames)

source("https://raw.githubusercontent.com/firms-gta/geoflow-tunaatlas/master/sardara_functions/FUN_efforts_ICCAT_CE_keep_all_efforts.R")
efforts_pivot_ICCAT<-FUN_efforts_ICCAT_CE_keep_all_efforts(efforts_pivot_ICCAT,c("Eff1","Eff2"),c("Eff1Type","Eff2Type"))

# School
efforts_pivot_ICCAT$School<-"ALL"

# Flag
efforts_pivot_ICCAT$Flag<-efforts_pivot_ICCAT$FlagCode

names(efforts_pivot_ICCAT)[names(efforts_pivot_ICCAT) == 'FleetCode'] <- 'FishingFleet'
# Reach the efforts harmonized DSD using a function in ICCAT_functions.R
colToKeep_efforts <- c("FishingFleet","Gear","time_start","time_end","AreaName","School","EffortUnits","Effort")
source("https://raw.githubusercontent.com/firms-gta/geoflow-tunaatlas/master/sardara_functions/ICCAT_CE_effort_pivotDSD_to_harmonizedDSD.R")
efforts<-ICCAT_CE_effort_pivotDSD_to_harmonizedDSD(efforts_pivot_ICCAT,colToKeep_efforts)
efforts$CatchType <- "C" #bastien adding as it is not in effort function but it is in chatch function
colnames(efforts)<-c("fishing_fleet","gear_type","time_start","time_end","geographic_identifier","fishing_mode","measurement_unit","measurement_value","measurement_type")
efforts$source_authority<-"ICCAT"

#----------------------------------------------------------------------------------------------------------------------------
efforts$time_start <- as.Date(efforts$time_start)
efforts$time_end <- as.Date(efforts$time_end)
#we enrich the entity with temporal coverage
dataset_temporal_extent <- paste(
  paste0(format(min(efforts$time_start), "%Y"), "-01-01"),
  paste0(format(max(efforts$time_end), "%Y"), "-12-31"),
  sep = "/"
)

entity$setTemporalExtent(dataset_temporal_extent)

output_name_dataset <- gsub(filename1, paste0(unlist(strsplit(filename1,".csv"))[1], "_harmonized.csv"), path_to_raw_dataset)
write.csv(efforts, output_name_dataset, row.names = FALSE)
output_name_codelists <- gsub(filename1, paste0(unlist(strsplit(filename1,".csv"))[1], "_codelists.csv"), path_to_raw_dataset)
file.rename(from = entity$getJobDataResource(config, filename2), to = output_name_codelists)
#----------------------------------------------------------------------------------------------------------------------------
entity$addResource("source", path_to_raw_dataset)
entity$addResource("harmonized", output_name_dataset)
entity$addResource("codelists", output_name_codelists)}
