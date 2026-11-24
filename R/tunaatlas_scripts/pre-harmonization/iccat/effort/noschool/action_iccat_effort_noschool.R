#' Harmonize Data Structure of ICCAT Effort Dataset (geoflow action)
#'
#' geoflow action of the entity: finds its files, runs the core function gta_rawdata_iccat_effort_noschool()
#' (R/rawdata_iccat_effort_noschool.R), writes the outputs and declares the resources of
#' the entity. The processing itself is in the core function, usable without geoflow.
#'
#' @param action The action context from geoflow.
#' @param entity The entity context from geoflow.
#' @param config The configuration context from geoflow.
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
#  ARG   LL 1967-02-01 1967-03-01  7330040    OTH    NO.HOOKS  13000
#  ARG   LL 1967-03-01 1967-04-01  7330040    OTH    NO.HOOKS  67000
#  ARG   LL 1967-04-01 1967-05-01  7330040    OTH    NO.HOOKS 107000
#  ARG   LL 1967-05-01 1967-06-01  7330040    OTH    NO.HOOKS  88000
#  ARG   LL 1967-06-01 1967-07-01  7330040    OTH    NO.HOOKS  66000
#  ARG   LL 1967-07-01 1967-08-01  7330040    OTH    NO.HOOKS  50000

## download database
# to get .mdb ./R/tunaatlas_scripts/pre-harmonization/data_iccat_from_mdb.R
filename1 <- entity$data$source[[1]] #data
# Historical name for the dataset at source  t2ce_noSchool.csv
filename2 <- entity$data$source[[2]] #structure
# Historical name for the dataset at source  iccat_effort_code_lists.csv
path_to_raw_dataset <- entity$getJobDataResource(config, filename1)

#----------------------------------------------------------------------------------------------------------------------------



efforts <- gta_rawdata_iccat_effort_noschool(path_to_raw_dataset)

dataset_temporal_extent <- paste(
  paste0(format(min(efforts$time_start), "%Y"), "-01-01"),
  paste0(format(max(efforts$time_end), "%Y"), "-12-31"),
  sep = "/"
)

entity$setTemporalExtent(dataset_temporal_extent)

base1 <- tools::file_path_sans_ext(basename(filename1))
#@geoflow -> export as csv
# output in same folder as path_to_raw_dataset 
output_name_dataset   <- file.path(dirname(path_to_raw_dataset), paste0(base1, "_harmonized.csv"))
output_name_codelists <- file.path(dirname(path_to_raw_dataset), paste0(base1, "_codelists.csv"))

write.csv(efforts, output_name_dataset, row.names = FALSE)

file.rename(  from = entity$getJobDataResource(config, filename2),  to   = output_name_codelists)
#----------------------------------------------------------------------------------------------------------------------------
entity$addResource("source", path_to_raw_dataset)
entity$addResource("harmonized", output_name_dataset)
entity$addResource("codelists", output_name_codelists)
}
