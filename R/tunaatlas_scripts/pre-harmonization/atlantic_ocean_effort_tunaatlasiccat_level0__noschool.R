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
config$logger.info(sprintf("Pre-harmonization of dataset '%s'", entity$identifiers[["id"]]))
#----------------------------------------------------------------------------------------------------------------------------

keep_fleet_instead_of_flag=FALSE  

t2ce <- as.data.frame(readr::read_csv(path_to_raw_dataset))
ICCAT_CE_species_colnames<-setdiff(colnames(t2ce),c("StrataID","DSetID",
                                                    "FleetID","GearGrpCode","GearCode","FileTypeCode","YearC","TimePeriodID","SquareTypeCode",
                                                    "QuadID","Lat","Lon","Eff1","Eff1Type","Eff2","Eff2Type","DSetTypeID","CatchUnit", "FleetCode", "FleetName", "FlagID", "FlagCode", 
                                                    "SchoolTypeCode", "FlagName", "StatusCode"))

# remove duplicated lines, as displayed in kilos and numbers 
eff_cols <- c("Eff1","Eff1Type","Eff2","Eff2Type")
species_cols <- ICCAT_CE_species_colnames

key_cols <- setdiff(
  colnames(t2ce),
  c("StrataID", "CatchUnit", eff_cols, species_cols)
)

unit_priority <- function(x) {
  dplyr::case_when(
    x == "kg" ~ 1L,
    x == "nr" ~ 2L,
    TRUE      ~ 3L
  )
}

t2ce <- t2ce %>%
  dplyr::mutate(.unit_rank = unit_priority(CatchUnit)) %>%
  dplyr::group_by(across(all_of(key_cols))) %>%
  dplyr::arrange(.unit_rank, .by_group = TRUE) %>%
  dplyr::slice(1) %>%
  dplyr::ungroup() %>%
  dplyr::select(-.unit_rank)

# remove data displayed annualy


config$logger.info(paste0("BEGIN  function   \n"))

# data_pivot_ICCAT<-left_join(t2ce,Flags,by="FleetID")  # equivalent to "select FlagCode,FlagID,t2ce.* from t2ce, Flags where t2ce.FleetID=Flags.FleetID"

## Efforts

## Reach the efforts pivot DSD using a function in ICCAT_functions.R

## If we want in the output dataset the column 'FleetCode' instead of 'flag'

# source("https://raw.githubusercontent.com/firms-gta/geoflow-tunaatlas/master/R/sardara_functions/FUN_efforts_ICCAT_CE_without_schooltype.R")
# efforts_pivot_ICCAT<-FUN_efforts_ICCAT_CE_without_schooltype(RFMO_CE = t2ce,ICCAT_CE_species_colnames)

efforts_pivot_ICCAT <- t2ce %>%
  dplyr::select(
    -dplyr::all_of(ICCAT_CE_species_colnames),
    -CatchUnit
  )

# source("https://raw.githubusercontent.com/firms-gta/geoflow-tunaatlas/master/R/sardara_functions/FUN_efforts_ICCAT_CE_keep_all_efforts.R")
# efforts_pivot_ICCAT<-FUN_efforts_ICCAT_CE_keep_all_efforts(efforts_pivot_ICCAT,c("Eff1","Eff2"),c("Eff1Type","Eff2Type"))

# Keep all columns except the raw effort/unit columns,
# because these will be rebuilt in a long format
cols_to_keep <- setdiff(
  names(efforts_pivot_ICCAT),
  c("Eff1", "Eff2", "Eff1Type", "Eff2Type")
)

# Add an identifier for each original row,
# so we can track which Eff1/Eff2 pairs come from the same source row
tmp <- efforts_pivot_ICCAT %>%
  dplyr::mutate(original_row_id = dplyr::row_number())

# Convert Eff1 and Eff2 to numeric safely
# (important because some effort values may use commas instead of dots)
tmp <- tmp %>%
  dplyr::mutate(
    Eff1_num = suppressWarnings(as.numeric(gsub(",", ".", Eff1))),
    Eff2_num = suppressWarnings(as.numeric(gsub(",", ".", Eff2)))
  )

# Build the Eff1 rows:
# these are always considered the primary effort rows
eff1_rows <- tmp %>%
  dplyr::transmute(
    dplyr::across(dplyr::all_of(cols_to_keep)),
    original_row_id,
    effort_source = "Eff1",
    Effort = Eff1_num,
    EffortUnits = Eff1Type,
    is_duplicate_strata = FALSE
  ) %>%
  # Remove empty or zero Eff1 values
  dplyr::filter(!is.na(Effort) & Effort != 0)

# Build the Eff2 rows:
# these may either replace missing Eff1, or become duplicated rows
eff2_rows <- tmp %>%
  dplyr::transmute(
    dplyr::across(dplyr::all_of(cols_to_keep)),
    original_row_id,
    Eff1_num,
    effort_source = "Eff2",
    Effort = Eff2_num,
    EffortUnits = Eff2Type
  ) %>%
  # Remove empty or zero Eff2 values
  dplyr::filter(!is.na(Effort) & Effort != 0) %>%
  # If Eff1 existed and was non-zero, then Eff2 is an additional duplicated row
  # If Eff1 was missing or zero, then Eff2 replaces Eff1 and is not a duplicate
  dplyr::mutate(
    is_duplicate_strata = !is.na(Eff1_num) & Eff1_num != 0
  ) %>%
  dplyr::select(-Eff1_num)%>% dplyr::mutate(EffortUnits = as.character(EffortUnits))

# Combine both sets of rows into one long dataset
efforts_pivot_ICCAT <- dplyr::bind_rows(eff1_rows, eff2_rows) %>%
  dplyr::select(
    original_row_id,
    effort_source,
    is_duplicate_strata,
    dplyr::everything()
  )

# School
efforts_pivot_ICCAT$School<-"OTH"

# Flag
efforts_pivot_ICCAT$FleetCode_short <- sub("-.*", "", efforts_pivot_ICCAT$FleetCode) # fleet code only what is after the '-'

names(efforts_pivot_ICCAT)[names(efforts_pivot_ICCAT) == 'FleetCode_short'] <- 'FishingFleet'
efforts_pivot_ICCAT <- efforts_pivot_ICCAT[, c("FishingFleet", setdiff(names(efforts_pivot_ICCAT), "FishingFleet"))] # put flag in first position

# Reach the efforts harmonized DSD using a function in ICCAT_functions.R
colToKeep_efforts <- c("FishingFleet","Gear","time_start","time_end","AreaName","School","EffortUnits","Effort")
source("https://raw.githubusercontent.com/firms-gta/geoflow-tunaatlas/master/R/sardara_functions/ICCAT_CE_effort_pivotDSD_to_harmonizedDSD.R")
efforts_pivot_ICCAT$Lat <- floor(abs(efforts_pivot_ICCAT$Lat)) # we put floor as independently of the quadrant the floor always correspond to the cwp
efforts_pivot_ICCAT$Lon <- floor(abs(efforts_pivot_ICCAT$Lon))

# handling duplicated stratas

efforts<-ICCAT_CE_effort_pivotDSD_to_harmonizedDSD(efforts_pivot_ICCAT,colToKeep_efforts)
efforts$CatchType <- "C" #bastien adding as it is not in effort function but it is in chatch function
colnames(efforts)<-c("fishing_fleet","gear_type","time_start","time_end","geographic_identifier","fishing_mode","measurement_unit","measurement_value","measurement_type")
efforts$source_authority<-"ICCAT"
efforts <- efforts %>% dplyr::mutate(fishing_mode = ifelse(fishing_mode == "UNK", "OTH", fishing_mode))

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
entity$addResource("codelists", output_name_codelists)}
