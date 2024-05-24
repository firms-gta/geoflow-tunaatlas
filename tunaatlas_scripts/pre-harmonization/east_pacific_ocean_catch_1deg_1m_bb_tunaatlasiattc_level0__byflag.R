#' Harmonize data structure of IATTC LP (Pole-and-line) catch datasets
#'
#' This function harmonizes the structure of IATTC catch-and-effort datasets specifically for
#' LP (Pole-and-line) catches under the 'LPTunaFlag' designation. The function transforms raw
#' dataset inputs into a harmonized format suitable for integration into the Tuna Atlas database.
#' The function assumes specific initial data columns and outputs a structured dataset with
#' additional metadata and code lists as needed.
#'
#' @param action The action context from geoflow.
#' @param entity The entity context from geoflow.
#' @param config The configuration context from geoflow.
#'
#' @return This function does not return a value but outputs harmonized datasets and
#'         related files specified by the process for integration within the Tuna Atlas database.
#'
#' @importFrom dplyr %>% select mutate
#' @import reshape
#' @seealso \code{\link{FUN_catches_IATTC_CE_Flag_or_SetType}} to convert IATTC nominal catch data structure,
#'          \code{\link{IATTC_CE_catches_pivotDSD_to_harmonizedDSD}} to convert IATTC LLTunaBillfish and LLShark data structure,
#' @keywords IATTC, tuna, fisheries, data harmonization
#' @export
#' @author Paul Taconet, IRD \email{paul.taconet@ird.fr}
#' @author Bastien Grasset, IRD \email{bastien.grasset@ird.fr}

  # Input data sample:
  # Year Month Flag LatC1  LonC1 NumSets ALB BET BKJ BZX PBF   SKJ TUN  YFT
  # 1978     1  USA   3.5  -79.5       2   0   0   0   0   0  6.05   0 4.74
  # 1978     1  USA  20.5 -114.5       2   0   0   0   0   0  3.53   0 2.76
  # 1978     1  USA  23.5 -111.5       2   0   0   0   0   0 20.80   0 4.50
  # 1978     1  USA  23.5 -109.5       1   0   0   0   0   0  0.00   0 0.90
  # 1978     1  USA  24.5 -111.5       1   0   0   0   0   0  1.51   0 1.18
  # 1978     1  USA  25.5 -114.5       2   0   0   0   0   0  5.00   0 3.60
  
  # Catch: final data sample:
  # FishingFleet Gear time_start   time_end AreaName School Species CatchType CatchUnits Catch
  #  USA   LL 1992-07-01 1992-08-01  6425135    ALL     BSH       ALL         NO     4
  #  USA   LL 1993-04-01 1993-05-01  6425135    ALL     BSH       ALL         NO    75
  #  USA   LL 1993-04-01 1993-05-01  6430135    ALL     BSH       ALL         NO    15
  #  USA   LL 1993-05-01 1993-06-01  6425135    ALL     BSH       ALL         NO    24
  #  USA   LL 1994-03-01 1994-04-01  6425135    ALL     BSH       ALL         NO    14
  #  USA   LL 1994-03-01 1994-04-01  6430135    ALL     BSH       ALL         NO     4

function(action, entity, config){
  
#packages

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
filename1 <- entity$data$source[[1]] #data
# Historical name for the dataset at source  PublicLPTunaFlag.csv
filename2 <- entity$data$source[[2]] #structure
# Historical name for the dataset at source  iattc_catch_code_lists.csv
path_to_raw_dataset <- entity$getJobDataResource(config, filename1)
config$logger.info(sprintf("Pre-harmonization of dataset '%s'", entity$identifiers[["id"]]))
opts <- options()
options(encoding = "UTF-8")
#----------------------------------------------------------------------------------------------------------------------------  
  
##Catches

# Reach the catches pivot DSD using a function stored in IATTC_functions.R
source("https://raw.githubusercontent.com/firms-gta/geoflow-tunaatlas/master/sardara_functions/FUN_catches_IATTC_CE_Flag_or_SetType.R")
catches_pivot_IATTC <-FUN_catches_IATTC_CE_Flag_or_SetType(path_to_raw_dataset,"Flag","LP")
catches_pivot_IATTC$NumSets<-NULL

colToKeep_captures <- c("FishingFleet","Gear","time_start","time_end","AreaName","School","Species","CatchType","CatchUnits","Catch")
source("https://raw.githubusercontent.com/firms-gta/geoflow-tunaatlas/master/sardara_functions/IATTC_CE_catches_pivotDSD_to_harmonizedDSD.R")
catches<-IATTC_CE_catches_pivotDSD_to_harmonizedDSD(catches_pivot_IATTC,colToKeep_captures)

colnames(catches)<-c("fishing_fleet","gear_type","time_start","time_end","geographic_identifier","fishing_mode","species","measurement_type","measurement_unit","measurement_value")
catches$source_authority<-"IATTC"
catches$measurement_type <- "RC" # Retained catches
catches$measurement <- "catch"
  
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
entity$addResource("codelists", output_name_codelists)}
