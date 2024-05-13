#' Harmonize IATTC LP Pole-and-line Effort Datasets
#'
#' This function processes and harmonizes Inter-American Tropical Tuna Commission (IATTC) LP (Pole-and-line) effort datasets.
#' It prepares the data for integration into the Tuna Atlas database by ensuring compliance with
#' data standardization requirements and optionally includes metadata if the dataset is intended for database loading.
#'
#' @param action The action context from geoflow, used for controlling workflow processes.
#' @param entity The entity context from geoflow, which manages dataset-specific details.
#' @param config The configuration context from geoflow, used for managing global settings.
#'
#' @return None; the function outputs files directly, including harmonized datasets,
#'         optional metadata, and code lists for integration within the Tuna Atlas database.
#'
#' @details This function modifies the dataset to include only essential fields, performs any necessary calculations
#'          for effort units, and standardizes the format for date fields and geographical identifiers.
#'          Metadata integration is contingent on the final use of the dataset within the Tuna Atlas database.
#'
#' @importFrom dplyr filter mutate
#' @importFrom readr read_csv write_csv
#' @seealso \code{\link{FUN_efforts_IATTC_CE_allbutLLTunaBillfish}} for initial effort data processing,
#'          \code{\link{IATTC_CE_efforts_pivotDSD_to_harmonizedDSD}} for converting effort data to a standardized structure.
#' @export
#' @keywords IATTC, tuna, fisheries, data harmonization, effort data
#' @author Paul Taconet, IRD \email{paul.taconet@ird.fr}
#' @author Bastien Grasset, IRD \email{bastien.grasset@ird.fr}
function(action, entity, config){
  

if(!require(reshape)){
  install.packages("reshape")
}
if(!require(dplyr)){
  install.packages("dplyr")
}

# Input data sample:
# Year Month Flag LatC1  LonC1 NumSets ALB BET BKJ BZX PBF   SKJ TUN  YFT
# 1978     1  USA   3.5  -79.5       2   0   0   0   0   0  6.05   0 4.74
# 1978     1  USA  20.5 -114.5       2   0   0   0   0   0  3.53   0 2.76
# 1978     1  USA  23.5 -111.5       2   0   0   0   0   0 20.80   0 4.50
# 1978     1  USA  23.5 -109.5       1   0   0   0   0   0  0.00   0 0.90
# 1978     1  USA  24.5 -111.5       1   0   0   0   0   0  1.51   0 1.18
# 1978     1  USA  25.5 -114.5       2   0   0   0   0   0  5.00   0 3.60


# Effort: final data sample:
# Flag Gear time_start   time_end AreaName School EffortUnits Effort
#  MEX   LP 1978-03-01 1978-04-01  5419112    ALL     NumSets      3
#  MEX   LP 1978-03-01 1978-04-01  5421111    ALL     NumSets      1
#  MEX   LP 1978-04-01 1978-05-01  5419107    ALL     NumSets      2
#  MEX   LP 1978-04-01 1978-05-01  5419112    ALL     NumSets      3
#  MEX   LP 1978-04-01 1978-05-01  5420105    ALL     NumSets      1
#  MEX   LP 1978-04-01 1978-05-01  5420106    ALL     NumSets      4

  filename1 <- entity$data$source[[1]] #data
  filename2 <- entity$data$source[[2]] #structure
  path_to_raw_dataset <- entity$getJobDataResource(config, filename1)
  config$logger.info(sprintf("Pre-harmonization of dataset '%s'", entity$identifiers[["id"]]))
  opts <- options()
  options(encoding = "UTF-8")

##Efforts

# Reach the efforts pivot DSD using a function in IATTC_functions.R
  source("https://raw.githubusercontent.com/firms-gta/geoflow-tunaatlas/master/sardara_functions/FUN_efforts_IATTC_CE_allbutLLTunaBillfish.R")
  efforts_pivot_IATTC <-FUN_efforts_IATTC_CE_allbutLLTunaBillfish(path_to_raw_dataset,"NumSets","Flag","LP")

colToKeep_efforts <- c("FishingFleet","Gear","time_start","time_end","AreaName","School","EffortUnits","Effort")
source("https://raw.githubusercontent.com/firms-gta/geoflow-tunaatlas/master/sardara_functions/IATTC_CE_efforts_pivotDSD_to_harmonizedDSD.R")
efforts<-IATTC_CE_efforts_pivotDSD_to_harmonizedDSD(efforts_pivot_IATTC,colToKeep_efforts)

colnames(efforts)<-c("fishing_fleet","gear_type","time_start","time_end","geographic_identifier","fishing_mode","measurement_unit","measurement_value")
efforts$source_authority<-"IATTC"
efforts$measurement <- "effort"
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
