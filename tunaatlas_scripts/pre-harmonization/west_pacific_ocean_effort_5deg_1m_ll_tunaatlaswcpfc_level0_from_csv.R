#' Harmonize Data Structure of WCPFC Longline Effort Datasets
#'
#' This function harmonizes the data structure of WCPFC Longline effort datasets from CSV files provided by WCPFC. 
#' It includes handling datasets such as 'longline_60' 'longline_70' 'longline_80' 'longline_90' 'longline_00'.
#' The function also manages optional metadata and code lists for integration within the Tuna Atlas database.
#'
#' @param action Specifies the action to be executed within the function.
#' @param entity Specifies the entity (dataset) being processed.
#' @param config Provides configuration specifics for processing.
#' @return Creates a CSV file with harmonized data structure along with metadata and code lists files.
#' @export
#'
#' @author Paul Taconet, IRD \email{paul.taconet@ird.fr}, Bastien Grasset, IRD \email{bastien.grassset@ird.fr}
#' @author Bastien Grasset, IRD \email{bastien.grasset@ird.fr}
#'
#' @seealso \code{\link{convertDSD_wcpfc_ce_Driftnet}}, \code{\link{convertDSD_wcpfc_ce_Longline}},
#' \code{\link{convertDSD_wcpfc_ce_Pole_and_line}}, \code{\link{convertDSD_wcpfc_ce_PurseSeine}},
#' \code{\link{convertDSD_wcpfc_nc}} for other specific conversions within WCPFC datasets.
#'
#' @examples
#' # Assuming 'action', 'entity', and 'config' are predefined and suitable for processing
#' harmonize_wcpfc_datasets(action, entity, config)
#'

function(action, entity, config){
  source("https://raw.githubusercontent.com/firms-gta/geoflow-tunaatlas/master/sardara_functions/harmo_time_2.R")
  source("https://raw.githubusercontent.com/firms-gta/geoflow-tunaatlas/master/sardara_functions/harmo_spatial_3.R")
  
  

  
  if(!require(readr)){
    install.packages("readr")
  }
  require(readr)
  
  if(!require(dplyr)){
    install.packages("dplyr")
  }
  require(dplyr)
  
  
  
  # Input data sample:
  # YY MM LAT5 LON5   HHOOKS ALB_C ALB_N   YFT_C YFT_N   BET_C BET_N MLS_C MLS_N  BLM_C BLM_N  BUM_C BUM_N  SWO_C SWO_N OTH_C OTH_N
  # 2000  1  00N 120E 12391.11 0.000     0 267.338 10056  58.850  1537 0.627    15 11.391   249 18.203   314  9.998   189 0.120     4
  # 2000  1  00N 125E 16349.59 0.000     0 352.417 13256  77.975  2036 0.827    19 15.030   329 24.018   414 13.192   249 0.158     5
  # 2000  1  00N 130E  7091.08 0.000     0 130.454  4630  37.695   903 0.200     5  3.870    83  6.418   109  4.714    93 0.038     1
  # 2000  1  00N 135E  6113.85 1.276    73  75.469  2431 115.868  2575 0.037     1  0.058     1  6.948    90  2.719    38 0.245     4
  # 2000  1  00N 140E  9904.92 1.350    77 176.963  6266 251.303  6084 0.462    11  1.527    38 12.150   187  4.200    52 0.296     9
  # 2000  1  00N 145E  8679.03 0.428    24 122.945  4613 144.910  3579 0.537    12 11.062   237  8.748   137  6.326   110 0.000     0
  
  # Effort: final data sample:
  # Flag Gear time_start   time_end AreaName School EffortUnits  Effort
  #  ALL    L 2000-01-01 2000-02-01  6100120    ALL      HHOOKS 1239111
  #  ALL    L 2000-01-01 2000-02-01  6100125    ALL      HHOOKS 1634959
  #  ALL    L 2000-01-01 2000-02-01  6100130    ALL      HHOOKS  709108
  #  ALL    L 2000-01-01 2000-02-01  6100135    ALL      HHOOKS  611385
  #  ALL    L 2000-01-01 2000-02-01  6100140    ALL      HHOOKS  990492
  #  ALL    L 2000-01-01 2000-02-01  6100145    ALL      HHOOKS  867903
  
  #----------------------------------------------------------------------------------------------------------------------------
  #@geoflow --> with this script 2 objects are pre-loaded
  #config --> the global config of the workflow
  #entity --> the entity you are managing
  #get data from geoflow current job dir
  filename1 <- entity$data$source[[1]] #data
# Historical name for the dataset at source  WCPFC_L_PUBLIC_BY_FLAG_MON.csv
  filename2 <- entity$data$source[[2]] #structure
# Historical name for the dataset at source  wcpfc_effort_code_lists.csv
  path_to_raw_dataset <- entity$getJobDataResource(config, filename1)
  config$logger.info(sprintf("Pre-harmonization of dataset '%s'", entity$identifiers[["id"]]))
  opts <- options()
  options(encoding = "UTF-8")
  #----------------------------------------------------------------------------------------------------------------------------
  
  
  ##Efforts
  DF <- read.table(path_to_raw_dataset, sep=",", header=TRUE, stringsAsFactors=FALSE,strip.white=TRUE)
  
  # Reach the efforts pivot DSD using a function in WCPFC_functions.R
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
  
  
  efforts_pivot_WCPFC=DF
  efforts_pivot_WCPFC$Gear<-"L"
  
  # Catchunits
  # Check data that exist both in number and weight
  
  number_of_units_by_strata<- dplyr::summarise(group_by_(efforts_pivot_WCPFC,.dots=setdiff(colnames(efforts_pivot_WCPFC),c("value","CatchUnits"))), count = n())
  
  strata_in_number_and_weight<-number_of_units_by_strata[number_of_units_by_strata$count>1,]
  
  efforts_pivot_WCPFC<-left_join (efforts_pivot_WCPFC,strata_in_number_and_weight,by=setdiff(colnames(strata_in_number_and_weight),"count"))
  
  index.catchinweightandnumber <- which(efforts_pivot_WCPFC[,"count"]==2 & efforts_pivot_WCPFC[,"CatchUnits"]=="N")
  efforts_pivot_WCPFC[index.catchinweightandnumber,"CatchUnits"]="NOMT"
  
  index.catchinweightandnumber <- which(efforts_pivot_WCPFC[,"count"]==2 & efforts_pivot_WCPFC[,"CatchUnits"]=="C")
  efforts_pivot_WCPFC[index.catchinweightandnumber,"CatchUnits"]="MTNO"
  
  index.catchinweightonly <- which(efforts_pivot_WCPFC[,"CatchUnits"]=="C")
  efforts_pivot_WCPFC[index.catchinweightonly,"CatchUnits"]="t"
  
  index.catchinnumberonly <- which(efforts_pivot_WCPFC[,"CatchUnits"]=="N")
  efforts_pivot_WCPFC[index.catchinnumberonly,"CatchUnits"]="no"
  
  # School
  efforts_pivot_WCPFC$School<-"ALL"
  
  ### Reach the efforts harmonized DSD using a function in WCPFC_functions.R
  colToKeep_efforts <- c("FishingFleet","Gear","time_start","time_end","AreaName","School","EffortUnits","Effort")
  #efforts<-WCPFC_CE_efforts_pivotDSD_to_harmonizedDSD(efforts_pivot_WCPFC,colToKeep_captures)
  #2020-11-13 @eblondel
  efforts_pivot_WCPFC$RFMO <- "WCPFC"
  efforts_pivot_WCPFC$Ocean <- "PAC_W"
  efforts_pivot_WCPFC$FishingFleet <- efforts_pivot_WCPFC$FLAG_ID #@eblondel added
  efforts_pivot_WCPFC <- harmo_time_2(efforts_pivot_WCPFC, "YY", "MM")
  efforts_pivot_WCPFC <- harmo_spatial_3(efforts_pivot_WCPFC, "LAT_SHORT", "LON_SHORT", 5, 6) #@eblondel change column names LAT5 -> LAT_SHORT, LON5 -> LON_SHORT
  efforts_pivot_WCPFC$CatchType <- "ALL"
  
  efforts_pivot_WCPFC$Effort <- efforts_pivot_WCPFC$value
  efforts <- efforts_pivot_WCPFC[colToKeep_efforts]
  rm(efforts_pivot_WCPFC)
  efforts[, c("AreaName", "FishingFleet")] <- as.data.frame(apply(efforts[, 
                                                                          c("AreaName", "FishingFleet")], 2, function(x) {
                                                                            gsub(" *$", "", x)
                                                                          }), stringsAsFactors = FALSE)
  efforts <- efforts %>% filter(!Effort %in% 0) %>% filter(!is.na(Effort))
  efforts <- as.data.frame(efforts)
  efforts <- aggregate(efforts$Effort,
                       by = list(
                         FishingFleet = efforts$FishingFleet,
                         Gear = efforts$Gear,
                         time_start = efforts$time_start,
                         time_end = efforts$time_end,
                         AreaName = efforts$AreaName,
                         School = efforts$School,
                         # Species = efforts$Species,
                         # CatchType = efforts$CatchType,
                         EffortUnits = efforts$EffortUnits
                       ),
                       FUN = sum)
  colnames(efforts)[colnames(efforts)=="x"] <- "Effort"
  
  colnames(efforts)<-c("fishing_fleet","gear_type","time_start","time_end","geographic_identifier","fishing_mode","measurement_unit","measurement_value")
  efforts$source_authority<-"WCPFC"
  efforts$measurement <- "effort" 
  #----------------------------------------------------------------------------------------------------------------------------
  #@eblondel additional formatting for next time support
  efforts$time_start <- as.Date(efforts$time_start)
  efforts$time_end <- as.Date(efforts$time_end)
  #we enrich the entity with temporal coverage
  dataset_temporal_extent <- paste(
    paste0(format(min(efforts$time_start), "%Y"), "-01-01"),
    paste0(format(max(efforts$time_end), "%Y"), "-12-31"),
    sep = "/"
  )
  entity$setTemporalExtent(dataset_temporal_extent)
  
  #@geoflow -> export as csv
  output_name_dataset <- gsub(filename1, paste0(unlist(strsplit(filename1,".csv"))[1], "_harmonized.csv"), path_to_raw_dataset)
  write.csv(efforts, output_name_dataset, row.names = FALSE)
  output_name_codelists <- gsub(filename1, paste0(unlist(strsplit(filename1,".csv"))[1], "_codelists.csv"), path_to_raw_dataset)
  file.rename(from = entity$getJobDataResource(config, filename2), to = output_name_codelists)
  #----------------------------------------------------------------------------------------------------------------------------
  entity$addResource("source", path_to_raw_dataset)
  entity$addResource("harmonized", output_name_dataset)
  entity$addResource("codelists", output_name_codelists)
}
