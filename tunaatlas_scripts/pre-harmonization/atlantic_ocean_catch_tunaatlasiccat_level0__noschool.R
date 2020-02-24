######################################################################
##### 52North WPS annotations ##########
######################################################################
# wps.des: id = atlantic_ocean_catch_tunaatlasiccat_level0__noschool, title = Harmonize data structure of ICCAT catch dataset, abstract = Harmonize the structure of ICCAT catch-and-effort datasets: (pid of output file = atlantic_ocean_catch_tunaatlasiccat_level0__noschool). The only mandatory field is the first one. The metadata must be filled-in only if the dataset will be loaded in the Tuna atlas database. ;
# wps.in: id = path_to_raw_dataset, type = String, title = Path to the input dataset to harmonize (Miscroft Access (.mdb)). The input database being voluminous, the execution of the function might take long time. Input file must be structured as follow: https://goo.gl/A6qVhb, value = "https://goo.gl/A6qVhb";
# wps.in: id = path_to_metadata_file, type = String, title = NULL or path to the csv of metadata. The template file can be found here: https://raw.githubusercontent.com/ptaconet/rtunaatlas_scripts/master/sardara_world/transform_trfmos_data_structure/metadata_source_datasets_to_database/metadata_source_datasets_to_database_template.csv . If NULL, no metadata will be outputted., value = "NULL";
# wps.in: id = keep_fleet_instead_of_flag, type = Boolean, title = By default the column "flag" is kept. By setting this argument to TRUE the column "fleet" will be kept (and "flag" will be removed), value = FALSE;
# wps.out: id = zip_namefile, type = text/zip, title = Dataset with structure harmonized + File of metadata (for integration within the Tuna Atlas database) + File of code lists (for integration within the Tuna Atlas database) ; 

#' @author Paul Taconet, IRD \email{paul.taconet@ird.fr}
#' 
#' @keywords Internal Commission for the Conservation of Atlantic Tuna tuna RFMO Sardara Global database on tuna fishieries
#'
#' @seealso \code{\link{convertDSD_iccat_ce_task2_ByOperationMode}} to convert ICCAT task 2 "by operation mode", \code{\link{convertDSD_iccat_nc}} to convert ICCAT nominal catch data structure


#----------------------------------------------------------------------------------------------------------------------------
#@geoflow --> with this script 2 objects are pre-loaded
#config --> the global config of the workflow
#entity --> the entity you are managing
#get data from geoflow current job dir
filename <- "t2ce_20171120web.mdb"
path_to_raw_dataset <- entity$getJobDataResource(config, filename)
config$logger.info(sprintf("Pre-harmonization of dataset '%s'", entity$identifiers[["id"]]))
#----------------------------------------------------------------------------------------------------------------------------


keep_fleet_instead_of_flag=FALSE

if(!require(rtunaatlas)){
  if(!require(devtools)){
    install.packages("devtools")
  }
  require(devtools)
  install_github("ptaconet/rtunaatlas")
  require(rtunaatlas)
}
if(!require(data.table)){
  install.packages("data.table")
  require(data.table)
}
if(!require(dplyr)){
  install.packages("dplyr")
  require(dplyr)
}

#----------------------------------------------------------------------------------------------------------------------------
#@geoflow - add DBI/odbx for Windows OS , keep using Hmisc for Linux OS
if(!require(Hmisc)){
  install.packages("Hmisc")
  require(Hmisc) # install mdb tools (http://svitsrv25.epfl.ch/R-doc/library/Hmisc/html/mdb.get.html)
}
if(!require(DBI)){
  install.packages("DBI")
  require(DBI)
}
if(!require(odbc)){
  install.packages("odbc")
  require(odbc)
}
#----------------------------------------------------------------------------------------------------------------------------

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
  # Flag Gear time_start   time_end AreaName School Species CatchType CatchUnits Catch
  #  ARG   LL 1960-01-01 1960-02-01  6320020    ALL     ALB         C       MTNO 107.1
  #  ARG   LL 1960-01-01 1960-02-01  6320020    ALL     SWO         C       MTNO  46.6
  #  ARG   LL 1960-01-01 1960-02-01  6330045    ALL     ALB         C       MTNO   7.1
  #  ARG   LL 1960-01-01 1960-02-01  6330045    ALL     BET         C       MTNO  27.6
  #  ARG   LL 1960-01-01 1960-02-01  6330045    ALL     SWO         C       MTNO   1.4
  #  ARG   LL 1960-01-01 1960-02-01  6330045    ALL     YFT         C       MTNO   0.4

##Catches

## download database
#working_directory_init=getwd()
#setwd(working_directory_init)
#cat("Downloading database...\n")
#cat("The harmonization of this dataset might take a long time\n")
#download.file(path_to_raw_dataset, paste0(working_directory_init,"/db.mdb"))


# Requires library(Hmisc)
# Open the tables directly from the access database  
#t2ce<-mdb.get(paste0(working_directory_init,"/db.mdb"),tables='t2ce',stringsAsFactors=FALSE,strip.white=TRUE)
#Flags<-mdb.get(paste0(working_directory_init,"/db.mdb"),tables='Flags',stringsAsFactors=FALSE,strip.white=TRUE)


#----------------------------------------------------------------------------------------------------------------------------
#@geoflow make the R code less plateform dependent
#rely on standard DBI / ODBC on Windows, and keep Hmisc for Linux OS
t2ce <- NULL
Flags <- NULL
OS <- Sys.info()[1]
if(OS == "Windows"){
	CON <- DBI::dbConnect(odbc::odbc(),
		Driver = "Microsoft Access Driver (*.mdb, *.accdb)",
		DBQ = path_to_raw_dataset
	)
	opts <- options()
	options(stringsAsFactors = FALSE)
	t2ce <- DBI::dbReadTable(CON, 't2ce')
	Flags <- DBI::dbReadTable(CON, 'Flags')
	options(opts)
	DBI::dbDisconnect(CON)
}else if(OS == "Linux"){
	t2ce <- mdb.get(path_to_raw_dataset,tables='t2ce',stringsAsFactors=FALSE,strip.white=TRUE)
	Flags <- mdb.get(path_to_raw_dataset,tables='Flags',stringsAsFactors=FALSE,strip.white=TRUE)	
}
#----------------------------------------------------------------------------------------------------------------------------




data_pivot_ICCAT<-left_join(t2ce,Flags,by="FleetID")  # equivalent to "select FlagCode,FlagID,t2ce.* from t2ce, Flags where t2ce.FleetID=Flags.FleetID"

ICCAT_CE_species_colnames<-setdiff(colnames(t2ce),c("StrataID","DSetID","FleetID","GearGrpCode","GearCode","FileTypeCode","YearC","TimePeriodID","SquareTypeCode","QuadID","Lat","Lon","Eff1","Eff1Type","Eff2","Eff2Type","DSetTypeID","CatchUnit"))

catches_pivot_ICCAT<-FUN_catches_ICCAT_CE(data_pivot_ICCAT,ICCAT_CE_species_colnames)

#School
catches_pivot_ICCAT$School<-"ALL"

#Flag
catches_pivot_ICCAT$Flag<-catches_pivot_ICCAT$FlagCode

#CatchUnits
catches_pivot_ICCAT$CatchUnits<-catches_pivot_ICCAT$CatchUnit

index.kg <- which( catches_pivot_ICCAT[,"CatchUnits"] == "kg" & catches_pivot_ICCAT[,"DSetTypeID"] == "-w" )
catches_pivot_ICCAT[index.kg,"CatchUnits"]<- "MT"

index.nr <- which( catches_pivot_ICCAT[,"CatchUnits"] == "nr"  & catches_pivot_ICCAT[,"DSetTypeID"] == "n-" )
catches_pivot_ICCAT[index.nr,"CatchUnits"]<- "NO"               

index.kgnr <- which( catches_pivot_ICCAT[,"CatchUnits"] == "kg" & catches_pivot_ICCAT[,"DSetTypeID"] == "nw" )
catches_pivot_ICCAT[index.kgnr,"CatchUnits"]<- "MTNO"

index.nrkg <- which( catches_pivot_ICCAT[,"CatchUnits"] == "nr"  & catches_pivot_ICCAT[,"DSetTypeID"] == "nw" )
catches_pivot_ICCAT[index.nrkg,"CatchUnits"]<- "NOMT"            


### Reach the catches harmonized DSD using a function in ICCAT_functions.R
  
## If we want in the output dataset the column 'FleetCode' instead of 'flag'
if(keep_fleet_instead_of_flag==TRUE){
	catches_pivot_ICCAT$Flag<-NULL
	names(catches_pivot_ICCAT)[names(catches_pivot_ICCAT) == 'FleetCode'] <- 'Flag'
}
  
colToKeep_captures <- c("Flag","Gear","time_start","time_end","AreaName","School","Species","CatchType","CatchUnits","Catch")
catches<-ICCAT_CE_catches_pivotDSD_to_harmonizedDSD(catches_pivot_ICCAT,colToKeep_captures)
colnames(catches)<-c("flag","gear","time_start","time_end","geographic_identifier","schooltype","species","catchtype","unit","value")
catches$source_authority<-"ICCAT"

#dataset<-catches
# remove incoherent cwp grid codes TO REMOVE AFTER!!!!
#dataset<-dataset %>% filter (!(geographic_identifier %in% c("1410045","6425013","6428016","6454045","7405050","7410045")))
catches <- catches[!catches$geographic_identifier %in% c("1410045","6425013","6428016","6454045","7405050","7410045"),]

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
catches$time_start <- as.Date(catches$time_start)
catches$time_end <- as.Date(catches$time_end)
#we enrich the entity with temporal coverage
dataset_temporal_extent <- paste(as.character(min(catches$time_start)), as.character(max(catches$time_end)), sep = "/")
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
	dataset = catches, 
	additional_metadata = NULL, #nothing here
	codelists = df_codelists #in case the entity was provided with a link to codelists
)
#@geoflow -> export as csv
output_name_dataset <- gsub(filename, paste0(unlist(strsplit(filename,".mdb"))[1], "_harmonized.csv"), path_to_raw_dataset)
write.csv(dataset$dataset, output_name_dataset, row.names = FALSE)
output_name_codelists <- gsub(filename, paste0(unlist(strsplit(filename,".mdb"))[1], "_codelists.csv"), path_to_raw_dataset)
write.csv(dataset$codelists, output_name_codelists, row.names = FALSE)
#----------------------------------------------------------------------------------------------------------------------------