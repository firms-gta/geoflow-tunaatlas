
if(!require(renv)){
    install.packages("renv")
    require(renv)
  }
renv::restore()
#
if(!require(remotes)){
  install.packages("remotes")
  require(remotes)
}




if(!require(tinytex)){
  install.packages("tinytex")
  require(tinytex)
}
if(!require(geoflow)){
  remotes::install_github("eblondel/geoflow")
  require(geoflow)}

if(!require(RSQLite)){
  install.packages("RSQLite")
  require(RSQLite)
}
if(!require(RPostgreSQL)){
  install.packages("RPostgreSQL")
  require(RPostgreSQL)
}
if(!require(RPostgres)){
  install.packages("RPostgres")
  require(RPostgres)
}

if(!require(googledrive)){
  install.packages("googledrive")
  require(googledrive)
}
if(!require(DBI)){
  install.packages("DBI")
  require(DBI)
}
if(!require(gsheet)){
  install.packages("gsheet")
  require(gsheet)
}
if(!require(readr)){
  install.packages("readr")
  require(readr)
}
if(!require(plotrix)){
  install.packages("plotrix")
  require(plotrix)
}
if(!require(rgeos)){
  install.packages("rgeos")
  require(rgeos)
}


if(!require(rpostgis)){
  install.packages("rpostgis")
  require(rpostgis)
}
if(!require(janitor)){
  install.packages("janitor")
  require(janitor)
}

if(!require(dotenv)){
  install.packages("dotenv")
  require(dotenv)
}

if(!require(data.table)){
  install.packages("data.table")
  require(data.table)
} #to be removed 

require(here)

default_file = ".env"

if(file.exists(here::here("geoserver_sdi_lab.env"))){
  default_file <- "geoserver_sdi_lab.env"
} # as it is the one used on Blue Cloud project, for personal use replace .env with your personal one

if(file.exists(here("geoserver_cines.env"))){
  default_file <- here("geoserver_cines.env")
} # as it is the one used on Blue Cloud project, for personal use replace .env with your personal one
require(here)

load_dot_env(file = here::here(default_file)) # to be replaced by the one used
# load_dot_env(file = "~/Documents/Tunaatlas_level1/catch_local.env")# source("https://raw.githubusercontent.com/firms-gta/geoflow-tunaatlas/master/cwp_grids.R")

# Create usefull datasets: 
# - Aggregation
# - Disaggregation
# - Continent shape

# As aggregation and disagreggation dataset are really long to create (around 1 to 2 days) they are stored in the googledrive and this is launched only if necessary, the code are here to keep track of the creation but these dataset should not change

executeWorkflow(here("tunaatlas_qa_dbmodel+codelists.json")) 
executeWorkflow(here("tunaatlas_qa_mappings.json"))

source("https://raw.githubusercontent.com/firms-gta/geoflow-tunaatlas/Developement/Analysis_markdown/Checking_raw_files_markdown/Summarising_invalid_data.R")


# Pre-harmonizing 

## nominal data

file.path <- executeWorkflow(here("Nominal_catch.json"))
config <- initWorkflow(here("Nominal_catch.json"))
con <- config$software$output$dbi
Summarising_invalid_data(file.path, connectionDB = con)



## georef catch

file.path <- executeWorkflow(here("All_raw_data_georef.json"))
source("https://raw.githubusercontent.com/firms-gta/geoflow-tunaatlas/Developement/Analysis_markdown/Checking_raw_files_markdown/Summarising_invalid_data.R")
config <- initWorkflow(here("All_raw_data_georef.json"))
con <- config$software$output$dbi
Summarising_invalid_data(file.path, connectionDB = con)

## georeferenced effort

file.path <- executeWorkflow(here("All_raw_data_georef_effort.json"))
source("https://raw.githubusercontent.com/firms-gta/geoflow-tunaatlas/Developement/Analysis_markdown/Checking_raw_files_markdown/Summarising_invalid_data.R")
config <- initWorkflow(here("All_raw_data_georef_effort.json"))
con <- config$software$output$dbi
Summarising_invalid_data(file.path, connectionDB = con)



# Create 4 datasets catch and effort

tunaatlas_qa_global_datasets_catch_path <- executeWorkflow(here::here("tunaatlas_qa_global_datasets_catch.json"))
source("https://raw.githubusercontent.com/firms-gta/geoflow-tunaatlas/Developement/Analysis_markdown/functions/Summarising_step.R")
config <- initWorkflow(here::here("tunaatlas_qa_global_datasets_catch.json"))
con <- config$software$output$dbi
Summarising_step(main_dir = tunaatlas_qa_global_datasets_catch_path, connectionDB = con, config  =config)

#netcdf creation
source("https://raw.githubusercontent.com/firms-gta/geoflow-tunaatlas/Developement/tunaatlas_actions/convert_to_netcdf.R")
entity_dirs <- list.dirs(file.path(tunaatlas_qa_global_datasets_catch_path, "entities"), full.names = TRUE, recursive = FALSE)

wd <- getwd()

for (entitynumber in 1:length(config$metadata$content$entities)){
  entity <- config$metadata$content$entities[[entitynumber]]
  dataset_pid <- entity$identifiers[["id"]]
  setwd(file.path(file.path,"entities", dataset_pid))
  action <- entity$data$actions[[1]]
  convert_to_netcdf(action, config, entity)
} #could also be in global action but keep in mind it is very long
setwd(wd)


all_files <- list.files(getwd(), pattern = "\\.nc$", full.names = TRUE, recursive = TRUE)
netcdf_file_to_enrich <- all_files[!grepl("nominal", all_files)]

# Some checking on created data

#check on georef not nominal etc.


# executeWorkflow(here("tunaatlas_qa_global_datasets_catch_new.json"))

# executeWorkflow("tunaatlas_qa_global_datasets_effort.json", dir = "jobs/tunaatlas_qa_global_datasets_effort")

dir.create("jobs/tunaatlas_qa_services")
executeWorkflow("tunaatlas_qa_services.json")
