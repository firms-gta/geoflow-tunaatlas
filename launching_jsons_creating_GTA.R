# 
# getwd()
setwd("~/Documents/Tunaatlas_level1")
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
if(!require(data.table)){
  install.packages("data.table")
  require(data.table)
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
  install_github("rpostgis")
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
load_dot_env(file = "catch_server.env")

executeWorkflow("tunaatlas_qa_dbmodel+codelists.json")#works
executeWorkflow("tunaatlas_qa_mappings.json")


executeWorkflow("tunaatlas_qa_datasets_iccat.json") # ok
executeWorkflow("tunaatlas_qa_datasets_ccsbt.json") # ok
executeWorkflow("tunaatlas_qa_datasets_wcpfc.json") # ok 
executeWorkflow("tunaatlas_qa_datasets_iattc.json") # ok
executeWorkflow("tunaatlas_qa_datasets_iotc.json") # ok
executeWorkflow("tunaatlas_qa_global_datasets_catch.json")


load_dot_env(file = "effort_server.env")



executeWorkflow("tunaatlas_qa_datasets_ccsbt_effort.json") #ok
executeWorkflow("tunaatlas_qa_datasets_wcpfc_effort.json") #ok
executeWorkflow("tunaatlas_qa_datasets_iattc_effort.json") # ok
executeWorkflow("tunaatlas_qa_datasets_iotc_effort.json") # ok
executeWorkflow("tunaatlas_qa_datasets_iccat_effort.json") # ok

executeWorkflow("tunaatlas_qa_global_datasets_effort.json")
