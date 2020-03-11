load_all_dataset <- function(entity, config, options){
  
  source("/home/juldebar/Bureau/CODES/geoflow-tunaatlas/tunaatlas_actions/load_codelist.R")
  source("/home/juldebar/Bureau/CODES/geoflow-tunaatlas/tunaatlas_actions/load_dataset.R")
  source("/home/juldebar/Bureau/CODES/geoflow-tunaatlas/tunaatlas_actions/load_mapping.R")
  codelist_pid <- entity$identifiers[["id"]]
  
  if(!grepl(pattern = "mapping",x=codelist_pid) && !grepl(pattern = "level0", x=codelist_pid)){
    load_codelist(entity, config, options)
  }else if(grepl(pattern = "mapping",x=codelist_pid)){
    load_mapping(entity, config, options)
  } else if(grepl(pattern = "level0",x=codelist_pid)){
    load_dataset(entity, config, options)
  }
	
}