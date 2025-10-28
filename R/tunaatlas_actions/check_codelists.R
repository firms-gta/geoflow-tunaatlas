check_codelists <- function(action,entity, config){
	
  opts <- action$options
	if(!require(readr)){
	  install.packages("readr")
	  require(readr)
	}
	
	CON = config$software$input$dbi
	config$logger.info("Action to check codelists updateness from datasets")
	
	filename <- entity$data$source[[1]]
	attributes(filename) <- NULL
	filename <- unlist(strsplit(filename, "\\."))[1]
	#data
	dataset_name <- entity$getJobDataResource(config, paste0(filename,"_harmonized.csv"))
	dataset <- as.data.frame(readr::read_csv(dataset_name))
	
	#testing
	#dataset <- rbind(dataset, data.frame(flag = "Fake", gear = "NEW", time_start = "2009-08-01", time_end = "2009-08-31", geographic_identifier = "999999", schooltype="blabla", species = "TTH", catchtype = "C", unit = "MT", value = 999, source_authority = "ICCAT"))
	#dataset <- rbind(dataset, data.frame(flag = "Fake2", gear = "NEW2", time_start = "2009-08-01", time_end = "2009-08-31", geographic_identifier = "9999922229", schooltype="blabla2", species = "HER", catchtype = "CTT", unit = "MTTTTTT", value = 999, source_authority = "ICCAT"))
	
	#codelists
	codelists_name <- entity$getJobDataResource(config, paste0(filename,"_codelists.csv"))
	codelists <- as.data.frame(readr::read_csv(codelists_name, guess_max=0))
	codelists <- codelists[codelists$dimension != "source_authority",] #there is no codelist for the source_authority!!
	
	#we iterate on each codelist, and we inspect entity harmonized dataset to see if they are missing codelist entries
	out <- do.call("rbind", lapply(1:nrow(codelists), function(i){
		outcl_missing_entries <- NULL
		codelist <- codelists[i,]
		codelist_tablename <- sprintf("%s.%s", codelist$dimension, codelist$code_list_identifier)
		codelist_df <- DBI::dbGetQuery(CON, sprintf("select distinct code from %s", codelist_tablename)) 
		codelist_values <- codelist_df$code
		
		dataset_target_column <- ifelse(codelist$dimension == "area", "geographic_identifier", codelist$dimension)
		nocl_indexes <- which(!dataset[,dataset_target_column] %in% codelist_values)
		if(length(nocl_indexes)>0){
			outcl_missing_entries <- do.call("rbind", lapply(nocl_indexes, function(nocl_index){
				nocl_index_df <- data.frame(
					dimension = codelist$dimension,
					code_list_identifier = codelist$code_list_identifier,
					code_list_value = dataset[nocl_index, dataset_target_column],
					dataset_row_number = nocl_index,
					stringsAsFactors = FALSE
				)
				return(nocl_index_df)
			}))
		}
		return(outcl_missing_entries)
	}))
	
	#if we have at least one missing codelist item we write a file for check by the source_authority
	if(!is.null(out)){
		if(!dir.exists("errors_codelists")) dir.create("errors_codelists")
		setwd("errors_codelists")
		source_authority <- dataset[1,"source_authority"]
		if(!dir.exists(source_authority)) dir.create(source_authority)
		setwd(source_authority)
		write.csv(out, paste0(entity$identifiers[["id"]], "_MISSING_CODELIST_ITEMS.csv"), row.names = FALSE)
		setwd("../..")
	}
}