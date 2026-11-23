#' Convert the units of a dataset (geoflow side)
#'
#' Reads the conversion factors (data frame, file, or Google Drive link), runs
#' the core function gta_unit_conversion() (R/core/unit_conversion.R) and, when
#' a geoflow entity is given, adds the description of the conversion to its
#' metadata. The database connection is `con`, or the one of `config`.
#'
do_unit_conversion  <- function(entity, config,fact,unit_conversion_csv_conversion_factor_url,unit_conversion_codelist_geoidentifiers_conversion_factors,mapping_map_code_lists = FALSE, georef_dataset, removing_numberfish_final = TRUE, converting_dataset_mapped  = TRUE, con = NULL) {
  # Database connection: given, or the one of the geoflow configuration
  if (is.null(con)) con <- config$software$output$dbi
  
  if(is.data.frame(unit_conversion_csv_conversion_factor_url)){
    
    df_conversion_factor <- unit_conversion_csv_conversion_factor_url
  } else {
  
  log_info("Reading the conversion factors dataset")
  googledrive_baseurl <- "https://drive.google.com/open?id="
  if(startsWith(unit_conversion_csv_conversion_factor_url, googledrive_baseurl)){
    #managing download through google drive
    log_info("Downloading file using Google Drive R interface")
    drive_id <- unlist(strsplit(unit_conversion_csv_conversion_factor_url, "id="))[2]
    drive_id <- unlist(strsplit(drive_id, "&export"))[1] #control in case export param is appended
    factors_file <- file.path("data", paste0(if (is.null(entity)) "unit" else entity$identifiers[["id"]], "_conversion_factors.csv"))
    googledrive::drive_download(file = googledrive::as_id(drive_id), path = factors_file, overwrite = TRUE)
    df_conversion_factor <- as.data.frame(readr::read_csv(factors_file,guess_max=0))
  }else{
    df_conversion_factor <- as.data.frame(readr::read_csv(unit_conversion_csv_conversion_factor_url, guess_max=0))
  }
  }
  ## If we have not mapped the code lists (i.e. if mapping_map_code_lists==FALSE), we need to map the source gear coding system with ISSCFG coding system. In fact, the conversion factors dataset is expressed with ISSCFG coding system for gears, while the primary tRFMOs datasets are expressed with their own gear coding system.
  log_info("Checking if 'mapping_map_code_lists' option is set to TRUE")
  
  # Conversion: core function gta_unit_conversion() (R/core/unit_conversion.R)
  source(here::here("R/core/unit_conversion.R"))
  georef_dataset <- gta_unit_conversion(
    georef_dataset, df_conversion_factor, con = con, fact = fact,
    unit_conversion_codelist_geoidentifiers_conversion_factors = unit_conversion_codelist_geoidentifiers_conversion_factors,
    mapping_map_code_lists = mapping_map_code_lists,
    removing_numberfish_final = removing_numberfish_final)

  log_info("Fill metadata elements accordingly")
  
  lineage <- ""
  description <- ""
  info <- NULL
  if(!is.data.frame(unit_conversion_csv_conversion_factor_url)){
  if (unit_conversion_csv_conversion_factor_url=="https://drive.google.com/open?id=1csQ5Ww8QRTaYd1DG8chwuw0UVUOGkjNL"){
    lineage <- paste0("The units used to express catches may vary between tRFMOs datasets. Catches are expressed in weight, or in number of fishes, or in both weights and numbers in the same stratum. Values expressed in weight were kept and numbers were converted into weight using simple conversion matrices (A. Fonteneau, pers. com). These conversion factors depend on the species, the gear, the year and the main geographical area (equatorial or tropical). They were computed from the Japanese and Taiwanese size-frequency data as well as from the Japanese total catches and catch-and-effort data. The factors of conversion are available here: ",unit_conversion_csv_conversion_factor_url," and the methodology to compute these factors is available here: http://data.d4science.org/ZWFMa3JJUHBXWk9NTXVPdFZhbU5BUFEyQnhUeWd1d3lHbWJQNStIS0N6Yz0. Some data might not be converted at all because no conversion factor exists for the stratum: these data were kept in number. Information regarding the conversions of catch units for this dataset: ratio_converted_number % of the the catches that were originally expressed in number have been converted into weight through the conversion factors.")
    description <- "- Values of catch were expressed in weight converting numbers using matrices of average weights varying with species, fishing gear, year and large geographical areas, i.e. equatorial or tropical (A. Fonteneau, pers.com). Average weights were computed from the Japanese and Taiwanese size-frequency data as well as from the Japanese total catches and catch-and-effort data. Some data might not be converted at all because no conversion factor exists for the stratum: those data were kept and the unit of catch was set to Number of fishes harvested."
    info <- "- Data provided in number of fishes harvested for the Southern Bluefin tuna (SBF) were not converted into weight of fishes, because no factors of conversion are available for this species. This might represent a great amount of the data for SBF."
  } else if (unit_conversion_csv_conversion_factor_url=="https://drive.google.com/open?id=15HpUd9muFwjjpjBnZE1a8eDzvISMgz1j"){
    lineage <- paste0("The units used to express efforts may vary between tRFMOs datasets. Values were harmonized at best using conversion factors. These conversion factors depend on the tRFMO and the gears. The factors of conversion are available here: ",unit_conversion_csv_conversion_factor_url,". Some data might not be converted at all because no conversion factor exists for the stratum: these data were kept in the unit they are originally expressed.")
    description <- "- Values of efforts were harmonized using conversion factors. Efforts were converted into number of hooks for longliners and number of fishing days for the other gears. Some data might not be converted at all because no conversion factor exists for the stratum: those data were kept in the unit they are originally expressed."
    info <- NULL
  } else {
    lineage <- paste0("The units used to express the measure may vary between tRFMOs datasets. The measures were harmonized through unit conversion factors located here: ",unit_conversion_csv_conversion_factor_url,". Information regarding the conversions of units for this dataset: ratio_converted_number % of the the catches that were originally expressed in number have been converted into weight through the conversion factors.")
    description <- "- Units for the measures were harmonized."
    info <- NULL
  }
  }
  #@juldebar modify FIRMS-Secretariat@fao.org 
  #@juldebar begin commented
  #@log_info("Fill contact metadata elements")
  #@firms_contact <- config$getContacts()[sapply(config$getContacts(), function(x){x$id == "FIRMS-Secretariat@fao.org"})][[1]]
  #@firms_contact$setRole("processor")
  #@conversion_step <- geoflow_process$new()
  #@conversion_step$setRationale(lineage)
  #@conversion_step$setProcessor(firms_contact)
  #@entity$provenance$processes <- c(entity$provenance$processes, conversion_step)
  #@juldebar end commented
  
  if (!is.null(entity)) {   # metadata of the geoflow entity, when there is one
  entity$descriptions[["abstract"]] <- paste0(entity$descriptions[["abstract"]], "\n", description)
  if(!is.null(info)){	
    if(!is.null(entity$descriptions[["info"]])){
      entity$descriptions[["info"]] <- paste0(entity$descriptions[["info"]], "\n", info)
    }else{
      entity$setDescription("info", info)
    }
    
  }
  }
  
  log_info("Converting units of georef_dataset OK")
  return(georef_dataset)
}
