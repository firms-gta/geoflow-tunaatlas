filter_species_from_asfis_list <- function(config, georef_dataset, level = "0") {
  
  url_asfis_list = paste0("https://raw.githubusercontent.com/fdiwg/fdi-codelists/main/global/firms/gta/cl_species_level", level, ".csv")
  config$logger.info(paste0("Filtering on species for level", level))
  species_to_be_kept <- readr::read_csv(url_asfis_list)
  
  georef_dataset <- georef_dataset %>%
    dplyr::inner_join(species_to_be_kept, by = c("species" = "code"))
  
  function_recap_each_step(
    paste0("Filtering species level ", level),
    georef_dataset,
    paste0(
      "Filtering species on the base of the file ",
      url_asfis_list,
      " to keep only the species. This file contains " ,
      as.character(length(nrow(
        species_to_be_kept
      ))),
      " species."
    ))
  
  return(georef_dataset)
}
