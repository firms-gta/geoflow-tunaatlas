#' Filter data in overlapping zones between RFMOs
#'
#' Identifies and filters records in overlapping geographic zones between
#' two Regional Fisheries Management Organizations (RFMOs), prioritizing one RFMO,
#' removing duplicates, and excluding ambiguous strata values.
#'
#' @param dataset A data.frame of catch or effort data, including a
#'   `geographic_identifier` field at 1° or 5° resolution.
#' @param con A database connection object (currently unused).
#' @param rfmo_to_keep String specifying the `source_authority` to prioritize.
#' @param rfmo_not_to_keep String specifying the `source_authority` to deprioritize.
#' @param strata Character vector of column names for overlap stratification.
#'   Default: c("geographic_identifier", "species", "year", "fishing_fleet").
#' @param opts List of options; must include `fact = "catch"` or `"effort"`
#'   to select the relevant columns.
#' @param removing_unk Logical; if TRUE (default), removes records with ambiguous
#'   strata values ("UNK", "NEI", "99.9", "MZZ") when present in both RFMOs.
#' @param use_five_degree Logical; if TRUE (default), aggregates 1° cells into
#'   5° cells for overlap comparison, then restores 1° resolution in the output,
#'   keeping original 1° codes in a new column `orig_geographic_identifier`.
#'
#' @return A filtered data.frame where:
#'   - Records outside overlap (other RFMOs) are unchanged.
#'   - In overlapping zones, records from `rfmo_to_keep` take priority.
#'   - Ambiguous strata values removed if `removing_unk = TRUE`.
#'
#' @examples
#' result <- function_overlapped(
#'   dataset = my_data,
#'   con = NULL,
#'   rfmo_to_keep = "WCPFC",
#'   rfmo_not_to_keep = "IATTC",
#'   strata = c("geographic_identifier", "species", "year", "fishing_fleet"),
#'   opts = list(fact = "catch"),
#'   removing_unk = TRUE,
#'   use_five_degree = TRUE
#' )
function_overlapped <- function(dataset,
                           con,
                           rfmo_to_keep,
                           rfmo_not_to_keep,
                           strata = c("geographic_identifier", "species", "year", "fishing_fleet"),
                           opts = list(fact="catch"),
                           removing_unk = TRUE,
                           use_five_degree = TRUE) {
  
  # Determine columns based on fact type
  variable <- opts$fact
  columns_to_keep <- switch(
    variable,
    "catch" = c(
      "source_authority", "species", "gear_type", "fishing_fleet",
      "fishing_mode", "time_start", "time_end", "geographic_identifier",
      "measurement", "measurement_type", "measurement_unit",
      "measurement_value", "measurement_processing_level"
    ),
    "effort" = c(
      "source_authority", "gear_type", "fishing_fleet",
      "fishing_mode", "time_start", "time_end", "geographic_identifier",
      "measurement_unit", "measurement_value",
      "measurement_processing_level", "measurement_type"
    ),
    stop("opts$fact must be 'catch' or 'effort'")
  )
  
  # Handle year stratification
  if ("year" %in% strata) {
    dataset <- dataset %>%
      dplyr::mutate(year = as.character(lubridate::year(time_start)))
    columns_to_keep_new <- unique(c(
      setdiff(columns_to_keep, c("time_start", "time_end")),
      "year"
    ))
  }
  
  # Filter out other RFMOs
  rfmo_restant <- dataset %>%
    dplyr::filter(!source_authority %in% c(rfmo_to_keep, rfmo_not_to_keep))
  
  # Determine effective strata
  strata <- intersect(strata, columns_to_keep_new)
  
  # Split datasets
  rfmo_keep <- dataset %>% dplyr::filter(source_authority == rfmo_to_keep)
  rfmo_drop <- dataset %>% dplyr::filter(source_authority == rfmo_not_to_keep)
  
  # Optional 5-degree aggregation for overlap comparison
  if (use_five_degree) {
    # Preserve original 1° codes
    rfmo_drop <- rfmo_drop %>% dplyr::mutate(orig_geographic_identifier = geographic_identifier)
    rfmo_keep <- rfmo_keep %>% dplyr::mutate(orig_geographic_identifier = geographic_identifier)
    
    # Separate 1° and 5° cells for drop
    one_deg <- rfmo_drop %>% dplyr::filter(grepl("^5", geographic_identifier))
    five_deg <- rfmo_drop %>% dplyr::filter(grepl("^6", geographic_identifier))
    
    if(nrow(one_deg) != 0){
    one_deg <- one_deg %>% dplyr::rowwise() %>%
      dplyr::mutate(geographic_identifier = transform_cwp_code_from_1deg_to_5deg(geographic_identifier))
    }
    rfmo_drop <- dplyr::bind_rows(one_deg, five_deg)
    # Convert keep dataset identifiers for comparison
    
    # Separate 1° and 5° cells for drop
    one_deg <- rfmo_keep %>% dplyr::filter(grepl("^5", geographic_identifier))
    five_deg <- rfmo_keep %>% dplyr::filter(grepl("^6", geographic_identifier))
    
    if(nrow(one_deg) != 0){
      one_deg <- one_deg %>% dplyr::rowwise() %>%
        dplyr::mutate(geographic_identifier = transform_cwp_code_from_1deg_to_5deg(geographic_identifier))
    }
    rfmo_keep <- dplyr::bind_rows(one_deg, five_deg)
  }
  
  # Anti-join to find non-overlapping records
  rfmo_not_equiv <- dplyr::anti_join(rfmo_drop, rfmo_keep, by = strata)
  overlapping_kept <- dplyr::bind_rows(rfmo_not_equiv, rfmo_keep)
  
  # Remove ambiguous codes if requested
  if (removing_unk) {
    overlapping_kept <- overlapping_kept %>%
      dplyr::group_by(across(c(strata, "source_authority"))) %>%
      dplyr::mutate(overlap = n_distinct(source_authority)) %>%
      dplyr::ungroup() %>%
      dplyr::filter(!(overlap == 2 & (species == "MZZ" | gear_type == "99.9" | fishing_fleet == "NEI" ))) %>% 
      dplyr::select(-overlap)
  }
  
  # Restore original 1° codes if aggregated
  if (use_five_degree) {
    overlapping_kept <- overlapping_kept %>%
      dplyr::mutate(geographic_identifier = orig_geographic_identifier) %>%
      dplyr::select(-orig_geographic_identifier)
  }
  
  # Combine with other RFMOs
  result <- dplyr::bind_rows(rfmo_restant, overlapping_kept) %>%
    dplyr::select(any_of(columns_to_keep)) %>%
    dplyr::ungroup()
  
  return(result)
}
