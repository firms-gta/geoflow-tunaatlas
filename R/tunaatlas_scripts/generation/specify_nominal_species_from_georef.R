specify_nominal_species_from_georef <- function(
    nominal_df,
    georef_df,
    parent_species = "SKH",
    child_species  = c("BLR","BSH","CCL","FAL","MAK","OCS","SMA","SPL","SPN","SPZ","THR"),
    strata_cols    = c("source_authority","year","geographic_identifier_nom","measurement_unit"),
    mode           = c("weak","strong"),
    logger         = message
) {
  mode <- match.arg(mode)
  
  # ensure year exists
  if (!"year" %in% names(nominal_df)) nominal_df <- nominal_df |> dplyr::mutate(year = lubridate::year(time_start))
  if (!"year" %in% names(georef_df))  georef_df  <- georef_df  |> dplyr::mutate(year = lubridate::year(time_start))
  
  # 1) georef proportions among child species, per stratum
  geo_props <- georef_df |>
    dplyr::filter(.data$species %in% child_species) |>
    dplyr::group_by(dplyr::across(dplyr::all_of(c(strata_cols, "species")))) |>
    dplyr::summarise(val = sum(measurement_value, na.rm = TRUE), .groups = "drop") |>
    dplyr::group_by(dplyr::across(dplyr::all_of(strata_cols))) |>
    dplyr::mutate(
      prop  = val / sum(val, na.rm = TRUE),
      total = sum(val, na.rm = TRUE)
    ) |>
    dplyr::ungroup()
  
  # 2) nominal rows at parent level only
  parent_rows <- nominal_df |>
    dplyr::filter(.data$species == parent_species)
  
  if (nrow(parent_rows) == 0) {
    logger(sprintf("No nominal rows with parent species '%s' -> nothing to specify.", parent_species))
    return(nominal_df)
  }
  
  # 3) join: keep only strata where georef has child breakdown
  to_specify <- parent_rows |>
    dplyr::inner_join(geo_props, by = strata_cols, suffix = c("", "_geo"))
  
  if (nrow(to_specify) == 0) {
    logger("No matching strata in georef for these parent rows -> nothing to specify.")
    return(nominal_df)
  }
  
  # 4) build specified rows
  if (mode == "strong") {
    specified <- to_specify |>
      dplyr::mutate(
        measurement_value = measurement_value * prop,
        species = .data$species_geo
      ) |>
      dplyr::select(names(nominal_df))
  } else {
    # weak: if nominal parent > georef total, copy georef detail and keep leftover as parent
    too_big <- to_specify |> dplyr::filter(measurement_value > total)
    ok      <- to_specify |> dplyr::filter(measurement_value <= total)
    
    ok_spec <- ok |>
      dplyr::mutate(
        measurement_value = measurement_value * prop,
        species = .data$species_geo
      ) |>
      dplyr::select(names(nominal_df))
    
    if (nrow(too_big) > 0) {
      copied <- too_big |>
        dplyr::mutate(
          total_nom = measurement_value,
          measurement_value = val,
          species = .data$species_geo,
          leftover = total_nom - total
        ) |>
        dplyr::select(dplyr::all_of(c(names(nominal_df), "leftover")))
      
      # one leftover parent row per stratum
      group_cols <- setdiff(names(nominal_df), "measurement_value")
      leftover_rows <- copied |>
        dplyr::group_by(dplyr::across(dplyr::all_of(group_cols))) |>
        dplyr::slice(1) |>
        dplyr::ungroup() |>
        dplyr::mutate(
          measurement_value = leftover,
          species = parent_species
        ) |>
        dplyr::select(names(nominal_df))
      
      copied_rows <- copied |> dplyr::select(names(nominal_df))
      
      specified <- dplyr::bind_rows(ok_spec, copied_rows, leftover_rows)
    } else {
      specified <- ok_spec
    }
  }
  
  # 5) remove parent rows for those strata and add specified
  replaced_keys <- to_specify |>
    dplyr::select(dplyr::all_of(setdiff(names(nominal_df), "measurement_value"))) |>
    dplyr::distinct()
  
  out <- nominal_df |>
    dplyr::anti_join(replaced_keys, by = names(replaced_keys)) |>
    dplyr::bind_rows(specified) |>
    dplyr::group_by(dplyr::across(-measurement_value)) |>
    dplyr::summarise(measurement_value = sum(measurement_value, na.rm = TRUE), .groups = "drop")
  
  logger(sprintf("Specified parent '%s' into %d child species (mode=%s).", parent_species, length(child_species), mode))
  out
}