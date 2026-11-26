# Columns used with non-standard evaluation (dplyr, data.table), declared so
# that R CMD check does not take them for undefined variables
utils::globalVariables(c(
  ".", ".allowed__", ".georef_row_id", ".needs_replace__", ".new_value__",
  ".nominal_row_id", ".row_id__", ".unit_rank", "acceptable_candidate",
  "adjusted_lat", "adjusted_lon", "AreaName", "Calendar_Year", "Catch",
  "Catch_mt", "CatchType", "CatchUnit", "CatchUnits", "chosen", "code",
  "consumed_value", "conversion_factor", "CWP_CODE", "cwp_code", "CWP_GRID",
  "DAYS", "decrease", "diff_fishing_fleet",
  "diff_fishing_mode_wcpfc_issue_unk_solved", "diff_gear_type",
  "diff_geographic_identifier_nom", "different_dims", "dimension",
  "dimensions_to_map", "Eff1", "Eff1_num", "Eff1Type", "Eff2", "Eff2_num",
  "Eff2Type", "EFFORT", "Effort", "effort_source", "EFFORT_UNIT_CODE",
  "EffortUnits", "fishing_fleet", "fishing_fleet_georef",
  "fishing_fleet_label", "fishing_fleet_nominal", "fishing_fleet_solved",
  "FISHING_GROUND_CODE", "fishing_mode", "fishing_mode_georef",
  "fishing_mode_nominal", "fishing_mode_wcpfc_issue_unk_solved",
  "fishing_mode_wcpfc_issue_unk_solved_georef",
  "fishing_mode_wcpfc_issue_unk_solved_nominal",
  "fishing_mode_wcpfc_issue_unk_solved_original", "FishingFleet",
  "fishingfleet", "FishMode", "Flag", "Flag_Code", "FLEET", "FLEET_CODE",
  "Gear", "gear", "GEAR_CODE", "gear_type", "gear_type_georef",
  "gear_type_nominal", "gear_type_solved", "geographic_identifier",
  "geographic_identifier_nom", "geographic_identifier_nom_georef",
  "geographic_identifier_nom_nominal", "geographic_identifier_nom_solved",
  "geographic_identifier_project", "georef_has_any_data",
  "georef_has_fallback", "georef_val", "georef_value", "GeoStrataCode",
  "grid_code", "gridtype", "group_gears", "group_species_iattc_sharks_georef",
  "has_multiple_candidates", "Hooks", "increase",
  "input_geographic_identifier", "is_duplicate_strata", "LatC5", "LonC5",
  "mapping_applied", "matching_pass", "max_date", "max_weight", "measurement",
  "measurement_processing_level", "measurement_type", "measurement_unit",
  "measurement_value", "measurement_value.x", "measurement_value.y",
  "measurement_value_decreased_with_tons", "measurement_value_georef",
  "measurement_value_nominal", "measurement_valuefinal", "min_complete_year",
  "min_conv_factor", "min_date", "min_weight", "MM", "Month", "MONTH_END",
  "MONTH_START", "MonthStart", "MT", "n_candidates_same_rank", "n_cases",
  "n_changed", "n_different_dims", "nb_units", "new_measurement", "new_value",
  "NO", "nominal_value", "number", "numberunit", "Ocean", "ON_LAND_P",
  "on_land_p", "orig_geographic_identifier", "original_row_id", "overlap",
  "percentageoftotalstrata", "Period", "preference_score", "prop",
  "proposal_rank", "quadrant", "relaxed_cols", "rf", "School",
  "SCHOOL_TYPE_CODE", "SETS_AFAD", "SETS_DFAD", "SETS_LOG", "SETS_OTH",
  "SETS_UNA", "source_authority", "source_authority_georef", "Species",
  "species", "species_unit", "Square_size", "src_code", "sum_mapped_unmapped",
  "sum_val", "sum_value_df_input_incomplete", "sum_value_df_input_total",
  "time_end", "time_start", "total", "total_nom", "trg_code", "unit",
  "unit_class", "unit_target", "unknown_to_add", "val", "value",
  "value_raised", "variable", "Year", "YEAR", "year_georef", "YY", "zone0"
))

# Known broken, not run with the options of the workflow (disaggregation
# options set to "none"): spatial_curation_downgrade_resolution() uses these
# objects without defining them. To fix before enabling the disaggregation.
utils::globalVariables(c(
  "dataset_final_disaggregated_on_resolution_to_disaggregate", "description", "lineage"
))

# Run summaries and their comparison (R/run_summary.R)
utils::globalVariables(c(
  "dataset", "difference", "difference_pct", "n_rows", "value_sum_1",
  "value_sum_2"
))
