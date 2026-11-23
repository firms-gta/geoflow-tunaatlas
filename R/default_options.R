#' Default datasets and options of create_global_tuna_atlas()
#'
#' The Global Tuna Atlas datasets as defined by the entities of the workflow
#' (config/entities/entities_<nominal_catch|catch_level0|effort|catch_level1|catch_level2>.csv):
#' for each one, its identifier (the name of its output file, <id>.csv), the
#' files it reads (outputs of the raw datasets, of the previous levels, and
#' auxiliary files) and its options. The options have the types geoflow gives
#' them: "true"/"false" become logical, the other values stay character
#' (e.g. dataset_level = "0").
#'
#' The georeferenced datasets (level 0, effort, level 2) read their sources
#' from the second one, as the geoflow action does: the first source is not
#' read (for the level 0, catch_5deg_1m_driftnet_wcpfc_level0.csv, so the WCPFC
#' driftnet catch is not in the level 0; to correct separately).
#'
#' @return A named list: nominal, level0, effort, level1, level2.
#' @export
gta_default_options <- function() {
  list(
    nominal = list(
      id = "global_nominal_catch_firms_level0_2026",
      sources = c(
        "nominal_catch_ccsbt_level0__bygear.csv",
        "nominal_catch_wcpfc_level0.csv",
        "nominal_catch_iattc_level0.csv",
        "nominal_catch_iccat_level0.csv",
        "nominal_catch_iotc_level0.csv",
        "global_nominal_catch_codelists.csv"),
      options = list(
        fact = "catch",
        dataset_level = "0",
        from_rawdata = TRUE,
        mapping_map_code_lists = TRUE,
        raising_georef_to_nominal = FALSE,
        mapping_keep_src_code = FALSE,
        geom_table = "area.erased_area_labels",
        recap_each_step = TRUE
      )
    ),
    level0 = list(
      id = "global_catch_ird_level0_1950_2024",
      sources = c(
        "catch_5deg_1m_driftnet_wcpfc_level0.csv",
        "catch_5deg_1m_ps_wcpfc_level0.csv",
        "catch_5deg_1m_ll_wcpfc_level0.csv",
        "catch_5deg_1m_bb_wcpfc_level0.csv",
        "catch_5deg_1m_ll_ccsbt_level0.csv",
        "catch_1deg_1m_ccsbt_level0__surface.csv",
        "catch_5deg_1m_ll_iattc_level0__tuna_billfish.csv",
        "catch_5deg_1m_ll_iattc_level0__shark.csv",
        "catch_1deg_1m_ps_iattc_level0__tuna_byflag.csv",
        "catch_1deg_1m_ps_iattc_level0__shark_byflag.csv",
        "catch_1deg_1m_ps_iattc_level0__billfish_byflag.csv",
        "catch_1deg_1m_bb_iattc_level0__tuna_byflag.csv",
        "effort_1deg_1m_ps_iattc_level0__shark_byflag.csv",
        "effort_1deg_1m_ps_iattc_level0__billfish_byflag.csv",
        "effort_1deg_1m_ps_iattc_level0__tuna_byflag.csv",
        "effort_5deg_1m_ll_iattc_level0__tuna_billfish.csv",
        "effort_5deg_1m_ll_iattc_level0__shark.csv",
        "catch_iccat_level0.csv",
        "catch_iotc_level0.csv",
        "global_catch_ird_codelists.csv"),
      options = list(
        fact = "catch",
        dataset_level = "0",
        from_rawdata = TRUE,
        mapping_map_code_lists = TRUE,
        unit_conversion_convert = FALSE,
        raising_georef_to_nominal = FALSE,
        mapping_keep_src_code = FALSE,
        iattc_ps_catch_billfish_shark_raise_to_effort = FALSE,
        aggregate_on_5deg_data_with_resolution_inferior_to_5deg = FALSE,
        geom_table = "area.grid_area_labels",
        include_IOTC = TRUE,
        recap_each_step = TRUE
      )
    ),
    effort = list(
      id = "global_effort_2026",
      sources = c(
        "effort_1deg_1m_ps_iattc_level0__shark_byflag.csv",
        "effort_1deg_1m_ps_iattc_level0__billfish_byflag.csv",
        "effort_1deg_1m_ps_iattc_level0__tuna_byflag.csv",
        "effort_1deg_1m_ps_iattc_level0__tuna_byschool.csv",
        "effort_1deg_1m_bb_iattc_level0__tuna_byflag.csv",
        "effort_iotc_level0.csv",
        "effort_5deg_1m_bb_wcpfc_level0.csv",
        "effort_5deg_1m_driftnet_wcpfc_level0.csv",
        "effort_5deg_1m_ll_wcpfc_level0.csv",
        "effort_5deg_1m_ps_wcpfc_level0.csv",
        "effort_5deg_1m_ll_iattc_level0__shark.csv",
        "effort_5deg_1m_ll_iattc_level0__tuna_billfish.csv",
        "effort_1deg_1m_ccsbt_level0__surface.csv",
        "effort_5deg_1m_ll_ccsbt_level0.csv",
        "effort_iccat_level0__byschool.csv",
        "effort_iccat_level0__noschool.csv",
        "global_effort_codelists.csv"),
      options = list(
        recap_each_step = TRUE,
        include_IATTC = TRUE,
        include_ICCAT = TRUE,
        include_IOTC = TRUE,
        include_WCPFC = TRUE,
        include_CCSBT = TRUE,
        fact = "effort",
        dataset_level = "0",
        from_level0 = TRUE,
        iccat_ps_include_type_of_school = TRUE,
        iattc_ps_raise_flags_to_schooltype = TRUE,
        iattc_ps_catch_billfish_shark_raise_to_effort = TRUE,
        mapping_map_code_lists = TRUE,
        mapping_keep_src_code = FALSE,
        aggregate_on_5deg_data_with_resolution_inferior_to_5deg = FALSE,
        overlapping_zone_iattc_wcpfc_data_to_keep = "IATTC",
        overlapping_zone_iotc_wcpfc_data_to_keep = "IOTC",
        spatial_curation_data_mislocated = "remove",
        geom_table = "area.grid_area_labels",
        iattc_ps_effort_to_extract = "tuna"
      )
    ),
    level1 = list(
      id = "global_catch_ird_level1_1950_2024",
      sources = c(
        "global_catch_ird_level0_1950_2024.csv",
        "IOTC_conv_fact_mapped.csv",
        "fact_conv_ird_complete_01_May_2026.csv",
        "global_catch_ird_codelists.csv"),
      options = list(
        fact = "catch",
        keylevel0 = "global_catch_ird_level0_1950_2024.csv",
        dataset_level = "1",
        unit_conversion_convert = TRUE,
        geom_table = "area.grid_area_labels",
        recap_each_step = TRUE
      )
    ),
    level2 = list(
      id = "global_catch_ird_level2_1950_2024",
      sources = c(
        "global_nominal_catch_firms_level0_2026.csv",
        "catch_5deg_1m_driftnet_wcpfc_level0.csv",
        "catch_5deg_1m_ps_wcpfc_level0.csv",
        "catch_5deg_1m_ll_wcpfc_level0.csv",
        "catch_5deg_1m_bb_wcpfc_level0.csv",
        "catch_5deg_1m_ll_ccsbt_level0.csv",
        "catch_1deg_1m_ccsbt_level0__surface.csv",
        "catch_5deg_1m_ll_iattc_level0__tuna_billfish.csv",
        "catch_5deg_1m_ll_iattc_level0__shark.csv",
        "catch_1deg_1m_ps_iattc_level0__tuna_byflag.csv",
        "catch_1deg_1m_ps_iattc_level0__shark_byflag.csv",
        "catch_1deg_1m_ps_iattc_level0__billfish_byflag.csv",
        "catch_1deg_1m_bb_iattc_level0__tuna_byflag.csv",
        "geographic_identifier_to_nominal.csv",
        "effort_1deg_1m_ps_iattc_level0__shark_byflag.csv",
        "effort_1deg_1m_ps_iattc_level0__billfish_byflag.csv",
        "effort_1deg_1m_ps_iattc_level0__tuna_byflag.csv",
        "effort_5deg_1m_ll_iattc_level0__tuna_billfish.csv",
        "effort_5deg_1m_ll_iattc_level0__shark.csv",
        "catch_iotc_level0.csv",
        "catch_iccat_level0.csv",
        "global_catch_ird_codelists.csv"),
      options = list(
        fact = "catch",
        dataset_level = "2",
        from_rawdata = TRUE,
        keylevel0 = "global_georef_level0_no_duplicated_units.csv",
        keynominal = "global_nominal_catch_firms_level0_2026.csv",
        mapping_map_code_lists = TRUE,
        unit_conversion_convert = FALSE,
        raising_georef_to_nominal = TRUE,
        mapping_keep_src_code = FALSE,
        strong_weak_upgrade = "strong",
        iattc_ps_catch_billfish_shark_raise_to_effort = TRUE,
        aggregate_on_5deg_data_with_resolution_inferior_to_5deg = FALSE,
        geom_table = "area.grid_area_labels",
        decrease_when_rf_inferior_to_one = FALSE,
        passes = "2",
        recap_each_step = TRUE,
        upgrading_nominal_on_georef = FALSE,
        do_not_raise_any_unk = FALSE
      )
    )
  )
}
