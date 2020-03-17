get_rfmos_datasets_level0 <- function (con, rfmo, variable, year_tunaatlas, iattc_ps_raise_flags_to_schooltype = TRUE, 
    iattc_ps_dimension_to_use_if_no_raising_flags_to_schooltype = NULL, 
    iattc_ps_catch_billfish_shark_raise_to_effort = FALSE, iattc_ps_effort_to_extract = "tuna", 
    iccat_ps_include_type_of_school = TRUE) {

    if (variable == "catch") {
        columns_to_keep <- c("source_authority", "species", 
            "gear", "flag", "schooltype", "time_start", 
            "time_end", "geographic_identifier", 
            "catchtype", "unit", "value")
    }
    else if (variable == "effort") {
        columns_to_keep <- c("source_authority", "gear", 
            "flag", "schooltype", "time_start", 
            "time_end", "geographic_identifier", 
            "unit", "value")
    }
    if (rfmo == "IOTC") {
        datasets_permanent_identifiers = paste0("'indian_ocean_", 
            variable, "_ll_tunaatlasiotc_level0','indian_ocean_", 
            variable, "_tunaatlasiotc_level0__coastal','indian_ocean_", 
            variable, "_tunaatlasiotc_level0__surface'")
    }
    else if (rfmo == "WCPFC") {
        datasets_permanent_identifiers = paste0("'west_pacific_ocean_", 
            variable, "_5deg_1m_ll_tunaatlaswcpfc_level0__1950to1970','west_pacific_ocean_", 
            variable, "_5deg_1m_ll_tunaatlaswcpfc_level0__1990to2000','west_pacific_ocean_", 
            variable, "_5deg_1m_tunaatlaswcpfc_level0__driftnet','west_pacific_ocean_", 
            variable, "_5deg_1m_ll_tunaatlaswcpfc_level0__2000','west_pacific_ocean_", 
            variable, "_5deg_1m_bb_tunaatlaswcpfc_level0','west_pacific_ocean_", 
            variable, "_5deg_1m_ps_tunaatlaswcpfc_level0','west_pacific_ocean_", 
            variable, "_5deg_1m_ll_tunaatlaswcpfc_level0__1970to1980','west_pacific_ocean_", 
            variable, "_5deg_1m_ll_tunaatlaswcpfc_level0__1980to1990'")
    }
    else if (rfmo == "CCSBT") {
        datasets_permanent_identifiers = paste0("'southern_hemisphere_oceans_", 
            variable, "_1deg_1m_tunaatlasccsbt_level0__surface','southern_hemisphere_oceans_", 
            variable, "_5deg_1m_ll_tunaatlasccsbt_level0'")
    }
    else if (rfmo == "IATTC") {
        datasets_permanent_identifiers = paste0("'east_pacific_ocean_", 
            variable, "_1deg_1m_bb_tunaatlasiattc_level0__tuna_byflag','east_pacific_ocean_", 
            variable, "_5deg_1m_ll_tunaatlasiattc_level0__tuna_billfish','east_pacific_ocean_", 
            variable, "_5deg_1m_ll_tunaatlasiattc_level0__shark'")
    }
    else if (rfmo == "ICCAT") {
        datasets_permanent_identifiers = paste0("'atlantic_ocean_", 
            variable, "_tunaatlasiccat_level0__noschool'")
    }
    metadata_datasets <- dbGetQuery(con, paste0("SELECT * from metadata.metadata where persistent_identifier IN (", 
        datasets_permanent_identifiers, ") and identifier LIKE '%__", 
        year_tunaatlas, "%'"))
    df_level0 <- extract_and_merge_multiple_datasets(con, metadata_datasets, 
        columns_to_keep)
    if (rfmo == "ICCAT" && iccat_ps_include_type_of_school == 
        TRUE) {
        datasets_permanent_identifiers = paste0("'atlantic_ocean_", 
            variable, "_1deg_1m_ps_tunaatlasiccat_level0__byschool'")
        metadata_datasets_WithSchooltypeInfo <- dbGetQuery(con, 
            paste0("SELECT * from metadata.metadata where persistent_identifier IN (", 
                datasets_permanent_identifiers, ") and identifier LIKE '%__", 
                year_tunaatlas, "%'"))
        iccat_ce_WithSchooltypeInfo <- extract_and_merge_multiple_datasets(con, 
            metadata_datasets_WithSchooltypeInfo, columns_to_keep)
        flag_mapping_flag_iccat_from_ncandcas_to_flag_iccat <- rtunaatlas::extract_dataset(con, 
            list_metadata_datasets(con, identifier = "codelist_mapping_flag_iccat_from_ncandcas_flag_iccat"))
        iccat_ce_WithSchooltypeInfo <- map_codelist(iccat_ce_WithSchooltypeInfo, 
            flag_mapping_flag_iccat_from_ncandcas_to_flag_iccat, 
            "flag")[[1]]
        strata_in_withoutschooltype_and_not_in_withshooltype <- anti_join(df_level0, 
            iccat_ce_WithSchooltypeInfo, by = setdiff(columns_to_keep, 
                c("value", "schooltype")))
        df_level0 <- rbind(strata_in_withoutschooltype_and_not_in_withshooltype, 
            iccat_ce_WithSchooltypeInfo)
    }
    if (rfmo == "IATTC") {
        df_level0 <- unique(df_level0)
        metadata_dataset_PSSetType_tuna_catch <- dbGetQuery(con, 
            paste0("SELECT * from metadata.metadata where persistent_identifier='east_pacific_ocean_catch_1deg_1m_ps_tunaatlasiattc_level0__tuna_byschool' and identifier LIKE '%__", 
                year_tunaatlas, "%'"))
        metadata_dataset_PSFlag_tuna_catch <- dbGetQuery(con, 
            paste0("SELECT * from metadata.metadata where persistent_identifier='east_pacific_ocean_catch_1deg_1m_ps_tunaatlasiattc_level0__tuna_byflag' and identifier LIKE '%__", 
                year_tunaatlas, "%'"))
        metadata_dataset_PSSetType_billfish_catch <- dbGetQuery(con, 
            paste0("SELECT * from metadata.metadata where persistent_identifier='east_pacific_ocean_catch_1deg_1m_ps_tunaatlasiattc_level0__billfish_byschool' and identifier LIKE '%__", 
                year_tunaatlas, "%'"))
        metadata_dataset_PSFlag_billfish_catch <- dbGetQuery(con, 
            paste0("SELECT * from metadata.metadata where persistent_identifier='east_pacific_ocean_catch_1deg_1m_ps_tunaatlasiattc_level0__billfish_byflag' and identifier LIKE '%__", 
                year_tunaatlas, "%'"))
        metadata_dataset_PSSetType_shark_catch <- dbGetQuery(con, 
            paste0("SELECT * from metadata.metadata where persistent_identifier='east_pacific_ocean_catch_1deg_1m_ps_tunaatlasiattc_level0__shark_byschool' and identifier LIKE '%__", 
                year_tunaatlas, "%'"))
        metadata_dataset_PSFlag_shark_catch <- dbGetQuery(con, 
            paste0("SELECT * from metadata.metadata where persistent_identifier='east_pacific_ocean_catch_1deg_1m_ps_tunaatlasiattc_level0__shark_byflag' and identifier LIKE '%__", 
                year_tunaatlas, "%'"))
        metadata_dataset_PSSetType_tuna_effort <- dbGetQuery(con, 
            paste0("SELECT * from metadata.metadata where persistent_identifier='east_pacific_ocean_effort_1deg_1m_ps_tunaatlasiattc_level0__tuna_byschool' and identifier LIKE '%__", 
                year_tunaatlas, "%'"))
        metadata_dataset_PSFlag_tuna_effort <- dbGetQuery(con, 
            paste0("SELECT * from metadata.metadata where persistent_identifier='east_pacific_ocean_effort_1deg_1m_ps_tunaatlasiattc_level0__tuna_byflag' and identifier LIKE '%__", 
                year_tunaatlas, "%'"))
        metadata_dataset_PSSetType_billfish_effort <- dbGetQuery(con, 
            paste0("SELECT * from metadata.metadata where persistent_identifier='east_pacific_ocean_effort_1deg_1m_ps_tunaatlasiattc_level0__billfish_byschool' and identifier LIKE '%__", 
                year_tunaatlas, "%'"))
        metadata_dataset_PSFlag_billfish_effort <- dbGetQuery(con, 
            paste0("SELECT * from metadata.metadata where persistent_identifier='east_pacific_ocean_effort_1deg_1m_ps_tunaatlasiattc_level0__billfish_byflag' and identifier LIKE '%__", 
                year_tunaatlas, "%'"))
        metadata_dataset_PSSetType_shark_effort <- dbGetQuery(con, 
            paste0("SELECT * from metadata.metadata where persistent_identifier='east_pacific_ocean_effort_1deg_1m_ps_tunaatlasiattc_level0__shark_byschool' and identifier LIKE '%__", 
                year_tunaatlas, "%'"))
        metadata_dataset_PSFlag_shark_effort <- dbGetQuery(con, 
            paste0("SELECT * from metadata.metadata where persistent_identifier='east_pacific_ocean_effort_1deg_1m_ps_tunaatlasiattc_level0__shark_byflag' and identifier LIKE '%__", 
                year_tunaatlas, "%'"))
        columns_to_keep_effort = c("source_authority", 
            "gear", "flag", "schooltype", "time_start", 
            "time_end", "geographic_identifier", 
            "unit", "value")
        if (variable == "effort") {
            if (iattc_ps_effort_to_extract == "tuna") {
                metadata_dataset_effort_flag <- metadata_dataset_PSFlag_tuna_effort
                metadata_dataset_effort_settype <- metadata_dataset_PSSetType_tuna_effort
            }
            else if (iattc_ps_effort_to_extract == "billfish") {
                metadata_dataset_effort_flag <- metadata_dataset_PSFlag_billfish_effort
                metadata_dataset_effort_settype <- metadata_dataset_PSSetType_billfish_effort
            }
            else if (iattc_ps_effort_to_extract == "shark") {
                metadata_dataset_effort_flag <- metadata_dataset_PSFlag_shark_effort
                metadata_dataset_effort_settype <- metadata_dataset_PSSetType_shark_effort
            }
            df_iattc_effort_PSSetType <- extract_and_merge_multiple_datasets(con, 
                metadata_dataset_effort_settype, columns_to_keep = columns_to_keep_effort)
            df_iattc_effort_PSFlag <- extract_and_merge_multiple_datasets(con, 
                metadata_dataset_effort_flag, columns_to_keep = columns_to_keep_effort)
            if (iattc_ps_raise_flags_to_schooltype == TRUE) {
                df <- raise_datasets_by_dimension(df1 = df_iattc_effort_PSFlag, 
                  df2 = df_iattc_effort_PSSetType, dimension_missing_df1 = "schooltype", 
                  dimension_missing_df2 = "flag")$df
            }
            else {
                if (iattc_ps_dimension_to_use_if_no_raising_flags_to_schooltype == 
                  "flag") {
                  df <- df_iattc_effort_PSFlag
                }
                else if (iattc_ps_dimension_to_use_if_no_raising_flags_to_schooltype == 
                  "schooltype") {
                  df <- df_iattc_effort_PSSetType
                }
            }
            df_level0 <- rbind(df_level0, df)
        }
        else {
            function_raise_catch_to_effort <- function(metadata_dataset_tuna_effort, 
                metadata_dataset_billfish_or_shark_catch, metadata_dataset_billfish_or_shark_effort, 
                raising_dimensions) {
                billfish_or_shark_catch <- extract_and_merge_multiple_datasets(con, 
                  metadata_dataset_billfish_or_shark_catch, columns_to_keep)
                billfish_or_shark_effort <- extract_and_merge_multiple_datasets(con, 
                  metadata_dataset_billfish_or_shark_effort, 
                  columns_to_keep = columns_to_keep_effort)
                tuna_effort <- extract_and_merge_multiple_datasets(con, 
                  metadata_dataset_tuna_effort, columns_to_keep = columns_to_keep_effort)
                df_rf <- raise_get_rf(df_input = billfish_or_shark_effort, 
                  df_input_total = tuna_effort, x_raising_dimensions = c(raising_dimensions, 
                    "unit"))
                df_rf$unit <- NULL
                catch_raised <- raise_incomplete_dataset_to_total_dataset(df_input_incomplete = billfish_or_shark_catch, 
                  df_input_total = billfish_or_shark_catch, df_rf = df_rf, 
                  x_raising_dimensions = raising_dimensions, 
                  threshold_rf = NULL)
                return(catch_raised$df)
            }
            df_catch_tuna_flag <- extract_and_merge_multiple_datasets(con, 
                metadata_dataset_PSFlag_tuna_catch, columns_to_keep = columns_to_keep)
            df_catch_tuna_settype <- extract_and_merge_multiple_datasets(con, 
                metadata_dataset_PSSetType_tuna_catch, columns_to_keep = columns_to_keep)
            if (iattc_ps_catch_billfish_shark_raise_to_effort == 
                TRUE) {
                df_catch_billfish_flag <- function_raise_catch_to_effort(metadata_dataset_PSFlag_tuna_effort, 
                  metadata_dataset_PSFlag_billfish_catch, metadata_dataset_PSFlag_billfish_effort, 
                  c("gear", "flag", "time_start", 
                    "time_end", "geographic_identifier"))
                df_catch_billfish_settype <- function_raise_catch_to_effort(metadata_dataset_PSSetType_tuna_effort, 
                  metadata_dataset_PSSetType_billfish_catch, 
                  metadata_dataset_PSSetType_billfish_effort, 
                  c("gear", "schooltype", "time_start", 
                    "time_end", "geographic_identifier"))
                df_catch_shark_flag <- function_raise_catch_to_effort(metadata_dataset_PSFlag_tuna_effort, 
                  metadata_dataset_PSFlag_shark_catch, metadata_dataset_PSFlag_shark_effort, 
                  c("gear", "flag", "time_start", 
                    "time_end", "geographic_identifier"))
                df_catch_shark_settype <- function_raise_catch_to_effort(metadata_dataset_PSSetType_tuna_effort, 
                  metadata_dataset_PSSetType_shark_catch, metadata_dataset_PSSetType_shark_effort, 
                  c("gear", "schooltype", "time_start", 
                    "time_end", "geographic_identifier"))
            }
            else {
                df_catch_billfish_flag <- extract_and_merge_multiple_datasets(con, 
                  metadata_dataset_PSFlag_billfish_catch, columns_to_keep = columns_to_keep)
                df_catch_billfish_settype <- extract_and_merge_multiple_datasets(con, 
                  metadata_dataset_PSSetType_billfish_catch, 
                  columns_to_keep = columns_to_keep)
                df_catch_shark_flag <- extract_and_merge_multiple_datasets(con, 
                  metadata_dataset_PSFlag_shark_catch, columns_to_keep = columns_to_keep)
                df_catch_shark_settype <- extract_and_merge_multiple_datasets(con, 
                  metadata_dataset_PSSetType_shark_catch, columns_to_keep = columns_to_keep)
            }
            if (iattc_ps_raise_flags_to_schooltype == TRUE) {
                df_catch_billfish <- raise_datasets_by_dimension(df1 = df_catch_billfish_flag, 
                  df2 = df_catch_billfish_settype, dimension_missing_df1 = "schooltype", 
                  dimension_missing_df2 = "flag")$df
                df_catch_shark <- raise_datasets_by_dimension(df1 = df_catch_shark_flag, 
                  df2 = df_catch_shark_settype, dimension_missing_df1 = "schooltype", 
                  dimension_missing_df2 = "flag")$df
                df_catch_tuna <- raise_datasets_by_dimension(df1 = df_catch_tuna_flag, 
                  df2 = df_catch_tuna_settype, dimension_missing_df1 = "schooltype", 
                  dimension_missing_df2 = "flag")$df
            }
            else {
                if (iattc_ps_dimension_to_use_if_no_raising_flags_to_schooltype == 
                  "flag") {
                  df_catch_billfish <- df_catch_billfish_flag
                  df_catch_shark <- df_catch_shark_flag
                  df_catch_tuna <- df_catch_tuna_flag
                }
                else if (iattc_ps_dimension_to_use_if_no_raising_flags_to_schooltype == 
                  "schooltype") {
                  df_catch_billfish <- df_catch_billfish_settype
                  df_catch_shark <- df_catch_shark_settype
                  df_catch_tuna <- df_catch_tuna_settype
                }
            }
            df_level0 <- rbind(df_level0, df_catch_billfish, 
                df_catch_shark, df_catch_tuna)
        }
    }

    return(df_level0)
}